namespace PureParse

open System.Threading.Channels
open System.Threading.Tasks

[<AutoOpen>]
module Events =

        type EventData<'TState> = {
            parserName: string
            message: string
            index: int
            line: int
            column: int
            timestamp: int64
            state: 'TState
            error: exn option
        }

        [<Struct>]
        type Event<'TState> =
            | EnterProduction of enterProduction: EventData<'TState>
            | ExitProductionSuccess of exitProductionSuccess: EventData<'TState>
            | ExitProductionFailure of exitProductionFailure: EventData<'TState>
            | ParseComplete of complete:EventData<'TState>
            | ParseFailure of parseFailure:EventData<'TState>

        type EventTree<'TState> =
            | SucceededProduction of ProductionData<'TState> 
            | FailedProduction of ProductionData<'TState> 
        and ProductionData<'TState> = { 
            enterData: EventData<'TState>; 
            exitData: EventData<'TState>;
            token: string option
            failures:List<EventData<'TState>> 
            children:EventTree<'TState> list }
        and ProductionDataBuilder<'TState> = { 
            mutable success: bool
            mutable enterData: Option<EventData<'TState>> 
            mutable exitData: Option<EventData<'TState>>
            mutable failures:ResizeArray<EventData<'TState>> 
            mutable children:ResizeArray<ProductionDataBuilder<'TState>> }

        let createEventTreeBuilder<'TState> (text:string) (eventChannel:Channel<Event<'TState>>) =
            task {
                let stack = System.Collections.Generic.Stack<ProductionDataBuilder<'TState>>()
                let mutable current: option<ProductionDataBuilder<'TState>> = None
                let mutable tree: option<EventTree<'TState>> = None
                let mutable complete = false
                while not complete do
                    let! r = eventChannel.Reader.ReadAsync ()
                    match r with
                    | EnterProduction eventData ->
                        let production = { 
                            success = false
                            enterData = Some eventData
                            exitData = None
                            failures = ResizeArray()
                            children = ResizeArray() }
                        match current with
                        | None -> ()
                        | Some c ->
                            c.children.Add production
                            stack.Push c
                        current <- Some production
                    | ExitProductionSuccess eventData ->
                        match current with
                        | None -> failwith "Invalid event sequence"
                        | Some c ->
                            c.success <- true
                            c.exitData <- Some eventData
                            if stack.Count > 0 then
                                current <- Some (stack.Pop ())
                    | ExitProductionFailure eventData ->
                        match current with
                        | None -> failwith "Invalid event sequence"
                        | Some c ->
                            c.success <- false
                            c.exitData <- Some eventData
                            if stack.Count > 0 then
                                current <- Some (stack.Pop ())
                    | ParseFailure eventData -> 
                        match current with
                        | None -> failwith "Invalid event sequence"
                        | Some c ->
                            c.failures.Add eventData
                    | ParseComplete eventData ->
                        if stack.Count <> 0 then
                            failwith "Invalid event sequence"

                        let rec build (d:ProductionDataBuilder<'TState>) =
                            match d with
                            | { success = true; enterData = Some enter; exitData = Some exit; children = _ } ->
                                    SucceededProduction { 
                                        enterData = enter 
                                        exitData = exit
                                        token = if d.children.Count > 0 then None else (Some <| text.Substring(enter.index, exit.index - enter.index))
                                        failures = d.failures |> Seq.toList
                                        children = d.children |> Seq.map build |> Seq.toList
                                    } 
                            | { success = false; enterData = Some enter; exitData = Some exit; children = _ } ->
                                    FailedProduction { 
                                        enterData = enter 
                                        exitData = exit
                                        token = if d.children.Count > 0 then None else (Some <| text.Substring(enter.index, exit.index - enter.index))
                                        failures = d.failures |> Seq.toList
                                        children = d.children |> Seq.map build |> Seq.toList
                                    } 
                            | _ -> failwith ""
                        tree <- Some (build current.Value)
                        complete <- true
                
                return tree.Value
            }


        let getTimeStamp () = System.DateTime.Now.Ticks        

        module EventTree = 

            let getDeepestFailure<'TState> (tree:EventTree<'TState>) : EventTree<'TState> option =
                let rec find (tree:EventTree<'TState>) (height:int) : EventTree<'TState> option * int = 
                    match tree with
                    | SucceededProduction _ -> None, height 
                    | FailedProduction d when d.children.IsEmpty -> Some tree, height
                    | FailedProduction d ->
                        d.children 
                        |> List.map (fun tree -> find tree (height + 1))
                        |> List.maxBy (fun (_, height) -> height)
                let result, _ = find tree 0
                result

            
            let createHtml<'TState> (eventTree:EventTree<'TState>) : string =
                let opening = $"""
                    <html>
                        <head>
                            <style type="text/css">
                                .production {{
                                    padding: 5px;
                                    margin: 2px;
                                    margin-left: 20px;
                                    /*border:2px dashed black;*/
                                }}
                                .production-name {{
                                    color:black;
                                    font-weight:bold;
                                    display:inline;
                                    margin-left:5px;
                                    font-size:20px;
                                }}
                                .expand {{
									cursor:pointer;
                                    font-size:20px;
                                }}
                                .success {{
                                    color:green;
                                }}
                                .failure {{
                                    color:red;
                                }}
                                .token {{
                                    color:black;
                                    font-size:25px;
                                    padding-left:10px;
                                }}
                                .failures {{
                                    margin-;
                                }}
                            </style>
							<script type="text/javascript">
								function expandOrCollapse(id) {{
									let element = document.getElementById(id);
									let expanded = element.getAttribute("expanded")
									let isExpanded = expanded == "true";
									let display = isExpanded ? "block" : "none"
									let text = isExpanded ? "&minus;" : "&plus;";
										
									element.setAttribute("expanded", isExpanded ? "false" : "true");
									document.querySelectorAll("#" + id + " > .collapse").forEach(a => a.style.display = display);
									document.querySelectorAll("#" + id + " > span.expand").forEach(a => a.innerHTML = text);
								}}
							</script>
                        </head>
                        <body>
                        
                """
                let rec run id (eventTree:EventTree<'TState>) =
                    let getBeginTag (data:ProductionData<'TState>) success = 
                        let resultClass = if success then "success" else "failure"
                        $"""
                            <div id="{id}" expanded="false" class="collapse {id} production {resultClass}">
                                <span onclick="expandOrCollapse('{id}')" class="expand {resultClass}">&minus;</span>
                                <span class="production-name {resultClass}">{data.enterData.parserName}</span>
                                <span class="">index={data.enterData.index}, column={data.enterData.column}, line={data.enterData.line}</span>
                                <span class="token">{System.Net.WebUtility.HtmlEncode(if data.token.IsSome && data.token.Value.Length > 0 then data.token.Value else "")}</span>
                        """
                    let beginTag, data, success =
                        match eventTree with
                        | SucceededProduction data -> getBeginTag data true, data, true
                        | FailedProduction data -> getBeginTag data false, data, false

                    let failures = 
                        if data.failures.IsEmpty then "" else
                            "<ul class=\"collapse failures failure\">" + (data.failures |> List.map(fun f -> "<li>" + f.error.Value.ToString() + "</li>") |> List.reduce (+)) + "</ul>"

                    let children = 
                        data.children 
                        |> List.mapi (fun index child -> run (id + "-c" + string index) child)
                    let body = System.String.Join ("", children)
                    beginTag + failures + body + "</div>"

                opening + (run "root" eventTree) + "</body></html>"