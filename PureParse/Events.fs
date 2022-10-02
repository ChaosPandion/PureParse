namespace PureParse

[<AutoOpen>]
module Events = begin

        type EventData<'TState> = {
            parserName: string
            message: string
            index: int
            line: int
            column: int
            timestamp: int64
            state: 'TState
        }

        [<Struct>]
        type Event<'TState> =
            | EnterProduction of enterProduction: EventData<'TState>
            | ExitProductionSuccess of exitProductionSuccess: EventData<'TState>
            | ExitProductionFailure of exitProductionFailure: EventData<'TState> * exitProductionFailureError:exn  
            | ParseSuccess of success: EventData<'TState>
            | ParseFailure of failure: EventData<'TState> * error:exn       

        type EventOptions =
            | CompleteTree
            | SuccessOnly
            | FailureOnly
            | NoEvents

        type EventSetup = {
                options: EventOptions option
                name: string option
                version: string option
            }

        type EventState<'TState> = {
            options: EventOptions
            name: string
            version: string
            timestamp: int64
            events: List<Event<'TState>>
        }

        let getTimeStamp () = System.DateTime.Now.Ticks

        let createInitialEventState<'TState> name version options : EventState<'TState> = 

            //if System.String.IsNullOrWhiteSpace name then
            //    invalidArg (nameof(name)) "A name is required."
            //if version = null then
            //    nullArg (nameof(version))

            { 
                options = options; 
                version = version;
                name = name; 
                timestamp = getTimeStamp ();
                events = [] 
            }

    end
