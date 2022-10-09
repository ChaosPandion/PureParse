namespace PureParse
open TextStream

type Result<'TState, 'TData> = 
    Success of stream:TextStream<'TState> * data:'TData 
    | Failure of stream:TextStream<'TState>

type RunResult<'TState, 'TData> = 
    | RunSuccess of state:'TState * data:'TData * tree:EventTree<'TState>
    | RunFailure of state:'TState * error:PureParseException<'TState> * tree:EventTree<'TState>
