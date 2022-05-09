namespace PureParse
open TextStream

type Result<'TState, 'TData> = 
    Success of stream:TextStream<'TState> * data:'TData 
    | Failure of stream:TextStream<'TState> * error:exn
