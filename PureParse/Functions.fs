namespace PureParse

open TextStream


/// Parser Monad Shorthand
type M<'TState, 'TData> = TextStream<'TState> -> Result<'TState, 'TData>

/// Parser Monad
type Parser<'TState, 'TData> = M<'TState, 'TData>
    
/// Monad Transform Shorthand
type F<'TState, 'TData1, 'TData2> = 'TData1 -> M<'TState, 'TData2>
    
/// Monad Transform
type Transform<'TState, 'TData1, 'TData2> = F<'TState, 'TData1, 'TData2>
    
/// Monad Delay Shorthand
type D<'TState, 'TData> = unit -> M<'TState, 'TData>
    
/// Monad Delay
type Delayed<'TState, 'TData> = unit -> M<'TState, 'TData>
    
/// The bind signature of the parser monad
type Bind<'TState, 'TData1, 'TData2> = 
    Parser<'TState, 'TData1> -> Transform<'TState, 'TData1, 'TData2> -> Parser<'TState, 'TData2>
    
/// The return signature of the parser monad
type Return<'TState, 'TData> = 'TData -> Parser<'TState, 'TData>