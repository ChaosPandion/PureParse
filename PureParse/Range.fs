namespace PureParse

[<Struct>]
type Num<'n when 'n : struct> =
    | Unbounded
    | Bounded of 'n

[<Struct>]
type Range<'n when 'n : struct> =

    /// Give exactly one.
    | One

    /// Give the exact amount specified
    | Exact of exact: Num<'n>

    /// Give anything in between the min and max values provided.
    | Between of min: Num<'n> * max: Num<'n>

    /// Give everything that remains.
    | Remaining