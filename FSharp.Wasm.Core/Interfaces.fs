/// Core BCL interfaces for WasmGC targets.
/// These mirror the subset of .NET interfaces that FSharp.Core depends on,
/// implemented as pure-F# types so the fsharp-wasm compiler can lower them
/// to WasmGC struct/func-ref layouts.
/// NOTE: This library targets fsharp-wasm only. It does NOT build on standard .NET.
namespace System

/// Supports a simple iteration over a generic collection.
type IEnumerator<'T> =
    abstract Current: 'T
    abstract MoveNext: unit -> bool
    abstract Reset: unit -> unit
    abstract Dispose: unit -> unit

/// Exposes the enumerator, which supports a simple iteration over a collection.
type IEnumerable<'T> =
    abstract GetEnumerator: unit -> IEnumerator<'T>

/// Defines a generalized comparison method.
type IComparable =
    abstract CompareTo: obj -> int

/// Defines a generalized, type-specific comparison method.
type IComparable<'T> =
    abstract CompareTo: 'T -> int
/// Defines a generalized method that a value type or class implements to create
/// a type-specific method for determining equality of instances.

type IEquatable<'T> =
    abstract Equals: 'T -> bool

/// Supports cloning, which creates a new instance of a class with the same
/// value as an existing instance.
type ICloneable =
    abstract Clone: unit -> obj

/// Defines methods to manipulate generic collections.
type ICollection<'T> =
    inherit IEnumerable<'T>
    abstract Count: int
    abstract IsReadOnly: bool
    abstract Add: 'T -> unit
    abstract Clear: unit -> unit
    abstract Contains: 'T -> bool
    abstract CopyTo: 'T[] * int -> unit
    abstract Remove: 'T -> bool

/// Represents a collection of objects that can be individually accessed by index.
type IList<'T> =
    inherit ICollection<'T>
    abstract Item: int -> 'T with get, set
    abstract IndexOf: 'T -> int
    abstract Insert: int * 'T -> unit
    abstract RemoveAt: int -> unit

/// Represents a generic collection of key/value pairs.
type IDictionary<'K, 'V> =
    inherit ICollection<struct('K * 'V)>
    abstract Item: 'K -> 'V with get, set
    abstract Keys: IEnumerable<'K>
    abstract Values: IEnumerable<'V>
    abstract ContainsKey: 'K -> bool
    abstract Add: 'K * 'V -> unit
    abstract Remove: 'K -> bool
    abstract TryGetValue: 'K * outref<'V> -> bool

