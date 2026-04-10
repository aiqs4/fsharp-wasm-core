/// System.Runtime.CompilerServices — subset for WasmGC targets.
/// FSharp.Core uses RuntimeHelpers for generic hash/equality helpers and
/// GetSubArray.  We provide the three methods FSharp.Core actually calls.
namespace System.Runtime.CompilerServices

open System

[<AbstractClass; Sealed>]
type RuntimeHelpers private () =

    /// Used by FSharp.Core's generic structural equality.
    /// On WasmGC this should dispatch to the per-type equality built-ins.
    static member GetHashCode(o: obj) : int =
        if isNull o then 0 else o.GetHashCode()

    /// Struct-equality helper used internally by FSharp.Core.
    static member Equals(x: obj, y: obj) : bool =
        obj.Equals(x, y)

    /// Used by FSharp.Core's array slice.
    static member GetSubArray<'T>(arr: 'T[], offset: int, count: int) : 'T[] =
        Array.sub arr offset count

    /// Initialize an array with a supplied value (used by FSharp.Core prim-types).
    static member InitializeArray(array: System.Array, fldHandle: System.RuntimeFieldHandle) : unit =
        // Stub: on WasmGC the compiler emits array.new_fixed directly.
        ignore (array, fldHandle)

namespace System.Runtime.InteropServices

open System

/// Minimal MemoryMarshal stub — FSharp.Core references this for span-based ops.
[<AbstractClass; Sealed>]
type MemoryMarshal private () =
    static member Cast<'TFrom, 'TTo>(span: 'TFrom[]) : 'TTo[] =
        raise (NotSupportedException "MemoryMarshal.Cast is not supported in the WasmGC subset.")

/// CollectionsMarshal stub.
[<AbstractClass; Sealed>]
type CollectionsMarshal private () =
    static member AsSpan<'T>(list: System.Collections.Generic.List<'T>) : 'T[] =
        list.ToArray()

namespace System.Threading

/// Minimal CancellationToken — FSharp.Core async uses this.
[<Struct>]
type CancellationToken(canceled: bool) =
    member _.IsCancellationRequested = canceled
    member _.CanBeCanceled            = canceled
    static member val None : CancellationToken = CancellationToken(false)

/// CancellationTokenSource — creates and manages CancellationTokens.
[<AllowNullLiteral>]
type CancellationTokenSource() =
    let mutable canceled = false
    member _.Token   = CancellationToken(canceled)
    member _.Cancel() = canceled <- true
    member _.Dispose() = ()
    static member CreateLinkedTokenSource(_t1: CancellationToken, _t2: CancellationToken) =
        new CancellationTokenSource()