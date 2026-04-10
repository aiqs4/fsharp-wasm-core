/// System.Collections.Generic.Stack<T> for WasmGC targets.
/// Pure-F# LIFO collection backed by a growable array.
/// The fsharp-wasm backend lowers this to a (array (mut anyref)) + i32 length.
namespace System.Collections.Generic

open System

[<AllowNullLiteral>]
type Stack<'T>(initialCapacity: int) =

    let mutable data  : 'T[] = Array.zeroCreate (max 4 initialCapacity)
    let mutable top   : int  = 0     // index of next free slot (= count)

    new() = Stack<'T>(4)

    new(collection: 'T seq) as this =
        Stack<'T>(4)
        then
            for item in collection do this.Push(item)

    // ── Internal growth ────────────────────────────────────────────────────

    member private _.EnsureCapacity() =
        if top >= data.Length then
            let newData = Array.zeroCreate (data.Length * 2)
            Array.blit data 0 newData 0 top
            data <- newData

    // ── Properties ─────────────────────────────────────────────────────────

    member _.Count = top

    // ── Core operations ────────────────────────────────────────────────────

    member this.Push(item: 'T) =
        this.EnsureCapacity()
        data.[top] <- item
        top <- top + 1

    member _.Pop() : 'T =
        if top = 0 then raise (InvalidOperationException "Stack is empty.")
        top <- top - 1
        let item = data.[top]
        data.[top] <- Unchecked.defaultof<'T>
        item

    member _.Peek() : 'T =
        if top = 0 then raise (InvalidOperationException "Stack is empty.")
        data.[top - 1]

    member this.TryPop(result: outref<'T>) : bool =
        if top = 0 then
            result <- Unchecked.defaultof<'T>
            false
        else
            result <- this.Pop()
            true

    member _.TryPeek(result: outref<'T>) : bool =
        if top = 0 then
            result <- Unchecked.defaultof<'T>
            false
        else
            result <- data.[top - 1]
            true

    member _.Contains(item: 'T) : bool =
        let mutable found = false
        let mutable i     = 0
        while not found && i < top do
            if obj.Equals(data.[i], item) then found <- true
            i <- i + 1
        found

    member _.Clear() =
        for i in 0 .. top - 1 do
            data.[i] <- Unchecked.defaultof<'T>
        top <- 0

    member _.ToArray() : 'T[] =
        // Stack.ToArray returns items in LIFO order (top first)
        let arr = Array.zeroCreate top
        for i in 0 .. top - 1 do
            arr.[i] <- data.[top - 1 - i]
        arr

    member this.CopyTo(array: 'T[], arrayIndex: int) =
        if isNull array then raise (ArgumentNullException "array")
        let arr = this.ToArray()
        Array.blit arr 0 array arrayIndex arr.Length

    // ── IEnumerable (LIFO order) ───────────────────────────────────────────

    interface System.Collections.Generic.IEnumerable<'T> with
        member this.GetEnumerator() =
            (this.ToArray() :> seq<'T>).GetEnumerator()

    interface System.Collections.IEnumerable with
        member this.GetEnumerator() =
            (this.ToArray() :> System.Collections.IEnumerable).GetEnumerator()

    override _.ToString() =
        sprintf "Stack<%s>[count=%d]" (typeof<'T>.Name) top