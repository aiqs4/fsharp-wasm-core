/// System.Text.StringBuilder for WasmGC targets.
/// Pure-F# implementation backed by a ResizeArray of string chunks.
/// The fsharp-wasm backend will lower this to a growable i8 array or
/// a linked list of WasmGC string segments.
namespace System.Text

open System

/// A mutable sequence of characters. Backed by a list of chunks so that
/// every Append is O(1) amortised; ToString joins them.
// [<AllowNullLiteral>]
type StringBuilder(initialCapacity: int) =

    let mutable chunks  : string list = []
    let mutable totalLen: int         = 0

    new()                    = StringBuilder(16)
    new(value: string) as this =
        StringBuilder(max 16 value.Length)
        then
            this.Append(value) |> ignore
    new(value: string, capacity: int) as this =
        StringBuilder(capacity)
        then
            this.Append(value) |> ignore

    member _.Length = totalLen

    member _.Capacity = initialCapacity // simplified — capacity is not tracked precisely

    // ── Append overloads ────────────────────────────────────────────────────

    member this.Append(value: string) : StringBuilder =
        if not (isNull value) && value.Length > 0 then
            chunks   <- value :: chunks
            totalLen <- totalLen + value.Length
        this

    member this.Append(value: char) : StringBuilder =
        this.Append(string value)

    member this.Append(value: bool)    : StringBuilder = this.Append(string value)
    member this.Append(value: int8)    : StringBuilder = this.Append(string value)
    member this.Append(value: uint8)   : StringBuilder = this.Append(string value)
    member this.Append(value: int16)   : StringBuilder = this.Append(string value)
    member this.Append(value: uint16)  : StringBuilder = this.Append(string value)
    member this.Append(value: int)     : StringBuilder = this.Append(string value)
    member this.Append(value: uint32)  : StringBuilder = this.Append(string value)
    member this.Append(value: int64)   : StringBuilder = this.Append(string value)
    member this.Append(value: uint64)  : StringBuilder = this.Append(string value)
    member this.Append(value: float32) : StringBuilder = this.Append(string value)
    member this.Append(value: float)   : StringBuilder = this.Append(string value)
    member this.Append(value: obj)     : StringBuilder =
        if isNull value then this else this.Append(value.ToString())

    member this.Append(value: char[], startIndex: int, count: int) : StringBuilder =
        this.Append(new string(value, startIndex, count))

    member this.Append(value: string, startIndex: int, count: int) : StringBuilder =
        if isNull value then
            if startIndex = 0 && count = 0 then this
            else raise (ArgumentNullException "value")
        else
            this.Append(value.Substring(startIndex, count))

    member this.AppendLine() : StringBuilder =
        this.Append(Environment.NewLine)

    member this.AppendLine(value: string) : StringBuilder =
        this.Append(value).Append(Environment.NewLine)

    member this.AppendFormat(format: string, [<ParamArray>] args: obj[]) : StringBuilder =
        this.Append(String.Format(format, args))

    // ── Insert ──────────────────────────────────────────────────────────────

    member this.Insert(index: int, value: string) : StringBuilder =
        let current = this.ToString()
        if index < 0 || index > current.Length then
            raise (ArgumentOutOfRangeException("index", "Index was out of range."))
        chunks   <- [ current.[0..index-1] + value + current.[index..] ]
        totalLen <- current.Length + (if isNull value then 0 else value.Length)
        this

    member this.Insert(index: int, value: char) : StringBuilder =
        this.Insert(index, string value)

    // ── Remove ──────────────────────────────────────────────────────────────

    member this.Remove(startIndex: int, length: int) : StringBuilder =
        let current = this.ToString()
        if startIndex < 0 || length < 0 || startIndex + length > current.Length then
            raise (ArgumentOutOfRangeException("startIndex", "Index and length must refer to a location within the string."))
        chunks   <- [ current.[0..startIndex-1] + current.[startIndex+length..] ]
        totalLen <- current.Length - length
        this

    // ── Replace ─────────────────────────────────────────────────────────────

    member this.Replace(oldValue: string, newValue: string) : StringBuilder =
        let current = this.ToString()
        let updated = current.Replace(oldValue, if isNull newValue then "" else newValue)
        chunks   <- [ updated ]
        totalLen <- updated.Length
        this

    member this.Replace(oldChar: char, newChar: char) : StringBuilder =
        this.Replace(string oldChar, string newChar)

    // ── Indexer ─────────────────────────────────────────────────────────────

    member this.Item
        with get(index: int) : char =
            let s = this.ToString()
            if index < 0 || index >= s.Length then
                raise (IndexOutOfRangeException())
            s.[index]

    // ── Clear ───────────────────────────────────────────────────────────────

    member this.Clear() : StringBuilder =
        chunks   <- []
        totalLen <- 0
        this

    // ── Output ──────────────────────────────────────────────────────────────

    /// Materialise all chunks into a single string.
    override _.ToString() =
        chunks |> List.rev |> String.concat ""

    member this.ToString(startIndex: int, length: int) : string =
        this.ToString().Substring(startIndex, length)

    member _.CopyTo(sourceIndex: int, destination: char[], destinationIndex: int, count: int) =
        ignore (sourceIndex, destination, destinationIndex, count)
        raise (NotImplementedException("StringBuilder.CopyTo is not supported in WasmGC subset."))