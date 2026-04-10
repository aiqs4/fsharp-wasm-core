/// System.Collections.Generic.Dictionary<K,V> for WasmGC targets.
/// Pure-F# open-addressing hash-table.  The fsharp-wasm backend lowers
/// the bucket array to a WasmGC (array (mut (ref null $KVSlot))) struct.
namespace System.Collections.Generic

open System

[<Struct>]
type private Slot<'K,'V> =
    val mutable IsOccupied : bool
    val mutable Key        : 'K
    val mutable Value      : 'V
    val mutable Hash       : int
    new(occupied, key, value, hash) =
        { IsOccupied = occupied; Key = key; Value = value; Hash = hash }

[<AllowNullLiteral>]
type Dictionary<'K, 'V when 'K: equality>(initialCapacity: int, comparer: IEqualityComparer<'K>) =

    let mutable slots   : Slot<'K,'V>[] = Array.zeroCreate (max 4 initialCapacity)
    let mutable count   : int           = 0
    let mutable deleted : int           = 0

    let cmp =
        if isNull (comparer :> obj) then
            { new IEqualityComparer<'K> with
                member _.Equals(a, b)   = obj.Equals(a, b)
                member _.GetHashCode(k) = if isNull (k :> obj) then 0 else k.GetHashCode() }
        else comparer

    // ── Internal helpers (must precede all members) ────────────────────────

    let hashOf (key: 'K) =
        let h = cmp.GetHashCode(key) &&& 0x7FFFFFFF
        if h = 0 then 1 else h   // 0 is reserved for "empty"

    new()                        = Dictionary<'K,'V>(4, null)
    new(capacity: int)           = Dictionary<'K,'V>(capacity, null)
    new(comparer: IEqualityComparer<'K>) = Dictionary<'K,'V>(4, comparer)
    new(other: Dictionary<'K,'V>) as this =
        Dictionary<'K,'V>(other.Count, null)
        then
            for kvp in (other :> seq<KeyValuePair<'K,'V>>) do
                this.[kvp.Key] <- kvp.Value

    member private this.FindSlot(key: 'K, hash: int) : int =
        let cap    = slots.Length
        let mutable i = hash % cap
        let mutable tombstone = -1
        let mutable result    = -1
        let mutable cont      = true
        while cont do
            let s = slots.[i]
            if not s.IsOccupied && s.Hash = 0 then
                // empty — not present
                result <- if tombstone >= 0 then tombstone else i
                cont   <- false
            elif not s.IsOccupied then
                // tombstone
                if tombstone = -1 then tombstone <- i
                i <- (i + 1) % cap
            elif s.Hash = hash && cmp.Equals(s.Key, key) then
                result <- i
                cont   <- false
            else
                i <- (i + 1) % cap
        result

    member private this.Resize() =
        let oldSlots = slots
        slots   <- Array.zeroCreate (oldSlots.Length * 2)
        count   <- 0
        deleted <- 0
        for s in oldSlots do
            if s.IsOccupied then
                this.SetInternal(s.Key, s.Value, s.Hash)

    member private this.SetInternal(key: 'K, value: 'V, hash: int) =
        let i = this.FindSlot(key, hash)
        if slots.[i].IsOccupied then
            slots.[i] <- Slot(true, key, value, hash)   // update
        else
            slots.[i] <- Slot(true, key, value, hash)
            count <- count + 1

    // ── Public surface ─────────────────────────────────────────────────────

    member _.Count    = count
    member _.Capacity = slots.Length

    member this.Item
        with get(key: 'K) : 'V =
            let h = hashOf key
            let i = this.FindSlot(key, h)
            if i >= 0 && slots.[i].IsOccupied then slots.[i].Value
            else raise (KeyNotFoundException(sprintf "The given key '%A' was not present in the dictionary." key))
        and  set(key: 'K) (value: 'V) =
            if (count + deleted) * 3 > slots.Length * 2 then this.Resize()
            let h = hashOf key
            this.SetInternal(key, value, h)

    member this.Add(key: 'K, value: 'V) =
        let h = hashOf key
        let i = this.FindSlot(key, h)
        if i >= 0 && slots.[i].IsOccupied then
            raise (ArgumentException(sprintf "An item with the same key '%A' has already been added." key))
        if (count + deleted) * 3 > slots.Length * 2 then this.Resize()
        this.SetInternal(key, value, h)

    member this.Remove(key: 'K) : bool =
        let h = hashOf key
        let i = this.FindSlot(key, h)
        if i >= 0 && slots.[i].IsOccupied then
            slots.[i] <- Slot(false, Unchecked.defaultof<'K>, Unchecked.defaultof<'V>, 1) // tombstone has hash!=0
            count   <- count   - 1
            deleted <- deleted + 1
            true
        else false

    member this.ContainsKey(key: 'K) : bool =
        let h = hashOf key
        let i = this.FindSlot(key, h)
        i >= 0 && slots.[i].IsOccupied

    member this.ContainsValue(value: 'V) : bool =
        slots |> Array.exists (fun s -> s.IsOccupied && obj.Equals(s.Value, value))

    member this.TryGetValue(key: 'K, value: outref<'V>) : bool =
        let h = hashOf key
        let i = this.FindSlot(key, h)
        if i >= 0 && slots.[i].IsOccupied then
            value <- slots.[i].Value
            true
        else
            value <- Unchecked.defaultof<'V>
            false

    member this.GetValueOrDefault(key: 'K, defaultValue: 'V) : 'V =
        let mutable v = Unchecked.defaultof<'V>
        if this.TryGetValue(key, &v) then v else defaultValue

    member _.Clear() =
        slots   <- Array.zeroCreate 4
        count   <- 0
        deleted <- 0

    member _.Keys   : 'K seq = seq { for s in slots do if s.IsOccupied then yield s.Key }
    member _.Values : 'V seq = seq { for s in slots do if s.IsOccupied then yield s.Value }

    // ── IEnumerable<KeyValuePair> ──────────────────────────────────────────

    interface IEnumerable<KeyValuePair<'K,'V>> with
        member _.GetEnumerator() =
            let items = slots |> Array.filter (fun s -> s.IsOccupied)
                               |> Array.map   (fun s -> KeyValuePair(s.Key, s.Value))
            (items :> seq<KeyValuePair<'K,'V>>).GetEnumerator()

    override this.ToString() =
        sprintf "Dictionary<%s,%s>[count=%d]" (typeof<'K>.Name) (typeof<'V>.Name) count

// IEqualityComparer and KeyValuePair are defined here because on WasmGC there is no BCL.
and IEqualityComparer<'T> =
    abstract Equals     : 'T * 'T -> bool
    abstract GetHashCode: 'T -> int

and KeyValuePair<'K,'V>(key: 'K, value: 'V) =
    member _.Key   = key
    member _.Value = value
    override _.ToString() = sprintf "[%A, %A]" key value