/// System.Collections.Generic.Dictionary<K,V> for WasmGC targets.
/// Pure-F# open-addressing hash-table  with linear probing + tombstones. 
/// The fsharp-wasm backend lowers
/// the bucket array to a WasmGC (array (mut (ref null $KVSlot))) struct.
namespace System.Collections.Generic

open System

// ─── Slot (private implementation detail) ────────────────────────────────────
[<Struct>]
type private Slot<'K,'V> =
    val mutable IsOccupied : bool
    val mutable Key        : 'K
    val mutable Value      : 'V
    val mutable Hash       : int
    new(occupied, key, value, hash) =
        { IsOccupied = occupied; Key = key; Value = value; Hash = hash }

// ─── KeyValuePair ─────────────────────────────────────────────────────────────
type KeyValuePair<'K,'V>(key: 'K, value: 'V) =
    member _.Key   = key
    member _.Value = value
    override _.ToString() = sprintf "[%A, %A]" key value

// ─── IEqualityComparer ────────────────────────────────────────────────────────
type IEqualityComparer<'T> =
    abstract Equals     : 'T * 'T -> bool
    abstract GetHashCode: 'T -> int

// ─── Array enumerator (used by Dictionary.GetEnumerator) ─────────────────────
type ArrayEnumerator<'T>(arr: 'T[]) =   // public: returned by Dictionary.GetEnumerator
    let mutable idx = -1
    member _.Current    = arr.[idx]
    member _.MoveNext() = idx <- idx + 1; idx < arr.Length
    member _.Reset()    = idx <- -1
    member _.Dispose()  = ()

// ─── Dictionary ───────────────────────────────────────────────────────────────

/// Dictionary<K,V>: open-addressing hash table for WasmGC.
/// NOTE: Never null in practice — no need for [<AllowNullLiteral>].
type Dictionary<'K, 'V when 'K: equality and 'K: not null and 'V: not null>(initialCapacity: int, comparer: IEqualityComparer<'K>) =

    let mutable slots   : Slot<'K,'V>[] = Array.zeroCreate (max 4 initialCapacity)
    let mutable count   : int           = 0
    let mutable deleted : int           = 0

    let cmp : IEqualityComparer<'K> =
        if obj.ReferenceEquals(comparer, null) then
            { new IEqualityComparer<'K> with
                member _.Equals(a, b)   = obj.Equals(a :> obj, b :> obj)
                member _.GetHashCode(k) =
                    if obj.ReferenceEquals(k :> obj, null) then 0
                    else (k :> obj).GetHashCode() }
        else comparer

    // ── Internal helpers ──────────────────────────────────────────────────────

    let hashOf (key: 'K) =
        let h = cmp.GetHashCode(key) &&& 0x7FFFFFFF
        if h = 0 then 1 else h   // 0 reserved for "empty slot"

    // ── Additional constructors ───────────────────────────────────────────────
    new()                             = Dictionary<'K,'V>(4, Unchecked.defaultof<_>)
    new(capacity: int)                = Dictionary<'K,'V>(capacity, Unchecked.defaultof<_>)
    new(comparer: IEqualityComparer<'K>) = Dictionary<'K,'V>(4, comparer)
    new(other: Dictionary<'K,'V>) as this =
        Dictionary<'K,'V>(max 4 other.Count, Unchecked.defaultof<_>)
        then this.CopyFrom(other)

    member private _.FindSlot(key: 'K, hash: int) : int =
        let cap       = slots.Length
        let mutable i         = hash % cap
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

    // ── Copy from another dictionary (without :> seq<> cast) ─────────────────

    /// Backing slot array (`let` fields of ANOTHER instance are not accessible; private members are).
    member private _.Slots : Slot<'K,'V>[] = slots

    member this.CopyFrom(other: Dictionary<'K,'V>) =
        let otherSlots = other.Slots
        for i in 0 .. otherSlots.Length - 1 do
            let s = otherSlots.[i]
            if s.IsOccupied then
                this.SetInternal(s.Key, s.Value, s.Hash)

    // ── Public surface ────────────────────────────────────────────────────────

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
            // Tombstone: IsOccupied=false, Hash=1 (non-zero → not empty)
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
        // TODO: we need Array.exists in the BCL first. So we work around it for now
        // slots |> Array.exists (fun s -> s.IsOccupied && obj.Equals(s.Value, value))
        let mutable found = false
        let mutable i = 0
        while not found && i < slots.Length do
            if slots.[i].IsOccupied && obj.Equals(box slots.[i].Value, box value) then
                found <- true
            i <- i + 1
        found

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

    // ── Key/Value collections (as arrays) ─────────────────────────────────────

    // TODO: disabled for now until seq support lands in the compiler
    // member _.Keys   : 'K seq = seq { for s in slots do if s.IsOccupied then yield s.Key }
    // member _.Values : 'V seq = seq { for s in slots do if s.IsOccupied then yield s.Value }

    member _.Keys : 'K[] =
        let result = Array.zeroCreate count
        let mutable j = 0
        for i in 0 .. slots.Length - 1 do
            if slots.[i].IsOccupied then
                result.[j] <- slots.[i].Key
                j <- j + 1
        result

    member _.Values : 'V[] =
        let result = Array.zeroCreate count
        let mutable j = 0
        for i in 0 .. slots.Length - 1 do
            if slots.[i].IsOccupied then
                result.[j] <- slots.[i].Value
                j <- j + 1
        result
    
    // ── GetEnumerator (direct, no IEnumerable VTable) ────────────────────────
    // NOTE: IEnumerable<KVP> interface not supported in WasmGC (VTable issues).
    // Provide GetEnumerator directly for iteration patterns (for .. in ..).
    //  TODO: 
    // member this.GetEnumerator() =
    //     let items = slots |> Array.filter (fun s -> s.IsOccupied)
    //                       |> Array.map   (fun s -> KeyValuePair(s.Key, s.Value))
    //     (items :> seq<KeyValuePair<'K,'V>>).GetEnumerator()

    // ── GetEnumerator — explicit, no interface cast ───────────────────────────
    // Returns KVP array snapshot. No IEnumerable<KVP> interface needed.

    // TODO: helper because of missing BCL support yet
    member _.ToKvpArray() : KeyValuePair<'K,'V>[] =
        let result = Array.zeroCreate count
        let mutable j = 0
        for i in 0 .. slots.Length - 1 do
            if slots.[i].IsOccupied then
                result.[j] <- KeyValuePair(slots.[i].Key, slots.[i].Value)
                j <- j + 1
        result

    // For `for kvp in dict do` — Fable desugars this to GetEnumerator().
    // We provide a non-interface version so no VTable registration needed.
    member this.GetEnumerator() =
        ArrayEnumerator(this.ToKvpArray())

    // Sum helper (avoids Seq.sumBy which needs IEnumerable)
    member this.SumValues(selector: 'V -> int) : int =
        let mutable total = 0
        for i in 0 .. slots.Length - 1 do
            if slots.[i].IsOccupied then
                total <- total + selector slots.[i].Value
        total

    override _.ToString() =
        // TODO: no typeof support yet
        // sprintf "Dictionary<%s,%s>[count=%d]" (typeof<'K>.Name) (typeof<'V>.Name) count
        sprintf "Dictionary[count=%d]" count



// ─── KeyNotFoundException (if not in Exception.fs already) ───────────────────
// Only define if not already defined in System namespace

