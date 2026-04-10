module Tests.Dictionary

open Expecto
open FsCheck
open System.Collections.Generic

[<Tests>]
let dictTests =
    testList "Dictionary<K,V>" [

        test "Add and retrieve" {
            let d = Dictionary<string, int>()
            d.["hello"] <- 42
            Expect.equal d.["hello"] 42 "retrieve added value"
        }

        test "ContainsKey" {
            let d = Dictionary<string, int>()
            d.Add("x", 1)
            Expect.isTrue  (d.ContainsKey "x") "key x should exist"
            Expect.isFalse (d.ContainsKey "y") "key y should not exist"
        }

        test "Remove" {
            let d = Dictionary<string, int>()
            d.Add("a", 1)
            let removed = d.Remove("a")
            Expect.isTrue  removed "Remove should return true"
            Expect.isFalse (d.ContainsKey "a") "key should be gone"
        }

        test "Remove nonexistent returns false" {
            let d = Dictionary<int, int>()
            Expect.isFalse (d.Remove 99) "removing nonexistent key = false"
        }

        test "TryGetValue: present" {
            let d = Dictionary<int, string>()
            d.[1] <- "one"
            let mutable v = ""
            let ok = d.TryGetValue(1, &v)
            Expect.isTrue  ok "should find key 1"
            Expect.equal v "one" "value should be 'one'"
        }

        test "TryGetValue: absent" {
            let d = Dictionary<int, string>()
            let mutable v = ""
            let ok = d.TryGetValue(99, &v)
            Expect.isFalse ok "should not find key 99"
        }

        test "Count reflects Add / Remove" {
            let d = Dictionary<int, int>()
            Expect.equal d.Count 0 "empty count"
            d.[1] <- 10
            d.[2] <- 20
            Expect.equal d.Count 2 "after two adds"
            d.Remove 1 |> ignore
            Expect.equal d.Count 1 "after one remove"
        }

        test "Update existing key does not increase Count" {
            let d = Dictionary<int, int>()
            d.[1] <- 10
            d.[1] <- 20   // update
            Expect.equal d.Count 1 "overwrite should keep count = 1"
            Expect.equal d.[1] 20 "value should be updated"
        }

        test "Duplicate Add throws" {
            let d = Dictionary<int, int>()
            d.Add(1, 10)
            Expect.throws (fun () -> d.Add(1, 99)) "Add with duplicate key must throw"
        }

        test "Clear resets Count" {
            let d = Dictionary<int, int>()
            for i in 1..10 do d.[i] <- i
            d.Clear()
            Expect.equal d.Count 0 "after Clear, count = 0"
        }

        test "Enumerate all KeyValuePairs" {
            let d = Dictionary<int, int>()
            for i in 1..5 do d.[i] <- i * i
            let sum = d |> Seq.sumBy (fun kv -> kv.Value)
            Expect.equal sum (1 + 4 + 9 + 16 + 25) "sum of squares 1..5"
        }

        test "Handles hash collision (many integers)" {
            // Force many inserts to trigger rehashing
            let d = Dictionary<int, int>()
            for i in 0..999 do d.[i] <- i * 2
            Expect.equal d.Count 1000 "1000 entries"
            for i in 0..999 do
                Expect.equal d.[i] (i * 2) $"value at {i}"
        }

        test "Copy constructor" {
            let a = Dictionary<string, int>()
            a.["x"] <- 1
            a.["y"] <- 2
            let b = Dictionary<string, int>(a)
            Expect.equal b.Count 2 "copy has same count"
            b.["z"] <- 3
            Expect.isFalse (a.ContainsKey "z") "original not mutated by copy"
        }

        // Property: what we put in, we get out
        testProperty "round-trip: any int -> string mapping" <| fun (pairs: List<int * string>) ->
            let d = Dictionary<int, string>()
            let unique = pairs |> Seq.distinctBy fst |> Seq.toList
            for (k, v) in unique do d.[k] <- v
            unique |> List.forall (fun (k, v) -> d.[k] = v)
    ]
