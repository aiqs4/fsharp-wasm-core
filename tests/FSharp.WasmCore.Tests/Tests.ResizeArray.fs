module Tests.ResizeArray

open Expecto
open System.Collections.Generic
open FsCheck

[<Tests>]
let raTests =
    testList "ResizeArray (List<T>)" [

        test "Empty list has count 0" {
            let ra = ResizeArray<int>()
            Expect.equal ra.Count 0 "empty count"
        }

        test "Add increases Count" {
            let ra = ResizeArray<int>()
            ra.Add(1)
            ra.Add(2)
            ra.Add(3)
            Expect.equal ra.Count 3 "count after 3 adds"
        }

        test "Indexer returns correct values" {
            let ra = ResizeArray<string>()
            ra.Add("a")
            ra.Add("b")
            ra.Add("c")
            Expect.equal ra.[0] "a" "[0]"
            Expect.equal ra.[1] "b" "[1]"
            Expect.equal ra.[2] "c" "[2]"
        }

        test "Contains finds existing element" {
            let ra = ResizeArray<int>([1; 2; 3; 4; 5])
            Expect.isTrue (ra.Contains(3)) "contains 3"
            Expect.isFalse (ra.Contains(99)) "does not contain 99"
        }

        test "Remove deletes first occurrence" {
            let ra = ResizeArray<int>([1; 2; 3; 2])
            ra.Remove(2) |> ignore
            Expect.equal ra.Count 3 "count after Remove"
            Expect.equal ra.[1] 3 "second slot now 3"
        }

        test "RemoveAt deletes by index" {
            let ra = ResizeArray<string>(["x"; "y"; "z"])
            ra.RemoveAt(1)
            Expect.equal ra.Count 2 "count after RemoveAt"
            Expect.equal ra.[1] "z" "z shifted to index 1"
        }

        test "Insert shifts elements right" {
            let ra = ResizeArray<int>([1; 3])
            ra.Insert(1, 2)
            Expect.equal ra.[0] 1 "index 0"
            Expect.equal ra.[1] 2 "inserted 2"
            Expect.equal ra.[2] 3 "shifted 3"
        }

        test "Clear resets Count to 0" {
            let ra = ResizeArray<int>([1; 2; 3])
            ra.Clear()
            Expect.equal ra.Count 0 "count after Clear"
        }

        test "IndexOf returns correct index" {
            let ra = ResizeArray<string>(["a"; "b"; "c"])
            Expect.equal (ra.IndexOf("b")) 1 "IndexOf b = 1"
            Expect.equal (ra.IndexOf("z")) -1 "IndexOf missing = -1"
        }

        test "Sort orders ascending" {
            let ra = ResizeArray<int>([5; 2; 8; 1; 4])
            ra.Sort()
            Expect.equal (List.ofSeq ra) [1; 2; 4; 5; 8] "sorted ascending"
        }

        test "Enumerate with for-in" {
            let ra = ResizeArray<int>([10; 20; 30])
            let mutable sum = 0
            for x in ra do sum <- sum + x
            Expect.equal sum 60 "sum via for-in"
        }

        test "ToArray snapshot is independent" {
            let ra = ResizeArray<int>([1; 2; 3])
            let arr = ra.ToArray()
            ra.Add(4)
            Expect.equal arr.Length 3 "snapshot not mutated"
        }

        testProperty "Add then Count == length of original list" <| fun (xs: int list) ->
            let ra = ResizeArray<int>()
            for x in xs do ra.Add(x)
            ra.Count = xs.Length

        testProperty "Seq.toList round-trip preserves order" <| fun (xs: int list) ->
            let ra = ResizeArray<int>(xs)
            List.ofSeq ra = xs
    ]
