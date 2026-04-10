module Tests.Stack

open Expecto
open System.Collections.Generic
open FsCheck

[<Tests>]
let stackTests =
    testList "Stack<T>" [

        test "New stack is empty" {
            let s = Stack<int>()
            Expect.equal s.Count 0 "empty count"
        }

        test "Push increases Count" {
            let s = Stack<int>()
            s.Push(1)
            s.Push(2)
            Expect.equal s.Count 2 "count = 2"
        }

        test "Pop returns LIFO order" {
            let s = Stack<int>()
            s.Push(1)
            s.Push(2)
            s.Push(3)
            Expect.equal (s.Pop()) 3 "pop returns 3 first"
            Expect.equal (s.Pop()) 2 "then 2"
            Expect.equal (s.Pop()) 1 "then 1"
        }

        test "Pop decreases Count" {
            let s = Stack<int>()
            s.Push(10)
            s.Push(20)
            s.Pop() |> ignore
            Expect.equal s.Count 1 "count drops to 1"
        }

        test "Peek does not remove" {
            let s = Stack<int>()
            s.Push(42)
            let v = s.Peek()
            Expect.equal v 42 "peek returns top"
            Expect.equal s.Count 1 "count unchanged after Peek"
        }

        test "Pop on empty stack throws" {
            let s = Stack<int>()
            Expect.throws (fun () -> s.Pop() |> ignore) "Pop on empty throws"
        }

        test "Peek on empty stack throws" {
            let s = Stack<int>()
            Expect.throws (fun () -> s.Peek() |> ignore) "Peek on empty throws"
        }

        test "TryPop returns false on empty" {
            let s = Stack<int>()
            let mutable v = 0
            let ok = s.TryPop(&v)
            Expect.isFalse ok "TryPop returns false"
        }

        test "TryPop returns true and value when non-empty" {
            let s = Stack<int>()
            s.Push(99)
            let mutable v = 0
            let ok = s.TryPop(&v)
            Expect.isTrue ok "TryPop returns true"
            Expect.equal v 99 "returns pushed value"
        }

        test "Contains finds pushed element" {
            let s = Stack<string>()
            s.Push("alpha")
            s.Push("beta")
            Expect.isTrue (s.Contains("alpha")) "contains alpha"
            Expect.isFalse (s.Contains("gamma")) "not contains gamma"
        }

        test "Clear empties stack" {
            let s = Stack<int>()
            s.Push(1); s.Push(2); s.Push(3)
            s.Clear()
            Expect.equal s.Count 0 "clear resets count"
        }

        test "ToArray returns top-first order" {
            let s = Stack<int>()
            s.Push(1); s.Push(2); s.Push(3)
            let arr = s.ToArray()
            Expect.equal arr.[0] 3 "first element is top"
            Expect.equal arr.[2] 1 "last element is bottom"
        }

        test "Constructor from seq" {
            let s = Stack<int>(seq { 1; 2; 3 })
            Expect.equal s.Count 3 "seq constructor populates stack"
        }

        testProperty "Push N items -> Count = N" <| fun (xs: int list) ->
            let s = Stack<int>()
            for x in xs do s.Push(x)
            s.Count = xs.Length

        testProperty "Push then pop reverses list" <| fun (xs: int list) ->
            let xs = xs |> List.truncate 50  // keep test fast
            let s = Stack<int>()
            for x in xs do s.Push(x)
            let popped = [ while s.Count > 0 do yield s.Pop() ]
            popped = List.rev xs
    ]
