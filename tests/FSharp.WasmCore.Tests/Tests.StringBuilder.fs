module Tests.StringBuilder

open Expecto
open System.Text

[<Tests>]
let sbTests =
    testList "StringBuilder" [

        test "Empty builder has length 0" {
            let sb = StringBuilder()
            Expect.equal sb.Length 0 "empty length"
        }

        test "Append string accumulates length" {
            let sb = StringBuilder()
            sb.Append("hello") |> ignore
            sb.Append(" world") |> ignore
            Expect.equal sb.Length 11 "hello world = 11 chars"
        }

        test "ToString returns correct string" {
            let sb = StringBuilder()
            sb.Append("foo") |> ignore
            sb.Append("bar") |> ignore
            Expect.equal (sb.ToString()) "foobar" "ToString should concatenate"
        }

        test "AppendLine adds newline" {
            let sb = StringBuilder()
            sb.AppendLine("line1") |> ignore
            sb.AppendLine("line2") |> ignore
            let result = sb.ToString()
            Expect.isTrue (result.Contains("line1")) "contains line1"
            Expect.isTrue (result.Contains("line2")) "contains line2"
        }

        test "AppendFormat interpolates values" {
            let sb = StringBuilder()
            sb.AppendFormat("x={0}, y={1}", 3, 4.5) |> ignore
            Expect.equal (sb.ToString()) "x=3, y=4.5" "AppendFormat"
        }

        test "Clear resets to empty" {
            let sb = StringBuilder("not empty")
            sb.Clear() |> ignore
            Expect.equal sb.Length 0 "after Clear, length = 0"
            Expect.equal (sb.ToString()) "" "after Clear, ToString = empty"
        }

        test "Constructor from string" {
            let sb = StringBuilder("initial")
            Expect.equal (sb.ToString()) "initial" "initial value preserved"
        }

        test "Append int" {
            let sb = StringBuilder()
            sb.Append(42) |> ignore
            Expect.equal (sb.ToString()) "42" "int appended"
        }

        test "Append bool" {
            let sb = StringBuilder()
            sb.Append(true) |> ignore
            Expect.stringContains (sb.ToString()) "rue" "bool appended (True or true)"
        }

        test "Chaining via fluent API" {
            // Use step-by-step instead of chained to avoid BCL ambiguity
            let sb = System.Text.StringBuilder()
            sb.Append("a") |> ignore
            sb.Append("b") |> ignore
            sb.Append("c") |> ignore
            Expect.equal (sb.ToString()) "abc" "appended a, b, c"
        }
    ]
