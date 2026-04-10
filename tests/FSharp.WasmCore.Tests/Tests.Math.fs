module Tests.Math

open Expecto
open FsCheck
open System

// ── System.Math tests ────────────────────────────────────────────────────────

[<Tests>]
let mathTests =
    testList "System.Math" [

        testList "Constants" [
            test "E is approximately 2.718" {
                Expect.floatClose Accuracy.high Math.E 2.718281828459045 "E constant"
            }
            test "PI is approximately 3.14159" {
                Expect.floatClose Accuracy.high Math.PI 3.141592653589793 "PI constant"
            }
            test "Tau = 2 * PI" {
                Expect.floatClose Accuracy.high Math.Tau (2.0 * Math.PI) "Tau = 2π"
            }
        ]

        testList "Abs" [
            test "Abs(-5) = 5"  { Expect.equal (Math.Abs(-5))  5  "int abs" }
            test "Abs(5) = 5"   { Expect.equal (Math.Abs(5))   5  "int abs positive" }
            test "Abs(-3.5) = 3.5" { Expect.floatClose Accuracy.high (Math.Abs(-3.5)) 3.5 "float abs" }
            test "Abs(-3.5f) = 3.5f" {
                Expect.equal (Math.Abs(-3.5f)) 3.5f "float32 abs"
            }
        ]

        testList "Min / Max" [
            test "Min(3, 7) = 3"  { Expect.equal (Math.Min(3, 7))        3    "int min" }
            test "Max(3, 7) = 7"  { Expect.equal (Math.Max(3, 7))        7    "int max" }
            test "Min(3.0, 7.0) = 3.0" { Expect.floatClose Accuracy.high (Math.Min(3.0, 7.0)) 3.0 "float min" }
            test "Min(3.0f, 7.0f) = 3.0f" { Expect.equal (Math.Min(3.0f, 7.0f)) 3.0f "float32 min" }
        ]

        testList "Clamp" [
            test "Clamp(5, 0, 10) = 5"    { Expect.equal (Math.Clamp(5, 0, 10))   5  "in range" }
            test "Clamp(-1, 0, 10) = 0"   { Expect.equal (Math.Clamp(-1, 0, 10))  0  "below min" }
            test "Clamp(15, 0, 10) = 10"  { Expect.equal (Math.Clamp(15, 0, 10))  10 "above max" }
            test "Clamp(0.5f, 0f, 1f) = 0.5f" { Expect.equal (Math.Clamp(0.5f, 0.0f, 1.0f)) 0.5f "float32 in range" }
        ]

        testList "Sqrt / Pow / Exp / Log" [
            test "Sqrt(4.0) = 2.0" {
                Expect.floatClose Accuracy.high (Math.Sqrt 4.0) 2.0 "sqrt 4"
            }
            test "Pow(2.0, 10.0) = 1024.0" {
                Expect.floatClose Accuracy.high (Math.Pow(2.0, 10.0)) 1024.0 "2^10"
            }
            test "Exp(0.0) = 1.0" {
                Expect.floatClose Accuracy.high (Math.Exp 0.0) 1.0 "e^0"
            }
            test "Log(1.0) = 0.0" {
                Expect.floatClose Accuracy.high (Math.Log 1.0) 0.0 "ln 1"
            }
            test "Log(E) = 1.0" {
                Expect.floatClose Accuracy.high (Math.Log Math.E) 1.0 "ln e"
            }
            test "Log10(100.0) = 2.0" {
                Expect.floatClose Accuracy.high (Math.Log10 100.0) 2.0 "log10 100"
            }
        ]

        testList "Floor / Ceiling / Truncate / Round" [
            test "Floor(3.7) = 3.0"    { Expect.floatClose Accuracy.high (Math.Floor(3.7))    3.0 "floor" }
            test "Ceiling(3.2) = 4.0"  { Expect.floatClose Accuracy.high (Math.Ceiling(3.2))  4.0 "ceiling" }
            test "Truncate(-3.9) = -3.0" { Expect.floatClose Accuracy.high (Math.Truncate(-3.9)) -3.0 "truncate" }
            test "Round(2.5) = 2.0 (banker's rounding)" {
                // .NET uses banker's rounding (round half to even)
                Expect.floatClose Accuracy.high (Math.Round 2.5) 2.0 "banker's round 2.5"
            }
            test "Round(3.5) = 4.0 (banker's rounding)" {
                Expect.floatClose Accuracy.high (Math.Round 3.5) 4.0 "banker's round 3.5"
            }
        ]

        testList "Sign" [
            test "Sign(-7) = -1"  { Expect.equal (Math.Sign(-7))  -1 "negative" }
            test "Sign(0) = 0"    { Expect.equal (Math.Sign(0))    0 "zero" }
            test "Sign(3) = 1"    { Expect.equal (Math.Sign(3))    1 "positive" }
        ]

        // Property: Min(a,b) <= a && Min(a,b) <= b
        testList "Properties" [
            testProperty "Min(a,b) is always <= both" <| fun (a: int) (b: int) ->
                let m = Math.Min(a, b)
                m <= a && m <= b

            testProperty "Max(a,b) is always >= both" <| fun (a: int) (b: int) ->
                let m = Math.Max(a, b)
                m >= a && m >= b

            testProperty "Clamp(v, lo, hi) is in [lo, hi] when lo <= hi" <| fun (v: int) (lo: int) (hi: int) ->
                if lo <= hi then
                    let c = Math.Clamp(v, lo, hi)
                    c >= lo && c <= hi
                else true  // precondition not met, skip

            testProperty "Abs(x) >= 0 for int" <| fun (x: int) ->
                x = System.Int32.MinValue || Math.Abs(x) >= 0
        ]
    ]
