module Tests.Interfaces

open Expecto
open System

// ── IEnumerator / IEnumerable ────────────────────────────────────────────────
[<Tests>]
let interfaceTests =
    testList "Interfaces" [

        test "IEquatable: custom struct implementing IEquatable" {
            let a = { new IEquatable<int> with member _.Equals(other) = other = 42 }
            Expect.isTrue  (a.Equals 42) "42 equals 42"
            Expect.isFalse (a.Equals  1) "1 does not equal 42"
        }

        test "IComparable: custom type implementing IComparable<int>" {
            let c = { new IComparable<int> with member _.CompareTo(other) = 5 - other }
            Expect.equal (sign (c.CompareTo 4)) 1  "5 > 4"
            Expect.equal (sign (c.CompareTo 5)) 0  "5 = 5"
            Expect.equal (sign (c.CompareTo 6)) -1 "5 < 6"
        }

        test "IEnumerable<int>: iterate three elements" {
            let items = [| 10; 20; 30 |]
            let mutable sum = 0
            let enum = (items :> System.Collections.Generic.IEnumerable<int>).GetEnumerator()
            while enum.MoveNext() do
                sum <- sum + enum.Current
            Expect.equal sum 60 "sum of 10+20+30 = 60"
        }

        test "ICollection<T>: array Count" {
            let col = [| 1; 2; 3; 4; 5 |] :> System.Collections.Generic.ICollection<int>
            Expect.equal col.Count 5 "array has 5 elements"
        }
    ]
