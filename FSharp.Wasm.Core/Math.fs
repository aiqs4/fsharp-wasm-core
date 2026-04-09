/// System.Math for WasmGC targets.
/// Pure-F# implementations of the mathematical functions FSharp.Core depends on.
/// On WasmGC these lower to the corresponding Wasm numeric instructions or
/// wasm32-wasi libm calls.
namespace System

[<AbstractClass; Sealed>]
type Math private () =

    // Constants
    static member val E  : float = 2.718281828459045
    static member val PI : float = 3.141592653589793
    static member val Tau: float = 6.283185307179586

    // Abs
    static member Abs(value: int8)    : int8    = if value < 0y then -value else value
    static member Abs(value: int16)   : int16   = if value < 0s then -value else value
    static member Abs(value: int)     : int     = if value < 0  then -value else value
    static member Abs(value: int64)   : int64   = if value < 0L then -value else value
    static member Abs(value: float32) : float32 = if value < 0.0f then -value else value
    static member Abs(value: float)   : float   = if value < 0.0  then -value else value

    // Sign
    static member Sign(value: int8)    : int = if value > 0y then 1 elif value < 0y then -1 else 0
    static member Sign(value: int16)   : int = if value > 0s then 1 elif value < 0s then -1 else 0
    static member Sign(value: int)     : int = if value > 0  then 1 elif value < 0  then -1 else 0
    static member Sign(value: int64)   : int = if value > 0L then 1 elif value < 0L then -1 else 0
    static member Sign(value: float32) : int = if value > 0.0f then 1 elif value < 0.0f then -1 else 0
    static member Sign(value: float)   : int = if value > 0.0  then 1 elif value < 0.0  then -1 else 0

    // Min
    static member Min(x: int8,    y: int8)    = if x < y then x else y
    static member Min(x: int16,   y: int16)   = if x < y then x else y
    static member Min(x: int,     y: int)     = if x < y then x else y
    static member Min(x: int64,   y: int64)   = if x < y then x else y
    static member Min(x: uint8,   y: uint8)   = if x < y then x else y
    static member Min(x: uint16,  y: uint16)  = if x < y then x else y
    static member Min(x: uint32,  y: uint32)  = if x < y then x else y
    static member Min(x: uint64,  y: uint64)  = if x < y then x else y
    static member Min(x: float32, y: float32) = if x < y then x else y
    static member Min(x: float,   y: float)   = if x < y then x else y

    // Max
    static member Max(x: int8,    y: int8)    = if x > y then x else y
    static member Max(x: int16,   y: int16)   = if x > y then x else y
    static member Max(x: int,     y: int)     = if x > y then x else y
    static member Max(x: int64,   y: int64)   = if x > y then x else y
    static member Max(x: uint8,   y: uint8)   = if x > y then x else y
    static member Max(x: uint16,  y: uint16)  = if x > y then x else y
    static member Max(x: uint32,  y: uint32)  = if x > y then x else y
    static member Max(x: uint64,  y: uint64)  = if x > y then x else y
    static member Max(x: float32, y: float32) = if x > y then x else y
    static member Max(x: float,   y: float)   = if x > y then x else y

    // Clamp
    static member Clamp(value: int,     min: int,     max: int)     = if value < min then min elif value > max then max else value
    static member Clamp(value: int64,   min: int64,   max: int64)   = if value < min then min elif value > max then max else value
    static member Clamp(value: float32, min: float32, max: float32) = if value < min then min elif value > max then max else value
    static member Clamp(value: float,   min: float,   max: float)   = if value < min then min elif value > max then max else value

    // Rounding — delegate to the host runtime (or Wasm f64.floor / f64.ceil / f64.trunc / f64.nearest)
    static member Floor(value: float)    : float   = floor value
    static member Floor(value: float32)  : float32 = float32 (floor (float value))
    static member Ceiling(value: float)  : float   = ceil value
    static member Ceiling(value: float32): float32 = float32 (ceil (float value))
    static member Truncate(value: float) : float   = truncate value
    static member Truncate(value: float32): float32 = float32 (truncate (float value))
    static member Round(value: float)    : float   = System.Math.Round(value)
    static member Round(value: float32)  : float32 = float32 (System.Math.Round(float value))
    static member Round(value: float, digits: int) : float = System.Math.Round(value, digits)

    // Sqrt / Cbrt / Pow / Exp / Log
    static member Sqrt(value: float)    : float   = sqrt value
    static member Sqrt(value: float32)  : float32 = sqrt value
    static member Pow(x: float, y: float): float  = x ** y
    static member Exp(value: float)     : float   = exp value
    static member Log(value: float)     : float   = log value
    static member Log(value: float, newBase: float): float = log value / log newBase
    static member Log2(value: float)    : float   = log value / log 2.0
    static member Log10(value: float)   : float   = log10 value

    // Trig
    static member Sin(value: float)     : float = sin value
    static member Cos(value: float)     : float = cos value
    static member Tan(value: float)     : float = tan value
    static member Asin(value: float)    : float = asin value
    static member Acos(value: float)    : float = acos value
    static member Atan(value: float)    : float = atan value
    static member Atan2(y: float, x: float): float = atan2 y x
    static member Sinh(value: float)    : float = sinh value
    static member Cosh(value: float)    : float = cosh value
    static member Tanh(value: float)    : float = tanh value

    // Integer helpers
    static member DivRem(a: int,   b: int,   remainder: byref<int>)   =
        remainder <- a % b; a / b
    static member DivRem(a: int64, b: int64, remainder: byref<int64>) =
        remainder <- a % b; a / b

    // Bit operations
    static member BitDecrement(x: float) : float = System.Math.BitDecrement(x)
    static member BitIncrement(x: float) : float = System.Math.BitIncrement(x)

    // Infinity / NaN checks
    static member IsNaN(value: float)      : bool = System.Double.IsNaN(value)
    static member IsInfinity(value: float) : bool = System.Double.IsInfinity(value)
    static member IsPositiveInfinity(value: float): bool = System.Double.IsPositiveInfinity(value)
    static member IsNegativeInfinity(value: float): bool = System.Double.IsNegativeInfinity(value)