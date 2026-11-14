/// System.Exception hierarchy for WasmGC targets.
/// Pure-F# implementations of the exception types FSharp.Core depends on,
/// designed to compile cleanly to WasmGC struct/tag layouts.
namespace System
/// Base class for all exceptions in the BCL subset.
type Exception(message: string, innerException: exn) =
    new(message: string) = Exception(message, null)
    new() = Exception("An error occurred.")
    member _.Message = message
    member _.InnerException = innerException
    override _.ToString() = message
/// Raised when a method is called with an invalid argument.
type ArgumentException(message: string, paramName: string, innerException: exn) =
    inherit Exception(message, innerException)
    new(message: string, paramName: string) = ArgumentException(message, paramName, null)
    new(message: string) = ArgumentException(message, null, null)
    new() = ArgumentException("Value does not fall within the expected range.")
    member _.ParamName = paramName
/// Raised when a null argument is passed to a method that does not accept it.
type ArgumentNullException(paramName: string, message: string) =
    inherit ArgumentException(message, paramName)
    new(paramName: string) = ArgumentNullException(paramName, sprintf "Value cannot be null. (Parameter '%s')" paramName)
    new() = ArgumentNullException(null, "Value cannot be null.")
/// Raised when an argument value is out of the allowed range.
type ArgumentOutOfRangeException(paramName: string, message: string) =
    inherit ArgumentException(message, paramName)
    new(paramName: string) = ArgumentOutOfRangeException(paramName, sprintf "Specified argument was out of the range of valid values. (Parameter '%s')" paramName)
    new() = ArgumentOutOfRangeException(null, "Specified argument was out of the range of valid values.")
/// Raised when the format of an argument is invalid.
type FormatException(message: string, innerException: exn) =
    inherit Exception(message, innerException)
    new(message: string) = FormatException(message, null)
    new() = FormatException("Input string was not in a correct format.")
/// Raised when an operation is not valid for the object's current state.
type InvalidOperationException(message: string, innerException: exn) =
    inherit Exception(message, innerException)
    new(message: string) = InvalidOperationException(message, null)
    new() = InvalidOperationException("Operation is not valid due to the current state of the object.")
/// Raised when a requested method or operation is not implemented.
type NotImplementedException(message: string, innerException: exn) =
    inherit Exception(message, innerException)
    new(message: string) = NotImplementedException(message, null)
    new() = NotImplementedException("The method or operation is not implemented.")
/// Raised when an invoked method is not supported.
type NotSupportedException(message: string, innerException: exn) =
    inherit Exception(message, innerException)
    new(message: string) = NotSupportedException(message, null)
    new() = NotSupportedException("Specified method is not supported.")
/// Raised when there is an attempt to dereference a null reference.
type NullReferenceException(message: string, innerException: exn) =
    inherit Exception(message, innerException)
    new(message: string) = NullReferenceException(message, null)
    new() = NullReferenceException("Object reference not set to an instance of an object.")
/// Raised when an arithmetic, casting, or conversion operation results in overflow.
type OverflowException(message: string, innerException: exn) =
    inherit Exception(message, innerException)
    new(message: string) = OverflowException(message, null)
    new() = OverflowException("Arithmetic operation resulted in an overflow.")
/// Raised when a cast or explicit conversion is not valid.
type InvalidCastException(message: string, innerException: exn) =
    inherit Exception(message, innerException)
    new(message: string) = InvalidCastException(message, null)
    new() = InvalidCastException("Specified cast is not valid.")
/// Raised when an index is outside the bounds of an array or collection.
type IndexOutOfRangeException(message: string, innerException: exn) =
    inherit Exception(message, innerException)
    new(message: string) = IndexOutOfRangeException(message, null)
    new() = IndexOutOfRangeException("Index was outside the bounds of the array.")
/// Raised when the execution stack overflows.
type StackOverflowException(message: string, innerException: exn) =
    inherit Exception(message, innerException)
    new(message: string) = StackOverflowException(message, null)
    new() = StackOverflowException("The requested operation caused a stack overflow.")
/// Raised when there is not enough memory to continue the execution of a program.
type OutOfMemoryException(message: string, innerException: exn) =
    inherit Exception(message, innerException)
    new(message: string) = OutOfMemoryException(message, null)
    new() = OutOfMemoryException("Insufficient memory to continue the execution of the program.")
/// Raised by a failed assertion, or when an explicitly failed operation occurs.
type AssertionException(message: string, innerException: exn) =
    inherit Exception(message, innerException)
    new(message: string) = AssertionException(message, null)
/// Raised when a key is not found in a dictionary.
type KeyNotFoundException(message: string, innerException: exn) =
    inherit Exception(message, innerException)
    new(message: string) = KeyNotFoundException(message, null)
    new() = KeyNotFoundException("The given key was not present in the dictionary.")
namespace System.Collections.Generic
/// Raised when a key is not found in a dictionary.
type KeyNotFoundException(message: string, innerException: exn) =
    inherit System.Exception(message, innerException)
    new(message: string) = KeyNotFoundException(message, null)
    new() = KeyNotFoundException("The given key was not present in the dictionary.")
