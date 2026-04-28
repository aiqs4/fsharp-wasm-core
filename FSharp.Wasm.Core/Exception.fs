/// System.Exception hierarchy for WasmGC targets.
/// Each type uses a PRIMARY constructor that takes ALL fields (inherited + own),
/// so our struct.new synthesis can map every field to a constructor arg by name.
/// Secondary constructors delegate to primary constructors.
namespace System


/// Base class: single field `message`.
type Exception(message: string) =
    inherit exn(message)
    new() = Exception("An error occurred.")
    new(message: string, _innerException: exn) = Exception(message)  // Inner exception discarded in WasmGC (no stack traces)
    member _.Message = message

/// Primary ctor: message + paramName (both fields explicit).
type ArgumentException(message: string, paramName: string) =
    inherit Exception(message)
    new(message: string) = ArgumentException(message, "")
    new()                = ArgumentException("Value does not fall within the expected range.", "")
    member _.ParamName = paramName

/// Primary ctor: message + paramName (delegates to ArgumentException).
type ArgumentNullException(message: string, paramName: string) =
    inherit ArgumentException(message, paramName)
    new(paramName: string) =
        ArgumentNullException(
            sprintf "Value cannot be null. (Parameter '%s')" paramName,
            paramName)
    new() = ArgumentNullException("Value cannot be null.", "")

/// Primary ctor: message + paramName.
type ArgumentOutOfRangeException(message: string, paramName: string) =
    inherit ArgumentException(message, paramName)
    new(paramName: string) =
        ArgumentOutOfRangeException(
            sprintf "Specified argument was out of the range of valid values. (Parameter '%s')" paramName,
            paramName)
    new() = ArgumentOutOfRangeException("Specified argument was out of the range of valid values.", "")

type FormatException(message: string) =
    inherit Exception(message)
    new() = FormatException("Input string was not in a correct format.")

type InvalidOperationException(message: string) =
    inherit Exception(message)
    new() = InvalidOperationException("Operation is not valid due to the current state of the object.")

type NotImplementedException(message: string) =
    inherit Exception(message)
    new() = NotImplementedException("The method or operation is not implemented.")

type NotSupportedException(message: string) =
    inherit Exception(message)
    new() = NotSupportedException("Specified method is not supported.")

type NullReferenceException(message: string) =
    inherit Exception(message)
    new() = NullReferenceException("Object reference not set to an instance of an object.")

type OverflowException(message: string) =
    inherit Exception(message)
    new() = OverflowException("Arithmetic operation resulted in an overflow.")

type InvalidCastException(message: string) =
    inherit Exception(message)
    new() = InvalidCastException("Specified cast is not valid.")

type IndexOutOfRangeException(message: string) =
    inherit Exception(message)
    new() = IndexOutOfRangeException("Index was outside the bounds of the array.")

type StackOverflowException(message: string) =
    inherit Exception(message)
    new() = StackOverflowException("The requested operation caused a stack overflow.")

type OutOfMemoryException(message: string) =
    inherit Exception(message)
    new() = OutOfMemoryException("Insufficient memory to continue the execution of the program.")

type AssertionException(message: string) =
    inherit Exception(message)

type KeyNotFoundException(message: string) =
    inherit Exception(message)
    new() = KeyNotFoundException("The given key was not present in the dictionary.")
