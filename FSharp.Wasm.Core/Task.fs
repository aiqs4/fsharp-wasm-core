/// System.Threading.Tasks — synchronous subset for WasmGC targets.
/// All tasks run synchronously. This covers FSharp.Core's async desugaring
/// on the synchronous path and direct Task.Run usage.
/// Fixes vs. original:
///   - No [<AllowNullLiteral>] (causes inheritance error in our compiler)
///   - No typeof<'T>.Name (generic erasure — Fable can't reflect on 'T)
///   - AggregateException base ctor uses two-arg Exception(msg, inner)
namespace System.Threading.Tasks

open System

// ── Task status ───────────────────────────────────────────────────────────────

type TaskStatus =
    | Created                      = 0
    | WaitingForActivation         = 1
    | WaitingToRun                 = 2
    | Running                      = 3
    | WaitingForChildrenToComplete = 4
    | RanToCompletion              = 5
    | Canceled                     = 6
    | Faulted                      = 7

// ── AggregateException ────────────────────────────────────────────────────────

/// AggregateException: wraps one or more exceptions.
/// NOTE: No [<AllowNullLiteral>] — never null in practice.
type AggregateException(innerExceptions: exn[]) =
    inherit Exception(
        (if innerExceptions.Length = 1
         then innerExceptions.[0].Message
         else
             let msgs = innerExceptions |> Array.map (fun e -> e.Message) |> String.concat "; "
             sprintf "One or more errors occurred. (%s)" msgs),
        if innerExceptions.Length = 1 then innerExceptions.[0] else null)
    new(ex: exn)          = AggregateException([| ex |])
    new(msg: string, exns: exn[]) = AggregateException(exns)
    member _.InnerExceptions = innerExceptions

// ── Non-generic Task ──────────────────────────────────────────────────────────

/// Non-generic Task: represents a synchronous void operation.
/// NOTE: No [<AllowNullLiteral>] — never null in practice.
type Task(action: unit -> unit) =

    let mutable status : TaskStatus = TaskStatus.Created
    let mutable exn'   : exn        = null

    new(action: unit -> unit, _state: obj) = Task(action)

    member _.Status      = status
    member _.IsCompleted = status = TaskStatus.RanToCompletion
                        || status = TaskStatus.Faulted
                        || status = TaskStatus.Canceled
    member _.IsFaulted   = status = TaskStatus.Faulted
    member _.IsCanceled  = status = TaskStatus.Canceled
    member _.Exception   = if exn' = null then null else AggregateException(exn')

    member this.Start() =
        status <- TaskStatus.Running
        try
            action ()
            status <- TaskStatus.RanToCompletion
        with ex ->
            exn'   <- ex
            status <- TaskStatus.Faulted

    member this.Wait() =
        if not this.IsCompleted then this.Start()
        if status = TaskStatus.Faulted then
            raise (AggregateException(exn'))

    member this.RunSynchronously() = this.Wait()

    static member Run(action: unit -> unit) : Task =
        let t = Task(action)
        t.Start()
        t

    static member val CompletedTask : Task =
        let t = Task(fun () -> ())
        t.Start()
        t

    static member Delay(_ms: int) : Task = Task.CompletedTask

    // TODO: use seq when available
    // static member WhenAll(tasks: Task seq) : Task =
    static member WhenAll(tasks: Task[]) : Task =
        Task(fun () -> for t in tasks do t.Wait())

    static member WhenAll(tasks: Task list) : Task =
        Task(fun () -> for t in tasks do t.Wait())

    // static member WhenAny(tasks: Task seq) : Task =
    //     Task(fun () ->
    //         let arr = Array.ofSeq tasks
    //         if arr.Length > 0 then arr.[0].Wait())

     // TODO: use seq when available
    // static member WhenAny(tasks: Task seq) : Task =
    static member WhenAny(tasks: Task[]) : Task =
        Task(fun () -> if tasks.Length > 0 then tasks.[0].Wait())

    static member FromException(ex: exn) : Task =
        let t = Task(fun () -> raise ex)
        t.Start()
        t

    override _.ToString() = sprintf "Task[%A]" status

// ── Generic Task<T> ───────────────────────────────────────────────────────────

/// Generic Task<T>: represents a synchronous operation that returns a value.
/// NOTE: No [<AllowNullLiteral>] — never null in practice.
type Task<'T>(func: unit -> 'T) =

    let mutable status : TaskStatus = TaskStatus.Created
    let mutable result : 'T         = Unchecked.defaultof<'T>
    let mutable exn'   : exn        = null

    new(func: unit -> 'T, _state: obj) = Task<'T>(func)

    member _.Status      = status
    member _.IsCompleted = status = TaskStatus.RanToCompletion
                        || status = TaskStatus.Faulted
                        || status = TaskStatus.Canceled
    member _.IsFaulted   = status = TaskStatus.Faulted
    member _.IsCanceled  = status = TaskStatus.Canceled
    member _.Exception   = if exn' = null then null else AggregateException(exn')

    member this.Result : 'T =
        if not this.IsCompleted then this.Start()
        if status = TaskStatus.Faulted then raise (AggregateException(exn'))
        result

    member this.Start() =
        status <- TaskStatus.Running
        try
            result <- func ()
            status <- TaskStatus.RanToCompletion
        with ex ->
            exn'   <- ex
            status <- TaskStatus.Faulted

    member this.Wait() =
        if not this.IsCompleted then this.Start()
        if status = TaskStatus.Faulted then raise (AggregateException(exn'))

    member this.RunSynchronously() = this.Wait()

    member this.ContinueWith(cont: Task<'T> -> unit) : Task =
        Task(fun () ->
            this.Wait()
            cont this)

    member this.ContinueWith(cont: Task<'T> -> 'U) : Task<'U> =
        Task<'U>(fun () ->
            this.Wait()
            cont this)

    static member Run(func: unit -> 'T) : Task<'T> =
        let t = Task<'T>(func)
        t.Start()
        t

    static member FromResult(value: 'T) : Task<'T> =
        Task<'T>.Run(fun () -> value)

    static member FromException(ex: exn) : Task<'T> =
        let t = Task<'T>(fun () -> raise ex)
        t.Start()
        t

    override _.ToString() = "Task"   // No typeof<'T>.Name — generic erasure
    // override _.ToString() = sprintf "Task[%A]" status

// ── ValueTask<T> (minimal stub) ──────────────────────────────────────────────

[<Struct>]
type ValueTask<'T>(result: 'T) =
    member _.IsCompleted = true
    member _.Result      = result
    member _.AsTask()    = Task<'T>.FromResult(result)

[<Struct>]
type ValueTask(isCompleted: bool) =
    member _.IsCompleted = isCompleted
    member _.AsTask()    = Task.CompletedTask
