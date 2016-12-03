open FsCompare
open ArgumentParser
open System

let Duration f =
    let sw = System.Diagnostics.Stopwatch.StartNew()
    let r = f()
    sw.Stop()
    printfn "%O" sw.Elapsed
    r

let CompareFiles f1 f2 =
    let result = FsCompare.Compare.CompareStream f1 f2
    match result with
    | true ->
        printfn "File match"
    | false ->
        printfn "File did not match"

let AsyncCompareFiles f1 f2 =
    let result = FsCompare.Compare.AsyncCompareStream f1 f2
    match result with
    | true ->
        printfn "File match"
    | false ->
        printfn "File did not match"


[<EntryPoint>]
let main argv = 
    let args = argv |> ArgumentParser.Parse
    [for arg in args do
        match arg with
        | Some(CompareFiles(f1, f2)) ->
             Duration (fun () -> CompareFiles f1 f2)
             Duration (fun () -> AsyncCompareFiles f1 f2)
        | None ->
            ()
    ] |> ignore

    Console.ReadKey();
    0 // return an integer exit code
