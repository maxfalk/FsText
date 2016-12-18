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


let CompareStringFiles f1 f2 =
    let result = Compare.StringCompare f1 f2
    match result with
    | [] ->
        printfn "File match"
    | l ->
        printfn "File did not match"
        l |> List.map (fun e -> printfn "%d, %d, %s, %s, %d" e.lineNumber, e.lineCharPosition, e.haveChar, e.expectedChar, e.filePosition) |> ignore

let SimpleCompareFiles f1 f2 =
    let result = SimpleCompare.Compare {file1 = f1; file2 = f2}
    match result with
    | SimpleCompare.MATCH ->
        printfn "File match"
    | SimpleCompare.NOMATCH(l) ->
        l |> List.map (fun e ->
            printfn "==== DIFF ====" 
            printfn "lineNr: %d" e.lineNumber
            printfn "lineCharPos: %d" e.lineCharPosition
            printfn "char1: %c" e.haveChar
            printfn "char2: %c" e.expectedChar
            printfn "filePos: %d" e.filePosition ) |> ignore

[<EntryPoint>]
[<STAThread>]
let main argv = 
    let args = argv |> ArgumentParser.Parse
    [for arg in args do
        match arg with
        | Some(CompareFiles(f1, f2)) ->
               SimpleCompareFiles f1 f2
        | None ->
            ()
    ] |> ignore
    0 // return an integer exit code
