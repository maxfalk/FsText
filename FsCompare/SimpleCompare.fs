module SimpleCompare
open System.IO
open System

type Input = {file1: string; file2:string}

type Difference = {filePosition : int; 
                lineNumber : int; 
                lineCharPosition : int; 
                haveChar: char; 
                expectedChar: char;}

type Output = MATCH | NOMATCH of Difference list

let rec private FindLineDifferences (l1:char array) (l2:char array) pos length line rootFilePos diffList =
    if pos < length then
        if l1.[pos] <> l2.[pos] then
            let newDiff = {filePosition = rootFilePos + pos; lineNumber = line; lineCharPosition = pos; haveChar = l1.[pos]; expectedChar = l2.[pos]}
            FindLineDifferences l1 l2 (pos+1) length line rootFilePos (newDiff::diffList)
        else
            FindLineDifferences l1 l2 (pos+1) length line rootFilePos diffList
    else
        diffList

let private LineComparer (l1:string) l2 line rootFilePos currDiffs =    
    if l1.CompareTo(l2) = 0 then
        (true, currDiffs)       
    else
        let l1CharAry = l1.ToCharArray()
        let l2CharAry = l2.ToCharArray() 
        let diffList = FindLineDifferences l1CharAry l2CharAry 0 (min l1CharAry.Length l2CharAry.Length) line rootFilePos currDiffs
        (false, diffList)
                

let rec private CompleteLineCompare (f1Contents:string array) (f2Contents:string array) filePosition pos length (result:Output) =
    if pos < length then
        let newfilePosition = pos + f1Contents.Length
        match result with
        | MATCH ->
            let (matched, diffs) = LineComparer f1Contents.[pos] f2Contents.[pos] pos filePosition []
            match matched with
            | true ->
                CompleteLineCompare f1Contents f2Contents (newfilePosition) (pos + 1) length MATCH
            | false ->
                CompleteLineCompare f1Contents f2Contents (newfilePosition) (pos + 1) length (NOMATCH(diffs))
        | NOMATCH(currentDiffs) ->
            let (matched, diffs) = LineComparer f1Contents.[pos] f2Contents.[pos] pos filePosition currentDiffs
            CompleteLineCompare f1Contents f2Contents (newfilePosition) (pos + 1) length (NOMATCH(diffs))
    else
        result

let Compare (input:Input) =
    let file1Contents = File.ReadAllLines input.file1
    let file2Contents = File.ReadAllLines input.file2
    CompleteLineCompare file1Contents file2Contents 0 0 (min file1Contents.Length file2Contents.Length) MATCH
    


    