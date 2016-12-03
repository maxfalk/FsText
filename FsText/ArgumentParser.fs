module ArgumentParser

type ArgResult = CompareFiles of string * string 
type ArgValue = { name : string; parseFunc : string list -> ArgResult Option;}

let argumentTwoFiles = {name = "-CompareFiles"; parseFunc = (fun args -> Some(CompareFiles(args.Head, args.Tail.Head)));}

let argValues = [argumentTwoFiles]

let private GetArgumentValues (argValue:ArgValue) argsList =
    match argsList with
    | [] ->
        None
    | h::t ->
        t |> argValue.parseFunc

let Parse (args:string[]) =
    let argsList = args |> Array.toList 
    argValues |> List.map (fun argValue -> argsList |> GetArgumentValues argValue)

