namespace FsCompare
open System.IO
open System

type FileCompareResult = 
    {filePosition : int; 
    lineNumber : int; 
    lineCharPosition : int; 
    haveChar: char; 
    expectedChar: char;}

module Compare =

    let private ArrayByteCompare a1 a2 =
        Array.compareWith (fun b1 b2 -> if b1 = b2 then 0 else 1) a1 a2

    let private AsyncArrayByteCompare a1 a2 =
        async{
            return (Array.compareWith (fun b1 b2 -> if b1 = b2 then 0 else 1) a1 a2) = 0
        }

    let private StreamReadBlock (s:FileStream) blockSize =
        let readBuffer = Array.zeroCreate(blockSize)
        let bytesRead = s.Read(readBuffer, 0, blockSize)
        (readBuffer, bytesRead)

    let rec private CompareStreamHelper (s1:FileStream) (s2:FileStream) blockSize (length:int64) =
        let (s1Buffer, s1Read) = StreamReadBlock s1 blockSize
        let (s2Buffer, s2Read) = StreamReadBlock s2 blockSize
        if length = 0L then
            true
        else if (ArrayByteCompare s1Buffer s2Buffer) = 0 then
            let newBlockSize = if int64(blockSize) > length then length else int64(blockSize)
            CompareStreamHelper s1 s2 (int32(newBlockSize)) (length - newBlockSize)
        else
            false
        
    //Byte comparison that is sequencial
    let CompareStream (p1) (p2) =
        let s1 = File.OpenRead(p1)
        let s2 = File.OpenRead(p2)
        let blockSize = 1024
        let result = CompareStreamHelper s1 s2 blockSize (if s1.Length < s2.Length then s1.Length else s2.Length)
        s1.Close()
        s2.Close()
        result

    
    let private AsyncReadAndCompareBlock (s1:FileStream) (s2:FileStream) blockSize =
        async{
            let (s1Buffer, s1Read) = StreamReadBlock s1 blockSize
            let (s2Buffer, s2Read) = StreamReadBlock s2 blockSize
            return! AsyncArrayByteCompare s1Buffer s2Buffer       
        }


    let rec private AsyncCompareStreamHelper (s1:FileStream) (s2:FileStream) (blockSize:int64) (length:int64) =
        let newBlockSize = if int64(blockSize) > length then length else int64(blockSize)
        let newLength = (length - newBlockSize)
        if newBlockSize > 0L then
            AsyncReadAndCompareBlock s1 s2 (int32(newBlockSize)) 
                :: (AsyncCompareStreamHelper s1 s2 newBlockSize newLength)
        else
            []

    //ByteComparison with async write and compare
    let AsyncCompareStream p1 p2 =
        let s1 = File.OpenRead(p1)
        let s2 = File.OpenRead(p2)
        let length = (if s1.Length < s2.Length then s1.Length else s2.Length)
        let blockSize = length / 100L
        let AsyncResult = (AsyncCompareStreamHelper s1 s2 blockSize length) |> Async.Parallel |> Async.RunSynchronously
        let result = AsyncResult |> Array.exists (fun elm -> not elm)
        s1.Close()
        s2.Close()
        result      


    let FindDiffCharPositions (l1:char array) (l2:char array) =
        l1 |> Array.mapi2 (fun i e1 e2 -> (e1 <> e2, e1, e2, i)) l2 
        |> Array.fold (fun state e1 -> match e1 with (true, v1, v2, i) -> (v1, v2, i)::state | (false, v1, v2, i) -> state) []

    let rec ReadAndCompareLines (s1:StreamReader) (s2:StreamReader) diffs =
        let l1 = s1.ReadLine()
        let l2 = s2.ReadLine()
        if (l1.Length <= 0) || (l2.Length <= 0) then
            diffs
        else if l1.CompareTo(l2) = 0 then
            ReadAndCompareLines s1 s2 diffs
        else
            let diff = (FindDiffCharPositions (l1.ToCharArray()) (l2.ToCharArray()))
            let TupleToFileCompare = (fun e -> match e with (v1, v2, i) -> {filePosition = 0; lineNumber = 0; lineCharPosition = i; haveChar = v1; expectedChar = v2;})
            let diffRec = diff |> List.map TupleToFileCompare                           
            (ReadAndCompareLines s1 s2 (diffRec |> List.append diffs))

    //Compare File reading line by line
    let StringCompare (p1:string) (p2:string) =
        let s1 = new StreamReader(p1);
        let s2 = new StreamReader(p2);
        let result = ReadAndCompareLines s1 s2 []
        s1.Close()
        s2.Close()
        result