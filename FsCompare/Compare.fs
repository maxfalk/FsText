namespace FsCompare
open System.IO
open System

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
        

    let CompareStream (p1) (p2) =
        let s1 = File.OpenRead(p1)
        let s2 = File.OpenRead(p2)
        let blockSize = 1024
        let result = CompareStreamHelper s1 s2 blockSize (if s1.Length < s2.Length then s1.Length else s2.Length)
        s1.Close()
        s2.Close()
        result

    let AsyncReadAndCompareBlock (s1:FileStream) (s2:FileStream) blockSize =
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