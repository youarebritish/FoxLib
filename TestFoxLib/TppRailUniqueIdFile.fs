module TestFoxLib.Rail.TppRailUniqueIdFile

open NUnit.Framework
open System.IO
open FoxLib.Tpp
open System
open FoxLib.Tpp.RailUniqueIdFile

let private createWriteFunctions (writer : BinaryWriter) =
    { WriteFunctions.WriteChar = new Action<char>(writer.Write);
    WriteUInt16 = new Action<uint16>(writer.Write);
    WriteUInt32 = new Action<uint32>(writer.Write); }

let private createReadFunctions (reader : BinaryReader) =
    { ReadUInt16 = new Func<uint16>(reader.ReadUInt16);
    ReadUInt32 = new Func<uint32>(reader.ReadUInt32);
    SkipBytes = new Action<int>(fun numBytes -> reader.ReadBytes numBytes |> ignore) }
        
[<Test>]
[<Category("TppRailUniqueIdFile")>]
let ``empty TppRailUniqueIdFile should be empty when read`` () =
    use stream = new MemoryStream()
    use writer = new BinaryWriter(stream)    
    RailUniqueIdFile.Write (createWriteFunctions writer) [||] |> ignore
    
    stream.Position <- 0L
    
    use reader = new BinaryReader(stream)
    createReadFunctions reader
    |> RailUniqueIdFile.Read
    |> Seq.isEmpty
    |> Assert.IsTrue

[<Test>]
[<Category("TppRailUniqueIdFile")>]
let ``TppRailUniqueIdFile with one entry should be then same when read`` () =
    use stream = new MemoryStream()
    use writer = new BinaryWriter(stream)    
    RailUniqueIdFile.Write (createWriteFunctions writer) [|93214u|] |> ignore
    
    stream.Position <- 0L
    
    use reader = new BinaryReader(stream)
    createReadFunctions reader
    |> RailUniqueIdFile.Read
    |> (fun ids -> Seq.length ids = 1 && Seq.item 0 ids = 93214u)
    |> Assert.IsTrue

[<Test>]
[<Category("TppRailUniqueIdFile")>]
let ``TppRailUniqueIdFile with 99 entries should be then same when read`` () =
    use stream = new MemoryStream()
    use writer = new BinaryWriter(stream)    

    let rand = System.Random()
    let ids = Array.init 99 (fun _ -> rand.Next () |> uint32)

    RailUniqueIdFile.Write (createWriteFunctions writer) ids |> ignore
    
    stream.Position <- 0L
    
    use reader = new BinaryReader(stream)
    createReadFunctions reader
    |> RailUniqueIdFile.Read
    |> (fun loadedIds -> Seq.map2 (fun a b -> a = b) loadedIds ids)
    |> Seq.contains false
    |> Assert.IsFalse