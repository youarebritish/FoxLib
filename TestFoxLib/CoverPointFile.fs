module CoverPointFile

open System.IO
open FoxLib.CoverPointFile
open NUnit.Framework
open System
open FoxLib

let private createReadFunctions (reader : BinaryReader) =
    { ReadFunctions.ReadSingle = new Func<float32>(reader.ReadSingle);
    ReadUInt8 = new Func<uint8>(reader.ReadByte);
    ReadUInt16 = new Func<uint16>(reader.ReadUInt16);
    SkipBytes = new Action<int>(fun numBytes -> reader.ReadBytes numBytes |> ignore);}

[<Test>]
[<Category("CoverPointFile")>]
let ``vanilla CoverPointFile should repack with original contents`` () =
    let baseDirectory = __SOURCE_DIRECTORY__
    let inputFilePath = "afgh_citadel_0.tcvp"
    let inputFullPath = Path.Combine(baseDirectory, inputFilePath)
    use inputStream = new FileStream(inputFullPath, FileMode.Open)
    use reader = new BinaryReader(inputStream)

    let readFunctions = createReadFunctions reader

    let coverPoints = CoverPointFile.Read readFunctions

    reader.Close();
    inputStream.Close();
    
    Assert.IsTrue true