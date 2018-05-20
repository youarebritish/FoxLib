module DataSetFile2

open System.IO
open FoxLib.DataSetFile2
open System
open NUnit.Framework
open FoxLib

let private alignRead (stream : Stream) alignment =
    let alignmentRequired = stream.Position % alignment
    
    stream.Position <- if alignmentRequired > 0L
                        then stream.Position + alignment - alignmentRequired
                        else stream.Position

let private readString (reader : BinaryReader) count =
    new string (reader.ReadChars (int count))

let private createReadFunctions (reader : BinaryReader) =
    { ReadFunctions.ReadSingle = new Func<float32>(reader.ReadSingle);
    ReadInt8 = new Func<int8>(reader.ReadSByte);
    ReadUInt8 = new Func<uint8>(reader.ReadByte);
    ReadInt16 = new Func<int16>(reader.ReadInt16);
    ReadUInt16 = new Func<uint16>(reader.ReadUInt16);
    ReadInt32 = new Func<int32>(reader.ReadInt32);
    ReadUInt32 = new Func<uint32>(reader.ReadUInt32);
    ReadInt64 = new Func<int64>(reader.ReadInt64);
    ReadUInt64 = new Func<uint64>(reader.ReadUInt64);
    ReadDouble = new Func<float>(reader.ReadDouble);
    ReadBool = new Func<bool>(reader.ReadBoolean);
    ReadString = new Func<uint32, string>(fun length -> readString reader length);
    SkipBytes = new Action<int>(fun numBytes -> reader.ReadBytes numBytes |> ignore);
    AlignRead = new Action<int>(fun alignment -> alignRead reader.BaseStream (int64 alignment)); }

[<Test>]
[<Category("DataSetFile2")>]
let ``vanilla DataSetFile2 should repack with original contents`` () =
    let baseDirectory = __SOURCE_DIRECTORY__
    let inputFilePath = "test.fox2"
    let inputFullPath = Path.Combine(baseDirectory, inputFilePath)
    use inputStream = new FileStream(inputFullPath, FileMode.Open)
    use reader = new BinaryReader(inputStream)

    let readFunctions = createReadFunctions reader

    let dataSet = DataSetFile2.Read readFunctions
    
    Assert.IsTrue true