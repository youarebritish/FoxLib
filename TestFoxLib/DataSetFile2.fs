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

let private writeZeroes (writer : BinaryWriter) (count : uint32) =
    let (zeroes : byte[]) = Array.zeroCreate (int count)
    writer.Write(zeroes)

let private alignWrite (output : Stream) (alignment : int) data =
    let alignmentRequired = output.Position % (int64 alignment)
    if alignmentRequired > 0L then let alignmentBytes = Seq.toArray (System.Linq.Enumerable.Repeat(data, (int)((int64 alignment) - alignmentRequired)))
                                   output.Write(alignmentBytes, 0, alignmentBytes.Length)

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
    GetStreamPosition = new Func<int64>(fun() -> reader.BaseStream.Position);
    SetStreamPosition = new Action<int64>(fun newPosition -> reader.BaseStream.Position <- newPosition);
    SkipBytes = new Action<int>(fun numBytes -> reader.ReadBytes numBytes |> ignore);
    AlignRead = new Action<int>(fun alignment -> alignRead reader.BaseStream (int64 alignment)); }

let private createWriteFunctions (writer : BinaryWriter) =
    { WriteFunctions.WriteBool = new Action<bool>(writer.Write);
    WriteFunctions.WriteInt8 = new Action<int8>(writer.Write);
    WriteFunctions.WriteUInt8 = new Action<uint8>(writer.Write);
    WriteFunctions.WriteInt16 = new Action<int16>(writer.Write);
    WriteFunctions.WriteUInt16 = new Action<uint16>(writer.Write);
    WriteFunctions.WriteInt32 = new Action<int32>(writer.Write);
    WriteFunctions.WriteUInt32 = new Action<uint32>(writer.Write);
    WriteFunctions.WriteInt64 = new Action<int64>(writer.Write);
    WriteFunctions.WriteUInt64 = new Action<uint64>(writer.Write);
    WriteFunctions.WriteSingle = new Action<float32>(writer.Write);
    WriteFunctions.WriteDouble = new Action<double>(writer.Write);
    WriteFunctions.WriteBytes = new Action<byte[]>(writer.Write);
    WriteFunctions.GetStreamPosition = new Func<int64>((fun _ -> writer.BaseStream.Position));
    WriteFunctions.SetStreamPosition = new Action<int64>((fun position -> writer.BaseStream.Position <- position));
    WriteFunctions.WriteZeroes = new Action<uint32>(writeZeroes writer);
    WriteFunctions.AlignWrite = new Action<int, byte>((fun alignment data -> alignWrite writer.BaseStream alignment data)) }

[<Test>]
[<Category("DataSetFile2")>]
let ``vanilla DataSetFile2 should repack with original contents`` () =
    let baseDirectory = __SOURCE_DIRECTORY__
    let inputFilePath = "mtbs_ly003_cl00_item.fox2"
    let inputFullPath = Path.Combine(baseDirectory, inputFilePath)
    use inputStream = new FileStream(inputFullPath, FileMode.Open)
    use reader = new BinaryReader(inputStream)
    let readFunctions = createReadFunctions reader

    let dataSet = DataSetFile2.Read readFunctions

    reader.Close()
    inputStream.Close()
    
    let outputFilePath = "mtbs_ly003_cl00_item_REPACKED.fox2"
    let outputFullPath = Path.Combine(baseDirectory, outputFilePath)
    use outputStream = new FileStream(outputFullPath, FileMode.OpenOrCreate)
    use writer = new BinaryWriter(outputStream)
    let writeFunctions = createWriteFunctions writer

    DataSetFile2.Write dataSet writeFunctions

    writer.Close()
    outputStream.Close()

    use inputStream2 = new FileStream(outputFullPath, FileMode.Open)
    use reader2 = new BinaryReader(inputStream2)
    let readFunctions2 = createReadFunctions reader2

    let dataSet2 = DataSetFile2.Read readFunctions2
    
    Assert.IsTrue true