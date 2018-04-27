module TestFoxLib.FormVariation

open System
open System.IO
open FoxLib.Core
open FoxLib.FormVariation
open NUnit.Framework
open System.Numerics

//let private createRandomMaterialParamBinary (random : Random)= 
//    let makeFloat = random.NextDouble >> float32

//    let specularColor : ColorRGB = { Red = makeFloat(); Green = makeFloat(); Blue = makeFloat(); }

//    { F0 = makeFloat();
//    RoughnessThreshold = makeFloat();
//    ReflectionDependDiffuse = makeFloat();
//    AnisotropicRoughness = makeFloat();
//    SpecularColor = specularColor;
//    Translucency = makeFloat();
//    }

let private createWriteFunctions (writer : BinaryWriter) =
    { WriteUInt16 = new Action<uint16>(writer.Write);
    WriteUInt32 = new Action<uint32>(writer.Write);
    WriteUInt64 = new Action<uint64>(writer.Write);
    WriteByte = new Action<byte>(writer.Write);
    WriteBytes = new Action<byte[]>(writer.Write);
    WriteChars = new Action<char[]>(writer.Write);
    WriteEmptyBytes = new Action<int>(fun numBytes -> Array.zeroCreate<byte> numBytes |> writer.Write);
    GetWriterPosition = new Func<int64>(fun _ -> newWriter.BaseStream.Position) }

let private createReadFunctions (reader : BinaryReader) =
    let readBytes = fun num -> reader.ReadBytes num

    { ReadUInt16 = new Func<uint16>(reader.ReadUInt16);
    ReadUInt32 = new Func<uint32>(reader.ReadUInt32);
    ReadUInt64 = new Func<uint64>(reader.ReadUInt64);
    ReadByte = new Func<byte>(reader.ReadByte);
    SkipBytes = new Action<int>(fun numBytes -> reader.ReadBytes numBytes |> ignore);
    AlignStream = new Action<int64>(fun numBytes -> reader.BaseStream.Position <- numBytes ) }

//[<Test>]
//[<Category("MaterialParamBinary")>]
//let ``one random MaterialParamBinary should have original value when read`` () =
//    let random = new System.Random()
//    let randomMaterialPresets = [|1..256|]
//                                |> Array.map (fun _ -> createRandomMaterialParamBinary random)
    
//    use stream = new MemoryStream()
//    use writer = new BinaryWriter(stream)
//    createWriteFunction writer
//    |> FoxLib.MaterialParamBinary.Write randomMaterialPresets
//    |> ignore
        
//    stream.Position <- 0L

//    use reader = new BinaryReader(stream)
//    createReadFunction reader
//    |> FoxLib.MaterialParamBinary.Read
//    |> fun newMaterialPreset ->    
//        match newMaterialPreset with
//        | i when i = randomMaterialPresets -> true |> Assert.IsTrue
//        | _ -> Assert.Fail()

//    reader.Close()

[<Test>]
[<Category("FormVariation")>]
let ``read and then written "test.fv2" should have original value when read`` () =
    let baseDirectory = __SOURCE_DIRECTORY__

    let originalFilePath = Path.Combine(baseDirectory, "test.fv2")
    use originalReadStream = new FileStream(originalFilePath, FileMode.Open)
    use originalReader = new BinaryReader(originalReadStream)

    let originalFile = createReadFunctions originalReader
                       |> Read

    originalReader.Close()

    let newFilePath = Path.Combine(baseDirectory, "test repacked.fv2")
    use newWriteStream = new FileStream(newFilePath, FileMode.Create)
    use newWriter = new BinaryWriter(newWriteStream)

    createWriteFunctions newWriter
    |> Write originalFile
    |> ignore

    newWriter.Close()

    true |> Assert.True

    //use newReadStream = new FileStream(newFilePath, FileMode.Open)
    //use newReader = new BinaryReader(newReadStream)

    //let newFile = createReadFunctions newReader
    //              |> Read

    //newReader.Close()

    //(originalFile = newFile) |> Assert.IsTrue