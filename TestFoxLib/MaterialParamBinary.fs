module TestFoxLib.MaterialParamBinary

open System
open System.IO
open FoxLib.Core
open FoxLib.MaterialParamBinary
open NUnit.Framework

let private createRandomMaterialParamBinary (random : Random)= 
    let makeFloat = random.NextDouble >> float32

    let specularColor : ColorRGB = { Red = makeFloat(); Green = makeFloat(); Blue = makeFloat(); }

    { F0 = makeFloat();
    RoughnessThreshold = makeFloat();
    ReflectionDependDiffuse = makeFloat();
    AnisotropicRoughness = makeFloat();
    SpecularColor = specularColor;
    Translucency = makeFloat();
    }

let private createWriteFunction (writer : BinaryWriter) =
    { WriteFunction.WriteSingle = new Action<float32>(writer.Write); }

let private createReadFunction (reader : BinaryReader) =
    { ReadFunction.ReadSingle = new Func<float32>(reader.ReadSingle); }

[<Test>]
[<Category("MaterialParamBinary")>]
let ``one random MaterialParamBinary should have original value when read`` () =
    let random = new System.Random()
    let randomMaterialPresets = [|1..256|]
                                |> Array.map (fun _ -> createRandomMaterialParamBinary random)
    
    use stream = new MemoryStream()
    use writer = new BinaryWriter(stream)
    createWriteFunction writer
    |> FoxLib.MaterialParamBinary.Write randomMaterialPresets
    |> ignore
        
    stream.Position <- 0L

    use reader = new BinaryReader(stream)
    createReadFunction reader
    |> FoxLib.MaterialParamBinary.Read
    |> fun newMaterialPreset ->    
        match newMaterialPreset with
        | i when i = randomMaterialPresets -> true |> Assert.IsTrue
        | _ -> Assert.Fail()

    reader.Close()

[<Test>]
[<Category("MaterialParamBinary")>]
let ``read and then written "test.fmtt" should have original value when read`` () =
    let baseDirectory = __SOURCE_DIRECTORY__

    let originalFilePath = Path.Combine(baseDirectory, "test.fmtt")
    use originalReadStream = new FileStream(originalFilePath, FileMode.Open)
    use originalReader = new BinaryReader(originalReadStream)
    
    let originalFile = createReadFunction originalReader
                       |> Read

    originalReader.Close()

    let newFilePath = Path.Combine(baseDirectory, "test repacked.fmtt")
    use newWriteStream = new FileStream(newFilePath, FileMode.Create)
    use newWriter = new BinaryWriter(newWriteStream)

    createWriteFunction newWriter
    |> Write originalFile
    |> ignore

    newWriter.Close()

    use newReadStream = new FileStream(newFilePath, FileMode.Open)
    use newReader = new BinaryReader(newReadStream)

    let newFile = createReadFunction newReader
                  |> Read

    newReader.Close()

    (originalFile = newFile) |> Assert.IsTrue
