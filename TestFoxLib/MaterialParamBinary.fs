module TestFoxLib.MaterialParamBinary

open System
open System.IO
open FoxLib.Core
open FoxLib.MaterialParamBinary
open NUnit.Framework
open TestFoxLib

let private createRandomMaterialParamBinary (random : Random)= 
    let makeFloat = random.NextDouble >> float32

    let specularColor = { Red = makeFloat(); Blue = makeFloat(); Green = makeFloat(); }

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
    let randomMaterialPreset = [|1..256|]
                                |> Array.map (fun _ -> createRandomMaterialParamBinary random)
    
    use stream = new MemoryStream()
    use writer = new BinaryWriter(stream)
    createWriteFunction writer
    |> FoxLib.MaterialParamBinary.Write randomMaterialPreset
    |> ignore
        
    stream.Position <- 0L

    use reader = new BinaryReader(stream)
    createReadFunction reader
    |> FoxLib.MaterialParamBinary.Read
    |> fun newMaterialPreset ->    
        match newMaterialPreset with
        | i when i = randomMaterialPreset -> true |> Assert.IsTrue
        | _ -> Assert.Fail()

[<Test>]
[<Category("MaterialParamBinary")>]
let ``read and then written "material_param.fmtt" should have original value when read`` () =
    use originalReadStream = new FileStream("C:/users/joey3/Downloads/material_params.fmtt", FileMode.Open)
    use originalReader = new BinaryReader(originalReadStream)
    
    let originalFile = createReadFunction originalReader
                       |> Read

    use originalWriteStream = new FileStream("C:/users/joey3/Downloads/test_material_params.fmtt", FileMode.Create)
    use originalWriter = new BinaryWriter(originalWriteStream)

    do createWriteFunction originalWriter
       |> Write originalFile
       |> ignore

    do originalWriter.Close()

    use newReadStream = new FileStream("C:/users/joey3/Downloads/test_material_params.fmtt", FileMode.Open)
    use newReader = new BinaryReader(newReadStream)

    let newFile = createReadFunction newReader
                  |> Read

    do match originalFile with
       | i when i = newFile -> true |> Assert.IsTrue
       | _ -> Assert.Fail()