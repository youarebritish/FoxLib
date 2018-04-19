module TestFoxLib.PrecomputedSkyParameters

open System
open System.IO
open FoxLib.Core
open FoxLib.PrecomputedSkyParameters
open NUnit.Framework

let private createRandomPixel (random : Random)= 
    let makeHalf = random.NextDouble >> Half

    { HalfColorRGBA.Red = makeHalf(); Green = makeHalf(); Blue = makeHalf(); Alpha = makeHalf(); }

let private createWriteFunctions (writer : BinaryWriter) =
    { WriteUInt32 = new Action<uint32>(writer.Write); WriteHalf = new Action<Half>((fun half -> writer.Write (Half.GetBytes half))); }

let private createReadFunctions (reader : BinaryReader) =
    { ReadUInt32 = new Func<uint32>(reader.ReadUInt32); ReadHalf = new Func<Half>(fun _ -> Half.ToHalf(reader.ReadUInt16())); }

[<Test>]
[<Category("PrecomputedSkyParameters")>]
let ``one random PrecomputedSkyParameters texture should have original value when read`` () =
    let random = new System.Random()
    let randomPixels = [|1..8192|]
                        |> Array.map (fun _ -> createRandomPixel random)
    
    use stream = new MemoryStream()
    use writer = new BinaryWriter(stream)
    createWriteFunctions writer
    |> FoxLib.PrecomputedSkyParameters.Write randomPixels
    |> ignore
        
    stream.Position <- 0L

    use reader = new BinaryReader(stream)
    let test = createReadFunctions reader
               |> FoxLib.PrecomputedSkyParameters.Read

    (test = randomPixels) |> Assert.IsTrue

[<Test>]
[<Category("PrecomputedSkyParameters")>]
let ``read and then written "test.pcsp" should have original value when read`` () =
    let baseDirectory = __SOURCE_DIRECTORY__

    let originalFilePath = Path.Combine(baseDirectory, "test.pcsp")
    use originalReadStream = new FileStream(originalFilePath, FileMode.Open)
    use originalReader = new BinaryReader(originalReadStream)
    
    let originalFile = createReadFunctions originalReader
                       |> Read

    originalReader.Close()

    let newFilePath = Path.Combine(baseDirectory, "test repacked.pcsp")
    use newWriteStream = new FileStream(newFilePath, FileMode.Create)
    use newWriter = new BinaryWriter(newWriteStream)

    createWriteFunctions newWriter
    |> Write originalFile
    |> ignore

    newWriter.Close()

    use newReadStream = new FileStream(newFilePath, FileMode.Open)
    use newReader = new BinaryReader(newReadStream)

    let newFile = createReadFunctions newReader
                  |> Read

    newReader.Close()

    (originalFile = newFile) |> Assert.IsTrue