module TestFoxLib.GimmickLocatorSet

open FoxLib.Core
open FoxLib.Tpp.GimmickLocator
open FoxLib.Tpp.GimmickLocatorSet

open NUnit.Framework
open System.IO
open FoxLib.Tpp
open System

let private createRandomPowerCutAreaGimmickLocator (random : System.Random) =
    let makeFloat = random.NextDouble >> float32

    let position = { Vector4.X = makeFloat(); Y = float32 <| makeFloat(); Z = float32 <| makeFloat(); W = float32 <| makeFloat() }
    let rotation = { Quaternion.X = makeFloat(); Y = makeFloat(); Z = makeFloat(); W = makeFloat() }
    { Position = position; Rotation = rotation }

let private createWriteFunctions (writer : BinaryWriter) =
    { WriteFunctions.WriteSingle = new Action<float32>(writer.Write);
    WriteUInt16 = new Action<uint16>(writer.Write);
    WriteUInt32 = new Action<uint32>(writer.Write);
    WriteInt32 = new Action<int32>(writer.Write);
    WriteEmptyBytes = new Action<int>(fun numBytes -> Array.zeroCreate<byte> numBytes |> writer.Write) }

let private createReadFunctions (reader : BinaryReader) =
    { ReadFunctions.ReadSingle = new Func<float32>(reader.ReadSingle);
    ReadUInt16 = new Func<uint16>(reader.ReadUInt16);
    ReadUInt32 = new Func<uint32>(reader.ReadUInt32);
    ReadInt32 = new Func<int32>(reader.ReadInt32);
    SkipBytes = new Action<int>(fun numBytes -> reader.ReadBytes numBytes |> ignore) }
        
[<Test>]
[<Category("GimmickLocatorSet")>]
let ``empty PowerCutAreaGimmickLocatorSet should be empty when read`` () =
    let locatorSet = PowerCutAreaGimmickLocatorSet([])

    use stream = new MemoryStream()
    use writer = new BinaryWriter(stream)    
    GimmickLocatorSet.Write locatorSet (createWriteFunctions writer) |> ignore
    
    stream.Position <- 0L
    
    use reader = new BinaryReader(stream)
    createReadFunctions reader
    |> GimmickLocatorSet.Read
    |> fun locatorSet ->
        match locatorSet with
        | PowerCutAreaGimmickLocatorSet locators -> Seq.isEmpty locators |> Assert.IsTrue
        | _ -> Assert.Fail()

[<Test>]
[<Category("GimmickLocatorSet")>]
let ``one random PowerCutAreaGimmickLocator should have original value when read`` () =
    let random = new System.Random()
    let locator = createRandomPowerCutAreaGimmickLocator random
    let locatorSet = PowerCutAreaGimmickLocatorSet([locator])
    
    use stream = new MemoryStream()
    use writer = new BinaryWriter(stream)
    createWriteFunctions writer
    |> GimmickLocatorSet.Write locatorSet
    |> ignore
        
    stream.Position <- 0L

    use reader = new BinaryReader(stream)
    createReadFunctions reader
    |> GimmickLocatorSet.Read
    |> fun locatorSet ->    
        match locatorSet with
        | PowerCutAreaGimmickLocatorSet locators ->
            Seq.head locators
            |> fun readLocator -> readLocator = locator |> Assert.IsTrue
        | _ -> Assert.Fail()
    
[<Test>]
[<Category("GimmickLocatorSet")>]
let ``one hundred random PowerCutAreaGimmickLocators should have original values when read`` () =
    let random = new System.Random()
    let locators = Array.init 100 (fun _ -> createRandomPowerCutAreaGimmickLocator random)
    let locatorSet = PowerCutAreaGimmickLocatorSet(locators)
    
    use stream = new MemoryStream()
    use writer = new BinaryWriter(stream)
    createWriteFunctions writer
    |> GimmickLocatorSet.Write locatorSet
    |> ignore
    
    stream.Position <- 0L

    use reader = new BinaryReader(stream)
    createReadFunctions reader
    |> GimmickLocatorSet.Read
    |> fun locatorSet -> 
        match locatorSet with
        | PowerCutAreaGimmickLocatorSet readLocators ->
            let compareSequences = Seq.compareWith Operators.compare
            compareSequences readLocators locators = 0 |> Assert.IsTrue
        | _ -> Assert.Fail()

let private createRandomNamedGimmickLocator (random : System.Random) =
    let makeFloat = random.NextDouble >> float32

    let position = { Vector4.X = makeFloat(); Y = float32 <| makeFloat(); Z = float32 <| makeFloat(); W = float32 <| makeFloat() }
    let rotation = { Quaternion.X = makeFloat(); Y = makeFloat(); Z = makeFloat(); W = makeFloat() }
    let locatorName = random.Next() |> uint32
    let dataSetName = random.Next() |> uint32
    { Position = position; Rotation = rotation; LocatorName = locatorName; DataSetName = dataSetName }

[<Test>]
[<Category("GimmickLocatorSet")>]
let ``empty NamedGimmickLocatorSet should be empty when read`` () =
    let locatorSet = NamedGimmickLocatorSet([])

    use stream = new MemoryStream()
    use writer = new BinaryWriter(stream)    
    GimmickLocatorSet.Write locatorSet (createWriteFunctions writer) |> ignore
    
    stream.Position <- 0L
    
    use reader = new BinaryReader(stream)
    createReadFunctions reader
    |> GimmickLocatorSet.Read
    |> fun locatorSet ->
        match locatorSet with
        | NamedGimmickLocatorSet locators -> Seq.isEmpty locators |> Assert.IsTrue
        | _ -> Assert.Fail()

[<Test>]
[<Category("GimmickLocatorSet")>]
let ``one random NamedGimmickLocator should have original value when read`` () =
    let random = new System.Random()
    let locator = createRandomNamedGimmickLocator random
    let locatorSet = NamedGimmickLocatorSet([locator])
    
    use stream = new MemoryStream()
    use writer = new BinaryWriter(stream)
    createWriteFunctions writer
    |> GimmickLocatorSet.Write locatorSet
    |> ignore
        
    stream.Position <- 0L

    use reader = new BinaryReader(stream)
    createReadFunctions reader
    |> GimmickLocatorSet.Read
    |> fun locatorSet ->    
        match locatorSet with
        | NamedGimmickLocatorSet locators ->
            Seq.head locators
            |> fun readLocator -> readLocator = locator |> Assert.IsTrue
        | _ -> Assert.Fail()
    
[<Test>]
[<Category("GimmickLocatorSet")>]
let ``one hundred random NamedGimmickLocators should have original values when read`` () =
    let random = new System.Random()
    let locators = Array.init 100 (fun _ -> createRandomNamedGimmickLocator random)
    let locatorSet = NamedGimmickLocatorSet(locators)
    
    use stream = new MemoryStream()
    use writer = new BinaryWriter(stream)
    createWriteFunctions writer
    |> GimmickLocatorSet.Write locatorSet
    |> ignore
    
    stream.Position <- 0L

    use reader = new BinaryReader(stream)
    createReadFunctions reader
    |> GimmickLocatorSet.Read
    |> fun locatorSet -> 
        match locatorSet with
        | NamedGimmickLocatorSet readLocators ->
            let compareSequences = Seq.compareWith Operators.compare
            compareSequences readLocators locators = 0 |> Assert.IsTrue
        | _ -> Assert.Fail()

let private createRandomScaledGimmickLocator (random : System.Random) =
    let makeFloat = random.NextDouble >> float32

    let position = { Vector4.X = makeFloat(); Y = float32 <| makeFloat(); Z = float32 <| makeFloat(); W = float32 <| makeFloat() }
    let rotation = { Quaternion.X = makeFloat(); Y = makeFloat(); Z = makeFloat(); W = makeFloat() }
    let scale = { WideVector3.X = makeFloat(); Y = makeFloat(); Z = makeFloat(); A = random.Next() |> uint16; B = random.Next() |> uint16}
    let locatorName = random.Next() |> uint32
    let dataSetName = random.Next() |> uint32
    { Position = position; Rotation = rotation; Scale = scale; LocatorName = locatorName; DataSetName = dataSetName }

[<Test>]
[<Category("GimmickLocatorSet")>]
let ``empty ScaledGimmickLocatorSet should be empty when read`` () =
    let locatorSet = ScaledGimmickLocatorSet([])

    use stream = new MemoryStream()
    use writer = new BinaryWriter(stream)    
    GimmickLocatorSet.Write locatorSet (createWriteFunctions writer) |> ignore
    
    stream.Position <- 0L
    
    use reader = new BinaryReader(stream)
    createReadFunctions reader
    |> GimmickLocatorSet.Read
    |> fun locatorSet ->
        match locatorSet with
        | ScaledGimmickLocatorSet locators -> Seq.isEmpty locators |> Assert.IsTrue
        | _ -> Assert.Fail()

[<Test>]
[<Category("GimmickLocatorSet")>]
let ``one random ScaledGimmickLocator should have original value when read`` () =
    let random = new System.Random()
    let locator = createRandomScaledGimmickLocator random
    let locatorSet = ScaledGimmickLocatorSet([locator])
    
    use stream = new MemoryStream()
    use writer = new BinaryWriter(stream)
    createWriteFunctions writer
    |> GimmickLocatorSet.Write locatorSet
    |> ignore
        
    stream.Position <- 0L

    use reader = new BinaryReader(stream)
    createReadFunctions reader
    |> GimmickLocatorSet.Read
    |> fun locatorSet ->    
        match locatorSet with
        | ScaledGimmickLocatorSet locators ->
            Seq.head locators
            |> fun readLocator -> readLocator = locator |> Assert.IsTrue
        | _ -> Assert.Fail()
    
[<Test>]
[<Category("GimmickLocatorSet")>]
let ``one hundred random ScaledGimmickLocators should have original values when read`` () =
    let random = new System.Random()
    let locators = Array.init 100 (fun _ -> createRandomScaledGimmickLocator random)
    let locatorSet = ScaledGimmickLocatorSet(locators)
    
    use stream = new MemoryStream()
    use writer = new BinaryWriter(stream)
    createWriteFunctions writer
    |> GimmickLocatorSet.Write locatorSet
    |> ignore
    
    stream.Position <- 0L

    use reader = new BinaryReader(stream)
    createReadFunctions reader
    |> GimmickLocatorSet.Read
    |> fun locatorSet -> 
        match locatorSet with
        | ScaledGimmickLocatorSet readLocators ->
            let compareSequences = Seq.compareWith Operators.compare
            compareSequences readLocators locators = 0 |> Assert.IsTrue
        | _ -> Assert.Fail()