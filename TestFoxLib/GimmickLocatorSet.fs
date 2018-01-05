module TestFoxLib.GimmickLocatorSet

open FoxLib.Core
open FoxLib.Tpp.GimmickLocator
open FoxLib.Tpp.GimmickLocatorSet

open NUnit.Framework
open System.IO
open FoxLib.Tpp

let private createRandomPowerCutAreaGimmickLocator (random : System.Random) =
    let makeFloat = random.NextDouble >> float32

    let position = { Vector4.X = makeFloat(); Y = float32 <| makeFloat(); Z = float32 <| makeFloat(); W = float32 <| makeFloat() }
    let rotation = { Quaternion.X = makeFloat(); Y = makeFloat(); Z = makeFloat(); W = makeFloat() }
    { Position = position; Rotation = rotation }
        
[<Test>]
[<Category("GimmickLocatorSet")>]
let ``empty PowerCutAreaGimmickLocatorSet should be empty when read`` () =
    let locators = Array.empty
    let locatorSet = PowerCutAreaGimmickLocatorSet(locators)

    use stream = new MemoryStream()
    use writer = new BinaryWriter(stream)
    let writeEmptyBytes numBytes = Array.zeroCreate<byte> numBytes |> writer.Write

    GimmickLocatorSet.Write writer.Write writer.Write writer.Write writer.Write writeEmptyBytes locatorSet
    
    stream.Position <- 0L

    use reader = new BinaryReader(stream)
    let skipBytes numBytes = reader.ReadBytes numBytes
    let locatorSet = GimmickLocatorSet.Read reader.ReadSingle reader.ReadUInt16 reader.ReadUInt32 reader.ReadInt32 skipBytes
    
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
    let writeEmptyBytes numBytes = Array.zeroCreate<byte> numBytes |> writer.Write

    GimmickLocatorSet.Write writer.Write writer.Write writer.Write writer.Write writeEmptyBytes locatorSet
    
    stream.Position <- 0L

    use reader = new BinaryReader(stream)
    let skipBytes numBytes = reader.ReadBytes numBytes
    let locatorSet = GimmickLocatorSet.Read reader.ReadSingle reader.ReadUInt16 reader.ReadUInt32 reader.ReadInt32 skipBytes
    
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
    let writeEmptyBytes numBytes = Array.zeroCreate<byte> numBytes |> writer.Write

    GimmickLocatorSet.Write writer.Write writer.Write writer.Write writer.Write writeEmptyBytes locatorSet
    
    stream.Position <- 0L

    use reader = new BinaryReader(stream)
    let skipBytes numBytes = reader.ReadBytes numBytes
    let locatorSet = GimmickLocatorSet.Read reader.ReadSingle reader.ReadUInt16 reader.ReadUInt32 reader.ReadInt32 skipBytes
    
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
    let locators = Array.empty
    let locatorSet = NamedGimmickLocatorSet(locators)

    use stream = new MemoryStream()
    use writer = new BinaryWriter(stream)
    let writeEmptyBytes numBytes = Array.zeroCreate<byte> numBytes |> writer.Write

    GimmickLocatorSet.Write writer.Write writer.Write writer.Write writer.Write writeEmptyBytes locatorSet
    
    stream.Position <- 0L

    use reader = new BinaryReader(stream)
    let skipBytes numBytes = reader.ReadBytes numBytes
    let locatorSet = GimmickLocatorSet.Read reader.ReadSingle reader.ReadUInt16 reader.ReadUInt32 reader.ReadInt32 skipBytes
    
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
    let writeEmptyBytes numBytes = Array.zeroCreate<byte> numBytes |> writer.Write

    GimmickLocatorSet.Write writer.Write writer.Write writer.Write writer.Write writeEmptyBytes locatorSet
    
    stream.Position <- 0L

    use reader = new BinaryReader(stream)
    let skipBytes numBytes = reader.ReadBytes numBytes
    let locatorSet = GimmickLocatorSet.Read reader.ReadSingle reader.ReadUInt16 reader.ReadUInt32 reader.ReadInt32 skipBytes
    
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
    let writeEmptyBytes numBytes = Array.zeroCreate<byte> numBytes |> writer.Write

    GimmickLocatorSet.Write writer.Write writer.Write writer.Write writer.Write writeEmptyBytes locatorSet
    
    stream.Position <- 0L

    use reader = new BinaryReader(stream)
    let skipBytes numBytes = reader.ReadBytes numBytes
    let locatorSet = GimmickLocatorSet.Read reader.ReadSingle reader.ReadUInt16 reader.ReadUInt32 reader.ReadInt32 skipBytes
    
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
    let locators = Array.empty
    let locatorSet = ScaledGimmickLocatorSet(locators)

    use stream = new MemoryStream()
    use writer = new BinaryWriter(stream)
    let writeEmptyBytes numBytes = Array.zeroCreate<byte> numBytes |> writer.Write

    GimmickLocatorSet.Write writer.Write writer.Write writer.Write writer.Write writeEmptyBytes locatorSet
    
    stream.Position <- 0L

    use reader = new BinaryReader(stream)
    let skipBytes numBytes = reader.ReadBytes numBytes
    let locatorSet = GimmickLocatorSet.Read reader.ReadSingle reader.ReadUInt16 reader.ReadUInt32 reader.ReadInt32 skipBytes
    
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
    let writeEmptyBytes numBytes = Array.zeroCreate<byte> numBytes |> writer.Write

    GimmickLocatorSet.Write writer.Write writer.Write writer.Write writer.Write writeEmptyBytes locatorSet
    
    stream.Position <- 0L

    use reader = new BinaryReader(stream)
    let skipBytes numBytes = reader.ReadBytes numBytes
    let locatorSet = GimmickLocatorSet.Read reader.ReadSingle reader.ReadUInt16 reader.ReadUInt32 reader.ReadInt32 skipBytes
    
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
    let writeEmptyBytes numBytes = Array.zeroCreate<byte> numBytes |> writer.Write

    GimmickLocatorSet.Write writer.Write writer.Write writer.Write writer.Write writeEmptyBytes locatorSet
    
    stream.Position <- 0L

    use reader = new BinaryReader(stream)
    let skipBytes numBytes = reader.ReadBytes numBytes
    let locatorSet = GimmickLocatorSet.Read reader.ReadSingle reader.ReadUInt16 reader.ReadUInt32 reader.ReadInt32 skipBytes
    
    match locatorSet with
    | ScaledGimmickLocatorSet readLocators ->
        let compareSequences = Seq.compareWith Operators.compare
        compareSequences readLocators locators = 0 |> Assert.IsTrue
    | _ -> Assert.Fail()