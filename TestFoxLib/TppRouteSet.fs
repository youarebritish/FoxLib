module TppRouteSet

open NUnit.Framework
open System.IO
open System

open FoxLib.Tpp.RouteSet
open FoxLib.Tpp
open FoxLib.Core
open System.Text

let private createWriteFunctions (writer : BinaryWriter) =
    { WriteFunctions.WriteSingle = new Action<float32>(writer.Write);
    WriteUInt16 = new Action<uint16>(writer.Write);
    WriteUInt32 = new Action<uint32>(writer.Write);
    WriteInt32 = new Action<int32>(writer.Write);
    WriteChar = new Action<char>(writer.Write);
    WriteEmptyBytes = new Action<int>(fun numBytes -> Array.zeroCreate<byte> numBytes |> writer.Write) }

let private createReadFunctions (reader : BinaryReader) =
    let readBytes = fun num ->
                            //let bytes = reader.ReadBytes 1
                            //let string = ASCIIEncoding.ASCII.GetString bytes
                            //string.Chars 0
                            reader.ReadBytes num

    { ReadFunctions.ReadSingle = new Func<float32>(reader.ReadSingle);
    ReadUInt16 = new Func<uint16>(reader.ReadUInt16);
    ReadUInt32 = new Func<uint32>(reader.ReadUInt32);
    ReadInt32 = new Func<int32>(reader.ReadInt32);
    ReadBytes = new Func<int, byte[]>(readBytes);
    SkipBytes = new Action<int>(fun numBytes -> reader.ReadBytes numBytes |> ignore) }

[<Test>]
[<Category("TppRouteSet")>]
let ``empty TppRouteSet should be empty when read`` () =
    let routeSet = { Routes = [] }
    use stream = new MemoryStream()
    use writer = new BinaryWriter(stream)
    (RouteSet.Write (createWriteFunctions writer) routeSet) |> ignore

    stream.Position <- 0L

    use reader = new BinaryReader(stream)
    createReadFunctions reader
    |> RouteSet.Read
    |> fun routeSet -> Seq.isEmpty routeSet.Routes |> Assert.IsTrue

let private createRandomRouteEvent() =
   new RouteEvent(999u,
    2u,
    3u,
    4u,
    5u,
    6u,
    7u,
    8u,
    9u,
    10u,
    11u,
    "test")

let private createRandomRouteNode (random : System.Random) eventCount =
    let makeFloat = random.NextDouble >> float32
    let position = { Vector3.X = makeFloat(); Y = float32 <| makeFloat(); Z = float32 <| makeFloat() }
    let edgeEvent = createRandomRouteEvent()
    let events = [0..eventCount - 1]
                    |> Seq.map (fun _ -> createRandomRouteEvent())
                    |> Seq.toArray
    { Position = position; EdgeEvent = edgeEvent; Events = events }

let private createRandomRoute (random : System.Random) nodeCount eventsPerNode =
    { Name = random.Next() |> uint32;
    Nodes = [0..nodeCount - 1]
            |> Seq.map (fun _ -> createRandomRouteNode random eventsPerNode)
            |> Seq.toArray }

let private createRandomRouteSet (random : System.Random) routeCount nodesPerRoute eventsPerNode =
    { Routes = [0..routeCount - 1]
                |> Seq.map (fun _ -> createRandomRoute random nodesPerRoute eventsPerNode)
                |> Seq.toArray }

let private areNodeEventSetsIdentical (nodeEventsA : seq<RouteEvent>) (nodeEventsB : seq<RouteEvent>) =
    [0 .. Seq.length nodeEventsA]
    |> Seq.exists (fun i ->
         (Seq.item i nodeEventsA) |> RouteEvent.isIdentical (Seq.item i nodeEventsB))

let private areNodesIdentical (nodeA : RouteNode) (nodeB : RouteNode) =
    nodeA.Position = nodeB.Position
    && nodeA.EdgeEvent |> RouteEvent.isIdentical nodeB.EdgeEvent
    && [0 .. Seq.length nodeA.Events - 1]
        |> Seq.exists (fun i ->
            not <| areNodeEventSetsIdentical nodeA.Events nodeB.Events)
        |> not

let private areRoutesIdentical routeA routeB =
    [0 .. Seq.length routeA.Nodes - 1]
    |> Seq.exists (fun i ->
        not <| areNodesIdentical (Seq.item i routeA.Nodes) (Seq.item i routeB.Nodes))
    |> not

let private areRouteSetsIdentical (routeSetA : RouteSet) (routeSetB : RouteSet) =
    let sortedRoutesA = routeSetA.Routes |> Seq.sortBy (fun route -> route.Name)
    let sortedRoutesB = routeSetB.Routes |> Seq.sortBy (fun route -> route.Name)

    [0 .. Seq.length sortedRoutesA - 1]
    |> Seq.exists (fun i ->
        not <| areRoutesIdentical (Seq.item i routeSetA.Routes) (Seq.item i sortedRoutesB))
    |> not

[<Test>]
[<Category("TppRouteSet")>]
let ``one route with one node and one event should have original values when read`` () =
    let random = new System.Random()
    let routeSet = createRandomRouteSet random 1 1 1

    use stream = new MemoryStream()
    use writer = new BinaryWriter(stream)
    RouteSet.Write (createWriteFunctions writer) routeSet |> ignore

    stream.Position <- 0L

    use reader = new BinaryReader(stream)
    createReadFunctions reader
    |> RouteSet.Read
    |> fun readRouteSet -> areRouteSetsIdentical readRouteSet routeSet |> Assert.IsTrue

[<Test>]
[<Category("TppRouteSet")>]
let ``ten routes with ten nodes and ten events should have original values when read`` () =
    let random = new System.Random()
    let routeSet = createRandomRouteSet random 10 10 10

    use stream = new MemoryStream()
    use writer = new BinaryWriter(stream)
    RouteSet.Write (createWriteFunctions writer) routeSet |> ignore

    stream.Position <- 0L

    use reader = new BinaryReader(stream)
    createReadFunctions reader
    |> RouteSet.Read
    |> fun readRouteSet -> areRouteSetsIdentical readRouteSet routeSet |> Assert.IsTrue

[<Test>]
[<Category("TppRouteSet")>]
let ``edge events should write even when not in edge list`` () =
    let nodeEvent = createRandomRouteEvent()
    let edgeEvent = createRandomRouteEvent()
    let node = { EdgeEvent = edgeEvent; Events = [nodeEvent]; Position = {Vector3.X = 1.0f; Y = 2.0f; Z = 3.0f} };
    let route = { Nodes = [node]; Name = 123456u }
    let routeSet = { Routes = [route] }

    use stream = new MemoryStream()
    use writer = new BinaryWriter(stream)
    RouteSet.Write (createWriteFunctions writer) routeSet

    stream.Position <- 0L

    use reader = new BinaryReader(stream)
    createReadFunctions reader
    |> RouteSet.Read
    |> fun readRouteSet -> areRouteSetsIdentical readRouteSet routeSet |> Assert.IsTrue

[<Test>]
[<Category("TppRouteSet")>]
let ``vanilla frt should repack with identical contents`` () =
    let baseDirectory = __SOURCE_DIRECTORY__
    let inputFilePath = "test.frt"
    let inputFullPath = Path.Combine(baseDirectory, inputFilePath)
    use inputStream = new FileStream(inputFullPath, FileMode.Open)
    use reader = new BinaryReader(inputStream, getEncoding())

    let routeSet = createReadFunctions reader |> RouteSet.Read

    let outputFilePath = "test repacked.frt"
    let outputFullPath = Path.Combine(baseDirectory, outputFilePath)
    use outputStream = new FileStream(outputFullPath, FileMode.Create)
    use writer = new BinaryWriter(outputStream, getEncoding())
    RouteSet.Write (createWriteFunctions writer) routeSet

    inputStream.Dispose()
    outputStream.Dispose()

    let inputBytes = File.ReadAllBytes inputFullPath
    let outputBytes = File.ReadAllBytes outputFullPath
    //Assert.IsTrue true
    Assert.AreEqual(inputBytes, outputBytes)