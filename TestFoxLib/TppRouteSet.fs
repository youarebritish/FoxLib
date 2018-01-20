module TppRouteSet

open NUnit.Framework
open System.IO
open System

open FoxLib.Tpp.RouteSet
open FoxLib.Tpp
open FoxLib.Core

let private createWriteFunctions (writer : BinaryWriter) =
    { WriteFunctions.WriteSingle = new Action<float32>(writer.Write);
    WriteUInt16 = new Action<uint16>(writer.Write);
    WriteUInt32 = new Action<uint32>(writer.Write);
    WriteInt32 = new Action<int32>(writer.Write);
    WriteChar = new Action<char>(writer.Write);
    WriteEmptyBytes = new Action<int>(fun numBytes -> Array.zeroCreate<byte> numBytes |> writer.Write) }

let private createReadFunctions (reader : BinaryReader) =
    { ReadFunctions.ReadSingle = new Func<float32>(reader.ReadSingle);
    ReadUInt16 = new Func<uint16>(reader.ReadUInt16);
    ReadUInt32 = new Func<uint32>(reader.ReadUInt32);
    ReadInt32 = new Func<int32>(reader.ReadInt32);
    ReadChar = new Func<char>(reader.ReadChar);
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

let private createRandomRouteEvent (random : System.Random) =
   new RouteEvent(random.Next() |> uint32,
    random.Next() |> uint32,
    random.Next() |> uint32,
    random.Next() |> uint32,
    random.Next() |> uint32,
    random.Next() |> uint32,
    random.Next() |> uint32,
    random.Next() |> uint32,
    random.Next() |> uint32,
    random.Next() |> uint32,
    random.Next() |> uint32,
    "test")

let private createRandomRouteNode (random : System.Random) eventCount =
    let makeFloat = random.NextDouble >> float32
    let position = { Vector3.X = makeFloat(); Y = float32 <| makeFloat(); Z = float32 <| makeFloat() }
    let edgeEvent = createRandomRouteEvent random
    let (events : seq<RouteEvent>) = [0..eventCount - 1] |> Seq.map (fun _ -> createRandomRouteEvent random)
    { Position = position; EdgeEvent = edgeEvent; Events = events }

let private createRandomRoute (random : System.Random) nodeCount eventsPerNode =
    { Name = random.Next() |> uint32;
    Nodes = [0..nodeCount - 1] |> Seq.map (fun _ -> createRandomRouteNode random eventsPerNode) }

let private createRandomRouteSet (random : System.Random) routeCount nodesPerRoute eventsPerNode =
    { Routes = [0..routeCount - 1] |> Seq.map (fun _ -> createRandomRoute random nodesPerRoute eventsPerNode) }

let private areNodeEventSetsIdentical (nodeEventsA : seq<RouteEvent>) (nodeEventsB : seq<RouteEvent>) =
    [0 .. Seq.length nodeEventsA]
    |> Seq.exists (fun i ->
         (Seq.item i nodeEventsA) = (Seq.item i nodeEventsB))

let private areNodesIdentical (nodeA : RouteNode) (nodeB : RouteNode) =
    nodeA.Position = nodeB.Position
    && nodeA.EdgeEvent = nodeB.EdgeEvent
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
    routeSetA.GetHashCode() = routeSetB.GetHashCode()
    (* [0 .. Seq.length routeSetA.Routes - 1]
    |> Seq.exists (fun i ->
        not <| areRoutesIdentical (Seq.item i routeSetA.Routes) (Seq.item i routeSetB.Routes))
    |> not *)

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