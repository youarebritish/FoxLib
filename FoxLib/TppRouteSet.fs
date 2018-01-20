module FoxLib.Tpp.RouteSet

open FoxLib.Core
open System

type public RouteEvent(eventType:StrCode32Hash, param1:uint32, param2:uint32, param3:uint32, param4:uint32, param5:uint32,
                        param6:uint32, param7:uint32, param8:uint32, param9:uint32, param10:uint32, snippet:string) =
    member this.EventType = eventType;
    member this.Param1 = param1;
    member this.Param2 = param2;
    member this.Param3 = param3;
    member this.Param4 = param4;
    member this.Param5 = param5;
    member this.Param6 = param6;
    member this.Param7 = param7;
    member this.Param8 = param8;
    member this.Param9 = param9;
    member this.Param10 = param10;
    member this.Snippet = snippet;

type public RouteNode = {
    Position : Vector3
    EdgeEvent : RouteEvent
    Events : seq<RouteEvent>
}

type public Route = {
    Name : StrCode32Hash
    Nodes : seq<RouteNode>
}

type public RouteSet = {
    Routes : seq<Route>
}

/// <summmary>
/// Input functions to the Read function.
/// </summmary>
type public ReadFunctions = {
    /// Function to read a float32.
    ReadSingle : Func<float32>
    /// Function to read a uint16.
    ReadUInt16 : Func<uint16>
    /// Function to read a uint32.
    ReadUInt32 : Func<uint32>
    /// Function to read a int32.
    ReadInt32 : Func<int32>
    /// Function to read a char.
    ReadChar : Func<char>
    /// Function to skip a number of bytes.
    SkipBytes : Action<int>
}

/// <summmary>
/// Read parameters converted to F# functions.
/// </summmary>
type private ConvertedReadFunctions = {
    ReadSingle : unit -> float32
    ReadUInt16 : unit -> uint16
    ReadUInt32 : unit -> uint32
    ReadInt32 : unit -> int32
    ReadChar : unit -> char
    SkipBytes : int -> unit
}

/// <summmary>
/// Converts the Read function's .NET Funcs into F# functions.
/// </summmary>
/// <param name="rawReadFunctions">Input functions supplied to the Read function.</param>
/// <returns>The converted functions.</returns>
let private convertReadFunctions (rawReadFunctions : ReadFunctions) =
    if rawReadFunctions.ReadInt32 |> isNull then nullArg "ReadInt32"
    if rawReadFunctions.ReadUInt16 |> isNull then nullArg "ReadUInt16"
    if rawReadFunctions.ReadUInt32 |> isNull then nullArg "ReadUInt32"
    if rawReadFunctions.ReadInt32 |> isNull then nullArg "ReadInt32"
    if rawReadFunctions.ReadChar |> isNull then nullArg "ReadChar"
    if rawReadFunctions.SkipBytes |> isNull then nullArg "SkipBytes"

    { ConvertedReadFunctions.ReadSingle = rawReadFunctions.ReadSingle.Invoke;
    ReadUInt16 = rawReadFunctions.ReadUInt16.Invoke;
    ReadUInt32 = rawReadFunctions.ReadUInt32.Invoke;
    ReadInt32 = rawReadFunctions.ReadInt32.Invoke;
    ReadChar = rawReadFunctions.ReadChar.Invoke;
    SkipBytes = rawReadFunctions.SkipBytes.Invoke; }

/// <summmary>
/// Parses a TppRouteSet from frt format.
/// </summmary>
/// <param name="readFunctions">Functions to read various data types from the input.</param>
/// <returns>The parsed TppRouteSet.</returns>
let public Read (readFunctions : ReadFunctions) =
    let convertedFunctions = convertReadFunctions readFunctions
    let routes = []
    { Routes = routes }

let private writeNode writeSingle (node : Vector3) =
    node.X |> writeSingle
    node.Y |> writeSingle
    node.Z |> writeSingle

type private Header = {
    Version : uint16;
    RouteCount : uint16;
    RouteIdsOffset : uint32;
    NodesOffset : uint32;
    EventTablesOffset : uint32;
    EventsOffset : uint32;
}

type private RouteDefinition = {
    NodeOffset : uint32;
    EventTableOffset : uint32;
    EventsOffset : uint32;
    NodeCount : uint16;
    EventCount : uint16;
}

type private EventTable = {
    EventCount : uint16;
    EdgeEventIndex : uint16;
}

let private writeEvent writeHash writeUInt32 writeChar (event:RouteEvent) =
    writeHash event.EventType

    writeUInt32 event.Param1
    writeUInt32 event.Param2
    writeUInt32 event.Param3
    writeUInt32 event.Param4
    writeUInt32 event.Param5
    writeUInt32 event.Param6
    writeUInt32 event.Param7
    writeUInt32 event.Param8
    writeUInt32 event.Param9
    writeUInt32 event.Param10
    
    writeChar event.Snippet.[0]
    writeChar event.Snippet.[1]
    writeChar event.Snippet.[2]
    writeChar event.Snippet.[3]

let private writeEventTable writeUInt16 eventTable =
    eventTable.EventCount |> writeUInt16
    eventTable.EdgeEventIndex |> writeUInt16

let private makeEventTable allEvents node =
    let edgeEventIndex = allEvents |> Seq.findIndex (fun event -> event = node.EdgeEvent)
    { EventCount = node.Events |> Seq.length |> uint16;
    EdgeEventIndex = edgeEventIndex |> uint16 }

let private getOffsetForNode (nodesOffset : uint32) nodeIndex =
    nodesOffset + (uint32 nodeIndex * uint32 sizeof<Vector3>)

let private getEventTableOffsetForNode (eventTablesOffset : uint32) nodeIndex =
    eventTablesOffset + (uint32 nodeIndex * uint32 sizeof<EventTable>)

let private getOffsetForEvent (eventsOffset : uint32) eventIndex =
    eventsOffset + (uint32 eventIndex * uint32(sizeof<StrCode32Hash> + 10 * sizeof<int> + 4 * sizeof<char>))
    
let private buildRouteDefinition allNodes allEvents nodesOffset eventTablesOffset eventsOffset route =
    let initialNode = Seq.head route.Nodes
    let nodeIndex =
        Seq.findIndex (fun entry -> entry = initialNode) allNodes
        |> uint32
    let nodeOffset = getOffsetForNode nodesOffset nodeIndex
    let eventTableOffset = getEventTableOffsetForNode eventTablesOffset nodeIndex

    // TODO: This assumes all EdgeEvents are unique, which isn't true. Figure this out.
    let eventIndex =
        Seq.findIndex (fun element -> element = initialNode.EdgeEvent) allEvents
        |> uint32
    let eventsOffset = getOffsetForEvent eventsOffset eventIndex |> uint32
    let nodeCount = Seq.length allNodes |> uint16
    let eventCount = allNodes |> Seq.sumBy (fun node -> Seq.length node.Events) |> uint16

    { NodeOffset = nodeOffset;
    EventTableOffset = eventTableOffset;
    EventsOffset = eventsOffset;
    NodeCount = nodeCount;
    EventCount = eventCount }

let private writeRouteDefinition writeOffset writeUInt16 routeDefinition =
    routeDefinition.NodeOffset |> writeOffset
    routeDefinition.EventTableOffset |> writeOffset
    routeDefinition.EventsOffset |> writeOffset
    routeDefinition.NodeCount |> writeUInt16
    routeDefinition.EventCount |> writeUInt16

let private getRouteIdsOffset() =
    28u

let private getRouteDefinitionsOffset routeIdsOffset (routeCount : uint16) =
    routeIdsOffset + (uint32 routeCount * uint32 sizeof<StrCode32Hash>)

let private getNodesOffset routeDefinitionsOffset (routeCount : uint16) =
    routeDefinitionsOffset + (uint32 routeCount * uint32 sizeof<RouteDefinition>)

let private getEventTablesOffset nodesOffset (nodeCount : uint16) =
    nodesOffset + (uint32 nodeCount * uint32 sizeof<Vector3>)

let private getEventsOffset eventTablesOffset (nodeCount : uint16) =
    eventTablesOffset + (uint32 nodeCount * uint32 sizeof<EventTable>)
    
let private buildHeader routeCount nodeCount =    
    // TODO: Find a more elegant way to do this.
    let routeIdsOffset = getRouteIdsOffset()
    let routeDefinitionsOffset = getRouteDefinitionsOffset routeIdsOffset routeCount
    let nodesOffset = getNodesOffset routeDefinitionsOffset routeCount
    let eventTablesOffset = getEventTablesOffset nodesOffset nodeCount
    let eventsOffset = getEventsOffset eventTablesOffset nodeCount

    { Version = 3us;
    RouteCount = routeCount;
    RouteIdsOffset = routeIdsOffset;
    NodesOffset = nodesOffset;
    EventTablesOffset = eventTablesOffset;
    EventsOffset = eventsOffset }

let private writeHeader writeChar writeUInt16 writeUInt32 header =
    ['R';'O';'U';'T'] |> Seq.iter (fun entry -> writeChar entry)
    header.Version |> writeUInt16
    header.RouteCount |> writeUInt16
    header.RouteIdsOffset |> writeUInt32
    header.NodesOffset |> writeUInt32
    header.EventTablesOffset |> writeUInt32
    header.EventsOffset |> writeUInt32

/// <summmary>
/// Input functions to the Write function.
/// </summmary>
type public WriteFunctions = {
    /// Function to write a float32.
    WriteSingle : Action<float32>
    /// Function to write a uint16.
    WriteUInt16 : Action<uint16>
    /// Function to write a uint32.
    WriteUInt32 : Action<uint32>
    /// Function to write a int32.
    WriteInt32 : Action<int32>
    /// Function to write a char.
    WriteChar : Action<char>
    /// Function to write a number of filler bytes.
    WriteEmptyBytes : Action<int>
}

/// <summmary>
/// Write parameters converted to F# functions.
/// </summmary>
type private ConvertedWriteFunctions = {
    WriteSingle : float32 -> unit
    WriteUInt16 : uint16 -> unit
    WriteUInt32 : uint32 -> unit
    WriteInt32 : int32 -> unit
    WriteChar : char -> unit
    WriteEmptyBytes : int32 -> unit
}

/// <summmary>
/// Converts the Write function's .NET Funcs into F# functions.
/// </summmary>
/// <param name="rawWriteFunctions">Input functions supplied to the Write function.</param>
/// <returns>The converted functions.</returns>
let private convertWriteFunctions (rawWriteFunctions : WriteFunctions) =
    if rawWriteFunctions.WriteSingle |> isNull then nullArg "WriteSingle"
    if rawWriteFunctions.WriteUInt16 |> isNull then nullArg "WriteUInt16"
    if rawWriteFunctions.WriteUInt32 |> isNull then nullArg "WriteUInt32"
    if rawWriteFunctions.WriteInt32 |> isNull then nullArg "WriteInt32"
    if rawWriteFunctions.WriteChar |> isNull then nullArg "WriteChar"
    if rawWriteFunctions.WriteEmptyBytes |> isNull then nullArg "WriteEmptyBytes"

    { ConvertedWriteFunctions.WriteSingle = rawWriteFunctions.WriteSingle.Invoke;
    WriteUInt16 = rawWriteFunctions.WriteUInt16.Invoke;
    WriteUInt32 = rawWriteFunctions.WriteUInt32.Invoke;
    WriteInt32 = rawWriteFunctions.WriteInt32.Invoke;
    WriteChar = rawWriteFunctions.WriteChar.Invoke;
    WriteEmptyBytes = rawWriteFunctions.WriteEmptyBytes.Invoke; }

/// <summary>
/// Writes a TppRouteSet to frt format.
/// </summary>
/// <param name="writeFunctions">Function to write various data types.</param>
/// <param name="routeSet">TppRouteSet to write.</param>
let public Write (writeFunctions : WriteFunctions) (routeSet : RouteSet) =
    let convertedWriteFunctions = convertWriteFunctions writeFunctions

    // Write header.
    let routeCount = Seq.length routeSet.Routes |> uint16
    let nodeCount = Seq.sumBy (fun route -> Seq.length route.Nodes) routeSet.Routes |> uint16    
    let header = buildHeader routeCount nodeCount

    header |> writeHeader
        convertedWriteFunctions.WriteChar
        convertedWriteFunctions.WriteUInt16
        convertedWriteFunctions.WriteUInt32

    // Write route definitions.
    let allNodes = routeSet.Routes |> Seq.collect (fun route -> route.Nodes)
    let allEvents = allNodes |> Seq.collect (fun node -> node.Events)
    
    routeSet.Routes
    |> Seq.map (buildRouteDefinition allNodes allEvents header.NodesOffset header.EventTablesOffset header.EventsOffset)
    |> Seq.iter (writeRouteDefinition convertedWriteFunctions.WriteUInt32 convertedWriteFunctions.WriteUInt16)

    // Write nodes.
    allNodes
    |> Seq.map (fun node -> node.Position)
    |> Seq.iter (writeNode convertedWriteFunctions.WriteSingle)

    // Write event tables.
    allNodes
    |> Seq.map (makeEventTable allEvents)
    |> Seq.iter (writeEventTable convertedWriteFunctions.WriteUInt16)

    // Write events.
    allNodes
    |> Seq.collect (fun node -> node.Events)
    |> Seq.distinct
    |> Seq.iter (writeEvent convertedWriteFunctions.WriteUInt32 convertedWriteFunctions.WriteUInt32 convertedWriteFunctions.WriteChar)