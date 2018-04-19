﻿module FoxLib.Tpp.RouteSet

open FoxLib.Core
open System
open FoxLib
open System.Linq
open System.Text

type public IRouteEvent =
    abstract member EventType : StrCode32Hash

    /// <summary>
    /// Iterable collection of event parameters.
    /// </summary>
    abstract member Params : seq<Object>

/// <summary>
/// Behavior for an AI agent to perform at a route node.
/// </summary>
type public RouteEvent(eventType:StrCode32Hash, param1:uint32, param2:uint32, param3:uint32, param4:uint32, param5:uint32,
                        param6:uint32, param7:uint32, param8:uint32, param9:uint32, param10:uint32, snippet:string) =

    let paramsSequence : seq<Object> = seq [param1; param2; param3; param4; param5; param6; param7; param8; param9; param10];
    
    member __.Param1 = param1;
    member __.Param2 = param2;
    member __.Param3 = param3;
    member __.Param4 = param4;
    member __.Param5 = param5;
    member __.Param6 = param6;
    member __.Param7 = param7;
    member __.Param8 = param8;
    member __.Param9 = param9;
    member __.Param10 = param10;
    member __.Snippet = snippet;
    member __.EventType = eventType
    member __.Params = paramsSequence
    
    interface IRouteEvent with
        member __.EventType = eventType
        member __.Params = paramsSequence

    // TODO: Move this into TestFoxLib; shouldn't be publicly exposed
    /// <summary>
    /// Determines whether or not two events are identical.
    /// </summary>
    /// <param name="this">The first node.</param>
    /// <param name="other">The node with which to compare.</param>
    static member isIdentical(this : RouteEvent) (other : RouteEvent) =
        ((this :> IRouteEvent).EventType, this.Param1, this.Param2, this.Param3,
                                            this.Param4, this.Param5, this.Param6, this.Param7,
                                            this.Param8, this.Param9, this.Param10, this.Snippet) =
                                            ((other :> IRouteEvent).EventType, other.Param1, other.Param2, other.Param3,
                                                other.Param4, other.Param5, other.Param6, other.Param7,
                                                other.Param8, other.Param9, other.Param10, other.Snippet)

/// <summary>
/// A discrete step along an AI route.
/// </summary>
type public RouteNode = {
    /// World position for the AI agent to navigate to.
    Position : Vector3
    /// Event to perform while traveling to this node.
    EdgeEvent : RouteEvent
    /// Events to perform upon arrival at this node.
    Events : seq<RouteEvent>
}

/// <summary>
/// Determines if two route nodes are identical.
/// </summary>
/// <param name="nodeA">The first node.</param>
/// <param name="nodeB">The node with which to compare.</param>
let private areRouteNodesIdentical nodeA nodeB =
    nodeA.Position = nodeB.Position
    && nodeA.EdgeEvent = nodeB.EdgeEvent
    && Enumerable.SequenceEqual(nodeA.Events, nodeB.Events)

/// <summary>
/// A sequence of positions for an AI to traverse, along with behavior events to trigger at each position.
/// </summary>
type public Route = {
    Name : StrCode32Hash
    Nodes : seq<RouteNode>
}

/// <summary>
/// A collection of AI routes.
/// </summary>
type public RouteSet = {
    Routes : seq<Route>
}

/// <summary>
/// Metadata for an frt file.
/// </summary>
type private Header = {
    Version : uint16;
    RouteCount : uint16;
    RouteIdsOffset : uint32;
    RouteDefinitionsOffset : uint32;
    NodesOffset : uint32;
    EventTablesOffset : uint32;
    EventsOffset : uint32;
}

/// <summary>
/// Metadata for a route.
/// </summary>
type private RouteDefinition = {
    NodeOffset : uint32;
    EventTableOffset : uint32;
    EventsOffset : uint32;
    NodeCount : uint16;
    EventCount : uint16;
}

/// <summary>
/// Node event metadata.
/// </summary>
type private EventTable = {
    /// Number of events on the node.
    EventCount : uint16;
    /// Index of the node's first event in the route's event list.
    RouteEventsIndex : uint16;
}

/// <summary>
/// Get the character encoding to use for reading/writing frt files.
/// </summary>
let public getEncoding() =
    Encoding.GetEncoding 1252

/// <summary>
/// Partition the list of all route events in a routeset into a list of events for each node.
/// </summary>
/// <param name="eventTables">Event tables for each node in each route.</param>
/// <param name="events">All events in the routeset.</param>
let private makeGlobalEventTable (eventTables:EventTable[][]) (events:RouteEvent[]) =
    let offset = ref 0

    eventTables
    |> Array.map (fun routeTables -> routeTables
                                     |> Array.map (fun eventTable -> eventTable.EventCount |> int)
                                     |> Array.map (fun eventCount ->    let initialEventIndex = !offset
                                                                        offset := !offset + eventCount

                                                                        events.[initialEventIndex..initialEventIndex + eventCount - 1]))
           
/// <summary>
/// Construct nodes.
/// </summary>
/// <param name="nodePositions">All node positions in the routeset.</param>
/// <param name="allEvents">Events for each node.</param>
let private buildNodes (nodePositions:Vector3[]) (allEvents:RouteEvent[][]) =
    Array.map2 (fun position events -> { Position = position;
                                       EdgeEvent = Seq.head events;
                                       Events = Seq.tail events } ) nodePositions allEvents

/// <summary>
/// Construct routes.
/// </summary>
/// <param name="routeIds">IDs of the routes in the routeset.</param>
/// <param name="nodePositions">Positions of the nodes in each route.</param>
/// <param name="globalEventTable">Events for each node in each route.</param>
let private makeRoutes routeIds nodePositions globalEventTable =    
    let nodesAndEventTables = Array.map2 buildNodes nodePositions globalEventTable

    let nodesEventTablesAndRouteIds = nodesAndEventTables
                                        |> Array.zip routeIds

    let routes = nodesEventTablesAndRouteIds
                 |> Array.map (fun routeData -> { Name = fst routeData; Nodes = snd routeData } )
                 
    routes
    
/// <summary>
/// Read a snippet as a string.
/// </summary>
/// <param name="readChars">Function to read a number of chars.</param>
let private readSnippetAsString readChars =
    let chars = readChars 4;
    System.Text.Encoding.Default.GetString(chars)

/// <summary>
/// Read a route event.
/// </summary>
/// <param name="readHash">Function to read a hash.</param>
/// <param name="readUInt32">Function to read a uint32.</param>
/// <param name="readSnippet">Function to read a snippet.</param>
let private readEvent readHash readUInt32 readSnippet =
    new RouteEvent(readHash(),
        readUInt32(),
        readUInt32(),
        readUInt32(),
        readUInt32(),
        readUInt32(),
        readUInt32(),
        readUInt32(),
        readUInt32(),
        readUInt32(),
        readUInt32(),
        readSnippet())

/// <summary>
/// Read an event table.
/// </summary>
/// <param name="readUInt16">Function to read a uint16.</param>
let private readEventTable readUInt16 = 
    { EventCount = readUInt16();
    RouteEventsIndex = readUInt16() }

/// <summary>
/// Read a route definition.
/// </summary>
/// <param name="readCount">Function to read a count.</param>
/// <param name="readOffset">Function to read an offset.</param>
let private readRouteDefinition readCount readOffset =
    { NodeOffset = readOffset();
    EventTableOffset = readOffset();
    EventsOffset = readOffset();
    NodeCount = readCount();
    EventCount = readCount() }

/// <summary>
/// Read an frt header.
/// </summary>
/// <param name="readChars">Function to read a number of chars.</param>
/// <param name="readVersion">Function to read a version.</param>
/// <param name="readCount">Function to read a count.</param>
/// <param name="readOffset">Function to read an offset.</param>
let private readHeader readChars readVersion readCount readOffset =
    let signature = readChars 4;
    
    { Version = readVersion();
    RouteCount = readCount();
    RouteIdsOffset = readOffset();
    RouteDefinitionsOffset = readOffset();
    NodesOffset = readOffset();
    EventTablesOffset = readOffset();
    EventsOffset = readOffset() }

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
    /// Function to read chars.
    ReadBytes : Func<int, byte[]>
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
    ReadBytes : int -> byte[]
    SkipBytes : int -> unit
}

/// <summmary>
/// Converts the Read function's .NET Funcs into F# functions.
/// </summmary>
/// <param name="rawReadFunctions">Input functions supplied to the Read function.</param>
/// <returns>The converted functions.</returns>
let private convertReadFunctions (rawReadFunctions : ReadFunctions) =
    if rawReadFunctions.ReadSingle |> isNull then nullArg "ReadSingle"
    if rawReadFunctions.ReadInt32 |> isNull then nullArg "ReadInt32"
    if rawReadFunctions.ReadUInt16 |> isNull then nullArg "ReadUInt16"
    if rawReadFunctions.ReadUInt32 |> isNull then nullArg "ReadUInt32"
    if rawReadFunctions.ReadInt32 |> isNull then nullArg "ReadInt32"
    if rawReadFunctions.ReadBytes |> isNull then nullArg "ReadBytes"
    if rawReadFunctions.SkipBytes |> isNull then nullArg "SkipBytes"

    { ConvertedReadFunctions.ReadSingle = rawReadFunctions.ReadSingle.Invoke;
    ReadUInt16 = rawReadFunctions.ReadUInt16.Invoke;
    ReadUInt32 = rawReadFunctions.ReadUInt32.Invoke;
    ReadInt32 = rawReadFunctions.ReadInt32.Invoke;
    ReadBytes = rawReadFunctions.ReadBytes.Invoke;
    SkipBytes = rawReadFunctions.SkipBytes.Invoke; }

/// <summmary>
/// Parses a TppRouteSet from frt format.
/// </summmary>
/// <param name="readFunctions">Functions to read various data types from the input.</param>
/// <returns>The parsed TppRouteSet.</returns>
let public Read (readFunctions : ReadFunctions) =
    let convertedFunctions = convertReadFunctions readFunctions

    let header = readHeader convertedFunctions.ReadBytes convertedFunctions.ReadUInt16 convertedFunctions.ReadUInt16 convertedFunctions.ReadUInt32
    match header.Version with
        | 3us -> ()
        | 2us -> raise (new NotSupportedException("Ground Zeroes .frt format is currently unsupported."))        
        | _ -> raise (new NotSupportedException("Unrecognized format."))
    
    let routeIds = [|1..header.RouteCount |> int|]
                    |> Array.map (fun _ -> convertedFunctions.ReadUInt32())

    let routeDefinitions = [|1..Array.length routeIds|]
                            |> Array.map (fun _ -> readRouteDefinition convertedFunctions.ReadUInt16 convertedFunctions.ReadUInt32)
    
    let nodePositions = routeDefinitions
                        |> Array.map (fun routeDefinition -> routeDefinition.NodeCount)
                        |> Array.map (fun nodeCount -> [|1..nodeCount |> int|]
                                                        |> Array.map (fun _ -> Vector3.Read convertedFunctions.ReadSingle))

    let eventTables = routeDefinitions
                    |> Array.map (fun routeDefinition -> routeDefinition.NodeCount)
                    |> Array.map (fun nodeCount -> [|1..nodeCount |> int|]
                                                    |> Array.map (fun _ -> readEventTable convertedFunctions.ReadUInt16))    

    let events = routeDefinitions
                    |> Array.map (fun routeDefinition -> routeDefinition.EventCount)
                    |> Array.collect (fun eventCount -> [|1..eventCount |> int|]
                                                        |> Array.map (fun _ -> readEvent
                                                                                    convertedFunctions.ReadUInt32
                                                                                    convertedFunctions.ReadUInt32                                                                            
                                                                                    (fun _ -> readSnippetAsString convertedFunctions.ReadBytes)))
    
    let globalEventTable = makeGlobalEventTable eventTables events

    let routes = (makeRoutes routeIds nodePositions globalEventTable)
    { Routes = routes }

/// <summary>
/// Write an event.
/// </summary>
/// <param name="writeHash">Function to write a hash.</param>
/// <param name="writeUInt32">Function to write a uint32.</param>
/// <param name="writeChar">Function to write a char.</param>
/// <param name="event">The event to write.</param>
let private writeEvent writeHash writeUInt32 writeChar (event:RouteEvent) =
    writeHash (event :> IRouteEvent).EventType

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
    
    let snippetCharArray = event.Snippet.ToCharArray()

    [|0..3|]
    |> Array.iter (fun index -> match index >= Array.length snippetCharArray with
                                | true -> writeChar '\000'
                                | false -> writeChar (snippetCharArray.[index]))

/// <summary>
/// Write a node event table.
/// </summary>
/// <param name="writeUInt16">Function to write a uint16.</param>
/// <param name="eventTable">Event table to write.</param>
let private writeEventTable writeUInt16 eventTable =
    eventTable.EventCount |> writeUInt16
    eventTable.RouteEventsIndex |> writeUInt16
    
/// <summary>
/// Make list of each node's first event's index into its respective route's event list.
/// </summary>
/// <param name="routes">All (sorted) routes in the routeset.</param>
let private makeRouteEventsListIndices routes =
    let offset = ref 0

    routes
    |> Seq.collect (fun route -> offset := 0
                                 route.Nodes
                                 |> Seq.map (fun node -> let initialEventIndex = !offset
                                                         let eventCount = Seq.length node.Events + 1
                                                         offset := !offset + eventCount
                                                         initialEventIndex))


/// <summary>
/// Calculate the number of events to write for a node.
/// </summary>
/// <param name="allNodes">All nodes in the routeset.</param>
/// <param name="node">The node.</param>
let private getNodeEventCount allNodes node =
    let nodeIndex = allNodes
                    |> Seq.findIndex (fun otherNode -> otherNode = node)
    
    let wasEdgeEventPreviouslyUsed = allNodes
                                    |> Seq.take nodeIndex
                                    |> Seq.collect (fun node -> Seq.append node.Events (seq [node.EdgeEvent]))
                                    |> Seq.contains node.EdgeEvent

    let nodeEventCount = Seq.length node.Events

    match wasEdgeEventPreviouslyUsed with
    | true -> nodeEventCount
    | false -> nodeEventCount + 1

/// <summary>
/// Make an event table for a node.
/// </summary>
/// <param name="allNodes">All nodes in the routeset.</param>
/// <param name="nodeIndex">The node's index.</param>
/// <param name="routeEventsIndex">Index of the node's first event in the route's events list.</param>
let private makeEventTable allNodes nodeIndex routeEventsIndex =
    let node = Seq.item nodeIndex allNodes
    let eventCount = getNodeEventCount allNodes node

    { EventCount = eventCount |> uint16;
    RouteEventsIndex = routeEventsIndex |> uint16 }
    
/// <summary>
/// Calculate the offset for a route definition.
/// </summary>
/// <param name="routeDefinitionOffset">First route definition offset.</param>
/// <param name="routeDefinitionIndex">Index of the route definition.</param>
let private getOffsetForRouteDefinition (routeDefinitionOffset : uint32) routeDefinitionIndex =
    routeDefinitionOffset + (uint32 routeDefinitionIndex * 16u)

/// <summary>
/// Calculate the offset for a node.
/// </summary>
/// <param name="nodesOffset">First node offset.</param>
/// <param name="nodeIndex">Index of the node.</param>
let private getOffsetForNode (nodesOffset : uint32) nodeIndex =
    nodesOffset + (uint32 nodeIndex * 12u)

/// <summary>
/// Calculate the offset for an event table.
/// </summary>
/// <param name="eventTablesOffset">First event table offset.</param>
/// <param name="nodeIndex">Index of the node.</param>
let private getEventTableOffsetForNode (eventTablesOffset : uint32) nodeIndex =
    eventTablesOffset + (uint32 nodeIndex * uint32 4u)

/// <summary>
/// Calculate the offset for an event.
/// </summary>
/// <param name="eventsOffset">First event offset.</param>
/// <param name="eventIndex">Index of the event.</param>
let private getOffsetForEvent (eventsOffset : uint32) eventIndex =
    eventsOffset + (uint32 eventIndex * 48u)

/// <summary>
/// Calculate the number of events to write for a route.
/// </summary>
/// <param name="allNodes">All nodes in the routeset.</param>
/// <param name="route">The route.</param>
let private getRouteEventCount allNodes route =
    route.Nodes
    |> Seq.map (fun node -> getNodeEventCount allNodes node)
    |> Seq.sum

/// <summary>
/// Construct a route definition.
/// </summary>
/// <param name="allNodes">All nodes in the routeset.</param>
/// <param name="nodesOffset">Offset for where nodes start in the frt file.</param>
/// <param name="eventTablesOffset">Offset for where event tables start in the frt file.</param>
/// <param name="eventsOffset">Offset for where events start in the frt file.</param>
/// <param name="getRouteDefinitionOffset">Offset for where route definitions start in the frt file.</param>
/// <param name="eventIndex">Index of the first event in the route.</param>
/// <param name="route">The route to write.</param>
let private buildRouteDefinition allNodes nodesOffset eventTablesOffset eventsOffset getRouteDefinitionOffset eventIndex route =
    let routeDefinitionOffset = getRouteDefinitionOffset()
    let initialNode = Seq.head route.Nodes
    let nodeIndex =
        Seq.findIndex (fun entry -> areRouteNodesIdentical entry initialNode) allNodes
        |> uint32
    let nodeOffset = (getOffsetForNode nodesOffset nodeIndex) - routeDefinitionOffset
    let eventTableOffset = (getEventTableOffsetForNode eventTablesOffset nodeIndex) - routeDefinitionOffset

    let eventsOffset = (getOffsetForEvent eventsOffset eventIndex) - routeDefinitionOffset |> uint32
    let nodeCount = Seq.length route.Nodes |> uint16
    let eventCount = route
                    |> getRouteEventCount allNodes
                    |> uint16

    { NodeOffset = nodeOffset;
    EventTableOffset = eventTableOffset;
    EventsOffset = eventsOffset;
    NodeCount = nodeCount;
    EventCount = eventCount }

/// <summary>
/// Write a route definition.
/// </summary>
/// <param name="writeOffset">Function to write an offset.</param>
/// <param name="writeUInt16">Function to write a uint16.</param>
/// <param name="routeDefinition">Route definition to write.</param>
let private writeRouteDefinition writeOffset writeUInt16 routeDefinition =
    routeDefinition.NodeOffset |> writeOffset
    routeDefinition.EventTableOffset |> writeOffset
    routeDefinition.EventsOffset |> writeOffset
    routeDefinition.NodeCount |> writeUInt16
    routeDefinition.EventCount |> writeUInt16

/// <summary>
/// Calculate the offset for where route IDs start in an frt file.
/// </summary>
let private getRouteIdsOffset() =
    28u

/// <summary>
/// Calculate the offset for where route definitions start in an frt file.
/// </summary>
/// <param name="routeIdsOffset">Offset for where route IDs start in the frt file.</param>
/// <param name="routeCount">Number of routes in the routeset.</param>
let private getRouteDefinitionsOffset routeIdsOffset (routeCount : uint16) =
    routeIdsOffset + (uint32 routeCount * uint32 sizeof<StrCode32Hash>)

/// <summary>
/// Calculate the offset for where nodes start in an frt file.
/// </summary>
/// <param name="routeDefinitionsOffset">Offset for where route definitions start in the frt file.</param>
/// <param name="routeCount">Number of routes in the routeset.</param>
let private getNodesOffset routeDefinitionsOffset (routeCount : uint16) =
    routeDefinitionsOffset + (uint32 routeCount * 16u)

/// <summary>
/// Calculate the offset for where event tables start in an frt file.
/// </summary>
/// <param name="nodesOffset">Offset for where nodes start in the frt file.</param>
/// <param name="nodeCount">Number of nodes in the routeset.</param>
let private getEventTablesOffset nodesOffset (nodeCount : uint16) =
    nodesOffset + (uint32 nodeCount * 12u)

/// <summary>
/// Calculate the offset for where events start in an frt file.
/// </summary>
/// <param name="eventTablesOffset">Offset for where event tables start in the frt file.</param>
/// <param name="nodeCount">Number of nodes in the routeset.</param>
let private getEventsOffset eventTablesOffset (nodeCount : uint16) =
    eventTablesOffset + (uint32 nodeCount * 4u)
    
/// <summary>
/// Construct header data for a routeset.
/// </summary>
/// <param name="routeCount">Number of routes in the routeset.</param>
/// <param name="nodeCount">Number of nodes in the routeset.</param>
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
    RouteDefinitionsOffset = routeDefinitionsOffset;
    NodesOffset = nodesOffset;
    EventTablesOffset = eventTablesOffset;
    EventsOffset = eventsOffset }

/// <summary>
/// Write an frt header.
/// </summary>
/// <param name="writeChar">Function to write a char.</param>
/// <param name="writeUInt16">Function to write a uint16.</param>
/// <param name="writeUInt32">Function to write a uint32.</param>
/// <param name="header"Header data to write.></param>
let private writeHeader writeChar writeUInt16 writeUInt32 header =
    ['R';'O';'U';'T'] |> Seq.iter (fun entry -> writeChar entry)
    header.Version |> writeUInt16
    header.RouteCount |> writeUInt16
    header.RouteIdsOffset |> writeUInt32
    header.RouteDefinitionsOffset |> writeUInt32
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

    // Sort routes in ascending order of ID.
    let sortedRoutes = routeSet.Routes
                        |> Seq.sortBy (fun route -> route.Name)

    // Write route IDs.
    sortedRoutes
    |> Seq.toArray
    |> Seq.iter (fun route -> convertedWriteFunctions.WriteUInt32 route.Name)
    
    // Write route definitions.
    let allNodes = sortedRoutes                   
                   |> Seq.collect (fun route -> route.Nodes)
                   |> Seq.toArray

    let allEvents = allNodes
                    |> Seq.collect (fun node -> node.Events |> Seq.append (seq [node.EdgeEvent]) )
                    |> Seq.toArray
                    |> Seq.distinct // Necessary?
                    
    // TODO: This is calculated twice. Do this earlier and pass this into the route definition build routine.
    let routeEventCounts = sortedRoutes
                            |> Seq.map (fun route -> getRouteEventCount allNodes route)

    let getRouteInitialEventIndex index = Seq.take index routeEventCounts |> Seq.sum

    let getRouteDefinitionOffset index = getOffsetForRouteDefinition header.RouteDefinitionsOffset index    
    
    sortedRoutes
    |> Seq.mapi (fun index route -> route |> buildRouteDefinition allNodes header.NodesOffset header.EventTablesOffset header.EventsOffset (fun () -> getRouteDefinitionOffset index) (getRouteInitialEventIndex index))
    |> Seq.iter (fun definition -> writeRouteDefinition convertedWriteFunctions.WriteUInt32 convertedWriteFunctions.WriteUInt16 definition)

    let routeEventsIndices = makeRouteEventsListIndices sortedRoutes
    
    // Write nodes.
    allNodes
    |> Seq.map (fun node -> node.Position)
    |> Seq.toArray
    |> Seq.iter (fun node -> Vector3.Write node convertedWriteFunctions.WriteSingle)    
    
    // Write event tables.
    [0..Seq.length allNodes - 1]
    |> Seq.map (fun index -> makeEventTable allNodes index (Seq.item index routeEventsIndices))
    |> Seq.toArray
    |> Seq.iter (writeEventTable convertedWriteFunctions.WriteUInt16)
    
    // Write events.
    allEvents
    |> Seq.iter (writeEvent convertedWriteFunctions.WriteUInt32 convertedWriteFunctions.WriteUInt32 convertedWriteFunctions.WriteChar)