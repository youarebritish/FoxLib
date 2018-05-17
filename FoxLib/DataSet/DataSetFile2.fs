module DataSetFile2

open System
open FoxLib.Core
open FoxLib

type private IContainer = interface end

type private Container<'T> =
    | StaticArray of 'T[]
    | DynamicArray of 'T[]
    | List of 'T[]
    | StringMap of Map<StrCodeHash, 'T>
    interface IContainer
    
type private ContainerType =
    | StaticArray = 0
    | DynamicArray = 1
    | StringMap = 2
    | List = 3

let private readContainer<'T> containerType arraySize readHash (readValue : unit -> 'T) =
    let readArray() = [|1..arraySize|]
                        |> Array.map (fun _ -> readValue())

    let readStringMap() = [|1..arraySize|]
                            |> Array.map (fun _ -> readHash(), readValue())
                            |> Map.ofArray

    match containerType with
    | ContainerType.StaticArray -> StaticArray (readArray())
    | ContainerType.DynamicArray -> DynamicArray (readArray())
    | ContainerType.StringMap -> StringMap (readStringMap())
    | ContainerType.List -> List (readArray())

let private readTypedContainer dataType containerType arraySize readInt8 readUInt8 readInt16 readUInt16 readInt32 readUInt32 readInt64 readUInt64 readSingle readDouble readBool =
    let readHash = readUInt64
    let readVector3 = (fun _ -> let value = { Vector3.X = readSingle(); Y = readSingle(); Z = readSingle(); }
                                readSingle() |> ignore
                                value)

    let readVector4() = Vector4.Read readSingle
    let readQuat() = Quaternion.Read readSingle

    match dataType with
    | PropertyInfoType.Int8 -> (readContainer<int8> containerType arraySize readHash readInt8) :> IContainer
    | PropertyInfoType.UInt8 -> (readContainer<uint8> containerType arraySize readHash readUInt8) :> IContainer
    | PropertyInfoType.Int16 -> (readContainer<int16> containerType arraySize readHash readInt16) :> IContainer
    | PropertyInfoType.UInt16 -> (readContainer<uint16> containerType arraySize readHash readUInt16) :> IContainer
    | PropertyInfoType.Int32 -> (readContainer<int32> containerType arraySize readHash readInt32) :> IContainer
    | PropertyInfoType.UInt32 -> (readContainer<uint32> containerType arraySize readHash readUInt32) :> IContainer
    | PropertyInfoType.Int64 -> (readContainer<int64> containerType arraySize readHash readInt64) :> IContainer
    | PropertyInfoType.UInt64 -> (readContainer<uint64> containerType arraySize readHash readUInt64) :> IContainer
    | PropertyInfoType.Float -> (readContainer<float32> containerType arraySize readHash readSingle) :> IContainer
    | PropertyInfoType.Double -> (readContainer<float> containerType arraySize readHash readDouble) :> IContainer
    | PropertyInfoType.Bool -> (readContainer<bool> containerType arraySize readHash readBool) :> IContainer
    | PropertyInfoType.String -> (readContainer<StrCodeHash> containerType arraySize readHash readHash) :> IContainer
    | PropertyInfoType.Path -> (readContainer<StrCodeHash> containerType arraySize readHash readHash) :> IContainer
    | PropertyInfoType.EntityPtr -> (readContainer<uint64> containerType arraySize readHash readUInt64) :> IContainer
    | PropertyInfoType.Vector3 -> (readContainer<Vector3> containerType arraySize readHash readVector3) :> IContainer
    | PropertyInfoType.Vector4 -> (readContainer<Vector4> containerType arraySize readHash readVector4) :> IContainer
    | PropertyInfoType.Quat -> (readContainer<Quaternion> containerType arraySize readHash readQuat) :> IContainer

let private readProperty readDataType readContainerType readUInt64 readUInt16 skipBytes alignRead =
    let nameHash = readUInt64()
    let dataType = readDataType()
    let containerType = readContainerType()
    let arraySize = readUInt16()

    skipBytes 160 |> ignore
    
    // TODO let container = readContainer containerType dataType arraySize

    alignRead 16 |> ignore

let private readEntity readUInt16 readUInt32 readUInt64 skipBytes alignRead =
    skipBytes 2 |> ignore
    
    let classId = readUInt16()

    skipBytes 6 |> ignore

    let address = readUInt32()

    skipBytes 12 |> ignore

    let version = readUInt16()
    let classNameHash = readUInt64()
    let staticPropertyCount = readUInt16()
    let dynamicPropertyCount = readUInt16()

    skipBytes 8 |> ignore
    alignRead 16 |> ignore

    // TODO Read static properties
    // TODO Read dynamic properties

let private readHeader readUInt32 skipBytes =
    skipBytes 8 |> ignore
    let entityCount = readUInt32()
    skipBytes 20 |> ignore

    entityCount

/// <summmary>
/// Input functions to the Read function.
/// </summmary>
type public ReadFunctions = {
    ReadInt8 : Func<int8>
    ReadUInt8 : Func<uint8>
    ReadInt16 : Func<int16>
    ReadUInt16 : Func<uint16>    
    /// Function to read a int32.
    ReadInt32 : Func<int32>
    /// Function to read a uint32.
    ReadUInt32 : Func<uint32>
    ReadInt64 : Func<int64>
    /// Function to read a uint32.
    ReadUInt64 : Func<uint64>
    /// Function to read a float32.
    ReadSingle : Func<float32>
    /// Function to read a float.
    ReadDouble : Func<float>
    ReadBool : Func<bool>
    /// Function to read chars.
    ReadBytes : Func<int, byte[]>
    /// Function to skip a number of bytes.
    SkipBytes : Action<int>
}

/// <summmary>
/// Read parameters converted to F# functions.
/// </summmary>
type private ConvertedReadFunctions = {
    ReadInt8 : unit -> int8
    ReadUInt8 : unit -> uint8
    ReadInt16 : unit -> int16
    ReadUInt16 : unit -> uint16    
    /// Function to read a int32.
    ReadInt32 : unit -> int32
    /// Function to read a uint32.
    ReadUInt32 : unit -> uint32
    ReadInt64 : unit -> int64
    /// Function to read a uint32.
    ReadUInt64 : unit -> uint64
    /// Function to read a float32.
    ReadSingle : unit -> float32
    /// Function to read a float.
    ReadDouble : unit -> float
    ReadBool : unit -> bool
    ReadBytes : int -> byte[]
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
    if rawReadFunctions.ReadBytes |> isNull then nullArg "ReadBytes"
    if rawReadFunctions.SkipBytes |> isNull then nullArg "SkipBytes"

    { ConvertedReadFunctions.ReadSingle = rawReadFunctions.ReadSingle.Invoke;
    ReadInt8 = rawReadFunctions.ReadInt8.Invoke;
    ReadUInt8 = rawReadFunctions.ReadUInt8.Invoke;
    ReadInt16 = rawReadFunctions.ReadInt16.Invoke;
    ReadUInt16 = rawReadFunctions.ReadUInt16.Invoke;
    ReadInt32 = rawReadFunctions.ReadInt32.Invoke;
    ReadUInt32 = rawReadFunctions.ReadUInt32.Invoke;
    ReadInt64 = rawReadFunctions.ReadInt64.Invoke;
    ReadUInt64 = rawReadFunctions.ReadUInt64.Invoke;
    ReadDouble = rawReadFunctions.ReadDouble.Invoke;
    ReadBool = rawReadFunctions.ReadBool.Invoke;
    ReadBytes = rawReadFunctions.ReadBytes.Invoke;
    SkipBytes = rawReadFunctions.SkipBytes.Invoke; }

let public Read readFunctions =
    let convertedReadFunctions = convertReadFunctions readFunctions

    let entityCount = readHeader convertedReadFunctions.ReadUInt32 convertedReadFunctions.SkipBytes
    let entities = [|1u..entityCount|]
                    |> Array.map (fun _ -> readEntity)

    ()