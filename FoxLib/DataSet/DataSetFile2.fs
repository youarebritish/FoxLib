module FoxLib.DataSetFile2

open System
open FoxLib.Core
open FoxLib

type private StringLiteral = {
    Hash : StrCodeHash
    Literal : string
}

let private tryGetStringFromHash hash (lookupTable : Map<StrCodeHash, string>) =
    lookupTable.TryFind hash

let private makeStringLookupTable literals =
    literals
    |> Array.map (fun literal -> literal.Hash, literal.Literal)
    |> Map.ofArray
    |> Map.add 203000540209048UL ""

let private readStringLiteral hash readString readUInt32 =
    let stringLength = readUInt32()
    let stringLiteral = readString stringLength

    { StringLiteral.Hash = hash; Literal = stringLiteral; }

let private readStringTableEntry readString readUInt32 readUInt64 =
    let hash = readUInt64()
    match hash with
    | 0UL -> None
    | _ -> Some (readStringLiteral hash readString readUInt32)

let private readStringTable readString readUInt32 readUInt64 =
    Seq.initInfinite (fun _ -> readStringTableEntry readString readUInt32 readUInt64)
    |> Seq.takeWhile (fun literal -> match literal with
                                        | Some _ -> true
                                        | None -> false)
    |> Seq.map (fun literal -> literal.Value)
    |> Seq.toArray
    
let private readContainer<'T> containerType arraySize readString (readValue : unit -> 'T) =
    let readArray() = [|1us..arraySize|]
                        |> Array.map (fun _ -> readValue())

    let readStringMap() = [|1us..arraySize|]
                            |> Array.map (fun _ -> readString(), readValue())
                            |> Map.ofArray

    let result = match containerType with
    | ContainerType.StaticArray -> StaticArray (readArray())
    | ContainerType.DynamicArray -> DynamicArray (readArray())
    | ContainerType.StringMap -> StringMap (readStringMap())
    | ContainerType.List -> List (readArray())

    result

let private readTypedContainer dataType containerType arraySize (unhashString : StrCodeHash -> string) readInt8 readUInt8 readInt16 readUInt16 readInt32 readUInt32 readInt64 readUInt64 readSingle readDouble readBool =
    let readHash = readUInt64

    let readVector3() = let value = { Vector3.X = readSingle(); Y = readSingle(); Z = readSingle(); }
                        readSingle() |> ignore
                        value

    let readVector4() = Vector4.Read readSingle
    let readQuat() = Quaternion.Read readSingle
    let readMatrix3() = Matrix3.Read readSingle
    let readMatrix4() = Matrix4.Read readSingle
    let readColor() = ColorRGBA.Read readSingle
    let readEntityLink() = { PackagePath = readHash(); ArchivePath = readHash(); NameInArchive = readHash(); EntityHandle = readUInt64() }
    let readWideVector3() = WideVector3.Read readSingle readUInt16    
    let readString() = let hash = readHash()
                       unhashString hash

    match dataType with
    | PropertyInfoType.Int8 -> (readContainer<int8> containerType arraySize readString readInt8) :> IContainer
    | PropertyInfoType.UInt8 -> (readContainer<uint8> containerType arraySize readString readUInt8) :> IContainer
    | PropertyInfoType.Int16 -> (readContainer<int16> containerType arraySize readString readInt16) :> IContainer
    | PropertyInfoType.UInt16 -> (readContainer<uint16> containerType arraySize readString readUInt16) :> IContainer
    | PropertyInfoType.Int32 -> (readContainer<int32> containerType arraySize readString readInt32) :> IContainer
    | PropertyInfoType.UInt32 -> (readContainer<uint32> containerType arraySize readString readUInt32) :> IContainer
    | PropertyInfoType.Int64 -> (readContainer<int64> containerType arraySize readString readInt64) :> IContainer
    | PropertyInfoType.UInt64 -> (readContainer<uint64> containerType arraySize readString readUInt64) :> IContainer
    | PropertyInfoType.Float -> (readContainer<float32> containerType arraySize readString readSingle) :> IContainer
    | PropertyInfoType.Double -> (readContainer<float> containerType arraySize readString readDouble) :> IContainer
    | PropertyInfoType.Bool -> (readContainer<bool> containerType arraySize readString readBool) :> IContainer
    | PropertyInfoType.String -> (readContainer<string> containerType arraySize readString readString) :> IContainer
    | PropertyInfoType.Path -> (readContainer<string> containerType arraySize readString readString) :> IContainer
    | PropertyInfoType.EntityPtr -> (readContainer<uint64> containerType arraySize readString readUInt64) :> IContainer
    | PropertyInfoType.Vector3 -> (readContainer<Vector3> containerType arraySize readString readVector3) :> IContainer
    | PropertyInfoType.Vector4 -> (readContainer<Vector4> containerType arraySize readString readVector4) :> IContainer
    | PropertyInfoType.Quat -> (readContainer<Quaternion> containerType arraySize readString readQuat) :> IContainer
    | PropertyInfoType.Matrix3 -> (readContainer<Matrix3> containerType arraySize readString readMatrix3) :> IContainer
    | PropertyInfoType.Matrix4 -> (readContainer<Matrix4> containerType arraySize readString readMatrix4) :> IContainer
    | PropertyInfoType.Color -> (readContainer<ColorRGBA> containerType arraySize readString readColor) :> IContainer
    | PropertyInfoType.FilePtr -> (readContainer<string> containerType arraySize readString readString) :> IContainer
    | PropertyInfoType.EntityHandle -> (readContainer<uint64> containerType arraySize readString readUInt64) :> IContainer
    | PropertyInfoType.EntityLink -> (readContainer<EntityLink> containerType arraySize readString readEntityLink) :> IContainer
    | PropertyInfoType.WideVector3 -> (readContainer<WideVector3> containerType arraySize readString readWideVector3) :> IContainer

let private readProperty readDataType readContainerType (tryUnhashString : StrCodeHash -> string) readContainerFunc readUInt64 readUInt16 skipBytes alignRead =
    let nameHash = readUInt64()
    let dataType = readDataType()
    let containerType = readContainerType()
    let arraySize = readUInt16()
        
    let offset = readUInt16()
    let size = readUInt16()

    let unknown2 = readUInt64()
    let unknown4 = readUInt64()
    
    let container = readContainerFunc dataType containerType arraySize 

    alignRead 16 |> ignore

    { Name = tryUnhashString nameHash;
    Type = dataType;
    Container = container }

let private readEntity readContainerFunc readPropertyInfoType readContainerType (unhashString : StrCodeHash -> string) readUInt16 readUInt32 readUInt64 skipBytes alignRead =
    skipBytes 2 |> ignore
    
    let classId = readUInt16()

    skipBytes 6 |> ignore

    let address = readUInt32()

    skipBytes 12 |> ignore

    let version = readUInt16()
    let classNameHash = readUInt64()
    let staticPropertyCount = readUInt16()
    let dynamicPropertyCount = readUInt16()

    skipBytes 12 |> ignore
    alignRead 16 |> ignore
    
    let staticProperties = [|1us.. staticPropertyCount|]
                            |> Array.map (fun _ -> readProperty readPropertyInfoType readContainerType unhashString readContainerFunc readUInt64 readUInt16 skipBytes alignRead)

    let dynamicProperties = [|1us.. dynamicPropertyCount|]
                            |> Array.map (fun _ -> readProperty readPropertyInfoType readContainerType unhashString readContainerFunc readUInt64 readUInt16 skipBytes alignRead)

    { ClassName = unhashString classNameHash;
    ClassId = classId;
    Version = version;
    Address = address;
    StaticProperties = staticProperties;
    DynamicProperties = dynamicProperties }
    
let private readHeader readUInt32 skipBytes =
    skipBytes 8 |> ignore

    let entityCount = readUInt32()
    let stringTableOffset = readUInt32() |> int64

    skipBytes 16 |> ignore

    entityCount, stringTableOffset

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
    ReadString : Func<uint32, string>
    GetStreamPosition : Func<int64>
    SetStreamPosition : Action<int64>
    /// Function to skip a number of bytes.
    SkipBytes : Action<int>
    /// Function to align the stream.
    AlignRead : Action<int>
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
    ReadString : uint32 -> string
    GetStreamPosition : unit -> int64
    SetStreamPosition : int64 -> unit
    SkipBytes : int -> unit
    AlignRead : int -> unit
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
    if rawReadFunctions.SkipBytes |> isNull then nullArg "SkipBytes"
    // TODO add checks to the rest

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
    ReadString = rawReadFunctions.ReadString.Invoke;
    GetStreamPosition = rawReadFunctions.GetStreamPosition.Invoke;
    SetStreamPosition = rawReadFunctions.SetStreamPosition.Invoke;
    SkipBytes = rawReadFunctions.SkipBytes.Invoke;
    AlignRead = rawReadFunctions.AlignRead.Invoke; }

let public Read readFunctions =
    let convertedReadFunctions = convertReadFunctions readFunctions

    let entityCount, stringTableOffset = readHeader convertedReadFunctions.ReadUInt32 convertedReadFunctions.SkipBytes
    

    let readPropertyInfoType() = convertedReadFunctions.ReadUInt8() |> LanguagePrimitives.EnumOfValue
    let readContainerType() = convertedReadFunctions.ReadUInt8() |> LanguagePrimitives.EnumOfValue
    
    // Memorize current stream position and jump to the string table.
    let entityOffset = convertedReadFunctions.GetStreamPosition()
    convertedReadFunctions.SetStreamPosition stringTableOffset

    // Read the string table and jump back to the entity definitions.
    let hashStringLiterals = readStringTable convertedReadFunctions.ReadString convertedReadFunctions.ReadUInt32 convertedReadFunctions.ReadUInt64
    convertedReadFunctions.SetStreamPosition entityOffset

    let stringLookupTable = makeStringLookupTable hashStringLiterals
    let unhashString hash = (tryGetStringFromHash hash stringLookupTable).Value

    let readContainerFunc = (fun dataType containerType arraySize -> readTypedContainer dataType containerType arraySize
                                                                        unhashString
                                                                        convertedReadFunctions.ReadInt8
                                                                        convertedReadFunctions.ReadUInt8
                                                                        convertedReadFunctions.ReadInt16
                                                                        convertedReadFunctions.ReadUInt16
                                                                        convertedReadFunctions.ReadInt32
                                                                        convertedReadFunctions.ReadUInt32
                                                                        convertedReadFunctions.ReadInt64
                                                                        convertedReadFunctions.ReadUInt64
                                                                        convertedReadFunctions.ReadSingle
                                                                        convertedReadFunctions.ReadDouble
                                                                        convertedReadFunctions.ReadBool)

    let entities = [|1u..entityCount|]
                    |> Array.map (fun _ -> readEntity readContainerFunc readPropertyInfoType readContainerType unhashString convertedReadFunctions.ReadUInt16 convertedReadFunctions.ReadUInt32 convertedReadFunctions.ReadUInt64 convertedReadFunctions.SkipBytes convertedReadFunctions.AlignRead)
    
    
    entities