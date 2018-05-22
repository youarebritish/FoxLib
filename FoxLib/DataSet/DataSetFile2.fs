module FoxLib.DataSetFile2

open System
open FoxLib.Core
open FoxLib

/// <summary>
/// A StrCode hash and its corresponding string.
/// </summary>
type private StringLiteral = {
    Hash : StrCodeHash
    Literal : string
}

/// <summary>
/// Tries to get the string for a StrCode hash.
/// </summary>
/// <param name="hash">StrCode hash whose string to retrieve.</param>
/// <param name="lookupTable">StrCode hash to string lookup table.</param>
let private tryGetStringFromHash hash (lookupTable : Map<StrCodeHash, string>) =
    lookupTable.TryFind hash

/// <summary>
/// Converts a sequence of StringLiterals to a hash to string lookup table.
/// </summary>
/// <param name="literals"></param>
let private makeStringLookupTable literals =
    literals
    |> Seq.map (fun literal -> literal.Hash, literal.Literal)
    |> Map.ofSeq
    |> Map.add 203000540209048UL ""

/// <summary>
/// Reads a string literal.
/// </summary>
/// <param name="hash">StrCode hash of the string literal.</param>
/// <param name="readString">Function to read a string of specified length.</param>
/// <param name="readUInt32">Function to read a uint32.</param>
let private readStringLiteral hash readString readUInt32 =
    let stringLength = readUInt32()
    let stringLiteral = readString stringLength

    { StringLiteral.Hash = hash; Literal = stringLiteral; }

/// <summary>
/// Reads a DataSetFile2 string table entry.
/// </summary>
/// <param name="readString">Function to read a string of specified length.</param>
/// <param name="readUInt32">Function to read a uint32.</param>
/// <param name="readUInt64">Function to read a uint64.</param>
let private readStringTableEntry readString readUInt32 readUInt64 =
    let hash = readUInt64()
    match hash with
    | 0UL -> None
    | _ -> Some (readStringLiteral hash readString readUInt32)

/// <summary>
/// Reads a DataSetFile2 string table.
/// </summary>
/// <param name="readString">Function to read a string of specified length.</param>
/// <param name="readUInt32">Function to read a uint32.</param>
/// <param name="readUInt64">Function to read a uint64.</param>
let private readStringTable readString readUInt32 readUInt64 =
    Seq.initInfinite (fun _ -> readStringTableEntry readString readUInt32 readUInt64)
    |> Seq.takeWhile (fun literal -> match literal with
                                        | Some _ -> true
                                        | None -> false)
    |> Seq.map (fun literal -> literal.Value)
    |> Seq.toArray
    
/// <summary>
/// Reads the contents of a PropertyInfo container.
/// </summary>
/// <param name="containerType">Type of the container.</param>
/// <param name="arraySize">Number of values in the container.</param>
/// <param name="readString">Function to read a string of specified length.</param>
/// <param name="readValue">Function to read a single value.</param>
/// <param name="alignRead">Function to align the reader.</param>
let private readContainerContents<'T> containerType arraySize readString (readValue : unit -> 'T) alignRead =
    let readArray() = [|1us..arraySize|]
                        |> Array.map (fun _ -> readValue())

    let readStringMap() = [|1us..arraySize|]
                            |> Array.map (fun _ ->  let key, value = readString(), readValue()
                                                    alignRead 16
                                                    key, value)
                            |> Map.ofArray

    let result = match containerType with
    | ContainerType.StaticArray -> StaticArray (readArray())
    | ContainerType.DynamicArray -> DynamicArray (readArray())
    | ContainerType.StringMap -> StringMap (readStringMap())
    | ContainerType.List -> List (readArray())
    | _ -> invalidOp "Unrecognized container type."

    result

/// <summary>
/// Reads a PropertyInfo container.
/// </summary>
/// <param name="dataType">Type of data in the container.</param>
/// <param name="containerType">Type of the container itself.</param>
/// <param name="arraySize">Number of values in the container.</param>
/// <param name="unhashString">Function to unhash a string.</param>
/// <param name="alignRead">Function to align the reader.</param>
/// <param name="readInt8">Function to read an int8.</param>
/// <param name="readUInt8">Function to read an uint8.</param>
/// <param name="readInt16">Function to read an int16.</param>
/// <param name="readUInt16">Function to read an uint16.</param>
/// <param name="readInt32">Function to read an int32.</param>
/// <param name="readUInt32">Function to read an uint32.</param>
/// <param name="readInt64">Function to read an int64.</param>
/// <param name="readUInt64">Function to read an uint64.</param>
/// <param name="readSingle">Function to read a float32.</param>
/// <param name="readDouble">Function to read an double.</param>
/// <param name="readBool">Function to read an bool.</param>
let private readContainer dataType containerType arraySize (unhashString : StrCodeHash -> string) alignRead readInt8 readUInt8 readInt16 readUInt16 readInt32 readUInt32 readInt64 readUInt64 readSingle readDouble readBool =
    let readHash = readUInt64

    let readVector3() = let value = { Vector3.X = readSingle(); Y = readSingle(); Z = readSingle(); }
                        readSingle() |> ignore
                        value

    let readVector4() = Vector4.Read readSingle
    let readQuat() = Quaternion.Read readSingle
    let readMatrix3() = Matrix3.Read readSingle
    let readMatrix4() = Matrix4.Read readSingle
    let readColor() = ColorRGBA.Read readSingle

    let readString() = let hash = readHash()
                       unhashString hash
    
    let readEntityLink() =  let packagePath = readString();
                            let archivePath = readString();
                            let nameInArchive = readString();
                            let entityHandle = readUInt64();
                            { PackagePath = packagePath; ArchivePath = archivePath; NameInArchive = nameInArchive; EntityHandle = entityHandle }
    
    let readWideVector3() = WideVector3.Read readSingle readUInt16    

    match dataType with
    | PropertyInfoType.Int8 -> (readContainerContents<int8> containerType arraySize readString readInt8 alignRead) :> IContainer
    | PropertyInfoType.UInt8 -> (readContainerContents<uint8> containerType arraySize readString readUInt8 alignRead) :> IContainer
    | PropertyInfoType.Int16 -> (readContainerContents<int16> containerType arraySize readString readInt16 alignRead) :> IContainer
    | PropertyInfoType.UInt16 -> (readContainerContents<uint16> containerType arraySize readString readUInt16 alignRead) :> IContainer
    | PropertyInfoType.Int32 -> (readContainerContents<int32> containerType arraySize readString readInt32 alignRead) :> IContainer
    | PropertyInfoType.UInt32 -> (readContainerContents<uint32> containerType arraySize readString readUInt32 alignRead) :> IContainer
    | PropertyInfoType.Int64 -> (readContainerContents<int64> containerType arraySize readString readInt64 alignRead) :> IContainer
    | PropertyInfoType.UInt64 -> (readContainerContents<uint64> containerType arraySize readString readUInt64 alignRead) :> IContainer
    | PropertyInfoType.Float -> (readContainerContents<float32> containerType arraySize readString readSingle alignRead) :> IContainer
    | PropertyInfoType.Double -> (readContainerContents<float> containerType arraySize readString readDouble alignRead) :> IContainer
    | PropertyInfoType.Bool -> (readContainerContents<bool> containerType arraySize readString readBool alignRead) :> IContainer
    | PropertyInfoType.String -> (readContainerContents<string> containerType arraySize readString readString alignRead) :> IContainer
    | PropertyInfoType.Path -> (readContainerContents<string> containerType arraySize readString readString alignRead) :> IContainer
    | PropertyInfoType.EntityPtr -> (readContainerContents<uint64> containerType arraySize readString readUInt64 alignRead) :> IContainer
    | PropertyInfoType.Vector3 -> (readContainerContents<Vector3> containerType arraySize readString readVector3 alignRead) :> IContainer
    | PropertyInfoType.Vector4 -> (readContainerContents<Vector4> containerType arraySize readString readVector4 alignRead) :> IContainer
    | PropertyInfoType.Quat -> (readContainerContents<Quaternion> containerType arraySize readString readQuat alignRead) :> IContainer
    | PropertyInfoType.Matrix3 -> (readContainerContents<Matrix3> containerType arraySize readString readMatrix3 alignRead) :> IContainer
    | PropertyInfoType.Matrix4 -> (readContainerContents<Matrix4> containerType arraySize readString readMatrix4 alignRead) :> IContainer
    | PropertyInfoType.Color -> (readContainerContents<ColorRGBA> containerType arraySize readString readColor alignRead) :> IContainer
    | PropertyInfoType.FilePtr -> (readContainerContents<string> containerType arraySize readString readString alignRead) :> IContainer
    | PropertyInfoType.EntityHandle -> (readContainerContents<uint64> containerType arraySize readString readUInt64 alignRead) :> IContainer
    | PropertyInfoType.EntityLink -> (readContainerContents<EntityLink> containerType arraySize readString readEntityLink alignRead) :> IContainer
    | PropertyInfoType.WideVector3 -> (readContainerContents<WideVector3> containerType arraySize readString readWideVector3 alignRead) :> IContainer
    | _ -> invalidOp "Unrecogfnized PropertyInfo type."

/// <summary>
/// Reads an Entity property.
/// </summary>
/// <param name="readDataType">Function to read the property's type.</param>
/// <param name="readContainerType">Function to read the container's type.</param>
/// <param name="tryUnhashString">Function to unhash a string.</param>
/// <param name="readContainerFunc">Function to read the property's container.</param>
/// <param name="readUInt64">Function to read a uint64.</param>
/// <param name="readUInt16">Function to read a uint16.</param>
/// <param name="skipBytes">Function to skip a number of bytes.</param>
/// <param name="alignRead">Function to align the reader.</param>
let private readProperty readDataType readContainerType (tryUnhashString : StrCodeHash -> string) readContainerFunc readUInt64 readUInt16 skipBytes alignRead =
    let nameHash = readUInt64()

    let dataType = readDataType()
    let containerType = readContainerType()
    let arraySize = readUInt16()
        
    let offset = readUInt16()
    let size = readUInt16()

    let padding0 = readUInt64()
    let padding1 = readUInt64()
    
    let container = readContainerFunc dataType containerType arraySize 

    alignRead 16

    { Name = tryUnhashString nameHash;
    Type = dataType;
    Container = container }

/// <summary>
/// Reads an Entity.
/// </summary>
/// <param name="readContainerFunc">Function to read a PropertyInfo container.</param>
/// <param name="readPropertyInfoType">Function to read a PropertyInfo's type.</param>
/// <param name="readContainerType">Function to read a PropertyInfo container's type.</param>
/// <param name="unhashString">Function to unhash a string.</param>
/// <param name="readUInt16">Function to read a uint16.</param>
/// <param name="readUInt32">Function to read a uint32.</param>
/// <param name="readUInt64">Function to read a uint64.</param>
/// <param name="skipBytes">Function to skip a number of bytes.</param>
/// <param name="alignRead">Function to align the reader.</param>
let private readEntity readContainerFunc readPropertyInfoType readContainerType (unhashString : StrCodeHash -> string) readUInt16 readUInt32 readUInt64 skipBytes alignRead =
    skipBytes 2
    
    let classId = readUInt16()

    skipBytes 6

    let address = readUInt32()

    skipBytes 12

    let version = readUInt16()
    let classNameHash = readUInt64()
    let staticPropertyCount = readUInt16()
    let dynamicPropertyCount = readUInt16()

    skipBytes 12
    alignRead 16
    
    let staticProperties = [|1us.. staticPropertyCount|]
                            |> Array.map (fun _ -> readProperty readPropertyInfoType readContainerType unhashString readContainerFunc readUInt64 readUInt16 skipBytes alignRead)

    let dynamicProperties = [|1us.. dynamicPropertyCount|]
                            |> Array.map (fun _ -> readProperty readPropertyInfoType readContainerType unhashString readContainerFunc readUInt64 readUInt16 skipBytes alignRead)

    { ClassName = unhashString classNameHash;
    Address = address;
    ClassId = classId;
    Version = version;
    StaticProperties = staticProperties;
    DynamicProperties = dynamicProperties }

/// <summary>
/// Data read from a DataSetFile2 header.
/// </summary>
type private HeaderReadData = {
    /// Number of entities in the file.
    EntityCount : uint32
    /// Location in the file of the string table.
    StringTableOffset : int64
}
    
/// <summary>
/// Reads header data.
/// </summary>
/// <param name="readUInt32">Function to read a uint32.</param>
/// <param name="skipBytes">Function to skip a number of bytes.</param>
/// <returns>Number of Entities and the string table offset.</returns>
let private readHeader readUInt32 skipBytes =
    skipBytes 8

    let entityCount = readUInt32()
    let stringTableOffset = readUInt32() |> int64

    skipBytes 16

    { EntityCount = entityCount; StringTableOffset = stringTableOffset}

/// <summmary>
/// Input functions to the Read function.
/// </summmary>
type public ReadFunctions = {
    /// Function to read a int8.
    ReadInt8 : Func<int8>
    /// Function to read a uint8.
    ReadUInt8 : Func<uint8>
    /// Function to read a int16.
    ReadInt16 : Func<int16>
    /// Function to read a int16.
    ReadUInt16 : Func<uint16>    
    /// Function to read a int32.
    ReadInt32 : Func<int32>
    /// Function to read a uint32.
    ReadUInt32 : Func<uint32>
    /// Function to read a int64.
    ReadInt64 : Func<int64>
    /// Function to read a uint64.
    ReadUInt64 : Func<uint64>
    /// Function to read a float32.
    ReadSingle : Func<float32>
    /// Function to read a double.
    ReadDouble : Func<float>
    /// Function to read a bool.
    ReadBool : Func<bool>
    /// Function to read a string of specified length.
    ReadString : Func<uint32, string>
    /// Function to get the current stream position.
    GetStreamPosition : Func<int64>
    /// Function to set the current stream position.
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
    /// Function to read a int8.
    ReadInt8 : unit -> int8
    /// Function to read a uint8.
    ReadUInt8 : unit -> uint8
    /// Function to read a int16.
    ReadInt16 : unit -> int16
    /// Function to read a uint16.
    ReadUInt16 : unit -> uint16    
    /// Function to read a int32.
    ReadInt32 : unit -> int32
    /// Function to read a uint32.
    ReadUInt32 : unit -> uint32
    /// Function to read a int64.
    ReadInt64 : unit -> int64
    /// Function to read a uint64.
    ReadUInt64 : unit -> uint64
    /// Function to read a float32.
    ReadSingle : unit -> float32
    /// Function to read a double.
    ReadDouble : unit -> float
    /// Function to read a bool.
    ReadBool : unit -> bool
    /// Function to read a string of specified length.
    ReadString : uint32 -> string
    /// Function to retrieve the current stream position.
    GetStreamPosition : unit -> int64
    /// Function to set the current stream position.
    SetStreamPosition : int64 -> unit
    /// Function to skip a number of bytes.
    SkipBytes : int -> unit
    /// Function to align the stream by a specified number of bytes.
    AlignRead : int -> unit
}

/// <summmary>
/// Converts the Read function's .NET Funcs into F# functions.
/// </summmary>
/// <param name="rawReadFunctions">Input functions supplied to the Read function.</param>
/// <returns>The converted functions.</returns>
let private convertReadFunctions (rawReadFunctions : ReadFunctions) =
    if rawReadFunctions.ReadInt8 |> isNull then nullArg "ReadInt8"
    if rawReadFunctions.ReadUInt8 |> isNull then nullArg "ReadUInt8"
    if rawReadFunctions.ReadInt16 |> isNull then nullArg "ReadInt16"
    if rawReadFunctions.ReadUInt16 |> isNull then nullArg "ReadUInt16"
    if rawReadFunctions.ReadInt32 |> isNull then nullArg "ReadInt32"
    if rawReadFunctions.ReadUInt32 |> isNull then nullArg "ReadUInt32"
    if rawReadFunctions.ReadInt64 |> isNull then nullArg "ReadInt64"
    if rawReadFunctions.ReadUInt64 |> isNull then nullArg "ReadUInt64"
    if rawReadFunctions.ReadSingle |> isNull then nullArg "ReadSingle"
    if rawReadFunctions.ReadDouble |> isNull then nullArg "ReadDouble"
    if rawReadFunctions.ReadBool |> isNull then nullArg "ReadBool"
    if rawReadFunctions.ReadString |> isNull then nullArg "ReadString"
    if rawReadFunctions.GetStreamPosition |> isNull then nullArg "GetStreamPosition"
    if rawReadFunctions.SetStreamPosition |> isNull then nullArg "SetStreamPosition"
    if rawReadFunctions.SkipBytes |> isNull then nullArg "SkipBytes"
    if rawReadFunctions.AlignRead |> isNull then nullArg "AlignRead"

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

/// <summary>
/// Reads Entities from a DataSetFile2.
/// </summary>
/// <param name="readFunctions">Functions to read various data types from the input.</param>
/// <returns>The parsed Entities.</returns>
let public Read readFunctions =
    let convertedReadFunctions = convertReadFunctions readFunctions

    let headerData = readHeader convertedReadFunctions.ReadUInt32 convertedReadFunctions.SkipBytes    

    let readPropertyInfoType() = convertedReadFunctions.ReadUInt8() |> LanguagePrimitives.EnumOfValue
    let readContainerType() = convertedReadFunctions.ReadUInt8() |> LanguagePrimitives.EnumOfValue
    
    // Memorize current stream position and jump to the string table.
    let entityOffset = convertedReadFunctions.GetStreamPosition()
    convertedReadFunctions.SetStreamPosition headerData.StringTableOffset

    // Read the string table and jump back to the entity definitions.
    let hashStringLiterals = readStringTable convertedReadFunctions.ReadString convertedReadFunctions.ReadUInt32 convertedReadFunctions.ReadUInt64
    convertedReadFunctions.SetStreamPosition entityOffset

    let stringLookupTable = makeStringLookupTable hashStringLiterals
    let unhashString hash = let unhashResult = (tryGetStringFromHash hash stringLookupTable)
                            if unhashResult.IsSome then unhashResult.Value
                            else null

    let readContainerFunc = (fun dataType containerType arraySize -> readContainer dataType containerType arraySize
                                                                        unhashString
                                                                        convertedReadFunctions.AlignRead
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

    [|1u..headerData.EntityCount|]
    |> Array.map (fun _ -> readEntity readContainerFunc readPropertyInfoType readContainerType unhashString convertedReadFunctions.ReadUInt16 convertedReadFunctions.ReadUInt32 convertedReadFunctions.ReadUInt64 convertedReadFunctions.SkipBytes convertedReadFunctions.AlignRead)
    |> Array.toSeq