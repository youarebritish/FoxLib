module FoxLib.FormVariation

open FoxLib.Core
open System

/// <summary>
/// Metadata for an fv2 file.
/// </summary>
type public Header = {
    Section2Offset : uint16;
    ExternalFileSectionOffset : uint16;
    Section2Entries : uint16;
    ExternalFileSectionEntries : uint16;
    Section3Offset : uint16;
    Section3Entries : uint16;
    NumTextures : uint16;
    NumHiddenMeshGroups : byte;
    NumShownMeshGroups : byte;
    NumMaterialInstances : byte;
    Unknown0 : byte;
    NumBoneAttachedModels : byte;
    NumCNPAttachedModels : byte;
}

/// <summary>
/// Read an fv2 header.
/// </summary>
/// <param name="readChars">Function to read a number of chars.</param>
/// <param name="readEntriesCount">Function to read an entries count.</param>
/// <param name="readOffset">Function to read an offset.</param>
/// <param name="readCount">Function to read a count.</param>
/// <param name="skipBytes">Function to skip bytes.</param>
let private readHeader readChars readEntriesCount readOffset readCount skipBytes =
    let signature = readChars 8;
    
    let section2Offset = readOffset();
    let externalFileSectionOffset = readOffset();
    let section2Entries = readEntriesCount();
    let externalFileSectionEntries = readEntriesCount();
    let section3Offset = readOffset();
    let section3Entries = readEntriesCount();

    skipBytes 4

    let numTextures = readEntriesCount();

    skipBytes 4

    let numHiddenMeshGroups = readCount();
    let numShownMeshGroups = readCount();
    let numMaterialInstances = readCount();
    let unknown0 = readCount();
    let numBoneAttachedModels = readCount();
    let numCNPAttachedModels = readCount();

    { Section2Offset = section2Offset;
    ExternalFileSectionOffset = externalFileSectionOffset;
    Section2Entries = section2Entries;
    ExternalFileSectionEntries = externalFileSectionEntries;
    Section3Offset = section3Offset;
    Section3Entries =  section3Entries;

    NumTextures = numTextures;
    
    NumHiddenMeshGroups = numHiddenMeshGroups;
    NumShownMeshGroups = numShownMeshGroups;
    NumMaterialInstances = numMaterialInstances;
    Unknown0 = unknown0;
    NumBoneAttachedModels = numBoneAttachedModels;
    NumCNPAttachedModels = numCNPAttachedModels;
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
    /// Function to read a byte.
    ReadByte : Func<byte>
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
    ReadByte : unit -> byte
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
    if rawReadFunctions.ReadByte |> isNull then nullArg "ReadByte"
    if rawReadFunctions.ReadBytes |> isNull then nullArg "ReadBytes"
    if rawReadFunctions.SkipBytes |> isNull then nullArg "SkipBytes"

    { ConvertedReadFunctions.ReadSingle = rawReadFunctions.ReadSingle.Invoke;
    ReadUInt16 = rawReadFunctions.ReadUInt16.Invoke;
    ReadUInt32 = rawReadFunctions.ReadUInt32.Invoke;
    ReadInt32 = rawReadFunctions.ReadInt32.Invoke;
    ReadByte = rawReadFunctions.ReadByte.Invoke;
    ReadBytes = rawReadFunctions.ReadBytes.Invoke;
    SkipBytes = rawReadFunctions.SkipBytes.Invoke; }

let public Read (readFunctions : ReadFunctions) =
    let convertedFunctions = convertReadFunctions readFunctions

    let header = readHeader convertedFunctions.ReadBytes convertedFunctions.ReadUInt16 convertedFunctions.ReadUInt16 convertedFunctions.ReadByte convertedFunctions.SkipBytes   
    header