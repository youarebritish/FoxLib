module FoxLib.GrTexture

open FoxLib.Core
open System

[<Flags>]
type private FtexUnknownFlags = 
    | Flag0   = 0b0000000000000001
    | Flag1   = 0b0000000000010000
    | Flag2   = 0b0000000100000000 
    | Clp     = 0b0000000000000000 
    | Unknown = 0b0000000000010001
    | Default = 0b0000000100010001

type private TextureType = 
    | Linear = 0b1000000000000000000000001
    | SRGB   = 0b1000000000000000000000011
    | Cube   = 0b1000000000000000000000111
    | Normal = 0b1000000000000000000001001

/// <summary>
/// Ftex mipmap level description
/// </summary>
type private MipmapLevel = {
    Offset : uint32;
    DecompressedFileSize : uint32;
    Size : uint32;
    Index : byte;
    FtexFileNumber : byte;
    ChunkCount : uint16;
}

/// <summary>
/// Metadata for an ftex file.
/// </summary>
type private Header = {
    // Skip 4 bytes allocated to the signature ("FTEX")
    PixelFormatType : uint16;
    Width : uint16;
    Height : uint16;
    Depth : uint16;
    MipMapCount : byte;
    NrtFlag : uint16;
    UnknownFlags : uint16;
    // Skip 8 bytes for an int with a value of 1 and then an int with a value of 0
    TextureType : byte;
    ExtensionFileCount : byte;
    AdditionalExtensionFileCount : byte;
    // Skip 14 bytes
    Hash : uint64;
    Mipmaps : MipmapLevel[]
}

/// <summmary>
/// Input functions to the Read function.
/// </summmary>
type public ReadFunctions = {
    /// Function to read a uint16.
    ReadUInt16 : Func<uint16>
    /// Function to read a uint32.
    ReadUInt32 : Func<uint32>
    /// Function to read a uint64.
    ReadUInt64 : Func<uint64>
    /// Function to read a byte.
    ReadByte : Func<byte>
    /// Function to skip a number of bytes.
    SkipBytes : Action<int>
}

/// <summmary>
/// Read parameters converted to F# functions.
/// </summmary>
type private ConvertedReadFunctions = {
    ReadUInt16 : unit -> uint16
    ReadUInt32 : unit -> uint32
    ReadUInt64 : unit -> uint64
    ReadByte : unit -> byte
    SkipBytes : int -> unit
}

/// <summmary>
/// Converts the Read function's .NET Funcs into F# functions.
/// </summmary>
/// <param name="rawReadFunctions">Input functions supplied to the Read function.</param>
/// <returns>The converted functions.</returns>
let private convertReadFunctions (rawReadFunctions : ReadFunctions) =
    if rawReadFunctions.ReadUInt16 |> isNull then nullArg "ReadUInt16"
    if rawReadFunctions.ReadUInt32 |> isNull then nullArg "ReadUInt32"
    if rawReadFunctions.ReadUInt64 |> isNull then nullArg "ReadUInt64"
    if rawReadFunctions.ReadByte |> isNull then nullArg "ReadByte"
    if rawReadFunctions.SkipBytes |> isNull then nullArg "SkipBytes"

    { ConvertedReadFunctions.ReadUInt16 = rawReadFunctions.ReadUInt16.Invoke;
    ReadUInt32 = rawReadFunctions.ReadUInt32.Invoke;
    ReadUInt64 = rawReadFunctions.ReadUInt64.Invoke;
    ReadByte = rawReadFunctions.ReadByte.Invoke;
    SkipBytes = rawReadFunctions.SkipBytes.Invoke }

/// <summmary>
/// Parses a GrTexture from .ftex and .#.ftexs format.
/// </summmary>
/// <param name="readFunctions">Function to read a data type from the input.</param>
/// <returns>The parsed FormVariation list.</returns>
let public Read (readFunctions : ReadFunctions[]) =
