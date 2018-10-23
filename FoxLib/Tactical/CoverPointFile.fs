module FoxLib.CoverPointFile

open System
open FoxLib.TppGameKit

/// <summary>
/// Reads a cover point entry.
/// </summary>
/// <param name="readSingle">Function to read a float.</param>
/// <param name="readUInt8">Function to read a cover point param.</param>
let private readCoverPoint readSingle readParam =
    { TppCoverPoint.Position = Vector3.Read readSingle;
    LeftOpen = readParam();
    RightOpen = readParam();
    UpOpen = readParam();
    UnVaultable = readParam();
    IsUseVip = readParam();
    IsUseSniper = readParam();
    IsBreakDisable = readParam();
    IsBreakEnable = readParam(); }

/// <summary>
/// Reads header data.
/// </summary>
/// <param name="readUInt16">Function to read a uint16.</param>
/// <param name="skipBytes">Function to skip a number of bytes.</param>
/// <returns>Number of Entities and the string table offset.</returns>
let private readHeader readUInt16 skipBytes =
    skipBytes 4

    let version = readUInt16()
    let entryCount = readUInt16()

    skipBytes 4

    entryCount

/// <summmary>
/// Input functions to the Read function.
/// </summmary>
type public ReadFunctions = {
    /// Function to read a uint8.
    ReadUInt8 : Func<uint8>
    /// Function to read a uint16.
    ReadUInt16 : Func<uint16>
    /// Function to read a float32.
    ReadSingle :Func<float32>
    /// Function to skip a number of bytes.
    SkipBytes : Action<int>
}

/// <summmary>
/// Read parameters converted to F# functions.
/// </summmary>
type private ConvertedReadFunctions = {
    /// Function to read a uint8.
    ReadUInt8 : unit -> uint8
    /// Function to read a uint16.
    ReadUInt16 : unit -> uint16
    /// Function to read a float32.
    ReadSingle : unit -> float32
    /// Function to skip a number of bytes.
    SkipBytes : int -> unit
}

/// <summmary>
/// Converts the Read function's .NET Funcs into F# functions.
/// </summmary>
/// <param name="rawReadFunctions">Input functions supplied to the Read function.</param>
/// <returns>The converted functions.</returns>
let private convertReadFunctions (rawReadFunctions : ReadFunctions) =
    if rawReadFunctions.ReadUInt8 |> isNull then nullArg "ReadUInt8"
    if rawReadFunctions.ReadUInt16 |> isNull then nullArg "ReadUInt16"
    if rawReadFunctions.ReadSingle |> isNull then nullArg "ReadSingle"
    if rawReadFunctions.SkipBytes |> isNull then nullArg "SkipBytes"

    { ConvertedReadFunctions.ReadSingle = rawReadFunctions.ReadSingle.Invoke;
    ReadUInt8 = rawReadFunctions.ReadUInt8.Invoke;
    ReadUInt16 = rawReadFunctions.ReadUInt16.Invoke;
    SkipBytes = rawReadFunctions.SkipBytes.Invoke; }

/// <summary>
/// Reads TppCoverPoints from a CoverPointFile.
/// </summary>
/// <param name="readFunctions">Functions to read various data types from the input.</param>
/// <returns>The parsed TppCoverPoints.</returns>
let public Read readFunctions =
    let convertedReadFunctions = convertReadFunctions readFunctions

    let entryCount = readHeader convertedReadFunctions.ReadUInt16 convertedReadFunctions.SkipBytes

    [|1us..entryCount|]
    |> Array.map (fun _ -> readCoverPoint convertedReadFunctions.ReadSingle convertedReadFunctions.ReadUInt8)