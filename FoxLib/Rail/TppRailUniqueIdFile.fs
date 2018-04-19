module FoxLib.Tpp.RailUniqueIdFile

open System

/// <summmary>
/// Input functions to the Read function.
/// </summmary>
type public ReadFunctions = {
    /// Function to read a uint16.
    ReadUInt16 : Func<uint16>
    /// Function to read a uint32.
    ReadUInt32 : Func<uint32>
    /// Function to skip a number of bytes.
    SkipBytes : Action<int>
}

/// <summmary>
/// Read parameters converted to F# functions.
/// </summmary>
type private ConvertedReadFunctions = {
    ReadUInt16 : unit -> uint16
    ReadUInt32 : unit -> uint32
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
    if rawReadFunctions.SkipBytes |> isNull then nullArg "SkipBytes"

    { ConvertedReadFunctions.ReadUInt16 = rawReadFunctions.ReadUInt16.Invoke;
    ReadUInt32 = rawReadFunctions.ReadUInt32.Invoke;
    SkipBytes = rawReadFunctions.SkipBytes.Invoke; }

/// <summmary>
/// Parses rail IDs from frld format.
/// </summmary>
/// <param name="readFunctions">Functions to read various data types from the input.</param>
/// <returns>The parsed rail IDs.</returns>
let public Read (readFunctions : ReadFunctions) =
    let convertedFunctions = convertReadFunctions readFunctions

    convertedFunctions.SkipBytes 4 |> ignore

    let version = convertedFunctions.ReadUInt16()
    let entryCount = convertedFunctions.ReadUInt16() |> int
    
    [|1..entryCount|]
    |> Array.map (fun _ -> convertedFunctions.ReadUInt32())

/// <summmary>
/// Input functions to the Write function.
/// </summmary>
type public WriteFunctions = {
    /// Function to write a char.
    WriteChar : Action<char>
    /// Function to write a uint16.
    WriteUInt16 : Action<uint16>
    /// Function to write a uint32.
    WriteUInt32 : Action<uint32>
}

/// <summmary>
/// Write parameters converted to F# functions.
/// </summmary>
type private ConvertedWriteFunctions = {
    WriteChar : char -> unit
    WriteUInt16 : uint16 -> unit
    WriteUInt32 : uint32 -> unit
}

/// <summmary>
/// Converts the Write function's .NET Funcs into F# functions.
/// </summmary>
/// <param name="rawWriteFunctions">Input functions supplied to the Write function.</param>
/// <returns>The converted functions.</returns>
let private convertWriteFunctions (rawWriteFunctions : WriteFunctions) =
    if rawWriteFunctions.WriteChar |> isNull then nullArg "WriteChar"
    if rawWriteFunctions.WriteUInt16 |> isNull then nullArg "WriteUInt16"
    if rawWriteFunctions.WriteUInt32 |> isNull then nullArg "WriteUInt32"

    { ConvertedWriteFunctions.WriteChar = rawWriteFunctions.WriteChar.Invoke;
    WriteUInt16 = rawWriteFunctions.WriteUInt16.Invoke;
    WriteUInt32 = rawWriteFunctions.WriteUInt32.Invoke }

/// <summary>
/// Writes a set of rail IDs to frld format.
/// </summary>
/// <param name="writeFunctions">Function to write various data types.</param>
/// <param name="railIds">Rail IDs to write.</param>
let public Write (writeFunctions : WriteFunctions) railIds =
    let convertedWriteFunctions = convertWriteFunctions writeFunctions

    convertedWriteFunctions.WriteChar 'R'
    convertedWriteFunctions.WriteChar 'A'
    convertedWriteFunctions.WriteChar 'I'
    convertedWriteFunctions.WriteChar 'L'

    convertedWriteFunctions.WriteUInt16 2us
    convertedWriteFunctions.WriteUInt16 (Seq.length railIds |> uint16)

    railIds
    |> Array.iter (fun id -> convertedWriteFunctions.WriteUInt32 id)