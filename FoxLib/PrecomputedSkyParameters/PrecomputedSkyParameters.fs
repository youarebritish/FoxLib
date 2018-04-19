module FoxLib.PrecomputedSkyParameters

open System
open FoxLib.Core

/// <summmary>
/// Input functions to the Read function.
/// </summmary>
type public ReadFunctions = {
    /// Function to read a binary16.
    ReadHalf : Func<Half>
    /// Function to read a uint32.
    ReadUInt32 : Func<uint32>
}

/// <summmary>
/// Read parameters converted to F# functions.
/// </summmary>
type private ConvertedReadFunctions = {
    ReadHalf : unit -> Half
    ReadUInt32 : unit -> uint32
}

/// <summmary>
/// Converts the Read function's .NET Funcs into F# functions.
/// </summmary>
/// <param name="rawReadFunctions">Input functions supplied to the Read function.</param>
/// <returns>The converted functions.</returns>
let private convertReadFunctions (rawReadFunctions : ReadFunctions) =
    if rawReadFunctions.ReadHalf |> isNull then nullArg "ReadHalf"
    if rawReadFunctions.ReadUInt32 |> isNull then nullArg "ReadUInt32"

    { ConvertedReadFunctions.ReadHalf = rawReadFunctions.ReadHalf.Invoke;
    ReadUInt32 = rawReadFunctions.ReadUInt32.Invoke; }

/// <summmary>
/// Parses a PrecomputedSkyParameter from pcsp format.
/// </summmary>
/// <param name="readFunction">Function to read a data type from the input.</param>
/// <returns>The parsed PrecomputedSkyParameter.</returns>
let public Read readFunctions =
    let convertedFunctions = convertReadFunctions readFunctions

    convertedFunctions.ReadUInt32() |> ignore

    [|1..8192|] |> Array.map (fun _ -> FoxLib.HalfColorRGBA.Read convertedFunctions.ReadHalf)

/// <summmary>
/// Input functions to the Write function.
/// </summmary>
type public WriteFunctions = {
    /// Function to write a binary16.
    WriteHalf : Action<Half>
    /// Function to write a uint32.
    WriteUInt32 : Action<uint32>
}

/// <summmary>
/// Write parameters converted to F# functions.
/// </summmary>
type private ConvertedWriteFunctions = {
    WriteHalf : Half -> unit
    WriteUInt32 : uint32 -> unit
}

/// <summmary>
/// Converts the Write function's .NET Funcs into F# functions.
/// </summmary>
/// <param name="rawWriteFunctions">Input functions supplied to the Write function.</param>
/// <returns>The converted functions.</returns>
let private convertWriteFunctions (rawWriteFunctions : WriteFunctions) =
    if rawWriteFunctions.WriteHalf |> isNull then nullArg "WriteHalf"
    if rawWriteFunctions.WriteUInt32 |> isNull then nullArg "WriteUInt32"

    { ConvertedWriteFunctions.WriteHalf = rawWriteFunctions.WriteHalf.Invoke;
    WriteUInt32 = rawWriteFunctions.WriteUInt32.Invoke; }

/// <summary>
/// Writes a PrecomputedSkyParameters texture to pcsp format.
/// </summary>
/// <param name="writeFunction">Function to write a data type.</param>
/// <param name="pixels">Texture to write.</param>
let public Write pixels writeFunctions =
    let convertedFunctions = convertWriteFunctions writeFunctions

    convertedFunctions.WriteUInt32 1u

    pixels 
    |> Array.map (fun pixel -> pixel |> FoxLib.HalfColorRGBA.Write convertedFunctions.WriteHalf)