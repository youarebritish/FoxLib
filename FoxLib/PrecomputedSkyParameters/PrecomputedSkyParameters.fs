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

let public Read readFunctions =
    let convertedFunctions = convertReadFunctions readFunctions

    let xDimension = 128
    let yDimension = 64

    convertedFunctions.ReadUInt32() |> ignore

    //let pixels : HalfColorRGBA[] =

    //    let rows = [|1..xDimension|]
    //                |> Array.map (fun _ -> 
    //                                       let test = FoxLib.HalfColorRGBA.Read convertedFunctions.ReadHalf
    //                                       test)

    //    let columns = [|1..yDimension|]
    //                   |> Array.map (fun _ -> rows)

    //    let readPixel = Array.concat columns
    //    readPixel
    //pixels

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

let public Write (pixels : HalfColorRGBA[]) writeFunctions =
    let convertedFunctions = convertWriteFunctions writeFunctions

    //let xDimension = 128
    //let yDimension = 64

    convertedFunctions.WriteUInt32 1u

    //[|0..(yDimension - 1)|]
    // |> Array.map (fun yCoord -> [|0..(xDimension - 1)|]
    //                              |> Array.map (fun xCoord -> let coordinate = (xCoord + (yCoord * xDimension))
    //                                                          FoxLib.HalfColorRGBA.Write convertedFunctions.WriteHalf pixels.[coordinate] ))

    pixels 
           |> 
              Array.map 
                        (fun i -> 
                                  i 
                                    |> 
                                       FoxLib.HalfColorRGBA.Write convertedFunctions.WriteHalf)