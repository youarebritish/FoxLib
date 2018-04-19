module FoxLib.MaterialParamBinary

open System
open FoxLib.Core

/// <summary>
/// A Fox Engine material preset.
/// </summary>
type public MaterialPreset = {
    F0 : float32
    RoughnessThreshold : float32
    ReflectionDependDiffuse : float32
    AnisotropicRoughness : float32
    SpecularColor : ColorRGB
    Translucency : float32
}

/// <summmary>
/// Input function to the Read function.
/// </summmary>
type public ReadFunction = {
    /// Function to read a float32.
    ReadSingle : Func<float32>
}

/// <summmary>
/// Read parameter converted to an F# function.
/// </summmary>
type private ConvertedReadFunction = {
    ReadSingle : unit -> float32
}

/// <summmary>
/// Converts the Read function's .NET Func into an F# function.
/// </summmary>
/// <param name="rawReadFunction">Input function supplied to the Read function.</param>
/// <returns>The converted function.</returns>
let private convertReadFunction (rawReadFunction : ReadFunction) =
    if rawReadFunction.ReadSingle |> isNull then nullArg "ReadSingle"

    { ConvertedReadFunction.ReadSingle = rawReadFunction.ReadSingle.Invoke }

/// <summary>
/// Read a material preset.
/// </summary>
let private readMaterialPreset readSingle =
    { F0 = readSingle();
    RoughnessThreshold = readSingle();
    ReflectionDependDiffuse = readSingle();
    AnisotropicRoughness = readSingle();
    SpecularColor = FoxLib.ColorRGB.Read readSingle;
    Translucency = readSingle();
    }

/// <summmary>
/// Parses a MaterialParamBinary from fmtt format.
/// </summmary>
/// <param name="readFunction">Function to read a data type from the input.</param>
/// <returns>The parsed MaterialParamBinary.</returns>
let public Read readFunction =
    let convertedFunction = convertReadFunction readFunction

    let numMaterialPresets = 256

    let materialPresets : MaterialPreset[] = [|1..numMaterialPresets|] 
                                              |> Array.map (fun _ -> readMaterialPreset convertedFunction.ReadSingle )
    materialPresets

/// <summmary>
/// Input function to the Write function.
/// </summmary>
type public WriteFunction = {
    /// Function to write a float32.
    WriteSingle : Action<float32>
}

/// <summmary>
/// Write parameter converted to an F# function.
/// </summmary>
type private ConvertedWriteFunction = {
    WriteSingle : float32 -> unit
}

/// <summmary>
/// Converts the Write function's .NET Func into an F# function.
/// </summmary>
/// <param name="rawWriteFunction">Input function supplied to the Write function.</param>
/// <returns>The converted function.</returns>
let private convertWriteFunction (rawWriteFunction : WriteFunction) =
    if rawWriteFunction.WriteSingle |> isNull then nullArg "WriteSingle"

    { ConvertedWriteFunction.WriteSingle = rawWriteFunction.WriteSingle.Invoke }

/// <summary>
/// Creates pseudo-null material presets.
/// </summary>
/// <param name="numPresets"></param>
/// <remarks>
/// "Null" presets are not quite null; the F0 value is 1.
/// </remarks>
/// <returns>The specified number of "null" presets.</returns>
let private createNullMaterialPresets numPresets = [|1..numPresets|] 
                                                    |> Array.map (fun _ -> { F0 = 1.0f; RoughnessThreshold = 0.0f; ReflectionDependDiffuse = 0.0f; AnisotropicRoughness = 0.0f; SpecularColor = { Red = 0.0f; Green = 0.0f; Blue = 0.0f }; Translucency = 0.0f; } )

/// <summary>
/// Gets a correctly-sized material preset set, as there need to be exactly 256 material presets.
/// </summary>
/// <param name="materialPresets">The array of material presets to be set up for writing.</param>
/// <param name="neededMaterialPresetCount>The amount of material presets that should be written.</param>
/// <returns>The correctly-sized array of material presets.</returns>
let private getCorrectedNumberOfMaterialPresets (materialPresets : MaterialPreset[]) neededMaterialPresetCount =
    match materialPresets.Length with
    | i when i = neededMaterialPresetCount -> materialPresets
    | i when i < neededMaterialPresetCount -> (createNullMaterialPresets (neededMaterialPresetCount - materialPresets.Length)) |> Array.append materialPresets 
    | _ -> failwith "Error: Material preset count exceeds 256."

let private writeMaterialPreset materialParam writeSingle = 
    materialParam.F0 |> writeSingle
    materialParam.RoughnessThreshold |> writeSingle
    materialParam.ReflectionDependDiffuse |> writeSingle
    materialParam.AnisotropicRoughness |> writeSingle
    materialParam.SpecularColor |> FoxLib.ColorRGB.Write writeSingle
    materialParam.Translucency |> writeSingle

/// <summary>
/// Writes a set of MaterialPresets to fmtt format.
/// </summary>
/// <param name="writeFunction">Function to write a data type.</param>
/// <param name="material">MaterialParamBinary to write.</param>
/// <remarks>
/// An error will be thrown if more than 256 material presets are passed.
/// </remarks>
let public Write materialPresets writeFunction =
    let convertedWriteFunction = convertWriteFunction writeFunction

    let correctedMaterialParams = getCorrectedNumberOfMaterialPresets materialPresets 256

    correctedMaterialParams
    |> Array.map (fun materialPreset -> writeMaterialPreset materialPreset convertedWriteFunction.WriteSingle)