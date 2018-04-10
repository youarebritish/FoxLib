module PrecomputedSkyParameters

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
/// Input functions to the Read function.
/// </summmary>
type public ReadFunctions = {
    /// Function to read a float32.
    ReadSingle : Func<float32>
}

/// <summmary>
/// Read parameters converted to F# functions.
/// </summmary>
type private ConvertedReadFunctions = {
    ReadSingle : unit -> float32
}

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
/// Converts the Read function's .NET Funcs into F# functions.
/// </summmary>
/// <param name="rawReadFunctions">Input functions supplied to the Read function.</param>
/// <returns>The converted functions.</returns>
let private convertReadFunctions (rawReadFunctions : ReadFunctions) =
    if rawReadFunctions.ReadSingle |> isNull then nullArg "ReadSingle"

    { ConvertedReadFunctions.ReadSingle = rawReadFunctions.ReadSingle.Invoke }

let public Read (readFunctions : ReadFunctions) =
    let convertedFunctions = convertReadFunctions readFunctions

    let xDimension = 256
    let yDimension = 2

    let pixels : Pixel[] = 
        let columns xCoord = [|1..yDimension|] 
                              |> Array.map (fun yCoord -> { X = xCoord; Y = yCoord; Color = FoxLib.ColorRGBA.Read convertedFunctions.ReadSingle } )

        let rows = [|1..xDimension|]
                    |> Array.map (fun xCoord -> columns xCoord)

        let readPixel = Array.concat rows
        readPixel
    pixels