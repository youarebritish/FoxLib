module FoxLib.ColorRGB

open FoxLib.Core

/// <summary>
/// Read a ColorRGB.
/// </summary>
/// <param name="readSingle">Function to read a float32.</param>
/// <returns>The parsed ColorRGB.</returns>
let internal Read readSingle = {
    ColorRGB.Red = readSingle();
    Green = readSingle();
    Blue = readSingle();
    }

/// <summary>
/// Write a ColorRGB.
/// </summary>
/// <param name="writeSingle">Function to write a float32.</param>
let internal Write writeSingle (color : ColorRGB)  =
    writeSingle color.Red
    writeSingle color.Green
    writeSingle color.Blue