module FoxLib.ColorRGBA

open FoxLib.Core

/// <summary>
/// Read a ColorRGBA.
/// </summary>
/// <param name="readSingle">Function to read a float32.</param>
/// <returns>The parsed ColorRGBA.</returns>
let internal Read readSingle = {
    ColorRGBA.Red = readSingle();
    Green = readSingle();
    Blue = readSingle();
    Alpha = readSingle();
    }

/// <summary>
/// Write a ColorRGBA.
/// </summary>
/// <param name="writeSingle">Function to write a float32.</param>
let internal Write writeSingle (color : ColorRGBA)  =
    writeSingle color.Red
    writeSingle color.Green
    writeSingle color.Blue
    writeSingle color.Alpha