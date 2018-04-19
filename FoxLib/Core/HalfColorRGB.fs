module FoxLib.HalfColorRGB

open FoxLib.Core

/// <summary>
/// Read a HalfColorRGB.
/// </summary>
/// <param name="readSingle">Function to read a binary16.</param>
/// <returns>The parsed HalfColorRGBA.</returns>
let internal Read readHalf = {
    HalfColorRGB.Red = readHalf();
    Green = readHalf();
    Blue = readHalf();
    }

/// <summary>
/// Write a HalfColorRGB.
/// </summary>
/// <param name="writeSingle">Function to write a binary16.</param>
let internal Write writeHalf (color : HalfColorRGB)  =
    writeHalf color.Red
    writeHalf color.Green
    writeHalf color.Blue