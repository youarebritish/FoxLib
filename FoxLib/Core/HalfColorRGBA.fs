module FoxLib.HalfColorRGBA

open FoxLib.Core
open System

/// <summary>
/// Read a HalfColorRGBA.
/// </summary>
/// <param name="readSingle">Function to read a binary16.</param>
/// <returns>The parsed HalfColorRGBA.</returns>
let internal Read readHalf = {
    HalfColorRGBA.Red = readHalf();
    Green = readHalf();
    Blue = readHalf();
    Alpha = readHalf();
    }

/// <summary>
/// Write a HalfColorRGBA.
/// </summary>
/// <param name="writeSingle">Function to write a binary16.</param>
let internal Write (writeHalf : Half -> unit) (color : HalfColorRGBA) =
    writeHalf color.Red
    writeHalf color.Green
    writeHalf color.Blue
    writeHalf color.Alpha