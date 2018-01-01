module FoxLib.Vector4

open FoxLib.Core

/// <summary>
/// Read a Vector4.
/// </summary>
/// <param name="readSingle">Function to read a float32.</param>
/// <returns>The parsed Vector4.</returns>
let internal Read readSingle = {
    Vector4.X = readSingle();
    Y = readSingle();
    Z = readSingle();
    W = readSingle()
}

/// <summary>
/// Write a Vector4.
/// </summary>
/// <param name="writeSingle">Function to write a float32.</param>
let internal Write (vector : Vector4) writeSingle =
    writeSingle vector.X
    writeSingle vector.Y
    writeSingle vector.Z
    writeSingle vector.W