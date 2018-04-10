module FoxLib.Vector3

open FoxLib.Core

/// <summary>
/// Read a Vector3.
/// </summary>
/// <param name="readSingle">Function to read a float32.</param>
/// <returns>The parsed Vector3.</returns>
let internal Read readSingle = {
    Vector3.X = readSingle();
    Y = readSingle();
    Z = readSingle();
}

/// <summary>
/// Write a Vector3.
/// </summary>
/// <param name="writeSingle">Function to write a float32.</param>
let internal Write (vector : Vector3) writeSingle =
    writeSingle vector.X
    writeSingle vector.Y
    writeSingle vector.Z