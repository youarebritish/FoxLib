module FoxLib.Quaternion

open FoxLib.Core

/// <summary>
/// Read a Quaternion.
/// </summary>
/// <param name="readSingle">Function to read a float32.</param>
/// <returns>The parsed Quaternion.</returns>
let internal Read readSingle = {
    Quaternion.X = readSingle();
    Y = readSingle();
    Z = readSingle();
    W = readSingle()
}

/// <summary>
/// Write a Quaternion.
/// </summary>
/// <param name="writeSingle">Function to write a float32.</param>
let Write (quaternion : Quaternion) writeSingle =
    writeSingle quaternion.X
    writeSingle quaternion.Y
    writeSingle quaternion.Z
    writeSingle quaternion.W