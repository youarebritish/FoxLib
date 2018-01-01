module FoxLib.Vector3

open FoxLib.Core

/// <summary>
/// Read a Vector3.
/// <summary>
let Read readSingle = {
    Vector3.X = readSingle();
    Y = readSingle();
    Z = readSingle();
}

/// <summary>
/// Write a Vector3.
/// </summary>
let Write (vector : Vector3) writeSingle =
    writeSingle vector.X
    writeSingle vector.Y
    writeSingle vector.Z