module FoxLib.Quaternion

open FoxLib.Core

/// <summary>Read from a binary representation.<summary>
let Read readSingle = {
    Quaternion.X = readSingle();
    Y = readSingle();
    Z = readSingle();
    W = readSingle()
}

/// <summary>Write to a binary representation.</summary>
let Write (quaternion : Quaternion) writeSingle =
    writeSingle quaternion.X
    writeSingle quaternion.Y
    writeSingle quaternion.Z
    writeSingle quaternion.W