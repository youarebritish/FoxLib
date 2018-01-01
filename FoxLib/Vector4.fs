module FoxLib.Vector4

open FoxLib.Core

/// <summary>Read from a binary representation.<summary>
let Read readSingle = {
    Vector4.X = readSingle();
    Y = readSingle();
    Z = readSingle();
    W = readSingle()
}

/// <summary>Write to a binary representation.</summary>
let Write (vector : Vector4) writeSingle =
    writeSingle vector.X
    writeSingle vector.Y
    writeSingle vector.Z
    writeSingle vector.W