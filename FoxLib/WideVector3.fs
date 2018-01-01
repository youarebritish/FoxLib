module FoxLib.WideVector3

open FoxLib.Core

/// <summary>
/// Read from a binary representation.
/// <summary>
let Read readSingle readUInt16 = {
    X = readSingle();
    Y = readSingle();
    Z = readSingle();
    A = readUInt16();
    B = readUInt16();
}

/// <summary>
/// Write to a binary representation.
/// </summary>
let Write (vector : WideVector3) writeSingle writeUInt16 =
    writeSingle vector.X
    writeSingle vector.Y
    writeSingle vector.Z
    writeUInt16 vector.A
    writeUInt16 vector.B