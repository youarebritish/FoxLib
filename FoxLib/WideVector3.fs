module FoxLib.WideVector3

open FoxLib.Core

/// <summary>
/// Read a WideVector3.
/// <summary>
/// <param name="readSingle">Function to read a float32.</param>
/// <param name="readSingle">Function to read a uint16.</param>
/// <returns>The parsed WideVector3.</returns>
let internal Read readSingle readUInt16 = {
    X = readSingle();
    Y = readSingle();
    Z = readSingle();
    A = readUInt16();
    B = readUInt16();
}

/// <summary>
/// Write a WideVector3.
/// </summary>
/// <param name="writeSingle">Function to write a float32.</param>
/// <param name="writeUInt16">Function to write a uint16.</param>
let Write (vector : WideVector3) writeSingle writeUInt16 =
    writeSingle vector.X
    writeSingle vector.Y
    writeSingle vector.Z
    writeUInt16 vector.A
    writeUInt16 vector.B