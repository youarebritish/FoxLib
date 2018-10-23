module FoxLib.Matrix3

open FoxLib.Core

/// <summary>
/// Read a Matrix3.
/// </summary>
/// <param name="readSingle">Function to read a float32.</param>
/// <returns>The parsed Matrix3.</returns>
let internal Read readSingle =
    let col0row0 = readSingle()
    let col0row1 = readSingle()
    let col0row2 = readSingle()

    let col1row0 = readSingle()
    let col1row1 = readSingle()
    let col1row2 = readSingle()

    let col2row0 = readSingle()
    let col2row1 = readSingle()
    let col2row2 = readSingle()

    Matrix3.Create col0row0 col0row1 col0row2 col1row0 col1row1 col1row2 col2row0 col2row1 col2row2

/// <summary>
/// Write a Matrix3.
/// </summary>
/// <param name="writeSingle">Function to write a float32.</param>
let internal Write (matrix : Matrix3) writeSingle =
    matrix.Col0 |> Array.iter writeSingle
    matrix.Col1 |> Array.iter writeSingle
    matrix.Col2 |> Array.iter writeSingle