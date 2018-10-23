module FoxLib.Fox

/// <summary>
/// First of two seeds for the CityHash algorithm.
/// </summary>
let private cityHashSeed0 = 0x9ae16a3b2f90404fUL

/// <summary>
/// Calculates the StrCode hash of a string.
/// </summary>
/// <param name="text">The string to hash.</param>
let public StrCode text =
    if isNull text then nullArg text

    let seed1 = match text.Length with
                | 0 -> 0UL
                | _ ->  let term1 = uint32 (int text.[0] <<< 16)
                        let term2 = uint32 text.Length
                        term1 + term2
                        |> uint64

    CityHash.CityHash.CityHash64WithSeeds(text + "\000", cityHashSeed0, seed1)
    |> (&&&) 0xFFFFFFFFFFFFUL

/// <summary>
/// Calculates the StrCode32 hash of a string.
/// </summary>
/// <param name="text">The string to hash.</param>
let public StrCode32 text =
    StrCode text
    |> uint32