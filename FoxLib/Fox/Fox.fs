module FoxLib.Fox

let public StrCode (text : string) =
    let seed0 = 0x9ae16a3b2f90404fUL
    let seed1 = match text.Length with
                | 0 -> 0UL
                | _ ->  let term1 = (byte (text.Chars 0)) <<< 16
                        let term2 = text.Length
                        uint64 ((uint32 term1) + (uint32 term2))
    
    let cityHash = CityHash.CityHash.CityHash64WithSeeds(text + "\0", seed0, seed1)
    cityHash &&& 0xFFFFFFFFFFFFUL