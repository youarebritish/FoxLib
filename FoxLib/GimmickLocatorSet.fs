module FoxLib.Tpp.GimmickLocatorSet

open System.IO
open FoxLib
open FoxLib.Tpp.GimmickLocator
    
/// <summmary>
/// A sequence of locators for gimmicks, indicating where they should be placed.
/// </summary>
type public GimmickLocatorSet =
    | PowerCutAreaGimmickLocatorSet of Locators : seq<PowerCutAreaGimmickLocator>
    | NamedGimmickLocatorSet of Locators : seq<NamedGimmickLocator>
    | ScaledGimmickLocatorSet of Locators : seq<ScaledGimmickLocator>

/// <summmary>
/// Indicates the kind of GimmickLocatorSet contained in a .lba file.
/// </summmary>
type private GimmickLocatorSetFormat =
    | PowerCutArea = 0
    | Named = 2
    | Scaled = 3
        
let private readUnscaledTransform readSingle =
    Vector4.Read readSingle, Quaternion.Read readSingle

let private readScaledTransform readSingle readUInt16 =
    Vector4.Read readSingle, Quaternion.Read readSingle, WideVector3.Read readSingle readUInt16

let private readLocatorMetadata readUInt32 =
    readUInt32(), readUInt32()

let private readPowerCutAreaGimmickLocators readSingle locatorCount =
    [|1..locatorCount|]
    |> Array.map(fun _ -> readUnscaledTransform readSingle)
    |> Array.map(fun transform -> { Position = fst transform;
                                    Rotation = snd transform })

let private readNamedGimmickLocators readSingle readUInt32 locatorCount =
    [|1..locatorCount|]
    |> Array.map(fun _ -> readUnscaledTransform readSingle)
    |> Array.map(fun transform -> 
        let metadata = readLocatorMetadata readUInt32
        { Position = fst transform;
        Rotation = snd transform;
        LocatorName = fst metadata;
        DataSetName = snd metadata })

let private readScaledGimmickLocators readSingle readUInt16 readUInt32 locatorCount =
    [|1..locatorCount|]
    |> Array.map(fun _ -> readScaledTransform readSingle readUInt16)
    |> Array.map(fun transform -> 
        let metadata = readLocatorMetadata readUInt32
        let position, rotation, scale = transform
        { Position = position;
        Rotation = rotation;
        Scale = scale;
        LocatorName = fst metadata;
        DataSetName = snd metadata })

let private readGimmickLocatorSet readSingle readUInt16 readUInt32 format locatorCount =
    match format with
    | GimmickLocatorSetFormat.PowerCutArea -> PowerCutAreaGimmickLocatorSet(readPowerCutAreaGimmickLocators readSingle locatorCount)
    | GimmickLocatorSetFormat.Named -> NamedGimmickLocatorSet(readNamedGimmickLocators readSingle readUInt32 locatorCount)
    | GimmickLocatorSetFormat.Scaled -> ScaledGimmickLocatorSet(readScaledGimmickLocators readSingle readUInt16 readUInt32 locatorCount)
    | _ -> raise (new InvalidDataException("The file is not an .lba file or is an unsupported format."))
                
/// <summmary>
/// Parses a GimmickLocatorSet from lba format.
/// </summmary>
/// <param name="readSingle">Function to read a float32 from the input.</param>
/// <param name="readUInt16">Function to read a uint16 from the input.</param>
/// <param name="readUInt32">Function to read a uint32 from the input.</param>
/// <param name="readInt32">Function to read a int32 from the input.</param>
/// <param name="skipBytes">Function to skip a number of bytes.</param>
/// <returns>The parsed GimmickLocatorSet.</returns>
let public Read readSingle readUInt16 readUInt32 readInt32 skipBytes =
    let locatorCount = readInt32()
    let format = readInt32()  |> enum<GimmickLocatorSetFormat>
    skipBytes 8 |> ignore
    readGimmickLocatorSet readSingle readUInt16 readUInt32 format locatorCount

let private writeGimmickLocatorSetHeader writeInt32 (writeEmptyBytes : int -> unit) locatorCount format =
    writeInt32 locatorCount
    LanguagePrimitives.EnumToValue format |> writeInt32
    writeEmptyBytes 8

let private writeUnscaledGimmickLocatorTransform position rotation writeSingle =
    Vector4.Write position writeSingle
    Quaternion.Write rotation writeSingle

let private writeScaledGimmickLocatorTransform position rotation scale writeSingle writeUShort =
    writeUnscaledGimmickLocatorTransform position rotation writeSingle
    WideVector3.Write scale writeSingle writeUShort

let private writeGimmickLocatorMetadata locatorName dataSetName writeUInt32 =
    writeUInt32 locatorName
    writeUInt32 dataSetName

let private writePowerCutAreaGimmickLocatorSet writeSingle writeInt32 writeEmptyBytes locators =
    let locatorCount = Array.length locators
    writeGimmickLocatorSetHeader writeInt32 writeEmptyBytes locatorCount GimmickLocatorSetFormat.PowerCutArea
    locators 
    |> Array.iter (fun (locator : PowerCutAreaGimmickLocator) ->
        writeUnscaledGimmickLocatorTransform locator.Position locator.Rotation writeSingle)

let private writeNamedGimmickLocatorSet writeSingle writeInt32 writeUInt32 writeEmptyBytes locators =
    let locatorCount = Array.length locators
    writeGimmickLocatorSetHeader writeInt32 writeEmptyBytes locatorCount GimmickLocatorSetFormat.Named
    locators
    |> Array.map (fun (locator : NamedGimmickLocator) ->
        writeUnscaledGimmickLocatorTransform locator.Position locator.Rotation writeSingle
        locator.LocatorName, locator.DataSetName)
    |> Array.iter (fun locatorMetadata ->
        writeGimmickLocatorMetadata (fst locatorMetadata) (snd locatorMetadata) writeUInt32)

let private writeScaledGimmickLocatorSet writeSingle writeUShort writeInt32 writeUInt32 writeEmptyBytes locators =
    let locatorCount = Array.length locators
    writeGimmickLocatorSetHeader writeInt32 writeEmptyBytes locatorCount GimmickLocatorSetFormat.Scaled
    locators
    |> Array.map (fun (locator : ScaledGimmickLocator) ->
        writeScaledGimmickLocatorTransform locator.Position locator.Rotation locator.Scale writeSingle writeUShort
        locator.LocatorName, locator.DataSetName)
    |> Array.iter (fun locatorMetadata ->
        writeGimmickLocatorMetadata (fst locatorMetadata) (snd locatorMetadata) writeUInt32)

/// <summary>
/// Writes a GimmickLocatorSet to lba format.
/// </summary>
/// <param name="writeSingle">Function to write a float32.</param>
/// <param name="writeUInt16">Function to write a uint16.</param>
/// <param name="writeInt32">Function to write a int32.</param>
/// <param name="writeUInt32">Function to write a uint32.</param>
/// <param name="writeEmptyBytes">Function to write a number of empty bytes.</param>
/// <param name="locatorSet">GimmickLocatorSet to write.</param>
let public Write writeSingle writeUInt16 writeInt32 writeUInt32 writeEmptyBytes locatorSet =
    match locatorSet with
    | PowerCutAreaGimmickLocatorSet locators -> writePowerCutAreaGimmickLocatorSet writeSingle writeInt32 writeEmptyBytes (Seq.toArray locators)
    | NamedGimmickLocatorSet locators -> writeNamedGimmickLocatorSet writeSingle writeInt32 writeUInt32 writeEmptyBytes (Seq.toArray locators)
    | ScaledGimmickLocatorSet locators -> writeScaledGimmickLocatorSet writeSingle writeUInt16 writeInt32 writeUInt32 writeEmptyBytes (Seq.toArray locators)