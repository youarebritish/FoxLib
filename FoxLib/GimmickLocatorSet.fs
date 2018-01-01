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
/// Indicates the type of GimmickLocatorSet contained in a .lba file.
/// </summmary>
type private GimmickLocatorSetFormat =
    | PowerCutArea = 0
    | Named = 2
    | Scaled = 3
        
/// <summary>
/// Reads a PowerCutAreaGimmickLocator or NamedGimmickLocator transform.
/// </summary>
/// <param name="readSingle">Function to read a float32.</param>
/// <returns>The parsed transform as a tuple of position * rotation.
let private readUnscaledTransform readSingle =
    let position = Vector4.Read readSingle
    let rotation = Quaternion.Read readSingle
    position, rotation

/// <summary>
/// Reads a ScaledGimmickLocator transform.
/// </summary>
/// <param name="readSingle">Function to read a float32.</param>
/// <param name="readUInt16">Function to read a uint16.</param>
/// <returns>The parsed transform as a tuple of position * rotation * scale.
let private readScaledTransform readSingle readUInt16 =
    let position = Vector4.Read readSingle
    let rotation = Quaternion.Read readSingle
    let scale = WideVector3.Read readSingle readUInt16
    position, rotation, scale

/// <summary>
/// Reads a locator's metadata footer entry.
/// </summary>
/// <remarks>
/// PowerCutAreaGimmickLocatorSet lba files do not contain a footer.
/// </remarks>
/// <param name="readUInt32">Function to read a float32.</param>
/// <returns>The parsed locatorName and dataSetName, encoded as StrCode32Hashes.</returns>
let private readLocatorMetadata readUInt32 =
    let locatorName = readUInt32()
    let dataSetName = readUInt32()
    locatorName, dataSetName

/// <summary>
/// Reads PowerCutAreaGimmickLocators from lba format.
/// </summary>
/// <param name="readSingle">Function to read a float32.</param>
/// <param name="locatorCount">Number of locators.</param>
/// <returns>The parsed PowerCutAreaGimmickLocators.</returns>
let private readPowerCutAreaGimmickLocators readSingle locatorCount =
    [|1..locatorCount|]
    |> Array.map(fun _ -> readUnscaledTransform readSingle)
    |> Array.map(fun transform -> { Position = fst transform;
                                    Rotation = snd transform })

/// <summary>
/// Reads NamedGimmickLocators from lba format.
/// </summary>
/// <param name="readSingle">Function to read a float32.</param>
/// <param name="readUInt32">Function to read a uint32.</param>
/// <param name="locatorCount">Number of locators.</param>
/// <returns>The parsed NamedGimmickLocators.</returns>
let private readNamedGimmickLocators readSingle readUInt32 locatorCount =
    [|1..locatorCount|]
    |> Array.map(fun _ -> readUnscaledTransform readSingle)
    |> Array.map(fun transform -> 
        let metadata = readLocatorMetadata readUInt32
        { Position = fst transform;
        Rotation = snd transform;
        LocatorName = fst metadata;
        DataSetName = snd metadata })

/// <summary>
/// Reads ScaledGimmickLocators from lba format.
/// </summary>
/// <param name="readSingle">Function to read a float32.</param>
/// <param name="readUInt16">Function to read a uint16.</param>
/// <param name="readUInt32">Function to read a uint32.</param>
/// <param name="locatorCount">Number of locators.</param>
/// <returns>The parsed ScaledGimmickLocators.</returns>
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

/// <summary>
/// Parses the body of a GimmickLocatorSet from lba format.
/// </summary>
/// <param name="readSingle">Function to read a float32.</param>
/// <param name="readUInt16">Function to read a uint16.</param>
/// <param name="readUInt32">Function to read a uint32.</param>
/// <param name="format">Format of the GimmickLocatorSet.</param>
/// <param name="locatorCount">Number of locators.</param>
/// <returns>The parsed GimmickLocatorSet.</returns>
/// <exception cref="InvalidDataException">Raised if the format is unrecognized.</exception>
let private readGimmickLocatorSetBody readSingle readUInt16 readUInt32 format locatorCount =
    match format with
    | GimmickLocatorSetFormat.PowerCutArea -> PowerCutAreaGimmickLocatorSet(readPowerCutAreaGimmickLocators readSingle locatorCount)
    | GimmickLocatorSetFormat.Named -> NamedGimmickLocatorSet(readNamedGimmickLocators readSingle readUInt32 locatorCount)
    | GimmickLocatorSetFormat.Scaled -> ScaledGimmickLocatorSet(readScaledGimmickLocators readSingle readUInt16 readUInt32 locatorCount)
    | _ -> raise (new InvalidDataException("The file is not an .lba file or is an unrecognized format."))
                  
/// <summmary>
/// Parses a GimmickLocatorSet from lba format.
/// </summmary>
/// <param name="readSingle">Function to read a float32.</param>
/// <param name="readUInt16">Function to read a uint16.</param>
/// <param name="readUInt32">Function to read a uint32.</param>
/// <param name="readInt32">Function to read a int32.</param>
/// <param name="skipBytes">Function to skip a number of bytes.</param>
/// <returns>The parsed GimmickLocatorSet.</returns>
let public Read readSingle readUInt16 readUInt32 readInt32 skipBytes =
    let locatorCount = readInt32()
    let format = readInt32()  |> enum<GimmickLocatorSetFormat>
    skipBytes 8 |> ignore
    readGimmickLocatorSetBody readSingle readUInt16 readUInt32 format locatorCount

/// <summary>
/// Writes a GimmickLocatorSet's header.
/// </summary>
/// <param name="writeInt32">Functoin to write an int32.</param>
/// <param name="writeEmptyBytes"></param>
/// <param name="locatorCount"></param>
/// <param name="format"></param>
let private writeGimmickLocatorSetHeader writeInt32 (writeEmptyBytes : int -> unit) locatorCount format =
    writeInt32 locatorCount
    LanguagePrimitives.EnumToValue format |> writeInt32
    writeEmptyBytes 8

/// <summary>
/// Writes a PowerCutAreaGimmickLocator or NamedGimmickLocator's transform.
/// </summary>
/// <param name="position">The locator's position.</param>
/// <param name="rotation">The locator's rotation.</param>
/// <param name="writeSingle">Function to write a float32.</param>
let private writeUnscaledGimmickLocatorTransform writeSingle position rotation =
    Vector4.Write position writeSingle
    Quaternion.Write rotation writeSingle

/// <summary>
/// Writes a ScaledGimmickLocator's transform.
/// </summary>
/// <param name="position">The locator's position.</param>
/// <param name="rotation">The locator's rotation.</param>
/// <param name="scale">The locator's scale.</param>
/// <param name="writeSingle">Function to write a float32.</param>
/// <param name="writeUInt16">Function to write a uint16.</param>
let private writeScaledGimmickLocatorTransform position rotation scale writeSingle writeUInt16 =
    writeUnscaledGimmickLocatorTransform writeSingle position rotation
    WideVector3.Write scale writeSingle writeUInt16

/// <summary>
/// Writes the metadata footer entry for a locator.
/// </summary>
/// <remarks>
/// The footer is not present in PowerCutAreaGimmickLocatorSets.
/// </remarks>
/// <param name="writeUInt32">Function to write a uint32.</param>
/// <param name="locatorName">Name of the locator.</param>
/// <param name="dataSetName">Name of the locator's owning DataSet.</param>
let private writeGimmickLocatorMetadata writeUInt32 (locatorName, dataSetName) =
    writeUInt32 locatorName
    writeUInt32 dataSetName

/// <summary>
/// Writes a PowerCutAreaGimmickLocatorSet to lba format.
/// </summary>
/// <param name="writeSingle">Function to write a float32.</param>
/// <param name="writeInt32">Function to write a int32.</param>
/// <param name="writeEmptyBytes">Function to write a number of empty bytes.</param>
/// <param name="locators">ScaledGimmickLocatorSet to write.</param>
let private writePowerCutAreaGimmickLocatorSet writeSingle writeInt32 writeEmptyBytes locators =
    let locatorCount = Array.length locators
    writeGimmickLocatorSetHeader writeInt32 writeEmptyBytes locatorCount GimmickLocatorSetFormat.PowerCutArea
    locators 
    |> Array.iter (fun (locator : PowerCutAreaGimmickLocator) ->
        writeUnscaledGimmickLocatorTransform writeSingle locator.Position locator.Rotation)

/// <summary>
/// Writes a NamedGimmickLocatorSet to lba format.
/// </summary>
/// <param name="writeSingle">Function to write a float32.</param>
/// <param name="writeInt32">Function to write a int32.</param>
/// <param name="writeUInt32">Function to write a uint32.</param>
/// <param name="writeEmptyBytes">Function to write a number of empty bytes.</param>
/// <param name="locators">ScaledGimmickLocatorSet to write.</param>
let private writeNamedGimmickLocatorSet writeSingle writeInt32 writeUInt32 writeEmptyBytes locators =
    let locatorCount = Array.length locators
    writeGimmickLocatorSetHeader writeInt32 writeEmptyBytes locatorCount GimmickLocatorSetFormat.Named
    locators
    |> Array.map (fun (locator : NamedGimmickLocator) ->
        writeUnscaledGimmickLocatorTransform writeSingle locator.Position locator.Rotation
        locator.LocatorName, locator.DataSetName)
    |> Array.iter (fun locatorMetadata ->
        writeGimmickLocatorMetadata writeUInt32 locatorMetadata)

/// <summary>
/// Writes a ScaledGimmickLocatorSet to lba format.
/// </summary>
/// <param name="writeSingle">Function to write a float32.</param>
/// <param name="writeUInt16">Function to write a uint16.</param>
/// <param name="writeInt32">Function to write a int32.</param>
/// <param name="writeUInt32">Function to write a uint32.</param>
/// <param name="writeEmptyBytes">Function to write a number of empty bytes.</param>
/// <param name="locators">ScaledGimmickLocatorSet to write.</param>
let private writeScaledGimmickLocatorSet writeSingle writeUInt16 writeInt32 writeUInt32 writeEmptyBytes locators =
    let locatorCount = Array.length locators
    writeGimmickLocatorSetHeader writeInt32 writeEmptyBytes locatorCount GimmickLocatorSetFormat.Scaled
    locators
    |> Array.map (fun (locator : ScaledGimmickLocator) ->
        writeScaledGimmickLocatorTransform locator.Position locator.Rotation locator.Scale writeSingle writeUInt16
        locator.LocatorName, locator.DataSetName)
    |> Array.iter (fun locatorMetadata ->
        writeGimmickLocatorMetadata writeUInt32 locatorMetadata)

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