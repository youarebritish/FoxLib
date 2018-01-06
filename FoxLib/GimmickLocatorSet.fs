module FoxLib.Tpp.GimmickLocatorSet

open System.IO
open FoxLib
open FoxLib.Tpp.GimmickLocator
open System
    
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
let private readUnscaledTransform readVector4 readQuaternion =
    let position = readVector4()
    let rotation = readQuaternion()
    position, rotation

/// <summary>
/// Reads a ScaledGimmickLocator transform.
/// </summary>
/// <param name="readSingle">Function to read a float32.</param>
/// <param name="readUInt16">Function to read a uint16.</param>
/// <returns>The parsed transform as a tuple of position * rotation * scale.
let private readScaledTransform readVector4 readQuaternion readWideVector3 =
    let position = readVector4()
    let rotation = readQuaternion()
    let scale = readWideVector3()
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
/// <param name="readTransform">Function to read a scaled locator transform.</param>
/// <param name="locatorCount">Number of locators.</param>
/// <returns>The parsed PowerCutAreaGimmickLocators.</returns>
let private readPowerCutAreaGimmickLocators readTransform locatorCount =
    [|1..locatorCount|]
    |> Array.map(fun _ ->
        let position, rotation = readTransform()
        { Position = position; Rotation = rotation })

/// <summary>
/// Reads NamedGimmickLocators from lba format.
/// </summary>
/// <param name="readTransform">Function to read a scaled locator transform.</param>
/// <param name="readMetadata">Function to read a locator's metadata footer entry.</param>
/// <param name="locatorCount">Number of locators.</param>
/// <returns>The parsed NamedGimmickLocators.</returns>
let private readNamedGimmickLocators readTransform readMetadata locatorCount =
    [|1..locatorCount|]
    |> Array.map(fun _ -> readTransform())
    |> Array.map(fun transform -> 
        let metadata = readMetadata()
        { Position = fst transform;
        Rotation = snd transform;
        LocatorName = fst metadata;
        DataSetName = snd metadata })

/// <summary>
/// Reads ScaledGimmickLocators from lba format.
/// </summary>
/// <param name="readTransform">Function to read a scaled locator transform.</param>
/// <param name="readMetadata">Function to read a locator's metadata footer entry.</param>
/// <param name="locatorCount">Number of locators.</param>
/// <returns>The parsed ScaledGimmickLocators.</returns>
let private readScaledGimmickLocators readTransform readMetadata locatorCount =
    [|1..locatorCount|]
    |> Array.map(fun _ -> readTransform())
    |> Array.map(fun transform -> 
        let metadata = readMetadata()
        let position, rotation, scale = transform
        { Position = position;
        Rotation = rotation;
        Scale = scale;
        LocatorName = fst metadata;
        DataSetName = snd metadata })

/// <summary>
/// Parses the body of a GimmickLocatorSet from lba format.
/// </summary>
/// <param name="readVector4">Function to read a Vector4.</param>
/// <param name="readQuaternion">Function to read a Quaternion.</param>
/// <param name="readWideVector3">Function to read a WideVector3.</param>
/// <param name="readUInt32">Function to read a uint32.</param>
/// <param name="format">Format of the GimmickLocatorSet.</param>
/// <param name="locatorCount">Number of locators.</param>
/// <returns>The parsed GimmickLocatorSet.</returns>
/// <exception cref="InvalidDataException">Raised if the format is unrecognized.</exception>
let private readGimmickLocatorSetBody readVector4 readQuaternion readWideVector3 readUInt32 format locatorCount =
    match format with
    | GimmickLocatorSetFormat.PowerCutArea ->
        let readTransform = fun () -> readUnscaledTransform readVector4 readQuaternion
        PowerCutAreaGimmickLocatorSet(readPowerCutAreaGimmickLocators readTransform locatorCount)
    | GimmickLocatorSetFormat.Named ->
        let readTransform = fun () -> readUnscaledTransform readVector4 readQuaternion
        let readMetadata = fun () -> readLocatorMetadata readUInt32
        NamedGimmickLocatorSet(readNamedGimmickLocators readTransform readMetadata locatorCount)
    | GimmickLocatorSetFormat.Scaled ->
        let readTransform = fun () -> readScaledTransform readVector4 readQuaternion readWideVector3
        let readMetadata = fun () -> readLocatorMetadata readUInt32
        ScaledGimmickLocatorSet(readScaledGimmickLocators readTransform readMetadata locatorCount)
    | _ -> raise (new InvalidDataException("The file is not an .lba file or is an unrecognized format."))
  
/// <summmary>
/// Input functions to the Read function.
/// </summmary>
type public ReadFunctions = {
    /// Function to read a float32.
    ReadSingle : Func<float32>
    /// Function to read a uint16.
    ReadUInt16 : Func<uint16>
    /// Function to read a uint32.
    ReadUInt32 : Func<uint32>
    /// Function to read a int32.
    ReadInt32 : Func<int32>
    /// Function to skip a number of bytes.
    SkipBytes : Action<int>
}

/// <summmary>
/// Read parameters converted to F# functions.
/// </summmary>
type private ConvertedReadFunctions = {
    ReadSingle : unit -> float32
    ReadUInt16 : unit -> uint16
    ReadUInt32 : unit -> uint32
    ReadInt32 : unit -> int32
    SkipBytes : int -> unit
}

/// <summmary>
/// Converts the Read function's .NET Funcs into F# functions.
/// </summmary>
/// <param name="rawReadFunctions">Input functions supplied to the Read function.</param>
/// <returns>The converted functions.</returns>
let private convertReadFunctions (rawReadFunctions : ReadFunctions) =
    { ConvertedReadFunctions.ReadSingle = rawReadFunctions.ReadSingle.Invoke;
    ReadUInt16 = rawReadFunctions.ReadUInt16.Invoke;
    ReadUInt32 = rawReadFunctions.ReadUInt32.Invoke;
    ReadInt32 = rawReadFunctions.ReadInt32.Invoke;
    SkipBytes = rawReadFunctions.SkipBytes.Invoke; }

/// <summmary>
/// Parses a GimmickLocatorSet from lba format.
/// </summmary>
/// <param name="readFunctions">Functions to read various data types from the input.</param>
/// <returns>The parsed GimmickLocatorSet.</returns>
let public Read (readFunctions : ReadFunctions) =
    let convertedFunctions = convertReadFunctions readFunctions
    let locatorCount = convertedFunctions.ReadInt32()
    let format = convertedFunctions.ReadInt32()  |> enum<GimmickLocatorSetFormat>
    convertedFunctions.SkipBytes 8 |> ignore

    let readVector4 = fun () -> Vector4.Read convertedFunctions.ReadSingle
    let readQuaternion = fun () -> Quaternion.Read convertedFunctions.ReadSingle
    let readWideVector3 = fun () -> WideVector3.Read convertedFunctions.ReadSingle convertedFunctions.ReadUInt16    
    readGimmickLocatorSetBody readVector4 readQuaternion readWideVector3 convertedFunctions.ReadUInt32 format locatorCount

/// <summary>
/// Writes a GimmickLocatorSet's header.
/// </summary>
/// <param name="writeInt32">Function to write an int32.</param>
/// <param name="writeEmptyBytes">Function to write a number of empty bytes.</param>
/// <param name="locatorCount">Number of locators in the locator set.</param>
/// <param name="format">Format of the locator set.</param>
let private writeGimmickLocatorSetHeader writeInt32 writeEmptyBytes locatorCount format =
    writeInt32 locatorCount
    LanguagePrimitives.EnumToValue format |> writeInt32
    writeEmptyBytes 8

/// <summary>
/// Writes a PowerCutAreaGimmickLocator or NamedGimmickLocator's transform.
/// </summary>
/// <param name="writeSingle">Function to write a float32.</param>
/// <param name="position">The locator's position.</param>
/// <param name="rotation">The locator's rotation.</param>
let private writeUnscaledGimmickLocatorTransform writeSingle position rotation =
    Vector4.Write position writeSingle
    Quaternion.Write rotation writeSingle

/// <summary>
/// Writes a ScaledGimmickLocator's transform.
/// </summary>
/// <param name="writeSingle">Function to write a float32.</param>
/// <param name="writeUInt16">Function to write a uint16.</param>
/// <param name="position">The locator's position.</param>
/// <param name="rotation">The locator's rotation.</param>
/// <param name="scale">The locator's scale.</param>
let private writeScaledGimmickLocatorTransform writeSingle writeUInt16 position rotation scale =
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
/// <param name="writeTransform">Function to write an unscaled transform.</param>
/// <param name="locators">ScaledGimmickLocatorSet to write.</param>
let private writePowerCutAreaGimmickLocatorSet writeTransform locators =
    locators
    |> Array.iter (fun (locator : PowerCutAreaGimmickLocator) ->
        writeTransform locator.Position locator.Rotation)

/// <summary>
/// Writes a NamedGimmickLocatorSet to lba format.
/// </summary>
/// <param name="writeTransform">Function to write an unscaled transform.</param>
/// <param name="writeMetadata">Function to write a locator's metadata footer entry.</param>
/// <param name="locators">NamedGimmickLocatorSet to write.</param>
let private writeNamedGimmickLocatorSet writeTransform writeMetadata locators =
    locators
    |> Array.map (fun (locator : NamedGimmickLocator) ->
        writeTransform locator.Position locator.Rotation
        locator.LocatorName, locator.DataSetName)
    |> Array.iter (fun locatorMetadata -> writeMetadata locatorMetadata)

/// <summary>
/// Writes a ScaledGimmickLocatorSet to lba format.
/// </summary>
/// <param name="writeTransform">Function to write an unscaled transform.</param>
/// <param name="writeMetadata">Function to write a locator's metadata footer entry.</param>
/// <param name="locators">ScaledGimmickLocatorSet to write.</param>
let private writeScaledGimmickLocatorSet writeTransform writeMetadata locators =
    locators
    |> Array.map (fun (locator : ScaledGimmickLocator) ->
        writeTransform locator.Position locator.Rotation locator.Scale
        locator.LocatorName, locator.DataSetName)
    |> Array.iter (fun locatorMetadata -> writeMetadata locatorMetadata)

/// <summmary>
/// Input functions to the Write function.
/// </summmary>
type public WriteFunctions = {
    /// Function to write a float32.
    WriteSingle : Action<float32>
    /// Function to write a uint16.
    WriteUInt16 : Action<uint16>
    /// Function to write a uint32.
    WriteUInt32 : Action<uint32>
    /// Function to write a int32.
    WriteInt32 : Action<int32>
    /// Function to write a number of filler bytes.
    WriteEmptyBytes : Action<int>
}

/// <summmary>
/// Write parameters converted to F# functions.
/// </summmary>
type private ConvertedWriteFunctions = {
    WriteSingle : float32 -> unit
    WriteUInt16 : uint16 -> unit
    WriteUInt32 : uint32 -> unit
    WriteInt32 : int32 -> unit
    WriteEmptyBytes : int32 -> unit
}

/// <summmary>
/// Converts the Write function's .NET Funcs into F# functions.
/// </summmary>
/// <param name="rawWriteFunctions">Input functions supplied to the Write function.</param>
/// <returns>The converted functions.</returns>
let private convertWriteFunctions (rawWriteFunctions : WriteFunctions) =
    { ConvertedWriteFunctions.WriteSingle = rawWriteFunctions.WriteSingle.Invoke;
    WriteUInt16 = rawWriteFunctions.WriteUInt16.Invoke;
    WriteUInt32 = rawWriteFunctions.WriteUInt32.Invoke;
    WriteInt32 = rawWriteFunctions.WriteInt32.Invoke;
    WriteEmptyBytes = rawWriteFunctions.WriteEmptyBytes.Invoke; }

/// <summary>
/// Writes a GimmickLocatorSet to lba format.
/// </summary>
/// <param name="writeFunctions">Function to write various data types.</param>
/// <param name="locatorSet">GimmickLocatorSet to write.</param>
let public Write locatorSet (writeFunctions : WriteFunctions) =
    let convertedWriteFunctions = convertWriteFunctions writeFunctions
    let writeHeader = writeGimmickLocatorSetHeader convertedWriteFunctions.WriteInt32 convertedWriteFunctions.WriteEmptyBytes
    match locatorSet with
    | PowerCutAreaGimmickLocatorSet locators ->
        writeHeader (Seq.length locators) GimmickLocatorSetFormat.PowerCutArea
        let writeTransform = writeUnscaledGimmickLocatorTransform convertedWriteFunctions.WriteSingle
        writePowerCutAreaGimmickLocatorSet writeTransform (Seq.toArray locators)
    | NamedGimmickLocatorSet locators ->
        writeHeader (Seq.length locators) GimmickLocatorSetFormat.Named
        let writeTransform = writeUnscaledGimmickLocatorTransform convertedWriteFunctions.WriteSingle
        let writeMetadata = writeGimmickLocatorMetadata convertedWriteFunctions.WriteUInt32
        writeNamedGimmickLocatorSet writeTransform writeMetadata (Seq.toArray locators)
    | ScaledGimmickLocatorSet locators ->
        writeHeader (Seq.length locators) GimmickLocatorSetFormat.Scaled
        let writeTransform = writeScaledGimmickLocatorTransform convertedWriteFunctions.WriteSingle convertedWriteFunctions.WriteUInt16
        let writeMetadata = writeGimmickLocatorMetadata convertedWriteFunctions.WriteUInt32
        writeScaledGimmickLocatorSet writeTransform writeMetadata (Seq.toArray locators)