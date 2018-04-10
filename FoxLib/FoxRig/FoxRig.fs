module FoxLib.Tpp.FoxRig

open System
open System.Text
open FoxLib.Core
open FoxLib.Tpp.RouteSet

/// <summary>
/// Metadata for a frig file.
/// </summary>
type private Header = {
    Hash : uint32;
    TypeStringStartOffset : uint32;
    TypeStringEndOffset : uint32;
    GroupCount : uint32;
    TrackCount : uint32;
    FileSize : uint32;
    EndOfRigData : uint32;
    StartOfRigData : uint32;
}

type private ChannelInfo = {
    Unknown0 : uint16
    Unknown1 : uint16
    Unknown2 : uint16
    Unknown3 : uint16
    Unknown4 : uint16
    Unknown5 : uint16
    Unknown6 : uint16
    Unknown7 : uint16
}

type private Track = {
    Unknown0 : uint16
    Unknown1 : uint16
    Unknown2 : uint16
    BoneCount : uint16
    Unknown3 : uint16
    Unknown4 : uint16
    Unknown5 : uint16
    Unknown6 : uint16
    Unknown7 : uint16
    Unknown8 : uint16
    Unknown9 : uint16
    Unknown10 : uint16
    Unknown11 : uint16
    Unknown12 : uint16
    Unknown13 : uint16
    Unknown14 : uint16
}

type private RigMaskInfo = {
    Hash : StrCode32Hash
    Name : string
    Unknown0 : Vector3
    Unknown1 : Vector3
    Unknown2 : Vector3
    Unknown3 : Vector3
    Unknown4 : Vector3
    Unknown5 : Vector3
}

type private BoneInfo = {
    TrackID : uint32
    Hash : StrCode32Hash
}

/// <summary>
/// Get the character encoding to use for reading/writing frig files.
/// </summary>
let public getEncoding() =
    Encoding.GetEncoding 1252

/// <summary>
/// Read a snippet as a string.
/// </summary>
/// <param name="readChars">Function to read a number of chars.</param>
/// <param name="charCount">Number of chars to be read.</param>
let private readSnippetAsString readChars charCount=
    let chars = readChars charCount;
    System.Text.Encoding.Default.GetString(chars)

/// <summary>
/// Read a frig header.
/// </summary>
/// <param name="readHash">Function to read a hash.</param>
/// <param name="readVersion">Function to read a size.</param>
/// <param name="readCount">Function to read a count.</param>
/// <param name="readOffset">Function to read an offset.</param>
let private readHeader readHash readSize readCount readOffset =
    
    { Hash = readHash();
    TypeStringStartOffset = readOffset();
    TypeStringEndOffset = readOffset(); //Seemingly broken?
    GroupCount = readCount();
    TrackCount = readCount();
    FileSize = readSize();
    EndOfRigData = readOffset();
    StartOfRigData = readOffset() }

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
    /// Function to read chars.
    ReadBytes : Func<int, byte[]>
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
    ReadBytes : int -> byte[]
    SkipBytes : int -> unit
}

/// <summmary>
/// Converts the Read function's .NET Funcs into F# functions.
/// </summmary>
/// <param name="rawReadFunctions">Input functions supplied to the Read function.</param>
/// <returns>The converted functions.</returns>
let private convertReadFunctions (rawReadFunctions : ReadFunctions) =
    if rawReadFunctions.ReadInt32 |> isNull then nullArg "ReadInt32"
    if rawReadFunctions.ReadUInt16 |> isNull then nullArg "ReadUInt16"
    if rawReadFunctions.ReadUInt32 |> isNull then nullArg "ReadUInt32"
    if rawReadFunctions.ReadInt32 |> isNull then nullArg "ReadInt32"
    if rawReadFunctions.ReadBytes |> isNull then nullArg "ReadBytes"
    if rawReadFunctions.SkipBytes |> isNull then nullArg "SkipBytes"

    { ConvertedReadFunctions.ReadSingle = rawReadFunctions.ReadSingle.Invoke;
    ReadUInt16 = rawReadFunctions.ReadUInt16.Invoke;
    ReadUInt32 = rawReadFunctions.ReadUInt32.Invoke;
    ReadInt32 = rawReadFunctions.ReadInt32.Invoke;
    ReadBytes = rawReadFunctions.ReadBytes.Invoke;
    SkipBytes = rawReadFunctions.SkipBytes.Invoke; }

/// <summmary>
/// Parses rig information from frig format.
/// </summmary>
/// <param name="readFunctions">Functions to read various data types from the input.</param>
/// <returns>The parsed rig.</returns>
let public Read (readFunctions : ReadFunctions) =
    let convertedFunctions = convertReadFunctions readFunctions
    let header = readHeader convertedFunctions.ReadUInt32 convertedFunctions.ReadUInt32 convertedFunctions.ReadUInt32 convertedFunctions.ReadUInt32

    let groupAddr = [|1..header.GroupCount |> int|]
                     |> Array.map (fun _ -> convertedFunctions.ReadUInt32())

    let nameCharCount = (header.TypeStringStartOffset - header.TypeStringEndOffset) * 10u // Added a "* 10u" to "make it work", but this does NOT feel like the actual solution to this problem.// 
    let name = readSnippetAsString convertedFunctions.ReadBytes (int nameCharCount)

    //GOING TO BE TRACK INFO



    name

/// <summmary>
/// Input functions to the Write function.
/// </summmary>
type public WriteFunctions = {
    /// Function to write a char.
    WriteChar : Action<char>
    /// Function to write a uint16.
    WriteUInt16 : Action<uint16>
    /// Function to write a uint32.
    WriteUInt32 : Action<uint32>
}

/// <summmary>
/// Write parameters converted to F# functions.
/// </summmary>
type private ConvertedWriteFunctions = {
    WriteChar : char -> unit
    WriteUInt16 : uint16 -> unit
    WriteUInt32 : uint32 -> unit
}

/// <summmary>
/// Converts the Write function's .NET Funcs into F# functions.
/// </summmary>
/// <param name="rawWriteFunctions">Input functions supplied to the Write function.</param>
/// <returns>The converted functions.</returns>
let private convertWriteFunctions (rawWriteFunctions : WriteFunctions) =
    if rawWriteFunctions.WriteChar |> isNull then nullArg "WriteChar"
    if rawWriteFunctions.WriteUInt16 |> isNull then nullArg "WriteUInt16"
    if rawWriteFunctions.WriteUInt32 |> isNull then nullArg "WriteUInt32"

    { ConvertedWriteFunctions.WriteChar = rawWriteFunctions.WriteChar.Invoke;
    WriteUInt16 = rawWriteFunctions.WriteUInt16.Invoke;
    WriteUInt32 = rawWriteFunctions.WriteUInt32.Invoke }

/// <summary>
/// Writes a rig to frig format.
/// </summary>
/// <param name="writeFunctions">Function to write various data types.</param>
/// <param name="foxRig">Rig to write.</param>
let public Write (writeFunctions : WriteFunctions) foxRig =
    let convertedWriteFunctions = convertWriteFunctions writeFunctions

    convertedWriteFunctions.WriteUInt32 56899313u //Unlike almost all other formats, the frig format's "signature" is serialized as a unit32.

    convertedWriteFunctions.WriteUInt16 2us
    convertedWriteFunctions.WriteUInt16 (Seq.length foxRig |> uint16)

    foxRig
    |> Array.iter (fun id -> convertedWriteFunctions.WriteUInt32 id)