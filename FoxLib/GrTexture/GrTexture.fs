module FoxLib.GrTexture

open System

[<Flags>]
type public UnknownFlags = 
    | Flag0   = 0b0000000000000001
    | Flag1   = 0b0000000000010000
    | Flag2   = 0b0000000100000000 
    | Clp     = 0b0000000000000000 
    | Unknown = 0b0000000000010001
    | Default = 0b0000000100010001

[<Flags>]
type public TextureType = 
    | Linear = 0b1000000000000000000000001
    | SRGB   = 0b1000000000000000000000011
    | Cube   = 0b1000000000000000000000111
    | Normal = 0b1000000000000000000001001

/// <summary>
/// Metadata for an ftex file.
/// </summary>
type private Header = {
    // Skip 4 bytes allocated to the signature ("FTEX")
    // Skip 4 bytes for file version number (2.03 for TPP)
    PixelFormatType : uint16;
    Width : uint16;
    Height : uint16;
    Depth : uint16;
    MipMapCount : byte;
    NrtFlag : byte;
    UnknownFlags : UnknownFlags;
    // Skip 8 bytes for an int with a value of 1 and then an int with a value of 0
    TextureType : TextureType;
    ExtensionFileCount : byte;
    AdditionalExtensionFileCount : byte;
    // Skip 14 bytes
    UnknownHash0 : uint64;
    UnknownHash1 : uint64;
}

/// <summary>
/// Read an ftex header.
/// </summary>
/// <param name="readUInt16">Function to read a uint16.</param>
/// <param name="readByte">Function to read a byte.</param>
/// <param name="readBytes">Function to read a number of bytes.</param>
/// <param name="readUInt32">Function to read a uint32.</param>
/// <param name="readUInt64">Function to read a uint64.</param>
/// <param name="skipBytes">Function to skip bytes.</param>
/// <returns>Returns a parsed fv2 header.</returns>
let private readHeader readUInt16 readByte readBytes readUInt32 readUInt64 skipBytes =
    System.Diagnostics.Debug.Assert((System.Text.Encoding.UTF8.GetString(readBytes 4) = "FTEX"), "Error: Invalid signature.")
    System.Diagnostics.Debug.Assert((System.BitConverter.ToSingle(readBytes 4, 0) = 2.03f), "Error: Invalid version number.")

    let pixelFormatType = readUInt16()
    let width = readUInt16()
    let height = readUInt16()
    let depth = readUInt16()
    let mipMapCount = readByte()
    let nrtFlag = readByte()
    let unknownFlags = enum << int <| readUInt16()

    skipBytes 8

    let textureType = enum << int32 <| readUInt32()
    let extensionFileCount = readByte()
    let additionalExtensionFileCount = readByte()

    skipBytes 14

    let unknownHash0 = readUInt64()
    let unknownHash1 = readUInt64()

    { PixelFormatType = pixelFormatType;
    Width = width;
    Height = height;
    Depth = depth;
    MipMapCount = mipMapCount;
    NrtFlag = nrtFlag;
    UnknownFlags = unknownFlags;
    TextureType = textureType;
    ExtensionFileCount = extensionFileCount;
    AdditionalExtensionFileCount = additionalExtensionFileCount;
    UnknownHash0 = unknownHash0;
    UnknownHash1 = unknownHash1 }

/// <summary>
/// Ftex mipmap description
/// </summary>
type private MipmapDescription = {
    Offset : uint32;
    UncompressedFileSize : uint32;
    CompressedFileSize : uint32;
    Index : byte;
    FtexFileNumber : byte;
    ChunkCount : uint16;
}

/// <summary>
/// Read an array of mipmap description entries.
/// </summary>
/// <param name="mipmapCount">The amount of mipmaps the texture has.</param>
/// <param name="readUInt32">Function to read a uint32.</param>
/// <param name="readUInt16">Function to read a uint16.</param>
/// <param name="readByte">Function to read a byte.</param>
/// <returns>Returns a parsed array of mipmap descriptions.</returns>
let private readMipmapDescriptions mipmapCount readUInt32 readByte readUInt16 =
    [|1..mipmapCount|]
     |> Array.map (fun _ -> 
                            { Offset = readUInt32();
                            UncompressedFileSize = readUInt32();
                            CompressedFileSize = readUInt32();
                            Index = readByte();
                            FtexFileNumber = readByte();
                            ChunkCount = readUInt16() } )

/// <summary>
/// Determines if a given GrTexture is compressed.
/// </summary>
/// <param name="mipmapDescriptions">The descriptions of all mipmaps in the given GrTexture.</param>
/// <returns>Returns true if the texture is compressed, false if not.</returns>
let private getCompressed (mipmapDescriptions: MipmapDescription[]) =
    let totalUncompressedFileSize = mipmapDescriptions.[0].UncompressedFileSize
    let totalCompressedFileSize = mipmapDescriptions.[0].CompressedFileSize

    totalUncompressedFileSize <> totalCompressedFileSize
    
/// <summary>
/// Information on a given chunk for a given mipmap
/// </summary>
type private MipmapChunkDescription = {
    CompressedChunkSize : uint16
    UncompressedChunkSize : uint16
    DataOffset : uint32
}

/// <summary>
/// Read a number of mipmap chunk description entries.
/// </summary>
/// <param name="chunkCount">The amount of chunks the given mipmap is divided into.</param>
/// <param name="readUInt16">Function to read a uint16.</param>
/// <param name="readUInt32">Function to read a uint32.</param>
/// <returns>Returns a parsed array of mipmap chunk descriptions.</returns>
let private readMipmapChunkDescriptions chunkCount readUInt16 readUInt32 =
    [|1..chunkCount|]
     |> Array.map (fun _ -> 
                            { CompressedChunkSize = readUInt16();
                            UncompressedChunkSize = readUInt16();
                            DataOffset = readUInt32() } )

/// <summmary>
/// Parses a mipmap level that is separated into chunks.
/// </summmary>
/// <param name="mipmapDescription">The description of the chunked mipmap to read.</param>
/// <param name="readUInt16">Function to read a uint16.</param>
/// <param name="readUInt32">Function to read a uint32.</param>
/// <param name="readBytes">Function to read a number of bytes.</param>
/// <returns>The parsed mipmap.</returns>
let private readChunkedMipmap mipmapDescription readUInt16 readUInt32 (readBytes : int -> byte[]) alignStream isCompressed =
    alignStream << int64 <| mipmapDescription.Offset
    
    let mipmapChunkDescriptions = readMipmapChunkDescriptions (int <| mipmapDescription.ChunkCount) readUInt16 readUInt32

    match isCompressed with
    | true -> mipmapChunkDescriptions
               |> Array.map (fun chunkDesc -> readBytes << int <| chunkDesc.CompressedChunkSize)
               |> Array.concat
    | false -> mipmapChunkDescriptions
               |> Array.map (fun chunkDesc -> readBytes << int <| chunkDesc.UncompressedChunkSize)
               |> Array.concat

/// <summary>
/// A GrTexture mipmap, with the number of chunks and then the raw DDS texture data.
/// </summmary>
type public Mipmap = {
    ChunkCount : uint16
    Data : byte[]
}

/// <summary>
/// Function to read a mipmap.
/// </summary>
/// <param name="mipmapDescription">The description of the mipmap to read.</param>
/// <param name="readUInt16">Function to read a uint16.</param>
/// <param name="readUInt32">Function to read a uint32.</param>
/// <param name="readBytes">Function to read a number of bytes.</param>
/// <returns>Returns a parsed mipmap level.</returns>
let private readMipmap (mipmapDescription : MipmapDescription) readUInt16 readUInt32 readBytes alignStream isCompressed =
    { ChunkCount = mipmapDescription.ChunkCount;
    Data = match mipmapDescription.ChunkCount with
           | 0us -> readBytes (int <| mipmapDescription.UncompressedFileSize) //If the file doesn't have any chunks, the whole file is just a block of raw data
           | _ -> readChunkedMipmap mipmapDescription readUInt16 readUInt32 (fun i -> readBytes (int <| i)) alignStream isCompressed }

/// <summmary>
/// Input functions to the Read function.
/// </summmary>
type public ReadFunctions = {
    /// Function to read a uint16.
    ReadUInt16 : Func<uint16>
    /// Function to read a uint32.
    ReadUInt32 : Func<uint32>
    /// Function to read a uint64.
    ReadUInt64 : Func<uint64>
    /// Function to read a byte.
    ReadByte : Func<byte>
    /// Function to read a number of bytes.
    ReadBytes : Func<int, byte[]>
    /// Function to skip a number of bytes.
    SkipBytes : Action<int>
    /// Function to align the stream to a given offset.
    AlignStream : Action<int64>
}

/// <summmary>
/// Read parameters converted to F# functions.
/// </summmary>
type internal ConvertedReadFunctions = {
    ReadUInt16 : unit -> uint16
    ReadUInt32 : unit -> uint32
    ReadUInt64 : unit -> uint64
    ReadByte : unit -> byte
    ReadBytes : int -> byte[]
    SkipBytes : int -> unit
    AlignStream : int64 -> unit
}

/// <summmary>
/// Converts the Read function's .NET Funcs into F# functions.
/// </summmary>
/// <param name="rawReadFunctions">Input functions supplied to the Read function.</param>
/// <returns>The converted functions.</returns>
let private convertReadFunctions (rawReadFunctions : ReadFunctions) =
    if rawReadFunctions.ReadUInt16 |> isNull then nullArg "ReadUInt16"
    if rawReadFunctions.ReadUInt32 |> isNull then nullArg "ReadUInt32"
    if rawReadFunctions.ReadUInt64 |> isNull then nullArg "ReadUInt64"
    if rawReadFunctions.ReadByte |> isNull then nullArg "ReadByte"
    if rawReadFunctions.ReadBytes |> isNull then nullArg "ReadBytes"
    if rawReadFunctions.SkipBytes |> isNull then nullArg "SkipBytes"
    if rawReadFunctions.AlignStream |> isNull then nullArg "AlignStream"

    { ConvertedReadFunctions.ReadUInt16 = rawReadFunctions.ReadUInt16.Invoke;
    ReadUInt32 = rawReadFunctions.ReadUInt32.Invoke;
    ReadUInt64 = rawReadFunctions.ReadUInt64.Invoke;
    ReadByte = rawReadFunctions.ReadByte.Invoke;
    ReadBytes = rawReadFunctions.ReadBytes.Invoke;
    SkipBytes = rawReadFunctions.SkipBytes.Invoke;
    AlignStream = rawReadFunctions.AlignStream.Invoke }

/// <summary>
/// A texture used by the Fox Engine.
/// </summary>
type public GrTexture = {
    Height : uint16
    Width : uint16
    Depth : uint16
    Compressed : bool
    PixelFormat : uint16
    TextureType : TextureType
    UnknownFlags : UnknownFlags
    DDSData : byte[]
}

/// <summmary>
/// Parses a GrTexture from .ftex and .#.ftexs format.
/// </summmary>
/// <param name="readFunctions">An array of functions to read data types from the input. Read uses one readFunction array entry for each file.</param>
/// <returns>The parsed GrTexture.</returns>
let public Read (readFunctions : ReadFunctions[]) =
    let convertedReadFunctions = readFunctions |> Array.map convertReadFunctions

    let ftexReadFunctions = convertedReadFunctions.[0]

    let header = readHeader ftexReadFunctions.ReadUInt16 ftexReadFunctions.ReadByte ftexReadFunctions.ReadBytes ftexReadFunctions.ReadUInt32 ftexReadFunctions.ReadUInt64 ftexReadFunctions.SkipBytes

    let extensionFileCount = int <| header.ExtensionFileCount
    System.Diagnostics.Debug.Assert(((readFunctions.Length - 1) = extensionFileCount), String.Concat("Error: Can't find .", extensionFileCount, ".ftexs."))

    let mipmapDescriptions = readMipmapDescriptions (header.MipMapCount |> int) ftexReadFunctions.ReadUInt32 ftexReadFunctions.ReadByte ftexReadFunctions.ReadUInt16

    let compressed = getCompressed mipmapDescriptions

    let mipmaps = mipmapDescriptions
                  |> Array.map (fun mipmapDescription -> let ftexsIndex = int <| mipmapDescription.FtexFileNumber

                                                         readMipmap mipmapDescription convertedReadFunctions.[ftexsIndex].ReadUInt16 convertedReadFunctions.[ftexsIndex].ReadUInt32 convertedReadFunctions.[ftexsIndex].ReadBytes convertedReadFunctions.[ftexsIndex].AlignStream compressed
                                                         )

    { Height = header.Height;
    Width = header.Width;
    Depth = header.Depth;
    Compressed = compressed;
    PixelFormat = header.PixelFormatType;
    TextureType = header.TextureType;
    UnknownFlags = header.UnknownFlags;
    DDSData = mipmaps
              |> Array.map (fun mipmap -> mipmap.Data)
              |> Array.concat }