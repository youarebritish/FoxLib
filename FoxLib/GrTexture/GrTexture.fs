module FoxLib.GrTexture

open System
open ICSharpCode.SharpZipLib.Zip.Compression.Streams

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
    FtexsFileNumber : byte;
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
                            FtexsFileNumber = readByte();
                            ChunkCount = readUInt16() } )
    
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
/// Inflates a compressed DDS chunk.
/// </summmary>
/// <param name="data">The original, compressed, byte array.</param>
/// <returns>The inflated byte array.</returns>
let inflate (data : byte[]) = 
    let inflaterStream = new InflaterInputStream(new System.IO.MemoryStream(data));
    let outputStream = new System.IO.MemoryStream();
    inflaterStream.CopyTo(outputStream);
    outputStream.ToArray();

/// <summmary>
/// Parses a mipmap level that is separated into chunks.
/// </summmary>
/// <param name="mipmapDescription">The description of the chunked mipmap to read.</param>
/// <param name="readUInt16">Function to read a uint16.</param>
/// <param name="readUInt32">Function to read a uint32.</param>
/// <param name="readBytes">Function to read a number of bytes.</param>
/// <returns>The parsed mipmap.</returns>
let private readChunkedMipmap mipmapDescription readUInt16 readUInt32 (readBytes : int -> byte[]) alignStream =
    alignStream << int64 <| mipmapDescription.Offset
    
    let mipmapChunkDescriptions = readMipmapChunkDescriptions (int <| mipmapDescription.ChunkCount) readUInt16 readUInt32

    mipmapChunkDescriptions
    |> Array.map (fun chunkDesc -> //if chunkDesc.DataOffset > 0x80000000u then alignStream << int64 <| (mipmapDescription.Offset + (chunkDesc.DataOffset - 0x80000000u))
                                   let bytes = readBytes << int <| chunkDesc.CompressedChunkSize
                                   match chunkDesc.CompressedChunkSize = chunkDesc.UncompressedChunkSize with
                                   | true -> bytes
                                   | false -> (inflate bytes )
                                   )
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
let private readMipmap (mipmapDescription : MipmapDescription) readUInt16 readUInt32 readBytes alignStream =
    { ChunkCount = mipmapDescription.ChunkCount;
    Data = match mipmapDescription.ChunkCount with
           | 0us -> readBytes (int <| mipmapDescription.UncompressedFileSize) //If the file doesn't have any chunks, the whole file is just a block of raw data
           | _ -> readChunkedMipmap mipmapDescription readUInt16 readUInt32 (fun i -> readBytes (int <| i)) alignStream }

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
    PixelFormat : uint16
    NrtFlag : byte
    TextureType : TextureType
    UnknownFlags : UnknownFlags
    MipMapCount : byte
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

    let mipmaps = mipmapDescriptions
                  |> Array.map (fun mipmapDescription -> let ftexsIndex = int <| mipmapDescription.FtexsFileNumber

                                                         readMipmap mipmapDescription convertedReadFunctions.[ftexsIndex].ReadUInt16 convertedReadFunctions.[ftexsIndex].ReadUInt32 convertedReadFunctions.[ftexsIndex].ReadBytes convertedReadFunctions.[ftexsIndex].AlignStream
                                                         )

    { Height = header.Height;
    Width = header.Width;
    Depth = header.Depth;
    PixelFormat = header.PixelFormatType;
    NrtFlag = header.NrtFlag;
    TextureType = header.TextureType;
    UnknownFlags = header.UnknownFlags;
    MipMapCount = byte <| mipmaps.Length;
    DDSData = mipmaps
              |> Array.map (fun mipmap -> mipmap.Data)
              |> Array.concat }

/// <summmary>
/// Deflates a compressed DDS chunk.
/// </summmary>
/// <param name="data">The original, uncompressed, byte array.</param>
/// <returns>The deflated byte array.</returns>
let deflate (data : byte[]) = 
    let outputStream = new System.IO.MemoryStream();
    let deflaterStream = new DeflaterOutputStream(outputStream);
    deflaterStream.Write(data, 0, data.Length);
    deflaterStream.Close();
    outputStream.ToArray();

let private writeChunkDescription writeUInt16 writeUInt32 chunkDescription =
    writeUInt16 chunkDescription.CompressedChunkSize
    writeUInt16 chunkDescription.UncompressedChunkSize
    writeUInt32 chunkDescription.DataOffset

let maxChunkSize = System.Int16.MaxValue / 2s + 1s

let private writeChunksAndGetMipmapDescription mipmapIndex ftexsFileIndex (mipmapData : byte[]) (writeBytes : byte[] -> unit) (writeUInt16 : uint16 -> unit) (writeUInt32 : uint32 -> unit) (getWriterPosition : unit -> int64) = 
    let chunkCount = uint32 <| Math.Ceiling( (float mipmapData.Length) / (float maxChunkSize) )

    match chunkCount with
    | 0u -> writeBytes mipmapData
            { Offset = 0u;
            UncompressedFileSize = uint32 <| mipmapData.Length;
            CompressedFileSize = uint32 <| mipmapData.Length;
            Index = mipmapIndex;
            FtexsFileNumber = ftexsFileIndex;
            ChunkCount = 0us; }
    | i  -> let originalPosition = getWriterPosition()
            let mutable chunkOffset = 0u
            let mutable uncompressedChunkOffset = 0u

            let chunkTableOffset = i * 0x8u

            let chunks = [|1..(int i)|] // This calculates the number of chunks. No idea how it works; I got it from Avaark's code.
                          |> Array.map(fun chunkIndex -> let chunkSize = Math.Min(mipmapData.Length - (int chunkOffset), (int maxChunkSize))
                                                         let newChunkOffset = uncompressedChunkOffset + (uint32 chunkSize)
                                                         let originalData = mipmapData.[(int uncompressedChunkOffset)..(int newChunkOffset) - 1]
                                                         uncompressedChunkOffset <- newChunkOffset
                                                         let uncompressedSize = originalData.Length
                                                         let compressedData = deflate originalData
                                                         let data = if uncompressedSize = compressedData.Length then originalData else compressedData
                                                         let newSize = data.Length
                                                                                             
                                                         let dataOffset = if chunkCount = 1u && uncompressedSize <= newSize
                                                                              then 0x80000000u (*no idea what this is*) ||| 0x8u (*size of one chunk description*)
                                                                          else chunkOffset + chunkTableOffset

                                                         let chunkDesc = { CompressedChunkSize = uint16 newSize;
                                                                         UncompressedChunkSize = uint16 uncompressedSize;
                                                                         DataOffset = dataOffset }

                                                         chunkOffset <- chunkOffset + (uint32 newSize)

                                                         (chunkDesc, data) )

            let (chunkDescriptions, chunkData) = let chunkDescriptions = chunks
                                                                         |> Array.map(fun (chunkDescription, chunkData) -> chunkDescription)
                                                 let chunkData = chunks
                                                                 |> Array.map(fun (chunkDescription, chunkData) -> chunkData)

                                                 (chunkDescriptions, chunkData)

            chunkDescriptions
            |> Array.map(fun chunkDescription -> writeChunkDescription writeUInt16 writeUInt32 chunkDescription )
            |> ignore

            chunkData
            |> Array.map(fun chunkData -> writeBytes chunkData)
            |> ignore

            let fullUncompressedSize = chunkDescriptions
                                       |> Array.map(fun chunkDesc -> uint32 <| chunkDesc.UncompressedChunkSize)
                                       |> Array.sum
                                        
            let fullCompressedSize = chunkDescriptions
                                     |> Array.map(fun chunkDesc -> uint32 <| chunkDesc.CompressedChunkSize)
                                     |> Array.sum                                           

            { Offset = uint32 <| originalPosition;
            UncompressedFileSize = fullUncompressedSize;
            CompressedFileSize = fullCompressedSize;
            Index = mipmapIndex;
            FtexsFileNumber = ftexsFileIndex;
            ChunkCount = uint16 <| chunkCount }

let private writeMipmapDescription (writeUInt32 : uint32 -> unit) (writeUInt16 : uint16 -> unit) (writeByte : byte -> unit) mipmapDescription =
    writeUInt32 mipmapDescription.Offset
    writeUInt32 mipmapDescription.UncompressedFileSize
    writeUInt32 mipmapDescription.CompressedFileSize
    writeByte mipmapDescription.Index
    writeByte mipmapDescription.FtexsFileNumber
    writeUInt16 mipmapDescription.ChunkCount

let private writeHeader writeChars writeByte writeBytes writeUInt16 writeUInt32 writeEmptyBytes grTexture ftexFileCount =
    writeChars [| 'F'; 'T'; 'E'; 'X' |]
    writeBytes (BitConverter.GetBytes(float32 <| 2.03f))
    writeUInt16 grTexture.PixelFormat
    writeUInt16 grTexture.Height
    writeUInt16 grTexture.Width
    writeUInt16 grTexture.Depth
    writeByte grTexture.MipMapCount
    writeByte grTexture.NrtFlag
    writeUInt16 (uint16 <| grTexture.UnknownFlags)
    writeUInt32 1u
    writeUInt32 0u
    writeUInt32 (uint32 <| grTexture.TextureType)
    writeByte ftexFileCount
    writeByte (ftexFileCount - 1uy)
    writeEmptyBytes 30


/// <summmary>
/// Input functions to the Write function.
/// </summmary>
type public WriteFunctions = {
    /// Function to write a uint16.
    WriteUInt16 : Action<uint16>
    /// Function to write a uint32.
    WriteUInt32 : Action<uint32>
    /// Function to write a uint64.
    WriteUInt64 : Action<uint64>
    /// Function to write a byte.
    WriteByte : Action<byte>
    /// Function to write a number of bytes.
    WriteBytes : Action<byte[]>
    /// Function to write a number of chars.
    WriteChars : Action<char[]>
    /// Function to write a number of filler bytes.
    WriteEmptyBytes : Action<int>
    /// Function to get the writer's current position
    GetWriterPosition : Func<int64>
}

/// <summmary>
/// Write parameters converted to F# functions.
/// </summmary>
type private ConvertedWriteFunctions = {
    WriteUInt16 : uint16 -> unit
    WriteUInt32 : uint32 -> unit
    WriteUInt64 : uint64 -> unit
    WriteByte : byte -> unit
    WriteBytes : byte[] -> unit
    WriteChars : char[] -> unit
    WriteEmptyBytes : int32 -> unit
    GetWriterPosition : unit -> int64
}

/// <remarks (on the remarks)>
/// The following function and its remarks were taken from Atvaark's FtexTool (https://github.com/Atvaark/FtexTool).
/// </remarks (on the remarks)>
/// <remarks>
/// I derived this algorithm via decision tree learning the TPP data set. (with sklearn)
/// 
/// The features that I used (<c>fileSize</c> and <c>mipMapCount</c>) had to be available in
/// the DDS files that FtexTool generates. 
/// That means that some custom Ftex flags can't be used to determine the amount of .ftexs files.
/// Having no access to flags such as <see cref="FtexFile.UnknownFlags"/> means that this
/// algorithm can't correctly classify that a .ftex file has more than 3 .ftexs files.
/// 
/// Stats:
///       Predicts ~99,90% of the .ftexs file counts correctly. (The previos algorithm only got 44,00% correct.)
///       Of the ~0,10% incorrect predictions ~97,00% predict a too small ftexs file count.
/// </remarks>
let private getFtexsFileCount byteCount mipmapCount =
    let fileCount = if byteCount <= 76456
                        then 
                            if byteCount <= 19112 || mipmapCount <= 3
                                then 1
                                else 2
                    else
                        if mipmapCount <= 4
                            then 1
                            else 3

    Math.Min(fileCount, mipmapCount)

/// <summmary>
/// Converts the Write function's .NET Funcs into F# functions.
/// </summmary>
/// <param name="rawWriteFunctions">Input functions supplied to the Write function.</param>
/// <returns>The converted functions.</returns>
let private convertWriteFunctions (rawWriteFunctions : WriteFunctions) =
    if rawWriteFunctions.WriteUInt16 |> isNull then nullArg "WriteUInt16"
    if rawWriteFunctions.WriteUInt32 |> isNull then nullArg "WriteUInt32"
    if rawWriteFunctions.WriteUInt64 |> isNull then nullArg "WriteUInt64"
    if rawWriteFunctions.WriteByte |> isNull then nullArg "WriteByte"
    if rawWriteFunctions.WriteBytes |> isNull then nullArg "WriteBytes"
    if rawWriteFunctions.WriteChars |> isNull then nullArg "WriteChars"
    if rawWriteFunctions.WriteEmptyBytes |> isNull then nullArg "WriteEmptyBytes"
    if rawWriteFunctions.GetWriterPosition |> isNull then nullArg "GetWriterPosition"

    { WriteUInt16 = rawWriteFunctions.WriteUInt16.Invoke;
    WriteUInt32 = rawWriteFunctions.WriteUInt32.Invoke;
    WriteUInt64 = rawWriteFunctions.WriteUInt64.Invoke;
    WriteByte = rawWriteFunctions.WriteByte.Invoke;
    WriteBytes = rawWriteFunctions.WriteBytes.Invoke;
    WriteChars = rawWriteFunctions.WriteChars.Invoke;
    WriteEmptyBytes = rawWriteFunctions.WriteEmptyBytes.Invoke;
    GetWriterPosition = rawWriteFunctions.GetWriterPosition.Invoke }

let public Write grTexture writeFunctions (precomputedSlicePitches : int[]) =
    let convertedWriteFunctions = writeFunctions |> Array.map convertWriteFunctions
    let ftexWriteFunctions = convertedWriteFunctions.[0]

    let ftexsFileCount = getFtexsFileCount grTexture.DDSData.Length (int <| grTexture.MipMapCount)

    let mutable dataOffset = grTexture.DDSData.Length// - precomputedSlicePitches.[precomputedSlicePitches.Length - 1]
    let mutable ftexsIndex = 1
    
    let mipmapDescriptions = [|1..precomputedSlicePitches.Length|]
                              |> Array.rev
                              |> Array.map(fun i -> let writeFunctions = convertedWriteFunctions.[ftexsIndex]

                                                    let slicePitch = precomputedSlicePitches.[i - 1]
                                                    let mipmapData = grTexture.DDSData.[dataOffset - slicePitch..dataOffset - 1]
                                                    dataOffset <- dataOffset - slicePitch
                                                    let mipmapDescriptions = writeChunksAndGetMipmapDescription (byte <| (i - 1)) (byte <| ftexsIndex) mipmapData writeFunctions.WriteBytes writeFunctions.WriteUInt16 writeFunctions.WriteUInt32 writeFunctions.GetWriterPosition

                                                    if (precomputedSlicePitches.Length) - i > (precomputedSlicePitches.Length - ftexsFileCount - 1) then ftexsIndex <- ftexsIndex + 1
                                                    //if ftexsIndex > 1 then ftexsIndex <- ftexsIndex - 1

                                                    mipmapDescriptions
                                            )
    writeHeader ftexWriteFunctions.WriteChars ftexWriteFunctions.WriteByte ftexWriteFunctions.WriteBytes ftexWriteFunctions.WriteUInt16 ftexWriteFunctions.WriteUInt32 ftexWriteFunctions.WriteEmptyBytes grTexture (byte <| ftexsFileCount)

    let reversedReversedMipmapDescriptions = mipmapDescriptions |> Array.rev 

    reversedReversedMipmapDescriptions
    |> Array.map(fun mipmapDescription -> writeMipmapDescription ftexWriteFunctions.WriteUInt32 ftexWriteFunctions.WriteUInt16 ftexWriteFunctions.WriteByte mipmapDescription)