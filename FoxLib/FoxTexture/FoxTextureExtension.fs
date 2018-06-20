module FoxLib.GrTexture.GrTextureExtension

open FoxLib.Core
open System

type private FtexsFileChunkIndex = {
    //int IndexSize = 8
    //int RelativeOffsetValue = 0x80000000
    WrittenChunkSize : uint16;
    DataOffset : uint64;
    CompressData : bool;
    CompressedChunkSize : uint16;
    ChunkSize : uint16;
    EncodedDataOffset : uint32
}

type private FtexsFileChunk = {
    WrittenChunkSize : uint16;
    ChunkData : byte[];
    FtexsFileChunkIndex : FtexsFileChunkIndex
}

type private FtexsFileMipmap = {
    Chunks : FtexsFileChunk[];
    Alignment : uint32;
    Data : byte[];
    BaseOffset : uint32;
    BlockSize : uint32
}

type public FtexsFile = {
    Mipmaps : FtexsFileMipmap;
    FileNumber : byte;
    Data : byte[]
}

let private readSingleFileChunk fileSize (readBytes : int -> byte[]) =
    let chunkBytes = readBytes fileSize

    { ChunkData = chunkBytes } 

let private readFtexsFile chunkCount offset decompressedFileSize = 


let internal Read (readFunctions : ReadFunctions[]) chunkCount offset decompressedFileSize = 
    //let ftexsFiles = new List<FtexsFile>

    readFtexsFile chunkCount offset decompressedFileSize