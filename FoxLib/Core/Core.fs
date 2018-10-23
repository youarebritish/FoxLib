module FoxLib.Core

open System.Collections
open System.Collections.Generic
open System
    
/// <summary>
/// A 32-bit hashed string. Used in Lua, langIds, and various binary file formats to encode strings.
/// </summary>
type public StrCode32Hash = uint32

/// <summary>
/// A 64-bit hashed string.
/// </summary>
type public StrCodeHash = uint64

/// <summary>
/// A point or vector in 3D space.
/// </summary>
/// <remarks>
/// In Fox Engine, X is right, Y is up, and Z is forward.
/// </remarks>
type public Vector3 = {
    X : float32
    Y : float32
    Z : float32
}

/// <summary>
/// A point or vector in 4D space.
/// </summary>
/// <remarks>
/// In Fox Engine, when representing a position, X is right, Y is up, Z is forward, and W is typically ignored.
/// </remarks>
type public Vector4 =  {
    X : float32
    Y : float32
    Z : float32
    W : float32
}

/// <summary>
/// I'll be honest, I have no idea what this is. The only known use is seemingly to represent a scale factor in certain .lba files.
/// </summary>
type public WideVector3 = {
    X : float32
    Y : float32
    Z : float32
    A : uint16
    B : uint16 
}

/// <summary>
/// Represents a rotation using complex numbers.
/// </summary>
type public Quaternion = {
    X : float32
    Y : float32
    Z : float32
    W : float32
}

type public Matrix3 = private { values : float32[] } with
    member public this.Col0 = this.values.[0..2]
    member public this.Col1 = this.values.[3..5]
    member public this.Col2 = this.values.[6..8]
    /// <summary>
    /// Create a Matrix3.
    /// </summary>
    static member public Create col0row0 col0row1 col0row2 col1row0 col1row1 col1row2 col2row0 col2row1 col2row2 =
        { values =
            [|col0row0; col0row1; col0row2;
            col1row0; col1row1; col1row2;
            col2row0; col2row1; col2row2|] }

type public Matrix4 = private { values : float32[] } with
    member public this.Col0 = this.values.[0..3]
    member public this.Col1 = this.values.[4..7]
    member public this.Col2 = this.values.[8..11]
    member public this.Col3 = this.values.[12..15]
    /// <summary>
    /// Create a Matrix4.
    /// </summary>
    static member public Create col0row0 col0row1 col0row2 col0row3 col1row0 col1row1 col1row2 col1row3 col2row0 col2row1 col2row2 col2row3 col3row0 col3row1 col3row2 col3row3 =
        { values =
            [|col0row0; col0row1; col0row2; col0row3;
            col1row0; col1row1; col1row2; col1row3;
            col2row0; col2row1; col2row2; col2row3;
            col3row0; col3row1; col3row2; col3row3;|] }

type public EntityLink = {
    PackagePath : string
    ArchivePath : string
    NameInArchive : string
    EntityHandle : uint64
}

/// <summary>
/// An RGB color.
/// </summary>
type public ColorRGB =  {
    Red : float32
    Green : float32
    Blue : float32
}

/// <summary>
/// An RGBA color.
/// </summary>
type public ColorRGBA =  {
    Red : float32
    Green : float32
    Blue : float32
    Alpha : float32
}

/// <summary>
/// An RGB color.
/// </summary>
type public HalfColorRGB =  {
    Red : Half
    Green : Half
    Blue : Half
}

/// <summary>
/// An RGBA color.
/// </summary>
type public HalfColorRGBA =  {
    Red : Half
    Green : Half
    Blue : Half
    Alpha : Half
}

type public IContainer =
    inherit IEnumerable
    abstract member ArraySize : uint16 with get

type public PropertyInfoType =
    | Int8 = 0uy
    | UInt8 = 1uy
    | Int16 = 2uy
    | UInt16 = 3uy
    | Int32 = 4uy
    | UInt32 = 5uy
    | Int64 = 6uy
    | UInt64 = 7uy
    | Float = 8uy
    | Double = 9uy
    | Bool = 10uy
    | String = 11uy
    | Path = 12uy
    | EntityPtr = 13uy
    | Vector3 = 14uy
    | Vector4 = 15uy
    | Quat = 16uy
    | Matrix3 = 17uy
    | Matrix4 = 18uy
    | Color = 19uy
    | FilePtr = 20uy
    | EntityHandle = 21uy
    | EntityLink = 22uy
    | PropertyInfo = 23uy
    | WideVector3 = 24uy    

type public ContainerType =
    | StaticArray = 0uy
    | DynamicArray = 1uy
    | StringMap = 2uy
    | List = 3uy

type public PropertyInfo = {
    Name : string
    Type : PropertyInfoType
    ContainerType : ContainerType
    Container : IContainer
}

type public Entity = {
    ClassName : string
    Address : uint32
    Id : uint32
    ClassId : int16
    Version : uint16
    StaticProperties : PropertyInfo[]
    DynamicProperties : PropertyInfo[]
}

type public Container<'T> =
    | StaticArray of 'T[]
    | DynamicArray of 'T[]
    | List of 'T[]
    | StringMap of IDictionary<string, 'T>
    interface IContainer with
        member this.GetEnumerator() = 
            match this with
            | StaticArray staticArray -> staticArray.GetEnumerator()
            | DynamicArray dynamicArray -> dynamicArray.GetEnumerator()
            | List list -> list.GetEnumerator()
            | StringMap stringMap -> stringMap.GetEnumerator() :> IEnumerator
        member this.ArraySize with get() =
            match this with
            | StaticArray staticArray -> uint16 staticArray.Length
            | DynamicArray dynamicArray -> uint16 dynamicArray.Length
            | List list -> uint16 list.Length
            | StringMap stringMap -> uint16 stringMap.Count