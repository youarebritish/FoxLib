module FoxLib.Core

open System.Collections
open System.Collections.Generic
    
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
    PackagePath : StrCodeHash
    ArchivePath : StrCodeHash
    NameInArchive : StrCodeHash
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
/// An RGB color.
/// </summary>
type public ColorRGBA =  {
    Red : float32
    Green : float32
    Blue : float32
    Alpha : float32
}

type public Pixel = {
    X : int
    Y : int
    Color : ColorRGBA
}

type public IContainer = 
    interface
        inherit IEnumerable
    end

type public PropertyInfoType =
    | Int8 = 0
    | UInt8 = 1
    | Int16 = 2
    | UInt16 = 3
    | Int32 = 4
    | UInt32 = 5
    | Int64 = 6
    | UInt64 = 7
    | Float = 8
    | Double = 9
    | Bool = 10
    | String = 11
    | Path = 12
    | EntityPtr = 13
    | Vector3 = 14
    | Vector4 = 15
    | Quat = 16
    | Matrix3 = 17
    | Matrix4 = 18
    | Color = 19
    | FilePtr = 20
    | EntityHandle = 21
    | EntityLink = 22
    | PropertyInfo = 23
    | WideVector3 = 24
    
type public PropertyInfo = {
    Name : StrCodeHash
    Type : PropertyInfoType
    Container : IContainer
}

type public Entity = {
    ClassName : StrCodeHash
    ClassId : uint32
    Version : uint16
    Address : uint32
    StaticProperties : PropertyInfo[]
    DynamicProperties : PropertyInfo[]
}

type public ContainerType =
    | StaticArray = 0
    | DynamicArray = 1
    | StringMap = 2
    | List = 3

type public Container<'T> =
    | StaticArray of 'T[]
    | DynamicArray of 'T[]
    | List of 'T[]
    | StringMap of IDictionary<StrCodeHash, 'T>
    interface IContainer with
        member this.GetEnumerator(): IEnumerator = 
            match this with
            | StaticArray staticArray -> staticArray.GetEnumerator()
            | DynamicArray dynamicArray -> dynamicArray.GetEnumerator()
            | List list -> list.GetEnumerator()
            | StringMap stringMap -> stringMap.GetEnumerator() :> IEnumerator