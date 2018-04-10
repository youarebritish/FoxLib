module FoxLib.Core
open System

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
    
/// <summary>
/// A hashed string. Used in Lua, langIds, and various binary file formats to encode strings.
/// </summary>
type public StrCode32Hash = uint32