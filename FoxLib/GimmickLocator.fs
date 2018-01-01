module FoxLib.Tpp.GimmickLocator

open FoxLib.Core
    
/// <summary>
/// Used by TppGimmickPowerCutAreaData entities. Presumably indicates a point in space where you can turn off electricity.
/// </summary>
/// <remarks>
/// These locators have no name, so how are they referenced?
/// </remark>
type public PowerCutAreaGimmickLocator = {
    Position : Vector4
    Rotation : Quaternion
}

/// <summary>
/// Used by TppPermanentGimmickData and TppSharedGimmickData entities to indicate their position and rotation.
/// </summary>
/// <remarks>
/// Presumably the LocatorName matches up with the Name field of the owning gimmick data.
/// </remarks>
type public NamedGimmickLocator = {
    Position : Vector4
    Rotation : Quaternion
    LocatorName : StrCode32Hash
    DataSetName : StrCode32Hash
}

/// <summary>
/// Used by TppPermanentGimmickData and TppSharedGimmickData entities to indicate their position, rotation, and scale.
/// </summary>
/// <remarks>
/// Presumably the LocatorName matches up with the Name field of the owning gimmick data.
/// </remarks>
type public ScaledGimmickLocator = {
    Position : Vector4
    Rotation : Quaternion
    /// What are the A and B components of this for?
    Scale : WideVector3
    LocatorName : StrCode32Hash
    DataSetName : StrCode32Hash
}