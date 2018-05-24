module FoxLib.TppGameKit

open FoxLib.Core

/// <summary>
/// A cover position used by combat AI agents.
/// </summary>
type public TppCoverPoint = {
    /// Cover position.
    Position : Vector3
    /// Unknown.
    LeftOpen : uint8
    /// Unknown.
    RightOpen : uint8
    /// Unknown.
    UpOpen : uint8
    /// Unknown.
    UnVaultable : uint8
    /// Unknown.
    IsUseVip : uint8
    /// Unknown.
    IsUseSniper : uint8
    /// Unknown.
    IsBreakDisable : uint8
    /// Unknown.
    IsBreakEnable : uint8
}