module FoxLib.Fx

open FoxLib.Core

type public FxRandomGatherType =
    | Auto = 0
    | RelativeRootOffset = 1
    | AbsoluteValue = 2

type public FxVectorType =
    | Vector = 0
    | Rotates = 1
    | Color = 2

type public FxRenderBlendMode =
    | Alpha = 0
    | Add = 1
    | Sub = 2
    | Mul = 3
    | Min = 4
    | Opaque = 5

type public FxRenderSortMode =
    | None = 0
    | SimpleSort = 1
    | OnePointSort = 2
    | LocalSort = 3

type public FxPlayModeType =
    | OneShot = 0
    | Loop = 1
    | LoopFadeInOut = 2

type public FxUpdateType =
    | Normal = 0
    | DividesFrames = 1
    | DrawTiming = 2

type public FxExecutionPriorityType =
    | Must = 0
    | Normal = 1

type public FxBoundingBoxType =
    | None = 0
    | TimeProgresses = 1
    | Stop = 2

type public FxSimulationMode =
    | SimulationNormal
    | SimulationDecalPerf
    | SimulationMissileMove
    | SimulationCreateAndDestroyPerf
    | SimulationBulletLineMove
    | SimulationRpgWeaponMove
    | SimulationReceiveColorTest

type public FxShapeBoundBoxType =
    | Manual = 0

type public FxRotateOrderTyoe =
    | XyzOreder = 0

type public FxCameraLodType =
    | CameraDistance = 0
    | CameraArea = 1
    | LodPriority = 2

type public FxLodEmitPriorityLevel =
    | Level0 = 0
    | Level1 = 1
    | Level2 = 2
    | Level3 = 3
    | Level4 = 4
    | Level5 = 5
    | Level6 = 6
    | Level7 = 7
    | Level8 = 8
    | LevelMax = 9

type public FxGenerationFilterType =
    | Generation7 = 0
    | Generation8 = 1
    | Generation9 = 2

type public FxVariationGenerationFilterType =
    | None = 0
    | Generation7 = 1
    | Generation8 = 2

type public FxModuleGraph = {
    EffectName : StrCodeHash
    DebugInfo : bool
    AllFrame : uint32
    PlayMode : FxPlayModeType
    FadeInEndFrame : uint32
    FadeOutStartFrame : uint32
    UpdateType : FxUpdateType
    BoundingBoxType : int32
    BoundingBoxOffsetPos : Vector3
    BoundingBoxSize : Vector3
    ExecutionPriorityType : int32
}

type public IFxNode = interface end

type public FxIntervalProbabilityEmitNode = {
    DelayFrame : uint32
    DelayFrameRandomRange : uint32
    LifeFrame : uint32
    LifeRandomRangeFrame : uint32
    IntervalFrame : uint32
    Probability : float32
    NumMin : uint32
    NumMax : uint32
    FadeOutPosition : float32
    FadeOutReverse : bool
    RandomGatherType : FxRandomGatherType
    RandomGatherSeedValue : uint32
    ReceiveName : StrCodeHash
    EmitVersion : int32
} with interface IFxNode