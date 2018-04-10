module FoxLib.Tpp.RouteSetEvent

open RouteSet
open FoxLib.Core

/// <summary>
/// Base class for route events which take no parameters.
/// </summary>
[<AbstractClass>]
type public ParameterlessRouteEvent() as this =
    abstract member EventType : StrCode32Hash
    interface IRouteEvent with
        member __.EventType = this.EventType
        member __.Params = Seq.empty

type public ``1947843660``() =
    inherit ParameterlessRouteEvent() with
        override __.EventType = 1947843660u

type public ``4125130163``() =
    inherit ParameterlessRouteEvent() with
        override __.EventType = 4125130163u

/// <summary>
/// An event whose type hash matches the empty string.
/// </summary>
type public Unnamed() =
    inherit ParameterlessRouteEvent() with
        override __.EventType = 3205930904u
        
type public All() =
    inherit ParameterlessRouteEvent() with
        override __.EventType = 1425812200u

type public CarryHoneyBee() =
    inherit ParameterlessRouteEvent() with
        override __.EventType = 1947843660u

type public CautionDash() =
    inherit ParameterlessRouteEvent() with
        override __.EventType = 4103551892u

type public CautionRun() =
    inherit ParameterlessRouteEvent() with
        override __.EventType = 1068601125u

type public CautionStandWalkReady() =
    inherit ParameterlessRouteEvent() with
        override __.EventType = 3754167279u

type public JumpOnly() =
    inherit ParameterlessRouteEvent() with
        override __.EventType = 2319021156u

type public JumpWalk() =
    inherit ParameterlessRouteEvent() with
        override __.EventType = 631765835u

type public None() =
    inherit ParameterlessRouteEvent() with
        override __.EventType = 104983832u

type public RelaxedStandWalk() =
    inherit ParameterlessRouteEvent() with
        override __.EventType = 357669894u

type public RouteMove() =
    inherit ParameterlessRouteEvent() with
        override __.EventType = 3186624026u

type public RouteMoveFree() =
    inherit ParameterlessRouteEvent() with
        override __.EventType = 225225504u

type public RunOnly() =
    inherit ParameterlessRouteEvent() with
        override __.EventType = 190940128u

type public RunWalk() =
    inherit ParameterlessRouteEvent() with
        override __.EventType = 4250768714u

type public WalkOnly() =
    inherit ParameterlessRouteEvent() with
        override __.EventType = 2113759707u

type public WalkerGearCautionDash() =
    inherit ParameterlessRouteEvent() with
        override __.EventType = 4057988174u

type public WalkerGearCautionRun() =
    inherit ParameterlessRouteEvent() with
        override __.EventType = 2657224742u

type public WalkerGearRelaxedRun() =
    inherit ParameterlessRouteEvent() with
        override __.EventType = 489012528u