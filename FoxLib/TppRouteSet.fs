module FoxLib.Tpp.RouteSet

open FoxLib.Core

type public IRouteEvent = interface end

type public RouteEvent<'T1, 'T2, 'T3, 'T4, 'T5, 'T6, 'T7, 'T8, 'T9, 'T10> =
    {   Type : StrCode32Hash
        Param1 : 'T1
        Param2 : 'T2
        Param3 : 'T3
        Param4 : 'T4
        Param5 : 'T5
        Param6 : 'T6
        Param7 : 'T7
        Param8 : 'T8
        Param9 : 'T9
        Param10 : 'T10
        Snippet : string } interface IRouteEvent

type public RouteNode = {
    Position : Vector3
    EdgeEvent : IRouteEvent
    Events : seq<IRouteEvent>
}

type public Route = {
    Name : StrCode32Hash
    Nodes : seq<RouteNode>
}

type public RouteSet = {
    Routes : seq<Route>
}

let public Read =
    let routes = []
    { Routes = routes }

let public Write routeSet =
    ()