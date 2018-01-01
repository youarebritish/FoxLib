module FoxLib.Tpp.RouteSet

open FoxLib.Core

type public RouteNode = {
    Position : Vector3
}

type public Route = {
    Name : StrCode32Hash
    Nodes : array<RouteNode>
}

type public RouteSet = {
    Routes : array<Route>
}