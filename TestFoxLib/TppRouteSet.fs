module TppRouteSet

open FoxLib.Tpp
open FoxLib.Tpp.RouteSet
open System.IO
open NUnit.Framework

[<Test>]
[<Category("TppRouteSet")>]
let ``empty TppRouteSet should be empty when read`` () =
    let routeSet = { Routes = [] }
    use stream = new MemoryStream()
    use writer = new BinaryWriter(stream)
    RouteSet.Write routeSet

    stream.Position <- 0L

    use reader = new BinaryReader(stream)
    let readRouteSet = RouteSet.Read

    Seq.isEmpty routeSet.Routes |> Assert.IsTrue