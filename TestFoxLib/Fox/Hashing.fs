module Fox.Hashing

open NUnit.Framework
open FoxLib
open FoxLib.Fox

[<Test>]
[<Category("Fox.Hashing.StrCode")>]
let ``StrCode of `hello` should be 14366034504703`` () =
    let hash = StrCode "hello"
    Assert.AreEqual(14366034504703UL, hash)

[<Test>]
[<Category("Fox.Hashing.StrCode")>]
let ``StrCode of `hello world` should be 55535342535240`` () =
    let hash = StrCode "hello world"
    Assert.AreEqual(55535342535240UL, hash)

[<Test>]
[<Category("Fox.Hashing.StrCode")>]
let ``StrCode of `123456789` should be 267500960428468`` () =
    let hash = StrCode "123456789"
    Assert.AreEqual(267500960428468UL, hash)

[<Test>]
[<Category("Fox.Hashing.StrCode")>]
let ``StrCode of empty string should be 203000540209048`` () =
    let hash = StrCode ""
    Assert.AreEqual(203000540209048UL, hash)

[<Test>]
[<Category("Fox.Hashing.StrCode32")>]
let ``StrCode32 of `hello` should be 3663866879`` () =
    let hash = StrCode32 "hello"
    Assert.AreEqual(3663866879u, hash)

[<Test>]
[<Category("Fox.Hashing.StrCode32")>]
let ``StrCode32 of `hello world` should be 1415397960`` () =
    let hash = StrCode32 "hello world"
    Assert.AreEqual(1415397960u, hash)

[<Test>]
[<Category("Fox.Hashing.StrCode32")>]
let ``StrCode32 of `123456789` should be 1807298996`` () =
    let hash = StrCode32 "123456789"
    Assert.AreEqual(1807298996u, hash)

[<Test>]
[<Category("Fox.Hashing.StrCode32")>]
let ``StrCode32 of empty string should be 3205930904`` () =
    let hash = StrCode32 ""
    Assert.AreEqual(3205930904u, hash)