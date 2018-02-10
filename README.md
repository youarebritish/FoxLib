# FoxLib
Library for reading and writing Fox Engine file formats. Currently supported formats:

| Extension | Name                          | Import         | Export        |
|-----------|-------------------------------|----------------|---------------|
| lba       | GimmickLocatorSet             | Yes            | Yes           |
| frt       | TppRouteSet                   | Yes            | Yes           |
| frld      | RailUniqueIdFile              | Coming in 1.2  | Coming in 1.2 |
| pcsp      | PrecomputedSkyParameters      | Coming in 1.3  | Coming in 1.3 |

For examples of usage, see [FoxLibExamples](https://github.com/youarebritish/FoxLibExamples).

## Building
By default, FoxLib has a dependency on FSharp.Core.dll, which you can grab by installing the F# tools in Visual Studio or by installing it from NuGet. If you would prefer not to have to reference FSharp.Core, you can compile FoxLib as a standalone library. To do this, compile it with the flag --standalone. Official releases will be built this way.
