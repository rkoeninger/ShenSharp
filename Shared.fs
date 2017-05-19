module ShenSharp.Shared

open System.IO
open System.Reflection
open System.Runtime.InteropServices

[<Literal>]
let Product = "ShenSharp"

[<Literal>]
let Description = "Shen for the Common Language Runtime"

[<Literal>]
let Author = "Robert Koeninger"

[<Literal>]
let Copyright = "Copyright © 2015-2017 " + Author

[<Literal>]
let Revision = "0.9.2.0"

[<Literal>]
let KernelRevision = "20.1"

[<Literal>]
let KernelFolderName = "ShenOSKernel-" + KernelRevision

#if DEBUG
[<Literal>]
let BuildConfig = "Debug"
#else
[<Literal>]
let BuildConfig = "Release"
#endif

[<assembly: AssemblyProduct(Product)>]
[<assembly: AssemblyDescription(Description)>]
[<assembly: AssemblyCompany(Author)>]
[<assembly: AssemblyCopyright(Copyright)>]
[<assembly: AssemblyVersion(Revision)>]
[<assembly: AssemblyFileVersion(Revision)>]
[<assembly: AssemblyInformationalVersion(Revision)>]
[<assembly: AssemblyConfiguration(BuildConfig)>]
[<assembly: ComVisible false>]
do ()

let generatedModule = "Shen.Language"

/// <summary>
/// Combines file path fragments in platform specific way.
/// </summary>
let rec combine = function
    | [] -> "."
    | [x] -> x
    | x :: xs -> Path.Combine(x, combine xs)
