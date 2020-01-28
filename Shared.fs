module ShenSharp.Shared

open System.IO

// TODO: fetch these from Directory.Build.props?
//       remove unused ones

[<Literal>]
let Product = "ShenSharp"

[<Literal>]
let Description = "Shen for the Common Language Runtime"

[<Literal>]
let Author = "Robert Koeninger"

[<Literal>]
let Copyright = "Copyright © 2015-2020 " + Author

[<Literal>]
let Revision = "0.10.0.0"

[<Literal>]
let KernelRevision = "22.2"

[<Literal>]
let KernelFolderName = "ShenOSKernel-" + KernelRevision

#if DEBUG
[<Literal>]
let BuildConfig = "Debug"
#else
[<Literal>]
let BuildConfig = "Release"
#endif

let generatedModule = "Shen.Language"

/// <summary>
/// Combines file path fragments in platform specific way.
/// </summary>
let rec combine = function
    | [] -> "."
    | [x] -> x
    | x :: xs -> Path.Combine(x, combine xs)
