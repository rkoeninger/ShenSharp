module ShenSharp.Shared

open System
open System.IO

[<Literal>]
let Author = "Robert Koeninger"

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

[<Literal>]
let GeneratedModule = "Shen.Kernel"

/// <summary>
/// Combines file path fragments in platform specific way.
/// </summary>
let rec combine = function
    | [] -> "."
    | [x] -> x
    | x :: xs -> Path.Combine(x, combine xs)
    
/// <summary>
/// Path to the source root where the .sln files are.
/// </summary>
let private sourceRoot =
    let mutable current = Environment.CurrentDirectory
    while not <| current.EndsWith "ShenSharp" do
        current <- Path.GetDirectoryName current
    current

/// <summary>
/// Takes a list of path components and appends them to
/// the end of the source root for this solution.
/// </summary>
let fromRoot = combine << (@) [sourceRoot]
