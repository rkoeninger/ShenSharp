module ShenSharp.Shared

open System.Reflection
open System.Runtime.InteropServices

[<Literal>]
let Author = "Robert Koeninger"

[<Literal>]
let Version = "0.7.0.0"

[<assembly: AssemblyProduct "ShenSharp">]
[<assembly: AssemblyDescription "Shen for the Common Language Runtime">]
[<assembly: AssemblyCompany(Author)>]
[<assembly: AssemblyCopyright("Copyright © 2015-2017 " + Author)>]
[<assembly: AssemblyVersion(Version)>]
[<assembly: AssemblyFileVersion(Version)>]
[<assembly: AssemblyInformationalVersion(Version)>]

#if DEBUG
[<assembly: AssemblyConfiguration "Debug">]
#else
[<assembly: AssemblyConfiguration "Release">]
#endif

[<assembly: ComVisible false>]

do ()
