module ShenSharp.Shared

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
let Revision = "0.7.0.0"

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
