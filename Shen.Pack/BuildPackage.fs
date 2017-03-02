module Shen.BuildPackage

open System
open System.IO
open NuGet
open ShenSharp.Shared

let addFile (builder: PackageBuilder) source target =
    let file = new PhysicalPackageFile()
    file.SourcePath <- source
    file.TargetPath <- target
    builder.Files.Add file

[<EntryPoint>]
let main args =

    // TODO: do ILRepack, like in package.{bat | sh}

    let builder = new PackageBuilder()
    builder.Id <- Product
    builder.Title <- Product
    builder.Description <- Description
    builder.Summary <- Description // TODO: pull from readme.md
    builder.Authors.Add Author |> ignore
    builder.Owners.Add Author |> ignore
    builder.Copyright <- Copyright
    builder.Version <- SemanticVersion.Parse Revision
    // TODO: builder.LicenseUrl // TODO: pull from license.txt
    builder.RequireLicenseAcceptance <- false
    builder.Tags.Add "Shen" |> ignore // TODO: pull from ?
    //builder.ReleaseNotes // TODO: pull from changelog.md
    builder.ProjectUrl <- new Uri("https://github.com/rkoeninger/ShenSharp") // TODO: pull from .git ?
    // TODO: builder.MinClientVersion
    // TODO: use Values.combine and distinguish Debug/Release
    // TODO: select everything under Package/ folder
    addFile builder @"..\..\..\Artifacts\Debug\Package\lib\Shen.dll" @"lib\net40\Shen.dll"
    addFile builder @"..\..\..\Artifacts\Debug\Package\tools\Shen.exe" @"tools\Shen.exe"
    // TODO: move relative paths into ShenSharp.Shared?
    let packageFileName = sprintf "%s.%s.nupkg" Product Revision
    // builder.Save(File.Open(@"..\..\..\Artifacts\Debug\" + packageFileName, FileMode.OpenOrCreate, FileAccess.Write))
    // TODO: ^ raises IO exception
    0
