module Shen.BuildPackage

open System
open System.IO
open System.Text.RegularExpressions
open ILRepacking
open NuGet
open Kl.Values
open ShenSharp.Shared

let private fromRoot = combine << (@) [".."; ".."; ".."]
let private packageRoot = fromRoot ["Artifacts"; BuildConfig; "Package"]
let private packageFileName = sprintf "%s.%s.nupkg" Product Revision
let private packagePath = fromRoot ["Artifacts"; BuildConfig; packageFileName]
let private tags = ["Shen"] // TODO: pull from github api?

let private repackAssemblies kind target sources =
    let options =
        new RepackOptions(
            ["/target:library"
             "/targetplatform:v4"
             "/allowdup:ShenSharp.Shared"
             "/allowdup:ShenSharp.Metadata"
             "/out:" + target]
            @ sources)
    let repacker = new ILRepack(options)
    repacker.Repack()

let private regex pattern s = Regex.Match(s, pattern, RegexOptions.Multiline).Value

let private urlFromLicenseFile() =
    new Uri(regex @"http\S*" (File.ReadAllText(fromRoot ["LICENSE.txt"])))

let private urlFromGitConfig() =
    let configText = File.ReadAllText(fromRoot [".git"; "config"])
    let origin = regex @"[remote ""origin""].*.git" configText
    let urlString = regex @"http\S*" origin
    new Uri(urlString.Substring(0, urlString.Length - 4))

let private iconUrlFromReadme() =
    let readmeText = File.ReadAllText(fromRoot ["README.md"])
    let iconMarkup = regex "\!\[.*Logo\]\(.*\)" readmeText
    new Uri((regex "http.*\)" iconMarkup).TrimEnd(')'))

let private addFile (builder: PackageBuilder) source target =
    let file = new PhysicalPackageFile()
    file.SourcePath <- source
    file.TargetPath <- target
    builder.Files.Add file

let rec private addFiles (builder: PackageBuilder) (root: string) folder =
    for filePath in Directory.GetFiles folder do
        let target = combine [folder.Substring(root.Length + 1); Path.GetFileName filePath]
        addFile builder filePath target
    for folderPath in Directory.GetDirectories folder do
        addFiles builder root folderPath

let private packageNuget() =
    let builder = new PackageBuilder()
    builder.Id <- Product
    builder.Title <- Product
    builder.Description <- Description
    //builder.Summary <- Description // TODO: pull from README.md
    builder.ProjectUrl <- urlFromGitConfig()
    List.iter (builder.Tags.Add >> ignore) tags
    builder.Authors.Add Author |> ignore
    builder.Owners.Add Author |> ignore
    builder.Copyright <- Copyright
    builder.Version <- SemanticVersion.Parse Revision
    builder.LicenseUrl <- urlFromLicenseFile()
    builder.RequireLicenseAcceptance <- false
    //builder.ReleaseNotes // TODO: pull from CHANGELOG.md
    builder.IconUrl <- iconUrlFromReadme()
    addFiles builder packageRoot packageRoot
    let core = new PackageDependency("FSharp.Core", new VersionSpec(new SemanticVersion(4, 0, 0, 1)))
    let deps = new PackageDependencySet(Runtime.Versioning.FrameworkName(".NETFramework,Version=v4.5"), [core])
    builder.DependencySets.Add deps
    use stream = File.Open(packagePath, FileMode.OpenOrCreate)
    builder.Save stream

[<EntryPoint>]
let main _ =
    repackAssemblies ILRepack.Kind.Dll
        (fromRoot ["Artifacts"; BuildConfig; "Package"; "lib"; "net45"; "Shen.dll"])
        [fromRoot ["Artifacts"; BuildConfig; "Shen.Runtime.dll"]
         fromRoot ["Kl"; "bin"; BuildConfig; "Kl.dll"]]
    repackAssemblies ILRepack.Kind.Exe
        (fromRoot ["Artifacts"; BuildConfig; "Package"; "tools"; "Shen.exe"])
        [fromRoot ["Shen.Repl"; "bin"; BuildConfig; "Shen.Repl.exe"]
         fromRoot ["Artifacts"; BuildConfig; "Shen.Runtime.dll"]
         fromRoot ["Kl"; "bin"; BuildConfig; "Kl.dll"]]
    packageNuget()
    0
