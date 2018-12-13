module Shen.BuildPackage

open System
open System.IO
open System.IO.Compression
open System.Runtime.Versioning
open System.Text.RegularExpressions
open ILRepacking
open NuGet
open ShenSharp.Shared

let private outDir = combine ["bin"; BuildConfig]
let private artifactsRoot = fromRoot ["Artifacts"; BuildConfig]
let private klDllPath = fromRoot ["Kl"; outDir; "Kl.dll"]
let private shenLanguageDllPath = combine [artifactsRoot; sprintf "%s.dll" generatedModule]
let private shenApiDllPath = fromRoot ["Shen.Api"; outDir; "Shen.Api.dll"]
let private shenReplExePath = fromRoot ["Shen.Repl"; outDir; "Shen.Repl.exe"]
let private packageRoot = combine [artifactsRoot; "Package"]
let private shenDllPath = combine [packageRoot; "lib"; "net45"; "Shen.dll"]
let private shenExePath = combine [packageRoot; "tools"; "Shen.exe"]
let private nupkgFileName = sprintf "%s.%s.nupkg" Product Revision
let private nupkgPath = combine [artifactsRoot; nupkgFileName]
let private zipFileName = sprintf "%s.%s.zip" Product Revision
let private zipPath = combine [artifactsRoot; zipFileName]
let private tags = ["Shen"]

let private repackAssemblies kind target sources =
    let options =
        RepackOptions(
            ["/target:" + kind
             "/targetplatform:v4"
             "/allowdup:ShenSharp.Shared"
             "/allowdup:ShenSharp.Metadata"
             "/out:" + target]
            @ sources)
    let repacker = ILRepack(options)
    repacker.Repack()

let private regex pattern s = Regex.Match(s, pattern, RegexOptions.Multiline).Value

let private urlFromLicenseFile() =
    new Uri(regex @"http\S*" (File.ReadAllText(fromRoot ["LICENSE.txt"])))

let private urlFromGitConfig() =
    let configText = File.ReadAllText(fromRoot [".git"; "config"])
    let origin = regex @"[remote ""origin""].*.git" configText
    let urlString = regex @"http\S*" origin
    new Uri(urlString.Substring(0, urlString.Length - 4))

let private indexOf (s: string) (sub: string) =
    match s.IndexOf sub with
    | -1 -> None
    | x -> Some x

let private indexOfFrom (s: string) (sub: string) (from: int) =
    match s.IndexOf(sub, from) with
    | -1 -> None
    | x -> Some x

let private releaseNotesFromChangeLog() =
    let changeLogText = File.ReadAllText(fromRoot ["CHANGELOG.md"])
    let revisionTag = sprintf "## [%s]" Revision
    match indexOf changeLogText revisionTag with
    | None ->
        let unreleasedTag = "## [Unreleased]"
        match indexOf changeLogText unreleasedTag with
        | None -> null
        | Some unreleasedIndex ->
            match indexOfFrom changeLogText "## [" (unreleasedIndex + 1) with
            | None -> null
            | Some nextIndex ->
                let startIndex = unreleasedIndex + unreleasedTag.Length
                changeLogText.Substring(startIndex, nextIndex - startIndex).Trim()
    | Some revisionIndex ->
        match indexOfFrom changeLogText "## [" (revisionIndex + 1) with
        | None -> null
        | Some nextIndex ->
            let startIndex = changeLogText.IndexOf('\n', revisionIndex)
            changeLogText.Substring(startIndex, nextIndex - startIndex).Trim()

let private iconUrlFromReadme() =
    let readmeText = File.ReadAllText(fromRoot ["README.md"])
    let iconMarkup = regex "\!\[.*Logo\]\(.*\)" readmeText
    new Uri((regex "http.*\)" iconMarkup).TrimEnd(')'))

let private addFile (builder: PackageBuilder) source target =
    let file = PhysicalPackageFile()
    file.SourcePath <- source
    file.TargetPath <- target
    builder.Files.Add file

let rec private addFiles (builder: PackageBuilder) (root: string) folder =
    for filePath in Directory.GetFiles folder do
        let target = combine [folder.Substring(root.Length + 1); Path.GetFileName filePath]
        addFile builder filePath target
    for folderPath in Directory.GetDirectories folder do
        addFiles builder root folderPath

let private buildNupkg() =
    let builder = PackageBuilder()
    builder.Id <- Product
    builder.Title <- Product
    builder.Description <- Description
    builder.Summary <- Description
    builder.ProjectUrl <- urlFromGitConfig()
    List.iter (builder.Tags.Add >> ignore) tags
    builder.Authors.Add Author |> ignore
    builder.Owners.Add Author |> ignore
    builder.Copyright <- Copyright
    builder.Version <- SemanticVersion.Parse Revision
    builder.LicenseUrl <- urlFromLicenseFile()
    builder.RequireLicenseAcceptance <- false
    builder.ReleaseNotes <- releaseNotesFromChangeLog()
    builder.IconUrl <- iconUrlFromReadme()
    addFiles builder packageRoot packageRoot
    let core = PackageDependency("FSharp.Core", VersionSpec(SemanticVersion(4, 0, 0, 1)))
    let deps = PackageDependencySet(FrameworkName(".NETFramework,Version=v4.5"), [core])
    builder.DependencySets.Add deps
    use stream = File.Open(nupkgPath, FileMode.OpenOrCreate)
    builder.Save stream

let private buildZip () =
    if File.Exists zipPath
        then File.Delete zipPath
    ZipFile.CreateFromDirectory(packageRoot, zipPath)

[<EntryPoint>]
let main _ =
    repackAssemblies "library" shenDllPath [
        shenLanguageDllPath
        shenApiDllPath
        klDllPath]
    repackAssemblies "exe" shenExePath [
        shenReplExePath
        shenLanguageDllPath
        shenApiDllPath
        klDllPath]
    printfn "Generating NuGet package..."
    buildNupkg()
    buildZip()
    0
