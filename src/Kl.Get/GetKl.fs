module Kl.Get.GetKl

open System
open System.IO
open System.IO.Compression
open System.Net.Http
open ShenSharp.Shared

let url = sprintf "https://github.com/Shen-Language/shen-sources/releases/download/shen-%s/%s.zip" KernelRevision KernelFolderName
let root = fromRoot []
let extractedFolder = fromRoot [KernelFolderName]
let kernelFolder = fromRoot ["kernel"]
let zipPath = fromRoot [Path.GetFileName(Uri(url).LocalPath)]

let safeDelete x = if Directory.Exists x then Directory.Delete(x, true)

[<EntryPoint>]
let main _ =
    printfn "Shen sources url: \"%s\"" url
    printfn "Local path: \"%s\"" zipPath
    printfn "Extract to: \"%s\"" root
    printfn "Extracted folder: \"%s\"" extractedFolder
    printfn "Kernel folder: \"%s\"" kernelFolder
    printfn "Downloading sources package..."
    async {
        use client = new HttpClient()
        use zip = new FileStream(zipPath, FileMode.Create)
        let! req = client.GetStreamAsync url |> Async.AwaitTask
        do! req.CopyToAsync zip |> Async.AwaitTask
    } |> Async.RunSynchronously
    printfn "Extracting sources package..."
    safeDelete kernelFolder
    ZipFile.ExtractToDirectory(zipPath, root)
    Directory.Move(extractedFolder, kernelFolder)
    File.Delete zipPath
    0
