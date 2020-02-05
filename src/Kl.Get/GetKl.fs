module Kl.Get.GetKl

open System
open System.IO
open System.IO.Compression
open System.Net
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
    use client = new WebClient()
    client.DownloadFile(url, zipPath)
    printfn "Extracting sources package..."
    safeDelete kernelFolder
    ZipFile.ExtractToDirectory(zipPath, root)
    Directory.Move(extractedFolder, kernelFolder)
    File.Delete zipPath
    0
