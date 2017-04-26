module Kl.Get.GetKl

open System
open System.IO
open System.IO.Compression
open System.Net
open ShenSharp.Shared

let url = sprintf "https://github.com/Shen-Language/shen-sources/releases/download/shen-%s/%s-%s.zip" KernelRevision KernelFolderName KernelRevision
let packages = combine [".."; ".."; ".."; "packages"]
let extractedFolderPath = combine [packages; sprintf "%s-%s" KernelFolderName KernelRevision]
let kernelFolderPath = combine [packages; KernelFolderName]
let zipPath = combine [packages; Path.GetFileName(Uri(url).LocalPath)]

let safeDelete x = if Directory.Exists x then Directory.Delete(x, true)

[<EntryPoint>]
let main _ =
    printfn "Shen sources url: \"%s\"" url
    printfn "Local path: \"%s\"" zipPath
    printfn "Downloading sources package..."
    use client = new WebClient()
    client.DownloadFile(url, zipPath)
    printfn "Extracting sources package..."
    safeDelete extractedFolderPath
    safeDelete kernelFolderPath
    ZipFile.ExtractToDirectory(zipPath, packages)
    Directory.Move(extractedFolderPath, kernelFolderPath)
    0
