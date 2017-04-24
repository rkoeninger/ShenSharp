module Kl.Get.GetKl

open System
open System.IO
open System.IO.Compression
open System.Net
open ShenSharp.Shared

let url = "https://github.com/Shen-Language/shen-sources/releases/download/shen-20.0/ShenOSKernel-20.0.zip"
let packages = combine [".."; ".."; ".."; "packages"]
let zipPath = combine [packages; Path.GetFileName(Uri(url).LocalPath)]

[<EntryPoint>]
let main _ =
    printfn "Shen sources url: \"%s\"" url
    printfn "Local path: \"%s\"" zipPath
    printfn "Downloading sources package..."
    use client = new WebClient()
    client.DownloadFile(url, zipPath)
    printfn "Extracting sources package..."
    ZipFile.ExtractToDirectory(zipPath, packages)
    0
