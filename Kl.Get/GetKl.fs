module Kl.Get.GetKl

open System
open System.IO
open System.IO.Compression
open System.Net
open ShenSharp.Shared

let url = sprintf "https://github.com/Shen-Language/shen-sources/releases/download/shen-%s/%s.zip" KernelRevision KernelFolderName
let packages = fromRoot ["packages"]
let extractedFolderPath = combine [packages; KernelFolderName]
let zipPath = combine [packages; Path.GetFileName(Uri(url).LocalPath)]

let safeDelete x = if Directory.Exists x then Directory.Delete(x, true)

[<EntryPoint>]
let main _ =
    printfn "Shen sources url: \"%s\"" url
    printfn "Local path: \"%s\"" zipPath
    printfn "Downloading sources package..."
    Directory.CreateDirectory packages |> ignore
    use client = new WebClient()
    client.DownloadFile(url, zipPath)
    printfn "Extracting sources package..."
    safeDelete extractedFolderPath
    ZipFile.ExtractToDirectory(zipPath, packages)
    File.Delete zipPath
    0
