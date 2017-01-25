namespace Kl.Load

open System
open System.IO
open System.Reflection
open System.Text
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SimpleSourceCodeServices
open Kl
open Kl.Evaluator
open Kl.Startup
open Kl.Load.Reader

module Compiler =

    let private genId() = "array" + Guid.NewGuid().ToString().Substring(0, 8)

    let rec private findArrays = function
        | Cons(x, y) -> List.append (findArrays x) (findArrays y)
        | Vec array -> List.append (List.collect findArrays (Array.toList array)) [array]
        | _ -> []

    let private buildRefs arrays = List.map (fun a -> (a, genId())) arrays

    let rec private encode arrayRefs = function
        | Empty -> "Empty"
        | Num n -> sprintf "Num %sm" ((Num n).ToString())
        | Str s -> sprintf "Str \"%s\"" (s.Replace("\r", "\\r").Replace("\n", "\\n"))
        | Sym s -> sprintf "Sym \"%s\"" s
        | Cons(x, y) -> sprintf "Cons(%s, %s)" (encode arrayRefs x) (encode arrayRefs y)
        | Vec array -> sprintf "Vec %s" (snd (List.find (fun (a, _) -> obj.ReferenceEquals(a, array)) arrayRefs))
        | Func(Freeze(locals, body)) ->
            sprintf "Func(Freeze(%s, %s))" (encodeLocals arrayRefs locals) (encode arrayRefs body)
        | Func(Lambda(param, locals, body)) ->
            sprintf "Func(Lambda(\"%s\", %s, %s))" param (encodeLocals arrayRefs locals) (encode arrayRefs body)
        | x -> failwithf "%O can't be encoded" x

    and private encodeLocals arrayRefs locals =
        sprintf "Map [%s]" (String.Join("; ", Map.map (fun k v -> sprintf "%s, %s" k (encode arrayRefs v)) locals))

    let encodeSymbol line arrayRefs name value =
        line <| sprintf "globals.Symbols.[\"%s\"] <- %s" name (encode arrayRefs value)

    let encodeFunction line arrayRefs name = function
        | Defun(name, paramz, body) ->
            let paramzString = String.Join("; ", Seq.map (sprintf "\"%s\"") paramz)
            line <| sprintf "globals.Functions.[\"%s\"] <- Defun(\"%s\", [%s], %s)" name name paramzString (encode arrayRefs body)
        | _ -> ()

    let load klFolder klFiles =
        let globals = baseGlobals()
        for file in (List.map (fun f -> Path.Combine(klFolder, f)) klFiles) do
            printf "Loading %s " file
            stdout.Flush()
            let text = File.ReadAllText(file)
            for ast in readAll text do
                printf "."
                eval globals ast |> ignore
            printfn ""
        printfn ""
        globals

    let installerSyntax globals =
        let buffer = new StringBuilder()
        let line s = buffer.AppendLine s |> ignore
        line "namespace Kl.Installation"
        line "open Kl"
        line "open Kl.Values"
        line "type Installer() ="
        line "    static member Install(globals: Globals) ="
        let origGlobals = baseGlobals()
        for kv in globals.Symbols do
            if not(Seq.contains kv.Key origGlobals.Symbols.Keys) then
                let arrayRefs = buildRefs (findArrays kv.Value)
                for (array, arrayName) in arrayRefs do
                    line <| sprintf "        let %s = Array.create %i (Sym \"shen.fail!\")" arrayName (Array.length array)
                    for (index, item) in Seq.zip (seq {0 .. 20001}) array do
                        if item <> Sym "shen.fail!" then
                            line <| sprintf "        %s.[%i] <- %s" arrayName index (encode arrayRefs item)
                encodeSymbol (line << sprintf "        %s") arrayRefs kv.Key kv.Value
        for kv in globals.Functions do
            if not(Seq.contains kv.Key origGlobals.Functions.Keys || List.contains kv.Key ["exit"]) then
                encodeFunction (line << sprintf "        %s") [] kv.Key kv.Value
        line "        ()"
        string buffer

    let emitDll name =
        let service = new SimpleSourceCodeServices()
        let options = [|
            "fsc.exe"
            "-o"; name + ".dll"
            "-a"; name + ".fs"
            "-r"; "Kl.dll"
        |]
        let (errors, returnCode) = service.Compile options
        if returnCode <> 0 then
            raise <| new Exception(String.Join(", ", Seq.map (fun (e: FSharpErrorInfo) -> e.Message) errors))

    let compile klFolder klFiles =
        if not(File.Exists "Installer.dll") then
            let globals = load klFolder klFiles
            printfn "Generating installation code..."
            let syntax = installerSyntax globals
            File.WriteAllText("Installer.fs", syntax)
            printfn "Compiling installation code..."
            emitDll "Installer"
            printfn "Installation code cached."
            printfn ""
            globals
        else
            printfn "Loading cached runtime..."
            printfn ""
            let assembly = Assembly.LoadFile(Path.Combine(Environment.CurrentDirectory, "Installer.dll"))
            let installation = assembly.GetType("Kl.Installation.Installer")
            let install = installation.GetMethod("Install")
            let globals = baseGlobals()
            install.Invoke(null, [|globals|]) |> ignore
            globals
