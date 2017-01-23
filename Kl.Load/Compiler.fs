namespace Kl.Load

open System
open System.Diagnostics
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

    let private appendArrays xs ys =
        List.append xs (List.filter (fun y -> not(List.contains y xs)) ys)

    let rec private findArrays = function
        | Cons(x, y) -> appendArrays (findArrays x) (findArrays y)
        | Vec array -> [array]
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
            sprintf "Freeze(%s, %s)" (encodeLocals arrayRefs locals) (encode arrayRefs body)
        | Func(Lambda(param, locals, body)) ->
            sprintf "Lambda(\"%s\", %s, %s)" param (encodeLocals arrayRefs locals) (encode arrayRefs body)
        | x -> failwithf "%O can't be encoded" x

        // extract vectors and init them before building entire expression

    and private encodeVector line arrayRefs name array =
        let tempId = Guid.NewGuid().ToString().Substring(0, 8)
        line <| sprintf "let array%s = Array.create %i (Sym \"shen.fail!\")" tempId (Array.length array)
        for (index, value) in Seq.zip (seq {0 .. 20000}) array do
            match value with
            | Sym "shen.fail!" -> ()
            | _ -> line <| sprintf "array%s.[%i] <- %s" tempId index (encode arrayRefs value)
        line <| sprintf "globals.Symbols.[\"%s\"] <- Vec array%s" name tempId

    and private encodeLocals arrayRefs locals =
        sprintf "Map [%s] " (String.Join("; ", Map.map (fun k v -> sprintf "%s, %s" k (encode arrayRefs v)) locals))

    let encodeSymbol line arrayRefs name value =
        match value with
        // TODO: Just emit reference
        | Vec array -> encodeVector line arrayRefs name array
        | _ -> line <| sprintf "globals.Symbols.[\"%s\"] <- %s" name (encode arrayRefs value)

    let encodeFunction line arrayRefs name = function
        | Defun(name, paramz, body) ->
            let paramzString = String.Join("; ", paramz |> Seq.map (sprintf "\"%s\""))
            line <| sprintf "globals.Functions.[\"%s\"] <- Defun(\"%s\", [%s], %s)" name name paramzString (encode arrayRefs body)
        | _ -> ()

    let load klFolder klFiles =
        let stopwatch = Stopwatch.StartNew()
        let globals = baseGlobals()
        for file in (List.map (fun f -> Path.Combine(klFolder, f)) klFiles) do
            printfn ""
            printfn "Loading %s" file
            printfn ""
            stdout.Flush()
            let text = File.ReadAllText(file)
            for ast in readAll text do
                match ast with
                | Cons(command, Cons(symbol, _)) ->
                    printfn "%O %O" command symbol
                    eval globals ast |> ignore
                | _ -> () // ignore copyright block at top
        printfn ""
        printfn "Loading done"
        printfn "Time: %s" <| stopwatch.Elapsed.ToString()
        printfn ""
        globals

    let compile klFolder klFiles =
        if not(File.Exists "Installer.dll") then
            let globals = load klFolder klFiles
            printfn "Generating installation code..."
            let buffer = new StringBuilder()
            let line s = buffer.AppendLine s |> ignore
            line "namespace Kl.Installation"
            line "open Kl"
            line "open Kl.Values"
            line "type Installer() ="
            line "    member this.Install(globals: Globals) ="
            let origGlobals = baseGlobals()
            for kv in globals.Symbols do
                if not(Seq.contains kv.Key origGlobals.Symbols.Keys) then
                    let arrayRefs = buildRefs (findArrays kv.Value)
                    for (array, arrayName) in arrayRefs do
                        line <| "        let " + arrayName + " = Array.create " + string (Array.length array) + " (Sym \"shen.fail!\")"
                        for (index, item) in Seq.zip (seq {0 .. 20001}) array do
                            if item <> Sym "shen.fail!" then
                                line <| "        " + arrayName + ".[" + string index + "] <- " + encode arrayRefs item
                    encodeSymbol (fun s -> line <| "        " + s) arrayRefs kv.Key kv.Value
            for kv in globals.Functions do
                if not(Seq.contains kv.Key origGlobals.Functions.Keys) then
                    if not(List.contains kv.Key ["exit"]) then
                        encodeFunction (fun s -> line <| "        " + s) [] kv.Key kv.Value
            line "        ()"
            File.WriteAllText("Installer.fs", buffer.ToString())
            printfn "Compiling installation code..."
            let service = new SimpleSourceCodeServices()
            let options = [|
                "fsc.exe"
                "-o"; "Installer.dll"
                "-a"; "Installer.fs"
                "-r"; "Kl.dll"
            |]
            let (errors, returnCode) = service.Compile options
            if returnCode <> 0 then
                raise <| new Exception(String.Join(", ", Seq.map (fun (e: FSharpErrorInfo) -> e.Message) errors))
            printfn "Installation code cached."
            printfn ""
            globals
        else
            printfn "Loading cached assembly..."
            printfn ""
            let assembly = Assembly.LoadFile("Installer.dll")
            let installation = assembly.GetType("Kl.Installation.Installer")
            let install = installation.GetMethod("Install", BindingFlags.Static)
            let globals = baseGlobals()
            install.Invoke(null, [|globals|]) :?> Globals
