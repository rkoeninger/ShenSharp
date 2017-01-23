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

    [<CustomEquality; CustomComparison>]
    type private RefWrapper<'a when 'a: equality>(value: 'a) =
        member this.Value with get() = value
        override this.Equals x = obj.ReferenceEquals(value, x)
        override this.GetHashCode() = value.GetHashCode()
        interface IComparable with
            member this.CompareTo x = this.GetHashCode().CompareTo(x.GetHashCode())

    let private genId() = Guid.NewGuid()
    let private formatId id = id.ToString().Substring(0, 8)

    type private RefId = Guid

    type private LiftedValue =
        | LiftedEmpty
        | LiftedNum of decimal
        | LiftedStr of string
        | LiftedSym of string
        | LiftedCons of LiftedValue * LiftedValue
        | LiftedVec of RefId
        | LiftedFreeze of Locals * LiftedValue
        | LiftedLambda of string * Locals * LiftedValue

    type private ExtractedValue =
        | ExtractedVec of LiftedValue array

    type private Refs = (RefId * ExtractedValue) list

    // TODO: Vectors are not checked for equality (same vector referred to in multiple places)

    let rec private lift = function
        | Empty -> [], LiftedEmpty
        | Num n -> [], LiftedNum n
        | Str s -> [], LiftedStr s
        | Sym s -> [], LiftedSym s
        | Cons(x, y) ->
            let (xRefs, xl) = lift x
            let (yRefs, yl) = lift y
            List.append xRefs yRefs, LiftedCons(xl, yl)
        | Vec array ->
            let id = genId()
            let liftedArray = Array.map lift array
            let childIds = List.concat (Seq.map fst liftedArray)
            let extractedArray = Array.map snd liftedArray
            List.append childIds [id, ExtractedVec extractedArray], LiftedVec id
        | Func(Freeze(locals, body)) ->
            let (refs, liftedBody) = lift body
            refs, LiftedFreeze(locals, liftedBody)
        | Func(Lambda(param, locals, body)) ->
            let (refs, liftedBody) = lift body
            refs, LiftedLambda(param, locals, liftedBody)
        | x -> failwithf "%O can't be lifted" x

    let rec private encode = function
        | Empty -> "Empty"
        | Num n -> sprintf "Num %sm" ((Num n).ToString())
        | Str s -> sprintf "Str \"%s\"" (s.Replace("\r", "\\r").Replace("\n", "\\n"))
        | Sym s -> sprintf "Sym \"%s\"" s
        | Cons(x, y) -> sprintf "Cons(%s, %s)" (encode x) (encode y)
        | Func(Freeze(locals, body)) ->
            sprintf "Freeze(%s, %s)" (encodeLocals locals) (encode body)
        | Func(Lambda(param, locals, body)) ->
            sprintf "Lambda(\"%s\", %s, %s)" param (encodeLocals locals) (encode body)
        | x -> failwithf "%O can't be encoded" x

        // extract vectors and init them before building entire expression

    and private encodeVector line name array =
        let tempId = Guid.NewGuid().ToString().Substring(0, 8)
        line <| sprintf "let array%s = Array.create %i (Sym \"shen.fail!\")" tempId (Array.length array)
        for (index, value) in Seq.zip (seq {0 .. 20000}) array do
            match value with
            | Sym "shen.fail!" -> ()
            | _ -> line <| sprintf "array%s.[%i] <- %s" tempId index (encode value)
        line <| sprintf "globals.Symbols.[\"%s\"] <- Vec array%s" name tempId

    and private encodeLocals locals =
        sprintf "Map [%s] " (String.Join("; ", Map.map (fun k v -> sprintf "%s, %s" k (encode v)) locals))

    let encodeSymbol line name value =
        match value with
        | Vec array -> encodeVector line name array
        | _ -> line <| sprintf "globals.Symbols.[\"%s\"] <- %s" name (encode value)

    let encodeFunction line name = function
        | Defun(name, paramz, body) ->
            let paramzString = String.Join("; ", paramz |> Seq.map (sprintf "\"%s\""))
            line <| sprintf "globals.Functions.[\"%s\"] <- Defun(\"%s\", [%s], %s)" name name paramzString (encode body)
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
//            printfn "Generating installation code..."
//            let buffer = new StringBuilder()
//            let line s = buffer.AppendLine s |> ignore
//            line "namespace Kl.Installation"
//            line "open Kl"
//            line "open Kl.Values"
//            line "type Installer() ="
//            line "    member this.Install(globals: Globals) ="
//            let origGlobals = baseGlobals()
//            for kv in globals.Symbols do
//                if not(Seq.contains kv.Key origGlobals.Symbols.Keys) then
//                    encodeSymbol (fun s -> line <| "        " + s) kv.Key kv.Value
//            for kv in globals.Functions do
//                if not(Seq.contains kv.Key origGlobals.Functions.Keys) then
//                    if not(List.contains kv.Key ["exit"]) then
//                        encodeFunction (fun s -> line <| "        " + s) kv.Key kv.Value
//            line "        ()"
//            File.WriteAllText("Installer.fs", buffer.ToString())
//            printfn "Compiling installation code..."
//            let service = new SimpleSourceCodeServices()
//            let options = [|
//                "fsc.exe"
//                "-o"; "Installer.dll"
//                "-a"; "Installer.fs"
//                "-r"; "Kl.dll"
//            |]
//            let (errors, returnCode) = service.Compile options
//            if returnCode <> 0 then
//                raise <| new Exception(String.Join(", ", Seq.map (fun (e: FSharpErrorInfo) -> e.Message) errors))
//            printfn "Installation code cached."
//            printfn ""
            globals
        else
            printfn "Loading cached assembly..."
            printfn ""
            let assembly = Assembly.LoadFile("Installer.dll")
            let installation = assembly.GetType("Kl.Installation.Installer")
            let install = installation.GetMethod("Install", BindingFlags.Static)
            let globals = baseGlobals()
            install.Invoke(null, [|globals|]) :?> Globals
