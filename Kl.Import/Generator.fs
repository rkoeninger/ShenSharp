namespace Kl.Import

open System
open System.Text
open Kl
open Analysis

module Generator =

    let rec private encode arrayRefs = function
        | Empty -> "Empty"
        | Num n -> sprintf "Num %sm" ((Num n).ToString())
        | Str s -> sprintf "Str \"%s\"" (s.Replace("\r", "\\r").Replace("\n", "\\n"))
        | Sym s -> sprintf "Sym \"%s\"" s
        | Cons(x, y) -> sprintf "Cons(%s, %s)" (encode arrayRefs x) (encode arrayRefs y)
        | Vec array -> sprintf "Vec %s" (snd (List.find (fun (a, _) -> obj.ReferenceEquals(a, array)) arrayRefs))
        | Func(Freeze(InterpretedFreeze(locals, body))) ->
            sprintf "Func(Freeze(InterpretedFreeze(%s, %s)))" (encodeLocals arrayRefs locals) (encode arrayRefs body)
        | Func(Lambda(InterpretedLambda(locals, param, body))) ->
            sprintf "Func(Lambda(InterpretedLambda(%s, \"%s\", %s)))" (encodeLocals arrayRefs locals) param (encode arrayRefs body)
        | x -> failwithf "%O can't be encoded" x

    and private encodeLocals arrayRefs locals =
        sprintf "Map [%s]" (String.Join("; ", Map.map (fun k v -> sprintf "%s, %s" k (encode arrayRefs v)) locals))

    let private encodeSymbol line arrayRefs name value =
        line <| sprintf "globals.Symbols.[\"%s\"] <- %s" name (encode arrayRefs value)

    let private encodeFunction line arrayRefs name = function
        | Defun(name, arity, InterpretedDefun(paramz, body)) ->
            let paramzString = String.Join("; ", Seq.map (sprintf "\"%s\"") paramz)
            line <| sprintf "globals.Functions.[\"%s\"] <- Defun(\"%s\", %i, InterpretedDefun([%s], %s))" name name (List.length paramz) paramzString (encode arrayRefs body)
        | _ -> ()

    let generateInstallerCode excludeSymbols excludeFunctions globals =
        let buffer = new StringBuilder()
        let mutable indent = ""
        let line s = (buffer.Append(indent)).AppendLine s |> ignore
        line "namespace Kl.Installation"
        line "open Kl"
        line "open Kl.Values"
        line "type Installer() ="
        indent <- "    "
        line "static member Install(globals: Globals) ="
        indent <- "        "
        for kv in globals.Symbols do
            if not(Seq.contains kv.Key excludeSymbols) then
                let arrayRefs = buildRefs (findArrays kv.Value)
                for (array, arrayName) in arrayRefs do
                    line <| sprintf "let %s = Array.create %i (Sym \"shen.fail!\")" arrayName (Array.length array)
                    for (index, item) in Seq.zip (seq {0 .. 20001}) array do
                        if item <> Sym "shen.fail!" then
                            line <| sprintf "%s.[%i] <- %s" arrayName index (encode arrayRefs item)
                encodeSymbol line arrayRefs kv.Key kv.Value
        for kv in globals.Functions do
            if not(Seq.contains kv.Key excludeFunctions || List.contains kv.Key ["exit"]) then
                encodeFunction line [] kv.Key kv.Value
        line "()"
        string buffer
