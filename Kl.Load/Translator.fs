namespace Kl.Load

open System
open Kl

module Translator =

    let rec private encode = function
        | Empty -> "Empty"
        | Num n -> sprintf "Num %sm" ((Num n).ToString())
        | Str s -> sprintf "Str \"%s\"" (s.Replace("\r", "\\r").Replace("\n", "\\n"))
        | Sym s -> sprintf "Sym \"%s\"" s
        | Cons(x, y) -> sprintf "Cons(%s, %s)" (encode x) (encode y)
        | Vec array -> encodeVector array
        | Func(Freeze(locals, body)) ->
            sprintf "Freeze(%s, %s)" (encodeLocals locals) (encode body)
        | Func(Lambda(param, locals, body)) ->
            sprintf "Lambda(\"%s\", %s, %s)" param (encodeLocals locals) (encode body)
        | x -> failwithf "%O can't be encoded" x

    and private encodeVector array =
        let assign i = function
            | Sym "shen.fail!" -> ""
            | x -> sprintf "array.[%i] <- %s; " i (encode x)
        let elems = String.Join("", Seq.mapi assign array)
        sprintf "let array = Array.create %i (Sym \"shen.fail!\") in List.last [%sVec array]" array.Length elems

    and private encodeLocals locals =
        sprintf "Map [%s] " (String.Join("; ", Map.map (fun k v -> sprintf "%s, %s" k (encode v)) locals))

    let encodeSymbol name value =
        sprintf "globals.Symbols.[\"%s\"] <- %s" name (encode value)

    let encodeFunction name = function
        | Defun(name, paramz, body) ->
            let paramzString = String.Join("; ", paramz |> Seq.map (sprintf "\"%s\""))
            sprintf "globals.Functions.[\"%s\"] <- Defun(\"%s\", [%s], %s)" name name paramzString (encode body)
        | _ -> ""

    let private encodeAll f excludes defs =
        String.Join("\r\n",
            defs
            |> Seq.filter (fun (name, _) -> not(List.contains name excludes))
            |> Seq.map (fun (name, value) -> f name value))

    let encodeSymbols exclude = encodeAll encodeSymbol exclude

    let encodeFunctions exclude = encodeAll encodeFunction exclude
