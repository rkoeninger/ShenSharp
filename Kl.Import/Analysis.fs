namespace Kl.Import

open System
open System.Collections.Generic
open Kl
open Kl.Values

module Analysis =

    let private genId() = "array" + Guid.NewGuid().ToString().Substring(0, 8)

    let buildRefs arrays = List.map (fun a -> (a, genId())) arrays

    let rec findArrays = function
        | Cons(x, y) -> findArrays x @ findArrays y
        | Vec array -> List.collect findArrays (Array.toList array) @ [array]
        | _ -> []

    let rec flattenDo = function
        | DoExpr(first, second) -> flattenDo first @ flattenDo second
        | klExpr -> [klExpr]

    let rec functionArity = function
        | Defun(_, arity, _) -> arity
        | Lambda _ -> 1
        | Freeze _ -> 0
        | Partial(f, _) -> functionArity f

    let nonPrimitiveSymbols globals =
        globals.Symbols
        |> Seq.map (fun (kv: KeyValuePair<_, _>) -> (kv.Key, kv.Value))
        |> Seq.filter (fst >> globals.PrimitiveSymbols.Contains >> not)
        |> Seq.toList

    let nonPrimitiveFunctions globals =
        globals.Functions
        |> Seq.map (fun (kv: KeyValuePair<_, _>) -> (kv.Key, kv.Value))
        |> Seq.filter (fst >> globals.PrimitiveFunctions.Contains >> not)
        |> Seq.toList
