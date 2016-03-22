namespace Testing

open Kl

module Test =
    let d = new DateTime()
    let e = true && true
    let rec f (y:int) = if y >= 0 then y else f (y + 1)
    and g x y = if y = 1 then x else x + (y - 1 |> g x)
    let h = 123
