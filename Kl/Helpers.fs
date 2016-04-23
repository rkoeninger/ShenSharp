namespace Kl

open System
open System.IO
open System.Collections.Generic

type ConsoleIn(stream: Stream) =
    let reader = new StreamReader(stream)
    let mutable currentLine = ""
    let mutable currentPos = 0
    member this.Read() = 
        if currentPos >= currentLine.Length then
            currentLine <- reader.ReadLine()
            if Object.ReferenceEquals(currentLine, null) then
                -1
            else
                currentLine <- currentLine + "\n"
                currentPos <- 0
                let ch = currentLine.[currentPos]
                currentPos <- currentPos + 1
                (int) ch
        else
            let ch = currentLine.[currentPos]
            currentPos <- currentPos + 1
            (int) ch
    member this.Close() = stream.Close()

module Extensions =
    let (|Integral|Fractional|) x =
        if x % 1.0m = 0m
            then Integral
            else Fractional
    let (|Greater|Equal|Lesser|) (x, y) =
        if x > y
            then Greater
        elif x < y
            then Lesser
        else Equal
    type Dictionary<'a, 'b> with
        member this.GetMaybe(key: 'a) =
            match this.TryGetValue(key) with
            | true, x -> Some x
            | false, _ -> None
