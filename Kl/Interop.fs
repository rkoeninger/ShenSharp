module Kl.Interop

open System
open System.Reflection
open ShenSharp.Shared

let private trim (s: string) = s.Trim()
let private before i (s: string) = s.Substring(0, i)
let private after i (s: string) = s.Substring(i + 1, s.Length - 1 - i)
let private between i j (s: string) = s.Substring(i + 1, j - 1 - i)
let private getType x = x.GetType()
let private typeOf s = Type.GetType s

// TODO: merge into global aliases
let private primitiveAliases = [
    "System.Boolean", "bool"
    "System.Int32",   "int"
    "System.Decimal", "decimal"
    "System.String",  "string"
    "System.Object",  "object"
]

type private TypeName =
    | Simple of string
    | Compound of string * TypeName list

let rec private findRightBracket depth index (s: string) =
    match s.[index] with
    | '<' -> findRightBracket (depth + 1) (index + 1) s
    | '>' ->
        if depth = 0
            then index
            else findRightBracket (depth - 1) (index + 1) s
    | _ -> findRightBracket depth (index + 1) s

// TODO: classname aliasing from globals
let rec private parseTypeName (typeName: string) =
    let leftIndex = typeName.IndexOf '<'
    if leftIndex > 0 then
        let rightIndex = typeName.LastIndexOf '>'
        if rightIndex < 0 then
            failwithf "Malformed type name: \"%s\"" typeName
        let genericTypeName = before leftIndex typeName |> trim
        let typeArgsString = between leftIndex rightIndex typeName
        Compound(genericTypeName, parseTypeArgs typeArgsString)
    else
        Simple typeName

and private parseTypeArgs (typeArgsString: string) =
    let bracketIndex = typeArgsString.IndexOf '<'
    if bracketIndex < 0 then
        typeArgsString.Split ',' |> Array.toList |> List.map (trim >> Simple)
    else
        let commaIndex = typeArgsString.IndexOf ','
        if commaIndex < 0 then
            [parseTypeName typeArgsString]
        elif commaIndex < bracketIndex then
            let firstTypeName = before commaIndex typeArgsString |> trim
            let restTypeNames = after commaIndex typeArgsString
            Simple firstTypeName :: parseTypeArgs restTypeNames
        else
            let rightBracketIndex = findRightBracket 1 (bracketIndex + 1) typeArgsString
            List.Cons(
                parseTypeName(before rightBracketIndex typeArgsString),
                parseTypeArgs(after rightBracketIndex typeArgsString))

let rec private renderTypeName = function
    | Simple typeName ->
        match List.tryFind (fst >> (=) typeName) primitiveAliases with
        | Some(_, alias) -> alias
        | None -> typeName
    | Compound(typeName, typeArgs) ->
        let backtickIndex = typeName.IndexOf '`'
        let trimmedName =
            if backtickIndex < 0
                then typeName
                else before backtickIndex typeName
        sprintf "%s<%s>" trimmedName (String.Join(",", List.map renderTypeName typeArgs))

let rec private buildTypeName (t: Type) =
    let typeArgs = t.GetGenericArguments() |> Array.toList
    if typeArgs.Length = 0
        then Simple t.FullName
        else Compound(t.FullName, List.map buildTypeName typeArgs)

let rec private renderMethodSig methodName = function
    | [] -> sprintf "%s()" methodName
    | typeArgs -> sprintf "%s(%s)" methodName (String.Join(",", List.map renderTypeName typeArgs))

let rec private constructType = function
    | Simple name ->
        match List.tryFind (snd >> (=) name) primitiveAliases with
        | Some(proper, _) -> typeOf proper
        | None ->
            let result = typeOf name
            if result = null then
                failwithf "Type \"%s\" is not defined" name
            result
    | Compound(name, args) ->
        let genericType = typeOf(sprintf "%s`%i" name args.Length)
        let typeArgs = List.map constructType args
        genericType.MakeGenericType(List.toArray typeArgs)

let rec findType = parseTypeName >> constructType

let private publicInstance = BindingFlags.Public ||| BindingFlags.Instance

let private publicStatic = BindingFlags.Public ||| BindingFlags.Static

let findInstanceProperty target propertyName =
    let clazz = getType target
    let propertyInfo = clazz.GetProperty(propertyName, publicInstance)
    if propertyInfo = null then
        failwithf "Property \"%s\" is not defined on type \"%s\"" propertyName clazz.FullName
    if propertyInfo.GetIndexParameters().Length > 0 then
        failwithf "Property \"%s\" has index parameters, use \"clr.get-index\" instead" propertyName
    propertyInfo

let findIndexProperty target =
    let clazz = getType target
    let propertyInfo = clazz.GetProperty("Item", publicInstance)
    if propertyInfo = null then
        failwithf "Indexer property is not defined on type \"%s\"" clazz.FullName
    propertyInfo

let findStaticProperty className propertyName =
    let clazz = findType className
    let propertyInfo = clazz.GetProperty(propertyName, publicStatic)
    if propertyInfo = null then
        failwithf "Property \"%s\" is not defined on type \"%s\"" propertyName clazz.FullName
    if propertyInfo.GetIndexParameters().Length > 0 then
        failwithf "Property \"%s\" has index parameters, use \"clr.get-index-static\" instead" propertyName
    propertyInfo

let private findMethod instance (targetType: Type) methodName (args: obj list) =
    let argTypes = List.map getType args
    let className = targetType.FullName
    let methodInfo = targetType.GetMethod(methodName, List.toArray argTypes)
    if methodInfo = null then
        let methodSig = renderMethodSig methodName (List.map buildTypeName argTypes)
        let typeArgs = targetType.GetGenericArguments() |> Array.toList
        let typeSig =
            if typeArgs.Length = 0
                then Simple className
                else Compound(className, (List.map buildTypeName typeArgs))
        failwithf "Method \"%s\" is not defined on type \"%s\"" methodSig (renderTypeName typeSig)
    if instance = methodInfo.IsStatic then
        let instanceType = if instance then "an instance" else "a static"
        failwithf "Method \"%s\" is not %s method" methodName instanceType
    methodInfo

let findInstanceMethod (target: obj) = findMethod true (getType target)

let findStaticMethod className = findMethod false (findType className)
