module Kl.Interop

open System
open System.Reflection
open ShenSharp.Shared

// TODO: merge into global aliases
let private primitiveAliases = [
    "System.Boolean", "bool"
    "System.Int32",   "int"
    "System.Decimal", "decimal"
    "System.String",  "string"
    "System.Object",  "object"
]

// TODO: classname aliasing from globals
let rec findType (typeName: string): Type =
    match List.tryFind (snd >> (=) typeName) primitiveAliases with
    | Some(proper, _) -> Type.GetType proper
    | None ->
        let leftIndex = typeName.IndexOf '<'
        let rightIndex = typeName.IndexOf '>'
        if leftIndex > 0 && rightIndex > 0 then
            let genericTypeName = typeName.Substring(0, leftIndex)
            let typeArgNames =
                typeName
                    .Substring(leftIndex + 1, rightIndex - 1 - leftIndex)
                    .Split ','
                |> Array.map (fun x -> x.Trim())
            let typeArgCount = Array.length typeArgNames
            let genericType = findType(sprintf "%s`%i" genericTypeName typeArgCount)
            let typeArgs = Array.map findType typeArgNames
            genericType.MakeGenericType typeArgs
        else
            let result = Type.GetType typeName
            if result = null then
                failwithf "Type \"%O\" is not defined" result
            result

let private publicInstance = BindingFlags.Public ||| BindingFlags.Instance

let private publicStatic = BindingFlags.Public ||| BindingFlags.Static

let findInstanceProperty (target: obj) propertyName =
    let clazz = target.GetType()
    let propertyInfo = clazz.GetProperty(propertyName, publicInstance)
    if propertyInfo = null then
        failwithf "Property \"%s\" is not defined on type \"%s\"" propertyName clazz.Name
    if propertyInfo.GetIndexParameters().Length > 0 then
        failwithf "Property \"%s\" has index parameters, use \"clr.get-index\" instead" propertyName
    propertyInfo

let findIndexProperty (target: obj) =
    let clazz = target.GetType()
    let propertyInfo = clazz.GetProperty("Item", publicInstance)
    if propertyInfo = null then
        failwithf "Indexer property is not defined on type \"%s\"" clazz.Name
    propertyInfo

let findStaticProperty className propertyName =
    let clazz = findType className
    let propertyInfo = clazz.GetProperty(propertyName, publicStatic)
    if propertyInfo = null then
        failwithf "Property \"%s\" is not defined on type \"%s\"" propertyName clazz.Name
    if propertyInfo.GetIndexParameters().Length > 0 then
        failwithf "Property \"%s\" has index parameters, use \"clr.get-index-static\" instead" propertyName
    propertyInfo

let rec private typeString (baseName: string) leftBrace rightBrace = function
    | [] ->
        match List.tryFind (fst >> (=) baseName) primitiveAliases with
        | Some(_, alias) -> alias
        | _ -> baseName
    | typeArgNames ->
        let backtickIndex = baseName.IndexOf '`'
        let genericTypeName =
            if backtickIndex > 0 then
                baseName.Substring(0, backtickIndex)
            else
                baseName
        // TODO: nested parameterized types
        let paramString = String.Join(", ", List.map (fun t -> typeString t "<" ">" []) typeArgNames)
        sprintf "%s%s%s%s" genericTypeName leftBrace paramString rightBrace

let private findMethod instance (targetType: Type) methodName (args: obj list) =
    let argTypes = List.map (fun x -> x.GetType()) args
    let className = targetType.Name
    let methodInfo = targetType.GetMethod(methodName, List.toArray argTypes)
    if methodInfo = null then
        let methodSig = typeString methodName "(" ")" (List.map string argTypes)
        let typeSig = typeString className "<" ">" (List.map string (targetType.GetGenericArguments() |> Array.toList))
        failwithf "Method \"%s\" is not defined on type \"%s\"" methodSig typeSig
    if instance = methodInfo.IsStatic then
        let instanceType = if instance then "an instance" else "a static"
        failwithf "Method \"%s\" is not %s method" methodName instanceType
    methodInfo

let findInstanceMethod (target: obj) = findMethod true (target.GetType())

let findStaticMethod className = findMethod false (findType className)
