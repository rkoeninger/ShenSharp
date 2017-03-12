module Kl.Interop

open System
open System.Reflection
open ShenSharp.Shared

// TODO: classname aliasing from globals
let rec findType (typeName: string): Type =
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

// TODO: fix type name presentation
let rec private typeString (className: string) = function
    | [] -> className
    | typeArgNames ->
        let backtickIndex = className.IndexOf '`'
        let genericTypeName =
            if backtickIndex > 0 then
                className.Substring(0, backtickIndex)
            else
                className
        let paramString = String.Join(", ", List.map typeString typeArgNames)
        sprintf "%s(%s)" genericTypeName paramString

let findInstanceMethod (target: obj) methodName (args: obj list) =
    let argTypes = List.map (fun x -> x.GetType()) args
    let clazz = target.GetType()
    let methodInfo = clazz.GetMethod(methodName, List.toArray argTypes)
    if methodInfo = null then
        let typeSig = typeString methodName (List.map string argTypes)
        failwithf "Method \"%s\" is not defined on type \"%s\"" methodName clazz.Name
    if methodInfo.IsStatic then
        failwithf "Method \"%s\" is not an instance method" methodName
    methodInfo

let findStaticMethod className methodName (args: obj list) =
    let argTypes = List.map (fun x -> x.GetType()) args
    let clazz = findType className
    let methodInfo = clazz.GetMethod(methodName, List.toArray argTypes)
    if methodInfo = null then
        let typeSig = typeString methodName (List.map string argTypes)
        failwithf "Method \"%s\" is not defined on type \"%s\"" methodName clazz.Name
    if not(methodInfo.IsStatic) then
        failwithf "Method \"%s\" is not a static method" methodName
    methodInfo
