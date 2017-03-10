module Kl.Interop

open System
open System.Reflection

let private publicInstance = BindingFlags.Public ||| BindingFlags.Instance

let private publicStatic = BindingFlags.Public ||| BindingFlags.Static

let findInstanceProperty (target: obj) propertyName =
    let clazz = target.GetType()
    let propertyInfo = clazz.GetProperty(propertyName, publicInstance)
    if propertyInfo = null then
        failwithf "Property \"%s\" not defined on type \"%s\"" propertyName clazz.Name
    if propertyInfo.GetIndexParameters().Length > 0 then
        failwithf "Property \"%s\" has index parameters, use \"clr.get-index\" instead" propertyName
    propertyInfo

let findIndexProperty (target: obj) =
    let clazz = target.GetType()
    let propertyInfo = clazz.GetProperty("Item", publicInstance)
    if propertyInfo = null then
        failwithf "Indexer property not defined on type \"%s\"" clazz.Name
    propertyInfo

let findStaticProperty className propertyName =
    let clazz = Type.GetType className
    let propertyInfo = clazz.GetProperty(propertyName, publicStatic)
    if propertyInfo = null then
        failwithf "Property \"%s\" not defined on type \"%s\"" propertyName clazz.Name
    if propertyInfo.GetIndexParameters().Length > 0 then
        failwithf "Property \"%s\" has index parameters, use \"clr.get-index-static\" instead" propertyName
    propertyInfo

let findInstanceMethod (target: obj) methodName (args: obj list) =
    let argTypes = List.map (fun x -> x.GetType()) args
    let clazz = target.GetType()
    let methodInfo = clazz.GetMethod(methodName, List.toArray argTypes)
    if methodInfo.IsStatic then
        failwithf "Method \"%s\" is not an instance method" methodName
    methodInfo

let findStaticMethod className methodName (args: obj list) =
    let argTypes = List.map (fun x -> x.GetType()) args
    let clazz = Type.GetType className
    let methodInfo = clazz.GetMethod(methodName, List.toArray argTypes)
    if not(methodInfo.IsStatic) then
        failwithf "Method \"%s\" is not a static method" methodName
    methodInfo
