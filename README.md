[![Shen Version](https://img.shields.io/badge/shen-21.2-blue.svg)](https://github.com/Shen-Language)
[![Latest Nuget](https://img.shields.io/nuget/v/ShenSharp.svg)](https://www.nuget.org/packages/ShenSharp)
[![.Net Build Status](https://img.shields.io/appveyor/ci/rkoeninger/ShenSharp/master.svg?label=.net)](https://ci.appveyor.com/project/rkoeninger/shensharp/branch/master)
[![Mono Build Status](https://img.shields.io/travis/rkoeninger/ShenSharp/master.svg?label=mono)](https://travis-ci.org/rkoeninger/ShenSharp)

# Shen for the Common Language Runtime

<img src="https://raw.githubusercontent.com/rkoeninger/ShenSharp/master/Assets/ShenSharp.png" align="right">

ShenSharp is a port of the [Shen language](http://shenlanguage.org/) to the CLR.

Shen is a portable, functional programming language by [Mark Tarver](http://marktarver.com/). It is a descendant of the LISP family of languages which also includes features commonly found in ML family languages. Shen offers pattern matching, a unique macro system, optional lazy evaluation, configurable static type checking, logic programming and a built-in compiler-compiler.

ShenSharp is built by loading the Shen kernel as Kλ and translating the resulting environment to F# ASTs and building that into a CLR assembly. Shen code loaded by this compiled kernel is translated into optimized expressions and evaluated on demand. Some optimizations used include inlining direct references to global symbols, expression simplification and tail call optimization using trampolines.

This port passes all tests in the Shen standard test suite, making this a certifiable implementation. Current test run time is about 40 seconds in the Windows/.Net build on AppVeyor.

Refer to the CI build scripts ([`appveyor.yml`](https://github.com/rkoeninger/ShenSharp/blob/master/appveyor.yml), [`.travis.yml`](https://github.com/rkoeninger/ShenSharp/blob/master/.travis.yml)) for build instructions.

### Shen Resources
  * [Open Source Implementations](http://www.shenlanguage.org/download_form.html)
  * [Tutorial](http://www.shenlanguage.org/learn-shen/index.html)
  * [Sources](https://github.com/Shen-Language/shen-sources)
  * [Wiki](https://github.com/Shen-Language/wiki/wiki)
  * [Mailing List](https://groups.google.com/forum/#!forum/qilang)

### CLR Platform Support Status
  * [Microsoft.NET](https://www.microsoft.com/net) - v4.5
  * [Mono](http://www.mono-project.com/) - v4.5
  * [.NET Core](https://www.microsoft.com/net/core#windowsvs2015) - Not yet supported

### Guided Tour of the Code
  * [Types](https://github.com/rkoeninger/ShenSharp/blob/master/Kl/Types.fs) -
    Defines types for Kλ Values, Functions, Environment and IO
  * [Evaluator](https://github.com/rkoeninger/ShenSharp/blob/master/Kl/Evaluator.fs) -
    Interprets Kλ syntax trees, rendering Kλ values and performing side effects
  * [Builtins](https://github.com/rkoeninger/ShenSharp/blob/master/Kl/Builtins.fs) -
    Primitive functions required by Kλ
  * [Startup](https://github.com/rkoeninger/ShenSharp/blob/master/Kl/Startup.fs) -
    Builds a primitive Kλ environment
  * [Reader](https://github.com/rkoeninger/ShenSharp/blob/master/Kl.Make/Reader.fs) -
    Parses Kλ code
  * [Compiler](https://github.com/rkoeninger/ShenSharp/blob/master/Kl.Make/Compiler.fs) -
    Translates an environment of Kλ expressions into a F# module AST
  * [Runtime](https://github.com/rkoeninger/ShenSharp/blob/master/Shen.Api/Runtime.fs) -
    Helpful functions for embedding Shen in another application
  * [ExtensionMethods](https://github.com/rkoeninger/ShenSharp/blob/master/Shen.Api/ExtensionMethods.fs) -
    Extension method versions of functions in Runtime to give API a C# feel
  * [RunRepl](https://github.com/rkoeninger/ShenSharp/blob/master/Shen.Repl/RunRepl.fs) -
    Interpretation of command line arguments passed to `Shen.exe`
