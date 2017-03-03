[![Shen Version](https://img.shields.io/badge/Shen_Language-19.3.1-blue.svg)](https://github.com/Shen-Language)
[![Latest Nuget](https://img.shields.io/nuget/v/ShenSharp.svg?label=Nuget%20Latest)](https://www.nuget.org/packages/ShenSharp)
[![.Net Build Status](https://img.shields.io/appveyor/ci/rkoeninger/ShenSharp/master.svg?label=.Net%20Build)](https://ci.appveyor.com/project/rkoeninger/shensharp/branch/master)
[![Mono Build Status](https://img.shields.io/travis/rkoeninger/ShenSharp/master.svg?label=Mono%20Build)](https://travis-ci.org/rkoeninger/ShenSharp)

# ShenSharp

![ShenSharp Logo](https://raw.githubusercontent.com/rkoeninger/ShenSharp/master/Assets/ShenSharp.png)

ShenSharp is a port of the [Shen language](http://shenlanguage.org/) to the CLR.

Shen is a portable, functional programming language by [Mark Tarver](http://marktarver.com/). It is a descendant of the LISP family of languages which also includes features commonly found in ML family languages. Shen offers pattern matching, a unique macro system, optional lazy evaluation, configurable static type checking, logic programming and a built-in compiler-compiler.

ShenSharp is built by translating the Shen sources from Kλ to F# ASTs and building that into a CLR assembly. Further Shen code is translated into optimized expressions and evaluated on demand. Some optimizations used include forming direct references to global symbols, expression simplification and tail call optimization using trampolines.

This port passes all tests in the Shen standard test suite, making this a certifiable implementation. Current test run time is about 40 seconds in the Windows/.Net build on AppVeyor.

### Shen Resources
  * [Open Source Implementations](http://www.shenlanguage.org/download_form.html)
  * [Tutorial](http://www.shenlanguage.org/learn-shen/index.html)
  * [Sources](https://github.com/Shen-Language/shen-sources)
  * [Wiki](https://github.com/Shen-Language/wiki/wiki)
  * [Mailing List](https://groups.google.com/forum/#!forum/qilang)

### CLR Platform Support Status
  * [Microsoft.NET](https://www.microsoft.com/net) - Built in Visual Studio on Windows
  * [Mono](http://www.mono-project.com/) - Tests build and run on Travis-CI in Ubuntu
  * [.NET Core](https://www.microsoft.com/net/core#windowsvs2015) - Unknown/not yet supported

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
    Translates an environment of KL expressions into a F# module AST
  * [Loader](https://github.com/rkoeninger/ShenSharp/blob/master/Kl.Make/Loader.fs) -
    Reads and evaluates the Shen runtime from Kλ distribution or loads runtime from code
	generated on previous run
