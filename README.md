# ShenSharp

![神#](https://raw.githubusercontent.com/rkoeninger/ShenSharp/master/Assets/ShenSharp.png)

ShenSharp is a port of the [Shen language](http://shenlanguage.org/) to the CLR.

Shen is a portable, functional programming language by [Mark Tarver](http://www.shenlanguage.org/lambdassociates/htdocs/index.htm). It is a descendant of the LISP family of languages which also includes features commonly found in ML family languages. Shen offers pattern matching, a unique macro system, optional lazy evaluation, configurable static type checking, logic programming and a built-in compiler-compiler.

Currently implemented as an interpreter, ShenSharp loads and runs Shen code on demand. The interpreter approach suffers from poor performance, but optimizations are forthcoming.

This port [passes all tests in the Shen standard test suite](https://gist.github.com/rkoeninger/2f29ca2f6d5ab88c8f4dccacc4def3fc), making this a certifiable implementation.

### CI Builds

|    Platform    |      Status      |
|----------------|------------------|
| .NET - Windows | [![Build Status](https://ci.appveyor.com/api/projects/status/dy12w63pi5kqlhyr/branch/master?svg=true)](https://ci.appveyor.com/project/rkoeninger/shensharp/branch/master) |
| Mono - Ubuntu  | [![Build Status](https://api.travis-ci.org/rkoeninger/ShenSharp.svg?branch=master)](https://travis-ci.org/rkoeninger/ShenSharp) |

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
  * [Types](https://github.com/rkoeninger/ShenSharp/blob/master/Kl/Types.fs)
  * [Evaluator](https://github.com/rkoeninger/ShenSharp/blob/master/Kl/Evaluator.fs)
  * [Builtins](https://github.com/rkoeninger/ShenSharp/blob/master/Kl/Builtins.fs)
  * [Startup](https://github.com/rkoeninger/ShenSharp/blob/master/Kl/Startup.fs)
  * [Reader](https://github.com/rkoeninger/ShenSharp/blob/master/Kl.Import/Reader.fs)
  * [Generator](https://github.com/rkoeninger/ShenSharp/blob/master/Kl.Import/Generator.fs)
  * [Loader](https://github.com/rkoeninger/ShenSharp/blob/master/Kl.Import/Loader.fs)
