[![Build Status](https://api.travis-ci.org/rkoeninger/ShenSharp.svg?branch=master)](https://travis-ci.org/rkoeninger/ShenSharp)
[![Build status](https://ci.appveyor.com/api/projects/status/dy12w63pi5kqlhyr/branch/master?svg=true)](https://ci.appveyor.com/project/rkoeninger/shensharp/branch/master)

# ShenSharp

![神#](https://raw.githubusercontent.com/rkoeninger/ShenSharp/master/ShenSharp.png)

ShenSharp is a port of the [Shen language](http://shenlanguage.org/) to the CLR.

Shen is a portable, functional programming language by [Mark Tarver](http://www.shenlanguage.org/lambdassociates/htdocs/index.htm) that offers pattern matching, macros, optional lazy evaluation, static type checking, logic programming and a compiler-compiler. Shen is a descendant of the LISP family of languages that also includes features commonly found in ML family languages.

Currently implemented as an interpreter, ShenSharp loads and runs Shen code on demand. The interpreter approach suffers from poor performance, but optimizations are forthcoming.

This port [passes all tests in the Shen standard test suite](https://gist.github.com/rkoeninger/2f29ca2f6d5ab88c8f4dccacc4def3fc), making this a certifiable implementation.

Shen Resources:
  * [Open Source Implementations](http://www.shenlanguage.org/download_form.html)
  * [Tutorial](http://www.shenlanguage.org/learn-shen/index.html)
  * [Mailing List](https://groups.google.com/forum/#!forum/qilang)

CLR Platform Status:
  * [Microsoft.NET](https://www.microsoft.com/net) - Built in Visual Studio on Windows
  * [Mono](http://www.mono-project.com/) - Tests build and run on Travis-CI in Ubuntu
  * [.NET Core](https://www.microsoft.com/net/core#windowsvs2015) - Not yet supported

Tour the interpreter by browsing in the following order:
  * [Types](https://github.com/rkoeninger/ShenSharp/blob/master/Kl/Types.fs)
  * [Reader](https://github.com/rkoeninger/ShenSharp/blob/master/Kl.Load/Reader.fs)
  * [Evaluator](https://github.com/rkoeninger/ShenSharp/blob/master/Kl/Evaluator.fs)
  * [Builtins](https://github.com/rkoeninger/ShenSharp/blob/master/Kl/Builtins.fs)
  * [Startup](https://github.com/rkoeninger/ShenSharp/blob/master/Kl/Startup.fs)
