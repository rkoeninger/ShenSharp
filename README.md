[![Build Status](https://api.travis-ci.org/rkoeninger/ShenSharp.svg?branch=master)](https://travis-ci.org/rkoeninger/ShenSharp)
[![Build status](https://ci.appveyor.com/api/projects/status/dy12w63pi5kqlhyr/branch/master?svg=true)](https://ci.appveyor.com/project/rkoeninger/shensharp/branch/master)

# ShenSharp

![神#](https://raw.githubusercontent.com/rkoeninger/ShenSharp/master/ShenSharp.png)

Making a second attempt at implementing a certified Shen implementation. This time using F# to learn F# and functional idioms.

Learn Shen here:

http://www.shenlanguage.org/learn-shen/index.html

The proper order for browsing the KL interpreter is as follows:
  * [Types](https://github.com/rkoeninger/ShenSharp/blob/master/Kl/Types.fs)
  * [Reader](https://github.com/rkoeninger/ShenSharp/blob/master/Kl.Load/Reader.fs)
  * [Evaluator](https://github.com/rkoeninger/ShenSharp/blob/master/Kl/Evaluator.fs)
  * [Builtins](https://github.com/rkoeninger/ShenSharp/blob/master/Kl/Builtins.fs)
  * [Startup](https://github.com/rkoeninger/ShenSharp/blob/master/Kl/Startup.fs)

Status:
  * All tests pass except for `proof assistant.shen`, which seems to hang,
    don't know if it is making progress
  * Evaluator is still very slow
  * I can generate F# using FSharp.Compiler.Services, but it's really slow
  * I can generate F# by string concatenation (Fex), but it crashes when you try to compile it
    * It crashes with a StackOverflowException, no warnings or error messages
    * I've read that this might be caused by excessively long F# lambdas
      * I could try to do lambda lifting to address this
  * Need to package relevant FSharp.Core.dll,.optdata,.sigdata with Kl.dll, etc. (?)
