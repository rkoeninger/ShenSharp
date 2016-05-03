[![Build Status](https://api.travis-ci.org/rkoeninger/ShenSharp.svg?branch=master)](https://travis-ci.org/rkoeninger/ShenSharp)

# Shen&#35;

Making a second attempt at implementing a certified Shen implementation. This time using F# to learn F# and functional idioms.

Shen 19 doc can be found here:

http://www.shenlanguage.org/documentation/shendoc.htm

Status:
  * I can generate F# using FSharp.Compiler.Services, but it's really slow
  * I can generate F# by string concatenation (Fex), but it crashes when you try to compile it
  * Evaluator is still very slow
  * Shen Tests still do not pass
  * Need to package relevant FSharp.Core.dll,.optdata,.sigdata with Kl.dll, etc.
