# 神

## Shen.NET

http://shenlanguage.org/

Shen is a portable functional programming language by [Mark Tarver](http://www.lambdassociates.org/) that offers

* pattern matching,
* λ calculus consistency,
* macros,
* optional lazy evaluation,
* static type checking,
* an integrated fully functional Prolog,
* and an inbuilt compiler-compiler.

This project was greatly inspired by the work of [Håkan Råberg](https://github.com/hraberg) on his [Java implementation](https://github.com/hraberg/Shen.java) of Shen/Kλ.

## A .Net implementation

I'd find anything about a .Net implementation, so I thought I'd give it a try. I used the above mentioned Java implementation as a guide, but came up much of it myself, including how to do tail call optimization, which I am proud of despite of how miserably slow my version of the Kλ interpreter is.

And sometimes when I run the Shen repl, I get weird behavior with I/O. There's probably something wrong with my code. Does this thing really work at all? Who knows!?