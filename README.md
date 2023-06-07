This is interpreter of my derivative of brainfuck with almost full backwards compatability. 
Main new feature is that now you can define callable functions in any place of code (except loops).

<h1> How to build:</h1>
Run this command in the same directory as bf++.hs:

<b><i>ghc bf++ -O</i></b>
  
If you don't have ghc, you can find out how to install it here https://www.haskell.org/ghcup/

<h1> How to use:</h1>
To run code from file "fileName" run

<b><i>./bf++(.exe if Windows) fileName</i></b>

without any other args
  
<h1> How it differs from brainfuck:</h1>
Symbols "<>+-[].," means the same as in regular bf, except for two little differences:

1) Tape of numbers is infinity in both directions (limited only by your memory size) and uses as much memory as program really need. 
This feature is implemented by means of infinity lists.
2) To output symbols interpreter uses Unicode instead of ASCII, but values still can overflow like unsigned byte.

New available symbols:
 * '{' and '}' - use it to define functions ('{' means start and '}' means end). You can define functions inside other functions too, but not inside loops.
When interpreter reachs the end of the function, it returns current pointer (let it be n) and n next cells of the tape,
and then builds it into global (or just higher level) tape.
 * '$' and '\`' - these symbols are used to call functions. Every function call consists of two parts: one symbol '$' and n (n>=0) symbols '`', 
where n defines the ordinal number of called function. Functions are numbered in order from the beginning of the file. 
This means that construction "$```" will call third function from current context. When interpreter calling the function it passes to it the current pointer (let it be n)
and cuts next n cells of the current tape. Then it builds new tape by attaching to it infinity lists of zeros on both sides.