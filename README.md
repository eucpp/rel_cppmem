# RelCppMem

Relational interpreter of C/C++11 memory model.

## Overview

The goal of this project is to bring convenient tool for analyzing,
verifying and synthesizing code that is aim to work with C/C++11
threads, atomics, etc.

We introduce a relational interpreter of an operational formalization
of the C/C++11 memory model, based on the work by Kang J. et al.<sup>1</sup>
Our interpreter is implemented in [OCanren](https://github.com/dboulytchev/ocanren),
a strongly-typed embedding of miniKanren relational programming
language into OCaml.

With the help of relational interpreter we expect the following tasks could be accomplished:

* Generation of all possible final states of the memory by the given program;
* Tracing of the execution path by the given program, initial and final state of memory;
* Synthesis of the synchronization primitives with the desired behavior.

Our tool expects as an input code written not in C/C++ but in some simple language,
which includes all traditional constructions of imperative languages
such as condition operator (`if then else`), loops (`repeat`), etc;
as well as operators for spawning new threads
and accessing atomics with specified memory order.

The concrete syntax of this language is very similar to [cppmem](http://www.cl.cam.ac.uk/%7Epes20/cpp/)
and can be defined by the following EBNF:

```
const   ::= integer;
var     ::= string;
mo      ::= "sc" | "acq" | "rel" | "relAcq" | "rlx" | "na";
atomic  ::= var, "\_", mo;

expr    = const | var | atomic | binop ;
binop   ::= expr, op, expr;
op      ::= "+" | "-" | "*" | "=" | "!=" | "<" | "<=" | ">" | ">=";

stmt    ::= if | repeat | seq | spw | asgn | "ret", expr;
if      ::= "if", expr, "then", stmt, "else", stmt, "fi";
repeat  ::= "repeat", stmt, "end";
seq     ::= stmt, ";", stmt;
spw     ::= "spw {{{", stmt, "|||", stmt, "}}}";
asgn    ::= lval, ":=", rval;
lval    ::= var | atomic | write | "(", lval, ",", lval ")";
rval    ::= expr | spw;
```

## Installation

- `opam pin add GT https://github.com/dboulytchev/GT.git`
- `opam pin add logger https://github.com/dboulytchev/logger.git`
- `opam pin add https://github.com/eucpp/OCanren.git\#dev`
- `opam pin add relcppmem https://github.com/eucpp/rel_cppmem -y`

After installation you can test the package:

- `cd rel_cppmem`
- `make test`

## Usage

Let's see some examples of the usage of our library.

We will define the program that emulates message passing between two threads.
First thread prepares the value and stores it to the atomic variable `x`.
Then this thread sets atomic flag `f`.
Second thread waits for flag `f` to be set in the loop, and then reads the value of `x`. We assume that initially both `x` and `f` are set to 0.

In order to introduce proper synchronization between threads we need
to label the write of `f` as a write-release and read of `f` as read-acquire.
In this case the C/C++ standard guarantees that second thread will see
updated value of `x`.

We start our first example by opening required modules and
writing down our message-passing program.

```{OCaml}
open MiniKanren
open Relcppmem
open Relcppmem.Lang
open Relcppmem.Lang.Term
open Relcppmem.Memory

let prog = <:cppmem<
    spw {{{
        x_rlx := 42;
        f_rel := 1
    |||
        repeat f_acq end;
        r1 := x_rlx;
        ret r1
    }}}
>>
```
Note that program written in our language and enclosed into special quotation.

Next we specify the semantics.

First, we need to select a list of rewriting rules.

```{OCaml}
let rules = Rules.Basic.all @ Rules.ThreadSpawning.all @ Rules.Rlx.all @ Rules.RelAcq.all
```

In our case we have built the list of all basic rules
(i.e. the rules that handle if statement, repeat statement, etc),
then rules for spawning and joining threads and also rules for relaxed and
acquire-release atomic's accesses.  

The second stage is to make reduction-relation, i.e. step-relation,
from the list of rewriting rules.

```{OCaml}
module Step = (val make_reduction_relation rules)
```

Finally we can build our semantics:

```{OCaml}
module Sem = Semantics.Make(Step)
```

We also want to construct initial state of the program's memory:

```{OCaml}
let state = MemState.inj @@ MemState.preallocate ["r1"] ["x";"f"]
```

Here `MemState.preallocate ["r1"] ["x";"f"]` initializes memory
with single thread-local variable `r1` and two atomics `x` and `f`.
`MemState.inj` is auxiliary function that injects regular OCaml value into
OCanren's logic domain.

Finally we launch an OCanren's goal:

```{OCaml}
let stream = run qr Sem.(fun q  r  -> (prog, state) -->* (q, r))
                        (fun qs rs -> Stream.zip qs rs)
```

We ask OCanren to reduce the given term (i.e. our program) with initial state
and find out the final term (i.e. return value) and state.
As a result of running the goal ```fun q  r  -> (prog, state) -->* (q, r)``` OCanren obtains two possible infinite stream of answers.
For the sake of convenience we zip these streams into single stream of pairs.

Now, we need to print out all founded answers.
We will define the function that will print single pair of answers.

```{OCaml}
let handler (t, s) =
  let t' = Term.refine t in
  let s' = MemState.refine s in
  Printf.printf "Ret: %s;\n%s" (Term.pprint t') (MemState.pprint s')
```

Again, `Term.refine` and `MemState.refine` are auxiliary functions.
They take the answer given by OCanren, represented as certain OCaml object,
and produce logic term/state.
`Term.pprint`/`MemState.pprint` are predefined functions for pretty-printing.

As a last step, we need to apply our handler to every answer in the stream:

```{OCaml}
let _ =
  Stream.iter handler stream
```

The overall example:

```{OCaml}
open MiniKanren
open Relcppmem
open Relcppmem.Lang
open Relcppmem.Lang.Term
open Relcppmem.Memory

let prog = <:cppmem<
    spw {{{
        x_rlx := 42;
        f_rel := 1
    |||
        repeat f_acq end;
        r1 := x_rlx;
        ret r1
    }}}
>>

let rules = Rules.Basic.all @ Rules.ThreadSpawning.all @ Rules.Rlx.all @ Rules.RelAcq.all

module Step = (val make_reduction_relation rules)

module Sem = Semantics.Make(Step)

let state = MemState.inj @@ MemState.preallocate ["r1"] ["x";"f"]

let stream = run qr Sem.(fun q  r  -> (prog, state) -->* (q, r))
                        (fun qs rs -> Stream.zip qs rs)

let handler (t, s) =
  let t' = Term.refine t in
  let s' = MemState.refine s in
  Printf.printf "Ret: %s;\n%s" (Term.pprint t') (MemState.pprint s')

let _ =
  Stream.iter handler stream
```

We can build it with the following command:

```{bash}
ocamlfind opt -rectypes -syntax camlp5o -package ocanren,ocanren.syntax,GT,relcppmem,relcppmem.syntax -linkpkg -o message_passing message_passing.ml
```

After launching the message_passing binary you should see following output:

```
Ret: 42;
Thread 1:
     reg: [ r1=0; ]
     cur: [ x@1; f@1; ]
     acq: [ x@1; f@1; ]
     rel: [ x@1; f@0; ]

Memory :
    [
        [ {x@1=42, [ x@0; f@0; ]}; {x@0=0, [ x@0; f@0; ]}; ];
        [ {f@1=1,  [ x@1; f@0; ]}; {f@0=0, [ x@0; f@0; ]}; ];
    ]
```

As we expected, there is only one possible result.
The return value of the program (i.e. the value of `x` read by second thread) is 42.
You can also see the internal state of threads and memory after execution. 

[1]: Kang, J., Hur, C. K., Lahav, O., Vafeiadis, V., & Dreyer, D. (2017, January).
A promising semantics for relaxed-memory concurrency. In ACM Symp. on Principles of Programming Languages (POPL).
