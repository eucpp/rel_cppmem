# RelCppMem

Relational interpreter of memory models written in [OCanren](https://github.com/dboulytchev/ocanren).

The interpreter allows one to:

* execute concurrent program and to discover all possible outcomes;
* explore the state space of concurrent program;
* check invariants of concurrent program;
* in some cases synthesize the holes in program.

Currently execution in following memory models is supported:

* SC --- Sequential Consistency;
* TSO --- Total Store Ordering;
* SRA --- Strong Release Acuire.

## Installation

- `opam pin add GT https://github.com/dboulytchev/GT.git`
- `opam pin add logger https://github.com/dboulytchev/logger.git`
- `opam pin add https://github.com/eucpp/OCanren.git#dev`
- `opam pin add relcppmem https://github.com/eucpp/relcppmem -y`

After installation you can test the package:

- `cd relcppmem`
- `make test`

## Usage

### Executing the program

Let's see some examples of library usage.

We will define the program that emulates message passing between two threads.
First thread prepares the value and stores it to the atomic variable `x`.
Then this thread sets atomic flag `f`.
Second thread waits for flag `f` to be set in the loop, and then reads the value of `x`.

```ocaml
open MiniKanren

open Relcppmem.Lang
open Relcppmem.Lang.Expr
open Relcppmem.Lang.Stmt
open Relcppmem.Lang.Loc
open Relcppmem.Lang.Reg
open Relcppmem.Lang.Value

let mp_prog = <:cppmem_par<
  spw {{{
    x_rlx := 1;
    f_rlx := 1
  |||
    repeat
      r1 := f_rlx
    until (r1);
    r2 := x_rlx
  }}}
>>
```

Note that program written in imperative language is enclosed into special quotation.

We will execute this program under SRA memory model.
Let's see that with relaxed accesses to shared variables
this program (under SRA MM) may lead to unexpected behaviors.  

```ocaml
module IntrpSRA = Interpreter(RelAcq)

module IntrpSRA = Interpreter(RelAcq)
```

The following lines create initial state of interpreter:

```ocaml
let regs = ["r1"; "r2"]
let locs = ["x"; "f"]
let istate = IntrpSRA.State.alloc_istate ~regs ~locs mp_prog
```

Finally, we can execute the program and get the stream of all possible results:

```ocaml
let res = IntrpSRA.eval istate
```

Lets print the results:

```ocaml
module Trace = Relcppmem.Utils.Trace(IntrpSRA.State)

let () = Stream.iter (Format.printf "%a@;" Trace.trace) res
```

As a result one can see two possible outcomes,
one of which leads to `r1 = 1` and `r2 = 0` in second thread.
This is the example of weak behavior.
Let's rule it out by imposing stronger access modifiers on operations.
We will make the write to `f` a _release-write_
and read from `f` an _acquire-read_.

```ocaml
let mp_ra_prog = <:cppmem_par<
  spw {{{
    x_rlx := 1;
    f_rel := 1
  |||
    repeat
      r1 := f_acq
    until (r1);
    r2 := x_rlx
  }}}
>>

let istate = IntrpSRA.State.alloc_istate ~regs ~locs mp_ra_prog

let res = IntrpSRA.eval istate

let () = Stream.iter (Format.printf "%a@;" Trace.trace) res
```

### Exploring the state space

The library also allows to explore the state space of the program.
In order to do that one have to use `reachable` function instead of `eval`.
Function `reachable` returns a stream of all reachable states.

```ocaml
let res = IntrpSRA.reachable istate

let () = Stream.iter (Format.printf "%a@;" Trace.trace) res
```

It might be useful to explore only those reachable states that
additionly satisfies some proposition.
One can achieve that by passing optional `prop` argument to `reachable`.
The `Prop` module is used to construct propositions.
For example, the following line call to `reachable` will return only those states
which has the value `1` in location `f`.

```ocaml
let res = IntrpSRA.reachable ~prop:Prop.(loc "f" = 1) istate
```
### Checking invariants

Often it may be useful to check that all reachable states satisfy some proposition.
The library provides function `invariant` that does exactly that.
Let's check that in all possible terminals states of message passing program
the value of register `r2` in second thread is equal to `1`.

```ocaml
if IntrpSRA.invariant ~prop:Prop.(!(terminal ()) || (2%"r2" = 1)) istate
then Format.printf "Invariant holds!@\n"
else Format.printf "Invariant doen't hold!@\n"
```
