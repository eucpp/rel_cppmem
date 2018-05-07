open MiniKanren

open Lang
open Lang.Expr
open Lang.Stmt
open Lang.Loc
open Lang.Reg
open Lang.Value

let prog_PetersonLock_sc = <:cppmem_par<
  spw {{{
      x_sc := 1;
      turn_sc := 1;
      repeat
        r1 := y_sc;
        r2 := turn_sc
      until ((r1 != 1) || (r2 != 1));
      (* start of critical section *)
      r3 := v_sc;
      v_sc := (r3 + 1);
      (* end of critical section *)
      x_sc := 0
  |||
      y_sc := 1;
      turn_sc := 0;
      repeat
        r1 := x_sc;
        r2 := turn_sc
      until ((r1 != 1) || (r2 != 0));
      (* start of critical section *)
      r3 := v_sc;
      v_sc := (r3 + 1);
      (* end of critical section *)
      y_sc := 0
  }}}
>>

let prog_PetersonLock_sc_rlx = <:cppmem_par<
  spw {{{
      x_sc := 1;
      turn_sc := 1;
      repeat
        r1 := y_rlx;
        r2 := turn_rlx
      until ((r1 != 1) || (r2 != 1));
      (* start of critical section *)
      r3 := v_rlx;
      v_rlx := (r3 + 1);
      (* end of critical section *)
      x_rlx := 0
  |||
      y_sc := 1;
      turn_sc := 0;
      repeat
        r1 := x_rlx;
        r2 := turn_rlx
      until ((r1 != 1) || (r2 != 0));
      (* start of critical section *)
      r3 := v_rlx;
      v_rlx := (r3 + 1);
      (* end of critical section *)
      y_rlx := 0
  }}}
>>

let prog_PetersonLock_rlx = <:cppmem_par<
  spw {{{
      x_rlx := 1;
      turn_rlx := 1;
      repeat
        r1 := y_rlx;
        r2 := turn_rlx
      until ((r1 != 1) || (r2 != 1));
      (* start of critical section *)
      r3 := v_rlx;
      v_rlx := (r3 + 1);
      (* end of critical section *)
      x_rlx := 0
  |||
      y_rlx := 1;
      turn_rlx := 0;
      repeat
        r1 := x_rlx;
        r2 := turn_rlx
      until ((r1 != 1) || (r2 != 0));
      (* start of critical section *)
      r3 := v_rlx;
      v_rlx := (r3 + 1);
      (* end of critical section *)
      y_rlx := 0
  }}}
>>

let prog_PetersonLock_sc_acq_rel = <:cppmem_par<
  spw {{{
      x_sc := 1;
      turn_rlx := 1;
      repeat
        r1 := y_sc;
        r2 := turn_rlx
      until ((r1 != 1) || (r2 != 1));
      (* start of critical section *)
      r3 := v_rlx;
      v_rlx := (r3 + 1);
      (* end of critical section *)
      x_rel := 0
  |||
      y_sc := 1;
      turn_rlx := 0;
      repeat
        r1 := x_sc;
        r2 := turn_rlx
      until ((r1 != 1) || (r2 != 0));
      (* start of critical section *)
      r3 := v_rlx;
      v_rlx := (r3 + 1);
      (* end of critical section *)
      y_rel := 0
  }}}
>>

let prog_PetersonLock_tpl = <:cppmem_par<
  spw {{{
      x_unkw := 1;
      turn_unkw := 1;
      repeat
        r1 := y_unkw;
        r2 := turn_unkw
      until ((r1 != 1) || (r2 != 1));
      (* start of critical section *)
      r3 := v_rlx;
      v_rlx := (r3 + 1);
      (* end of critical section *)
      x_unkw := 0
  |||
      y_unkw := 1;
      turn_unkw := 0;
      repeat
        r1 := x_unkw;
        r2 := turn_unkw
      until ((r1 != 1) || (r2 != 0));
      (* start of critical section *)
      r3 := v_rlx;
      v_rlx := (r3 + 1);
      (* end of critical section *)
      y_unkw := 0
  }}}
>>

let test_verify_sc = Test.(make_test_desc
  ~name:"Verify"
  ~regs:["r1"; "r2"; "r3"]
  ~locs:["x"; "y"; "turn"; "v"]
  ~prop:Prop.(loc "v" = 2)
  ~kind:Safe
  ~len:Long
  prog_PetersonLock_sc
)

let test_sc = Test.(make_operational_testsuite ~model:SeqCst [
    test_verify_sc;
])

let test_verify_rlx_tso = Test.(make_test_desc
  ~name:"Verify+rlx"
  ~regs:["r1"; "r2"; "r3"]
  ~locs:["x"; "y"; "turn"; "v"]
  ~prop:Prop.(loc "v" = 2)
  ~kind:Unsafe
  ~len:Long
  ~n:1
  prog_PetersonLock_rlx
)

let test_verify_sc_rlx_tso = Test.(make_test_desc
  ~name:"Verify+sc+rlx"
  ~regs:["r1"; "r2"; "r3"]
  ~locs:["x"; "y"; "turn"; "v"]
  ~prop:Prop.(loc "v" = 2)
  ~kind:Safe
  ~len:Long
  ~n:1
  prog_PetersonLock_sc_rlx
)

let test_synth_tso = Test.(make_test_desc
  ~name:"Synth"
  ~regs:["r1"; "r2"; "r3"]
  ~locs:["x"; "y"; "turn"; "v"]
  ~prop:Prop.(loc "v" = 2)
  ~kind:Synth
  ~len:Huge
  ~n:1
  prog_PetersonLock_tpl
)

let test_tso = Test.(make_operational_testsuite ~model:TSO [
  test_verify_rlx_tso;
  test_verify_sc_rlx_tso;
  (* test_synth_tso; *)
])

let test_verify_rlx_ra = Test.(make_test_desc
  ~name:"Verify+rlx"
  ~regs:["r1"; "r2"; "r3"]
  ~locs:["x"; "y"; "turn"; "v"]
  ~prop:Prop.(loc "v" = 2)
  ~kind:Unsafe
  ~len:Long
  ~n:1
  prog_PetersonLock_rlx
)

let test_verify_sc_acq_rel_ra = Test.(make_test_desc
  ~name:"Verify+sc+acq+rel"
  ~regs:["r1"; "r2"; "r3"]
  ~locs:["x"; "y"; "turn"; "v"]
  ~prop:Prop.(loc "v" = 2)
  ~kind:Safe
  ~len:Long
  ~n:1
  prog_PetersonLock_sc_acq_rel
)

let test_synth_ra = Test.(make_test_desc
  ~name:"Synth"
  ~regs:["r1"; "r2"; "r3"]
  ~locs:["x"; "y"; "turn"; "v"]
  ~prop:Prop.(loc "v" = 2)
  ~kind:Synth
  ~len:Huge
  ~n:1
  prog_PetersonLock_tpl
)

let test_ra = Test.(make_operational_testsuite ~model:RelAcq [
  test_verify_rlx_ra;
  test_verify_sc_acq_rel_ra;
  (* test_synth_ra; *)
])

let tests = Test.(make_testsuite ~name:"PetersonLock" [
  test_sc;
  test_tso;
  test_ra;
])
