open MiniKanren


type ('tt, 'tl) assertion = ('tt, 'tl) Semantics.Term.ti -> MiniKanren.goal

let def_succ_k () = ()

let def_fail_k cexs =
  let ff = Format.str_formatter in
  Format.fprintf ff "Verification query fails!@; List of counterexamples:@;";
  List.iter (fun cex -> Format.fprintf ff "%a@;" Utils.Trace.trace cex) cexs;
  failwith @@ Format.flush_str_formatter ()

let verify ?n ?(succ_k=def_succ_k) ?(fail_k=def_fail_k) evalo asserto t =
  let stream = run q (fun t' -> (evalo t t') &&& ?~(asserto t')) (fun qs -> qs) in
  if Stream.is_empty stream then
    succ_k ()
  else
    fail_k @@ Stream.take ~n stream
