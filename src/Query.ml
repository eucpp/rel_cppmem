open MiniKanren


type ('tt, 'tl) assertion = ('tt, 'tl) Semantics.Term.ti -> MiniKanren.goal

let def_succ_k () = ()

let def_fail_k _ = failwith "Verification query fails!"

let verify ?n ?(succ_k=def_succ_k) ?(fail_k=def_fail_k) evalo asserto t =
  let stream = run q (fun t' -> (evalo t t') &&& (asserto t')) (fun qs -> qs) in
  if Stream.is_empty stream then
    succ_k ()
  else
    fail_k @@ Stream.take ?n stream
