open MiniKanren


type ('tt, 'tl) assertion = ('tt, 'tl) Semantics.Term.ti -> MiniKanren.goal

(* let verify ?n ?(succ_k=def_succ_k) ?(fail_k=def_fail_k) evalo asserto t =
  let stream = run q (fun t' -> (evalo t t') &&& (asserto t')) (fun qs -> qs) in
  if Stream.is_empty stream then
    succ_k ()
  else
    fail_k @@ Stream.take ?n stream *)

let verify intrpo inputo asserto prog =
  run q (fun input ->
    fresh (output)
      (inputo input)
      (intrpo prog input output)
    ?~(asserto input output)
  )
  (fun qs -> qs)

  (* in
  if Stream.is_empty stream then
    succ_k ()
  else
    fail_k @@ Stream.take ?n stream *)

let synth intrpo geno verifyo =
  run q (fun input ->
    fresh (prog)
      (geno prog)
      (verifyo prog)
  )
  (fun qs -> qs)
