open MiniKanren

type ('at, 'bt, 'al, 'bl) assertion =
  ('at, 'al) Semantics.Input.ti -> ('bt, 'bl) Semantics.Output.ti -> MiniKanren.goal

(* let verify ?n ?(succ_k=def_succ_k) ?(fail_k=def_fail_k) evalo asserto t =
  let stream = run q (fun t' -> (evalo t t') &&& (asserto t')) (fun qs -> qs) in
  if Stream.is_empty stream then
    succ_k ()
  else
    fail_k @@ Stream.take ?n stream *)

let exec intrpo prog input =
  run q (fun output -> intrpo prog input output)
  (fun qs -> qs)

let angelic intrpo inputo outputo prog =
  run qr (fun input output ->
      (inputo input) &&&
      (intrpo prog input output) &&&
      (outputo output)
  )
  (fun qs rs -> Stream.zip qs rs)

let verify intrpo inputo asserto prog =
  run qr (fun input output ->
      (inputo input) &&&
      (intrpo prog input output) &&&
    ?~(asserto input output)
  )
  (fun qs rs -> Stream.zip qs rs)

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
