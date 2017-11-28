open MiniKanren

type ('at, 'bt, 'al, 'bl) assertion =
  ('at, 'al) Semantics.Input.ti -> ('bt, 'bl) Semantics.Output.ti -> MiniKanren.goal

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

let synth ~interpo ~tplo ~positive ~negative =
  run q (fun prog ->
    (* search among program candidates/templates *)
    (tplo prog) &&&
    (* evaluate candidate on all positive examples *)
    ?& (ListLabels.map positive ~f:(fun io ->
      fresh (i o)
        (io i o)
        (interpo prog i o)
    )) &&&
    (* ensure that program doesn't evalute on negative examples *)
    ?& (ListLabels.map negative ~f:(fun io ->
      fresh (i o)
        (io i o)
      ?~(interpo prog i o)
    ))
  )
  (fun qs -> qs)
