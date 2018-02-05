(* Copyright (c) 2016-2018
 * Evgenii Moiseenko and Anton Podkopaev
 * St.Petersburg State University, JetBrains Research
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *)

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

let verify ~interpo ~asserto inputo prog =
  run qr (fun input output ->
      (inputo input) &&&
      (interpo prog input output) &&&
    ?~(asserto input output)
  )
  (fun qs rs -> Stream.zip qs rs)

let synth ?positive ?negative interpo tplo =
  let pexso prog = match positive with
    | None      -> success
    | Some exs  -> ?& (ListLabels.map exs ~f:(fun io ->
      fresh (i o)
        (io i o)
        (interpo prog i o)
      ))
  in
  let nexso prog = match negative with
    | None      -> success
    | Some exs  -> ?& (ListLabels.map exs ~f:(fun io -> ?~(
      fresh (i o)
        (io i o)
        (interpo prog i o)
      )))
  in
  run q (fun prog ->
    (* search among program candidates/templates *)
    (tplo prog) &&&
    (* evaluate candidate on all positive examples *)
    (pexso prog) &&&
    (* ensure that program doesn't evalute on negative examples *)
    (nexso prog)
  )
  (fun qs -> qs)
