open MiniKanren

let nat = inj_nat

let pprint rr =
  let n = rr#refine Nat.reify ~inj:Nat.to_logic in
  let s =
    Utils.pprint_nat Format.str_formatter n;
    Format.flush_str_formatter ()
  in
  Printf.printf "{q: %s}\n" s

let () =
  run q (fun q  -> Nat.addo (nat 1) (nat 3) q)
        (fun qs -> Printf.printf "1 + 3 = q\n";
                   Stream.iter pprint qs;
                   Printf.printf "\n")

let () =
 run q (fun q  -> Nat.addo (nat 1) q (nat 4))
       (fun qs -> Printf.printf "1 + q = 4\n";
                  Stream.iter pprint qs;
                  Printf.printf "\n")


let pprint (rr1, rr2) =
  let n1 = rr1#refine Nat.reify ~inj:Nat.to_logic in
  let n2 = rr2#refine Nat.reify ~inj:Nat.to_logic in
  let s1 =
    Utils.pprint_nat Format.str_formatter n1;
    Format.flush_str_formatter ()
  in
  let s2 =
    Utils.pprint_nat Format.str_formatter n2;
    Format.flush_str_formatter ()
  in
  Printf.printf "{q: %s; r: %s}\n" s1 s2

  let () =
   run qr (fun q  r  -> Nat.addo q r (nat 4))
          (fun qs rs -> Printf.printf "q + r = 4\n";
                        let stream = Stream.zip qs rs in
                        Stream.iter pprint stream;
                        Printf.printf "\n")
