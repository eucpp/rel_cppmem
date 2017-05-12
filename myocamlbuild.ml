open Ocamlbuild_plugin;;
module Pack = Ocamlbuild_pack

let () = dispatch (function
 | Before_rules ->
     ()

 | After_rules ->
     flag ["hack_pr_o"; "compile"] (S[A"-ppopt"; A"pr_o.cmo"; ]);

     flag ["ocamldep"; "use_pa_cppmem"]
      (S[A"-pp";A"camlp5o ./camlp5/pa_cppmem.cmo"]);

     flag ["compile"; "ocaml"; "use_pa_cppmem"] (S[A"-pp";A"camlp5o ./camlp5/pa_cppmem.cmo";]);
     (* dep ["ocaml"; "ocamldep"; "use_cppmem"] ["pa_cppmem.cmo"]; *)
   ()
 | _ -> ()
)
