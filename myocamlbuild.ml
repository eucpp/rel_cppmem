open Ocamlbuild_plugin;;
module Pack = Ocamlbuild_pack

let () = dispatch (function
 | Before_rules ->
     ()

 | After_rules ->
    flag ["hack_pr_o"; "compile"] (S[A"-ppopt"; A"pr_o.cmo"; ]);

    (* flag ["ocamldep"; "use_pa_cppmem"]
      (S[
        (* A"-ppopt";A"-I";A"./src/"; *)
        A"-ppopt";A"GT.cma";
        A"-ppopt";A"MiniKanren.cma";
        A"-ppopt";A"./src/Utils.cmo";
        A"-ppopt";A"./src/Lang.cmo";
        A"-ppopt";A"./camlp5/pa_cppmem.cmo";
        (* A"-ppopt";A"./src/Lang.cma"; *)
      ]);

    flag ["compile"; "ocaml"; "use_pa_cppmem"]
      (S[
        (* A"-ppopt";A"-I";A"./src/"; *)
        A"-ppopt";A"GT.cma";
        A"-ppopt";A"MiniKanren.cma";
        A"-ppopt";A"./src/Utils.cmo";
        A"-ppopt";A"./src/Lang.cmo";
        A"-ppopt";A"./camlp5/pa_cppmem.cmo";
      ]); *)

    (* flag ["ocamldep"; "use_pa_cppmem"]
      (S[A"-pp";A"camlp5o ./camlp5/pa_cppmem.cmo"]);

    flag ["compile"; "ocaml"; "use_pa_cppmem"] (S[A"-pp";A"camlp5o ./camlp5/pa_cppmem.cmo";]); *)

    (* flag ["compile"; "ocaml"; "compile_pa_cppmem"] (S[A"-pp";A"camlp5o -loc loc";]); *)

    (* flag ["ocamldep"; "use_pa_cppmem"]
      (S[A"-pp";A"camlp5o camlp5/pa_cppmem.cmo"]); *)

    (* flag ["compile"; "ocaml"; "use_pa_cppmem"] (S[A"-pp";A"camlp5o  ./camlp5/pa_cppmem.cmo";]); *)
    (* flag ["compile"; "ocaml"; "use_pa_cppmem"] (S[A"-ppopt";A"camlp5/pa_cppmem.cmo";]); *)

    (* flag ["compile"; "ocaml"; "use_pa_cppmem"]
     (S [A"-pp";A"camlp5o camlp5/pa_cppmem.cmo"]); *)

    (* dep ["ocaml"; "ocamldep"; "use_cppmem"] ["pa_cppmem.cmo"]; *)
    ()
 | _ -> ()
)
