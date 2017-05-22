open Ocamlbuild_plugin;;
module Pack = Ocamlbuild_pack

let () = dispatch (function
 | Before_rules ->
     ()

 | After_rules ->
    flag ["hack_pr_o"; "compile"] (S[A"-ppopt"; A"pr_o.cmo"; ]);

    flag ["ocamldep"; "use_pa_cppmem"]
      (S[
        A"-ppopt";A"GT.cma";
        A"-ppopt";A"MiniKanren.cma";
        (* A"-ppopt";A"./src/Utils.cmo";
        A"-ppopt";A"./src/Memory.cmo";
        A"-ppopt";A"./src/Lang.cmo"; *)
        A"-ppopt";A"./relcppmem.cmo";
        A"-ppopt";A"./camlp5/pa_cppmem.cmo";
      ]);

    flag ["compile"; "ocaml"; "use_pa_cppmem"]
      (S[
        A"-ppopt";A"GT.cma";
        A"-ppopt";A"MiniKanren.cma";
        A"-ppopt";A"./relcppmem.cmo";
        (* A"-ppopt";A"./src/Memory.cmo";
        A"-ppopt";A"./src/.cmo";
        A"-ppopt";A"./src/Lang.cmo"; *)
        A"-ppopt";A"./camlp5/pa_cppmem.cmo";
      ]);

    ()
 | _ -> ()
)
