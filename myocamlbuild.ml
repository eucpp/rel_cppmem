open Ocamlbuild_plugin;;
module Pack = Ocamlbuild_pack

let () = dispatch (function
 | Before_rules ->
     ()

 | After_rules ->
     flag ["hack_pr_o"; "compile"] (S[A"-ppopt"; A"pr_o.cmo"]); 
   ()
 | _ -> ()
)
