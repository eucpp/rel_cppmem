(* module SequentialConsistent :
  sig
    module State :
      sig
        include Semantics.State

        val init : regs:Lang.Register.ti list -> locs:Lang.Loc.ti list -> ti
      end

    module TLSNode : module type of Semantics.TLSNode(Lang.Term)(State)

    val evalo : (TLSNode.tt, TLSNode.tl) Semantics.eval
  end *)

module ReleaseAcquire :
  sig
    module State :
      sig
        include Semantics.State

        val init : regs:Lang.Register.ti list -> locs:Lang.Loc.ti list -> ti
      end

    module TLSNode : module type of Semantics.TLSNode(Lang.Term)(State)

    val evalo : (TLSNode.tt, TLSNode.tl) Semantics.eval
  end
