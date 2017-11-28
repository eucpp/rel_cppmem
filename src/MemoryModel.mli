module type State =
  sig
    include Utils.Logic

    val init : regs:Lang.Register.ti list -> locs:Lang.Loc.ti list -> ti

    val regso : ti -> Lang.ThreadID.ti -> Memory.RegisterStorage.ti -> MiniKanren.goal

    val checko : ti -> Lang.Loc.ti -> Lang.Value.ti -> MiniKanren.goal

    val transitiono : Lang.Label.ti -> ti -> ti -> MiniKanren.goal
  end

module type T =
  sig
    module State : State

    module Node : module type of Semantics.MakeConfig(Lang.Term)(State)

    type nt = Node.tt
    type nl

    type npred = (Node.tt, Node.tl) Semantics.tpred

    val intrpo : (Lang.Term.tt, State.tt, Node.tt, Lang.Term.tl, State.tl, Node.tl) Semantics.interpreter

    (* val evalo : (Node.tt, Node.tt, Node.tl, Node.tl) Semantics.eval *)
  end

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

module ReleaseAcquire : T
