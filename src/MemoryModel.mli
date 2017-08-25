module ReleaseAcquire :
  sig
    include Utils.Logic

    val init : regs:Lang.Register.ti list -> locs:Lang.Loc.ti list ->  Lang.Term.ti -> ti

    val terminal : Lang.Term.ti -> Machines.Front.ti -> ti

    val evalo : (tt, tl) Semantics.eval
  end
