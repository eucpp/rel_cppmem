module Context :
  sig
    type tt
    type tl = inner MiniKanren.logic
      and inner
    type ti = (tt, tl) MiniKanren.injected

    val context : Lang.Context.ti -> Memory.RegisterStorage.ti -> ti

    val tlso    : ti -> Memory.RegisterStorage.ti -> MiniKanren.goal
    val thrdIdo : ti -> ThreadID.ti               -> MiniKanren.goal
  end

type rule =
  Lang.Label.ti -> Context.ti -> Lang.Term.ti -> Lang.Term.ti -> MiniKanren.goal

module Basic :
  sig
    val varo    : rule
    val binopo  : rule
    val asgno   : rule
    val ifo     : rule
    val repeato : rule
    val whileo  : rule
    val seqo    : rule
    val asserto : rule

    val all : rule list
  end

module ThreadSpawning :
  sig
    val spawno  : rule
    val joino   : rule

    val all : rule list
  end

module Atomic :
  sig
    val loado     : rule
    val storeo    : rule
    val dataraceo : rule

    val all : rule list
  end
