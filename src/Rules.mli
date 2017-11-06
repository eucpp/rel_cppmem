
type rule :
  Lang.Label.ti -> Lang.Context.ti -> Lang.Term.ti -> Lang.Term.ti -> MiniKanren.goal

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
    val loado  : rule
    val storeo : rule

    val data_raceo  : rule

    val all : rule list
  end
