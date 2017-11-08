type ('tt, 'tl) assertion = ('tt, 'tl) Semantics.Term.ti -> MiniKanren.goal

val verify : ?n : int -> ?succ_k : (unit -> unit) -> ?fail_k : (('tt, 'tl) MiniKanren.reified list -> unit) ->
  ('tt, 'tl) Semantics.eval -> ('tt, 'tl) assertion -> ('tt, 'tl) Semantics.Term.ti -> unit
