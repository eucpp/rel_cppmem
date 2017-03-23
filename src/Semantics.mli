
type t

type rule = (Lang.Context.ti -> Lang.Term.ti -> Memory.MemState.ti ->
             Lang.Context.ti -> Lang.Term.ti -> Memory.MemState.ti -> MiniKanren.goal)

val make : (string * rule) list -> t

(** Relational single step in given semantics *)
val stepo : t -> Lang.Term.ti -> Memory.MemState.ti -> Lang.Term.ti -> Memory.MemState.ti -> MiniKanren.goal

(** Relational step* *)
val spaceo : t -> Lang.Term.ti -> Memory.MemState.ti -> Lang.Term.ti -> Memory.MemState.ti -> MiniKanren.goal
