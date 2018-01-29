
module EventID :
  sig
    include Utils.Logic
  end

module EventSet :
  sig
    include Utils.Logic
  end

module Ord :
  sig
    include Utils.Logic
  end

module PO : module type of Ord

module PreExecution :
  sig
    include Lang.MemoryModel
  end

module SequentialConsistent :
  sig
    val consistento : PreExecution.ti -> MiniKanren.goal
  end
