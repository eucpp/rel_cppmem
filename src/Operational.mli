
module SequentialConsistent :
  sig
    include Lang.MemoryModel
  end

module ReleaseAcquire :
  sig
    include Lang.MemoryModel
  end
