module SequentialConsistent =
  struct
    module CFG = Semantics.Configuration(Lang.Term)(Machines.GlobalStore)

    type tt = CFG.tt
    type tl = CFG.tl
      and inner = CFG.inner
    type ti = CFG.ti

    let init ~regs ~locs p = CFG.cfg p (Machines.GlobalStore.preallocate regs locs)

    let terminal rc state = CFG.cfg rc state

    let inj = CFG.inj
    let reify = CFG.reify

    let pprint ff cfg =
      let prog, state = CFG.decompose cfg in
      Format.fprintf ff "@[<v>Return:@;<1 2>%a@;State:@;<1 2>%a@;@]" Lang.Term.pprint prog Machines.GlobalStore.pprint state

    module Basic = Rules.Basic(Machines.GlobalStore)
    module ThreadSpawning = Rules.ThreadSpawning(Machines.GlobalStore)
    module SequentialConsistent = Rules.SequentialConsistent(Machines.GlobalStore)

    let rules = List.concat [
      Basic.all;
      ThreadSpawning.all;
      SequentialConsistent.all;
    ]

    let stepo =
      Semantics.make_reduction_relation (CFG.lift_splitting Lang.splito) (CFG.lift_plugging Lang.plugo) rules

    let evalo = Semantics.make_eval stepo
  end

module ReleaseAcquire =
  struct
    module CFG = Semantics.Configuration(Lang.Term)(Machines.Front)

    type tt = CFG.tt
    type tl = CFG.tl
      and inner = CFG.inner
    type ti = CFG.ti

    let init ~regs ~locs p = CFG.cfg p (Machines.Front.preallocate regs locs)

    let terminal rc state = CFG.cfg rc state

    let inj = CFG.inj
    let reify = CFG.reify

    let pprint ff cfg =
      let prog, state = CFG.decompose cfg in
      Format.fprintf ff "@[<v>Return:@;<1 2>%a@;State:@;<1 2>%a@;@]" Lang.Term.pprint prog Machines.Front.pprint state

    module Basic = Rules.Basic(Machines.Front)
    module ThreadSpawning = Rules.ThreadSpawning(Machines.Front)
    module NonAtomic = Rules.NonAtomic(Machines.Front)
    module ReleaseAcquire = Rules.ReleaseAcquire(Machines.Front)

    let rules = List.concat [
      Basic.all;
      ThreadSpawning.all;
      NonAtomic.all;
      ReleaseAcquire.all;
    ]

    let stepo =
      Semantics.make_reduction_relation (CFG.lift_splitting Lang.splito) (CFG.lift_plugging Lang.plugo) rules

    let evalo = Semantics.make_eval stepo
  end
