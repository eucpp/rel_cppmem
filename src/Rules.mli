module BasicExpr =  
  struct
    type t  = Lang.StmtContext.t
    type lt = Lang.StmtContext.lt

    type c  = Lang.StmtContext.c
    type lc = Lang.StmtContext.lc

    type s
    type ls

    type rule = (lc -> lt -> ls -> lc -> lt -> ls -> MiniKanren.goal)

    val varo   : rule
    val binopo : rule
  end

module BasicStmt : 
  sig 
    type t  = Lang.StmtContext.t
    type lt = Lang.StmtContext.lt

    type c  = Lang.StmtContext.c
    type lc = Lang.StmtContext.lc

    type s
    type ls

    type rule =  (lc -> lt -> ls -> lc -> lt -> ls -> MiniKanren.goal)
    
    val expro   : rule
    val asgno   : rule
    val ifo     : rule
    val repeato : rule
    val seqo    : rule
  end
