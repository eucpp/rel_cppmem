module Option : 
  sig
    exception No_value
    
    val is_some : 'a option -> bool
    val is_none : 'a option -> bool
    val get : 'a option -> 'a    
  end
