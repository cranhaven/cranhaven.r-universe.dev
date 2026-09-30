normalizeqntadd <- function(abo,qnt.scale){
      ###############################################
      
      x   = affy::pm(abo)
      ord = sort.list(x)
      pm(abo) = qnt.scale[sort.list(ord)]
      return(abo)
      
}
