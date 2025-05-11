combine_lists <-
  function(...){
    comb_lists <- c(...)
    # Precedence is given to named elements of lists earliest in "..."
    # containing those elements.
    return(comb_lists[which(!duplicated(names(comb_lists)))])
  }
