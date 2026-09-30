baaddon <-
function(params, x, batch) {

  if(!(class(params) %in% c("fabatch", "combat", "svatrain", "meancenter",
    "standardize", "ratioa", "ratiog", "noba")))
    stop("Invalid class of 'params'.")
  
  if(inherits(params, "fabatch")) {
    return(fabatchaddon(params, x, batch))
  }
  if(inherits(params, "combat")) {
    return(combatbaaddon(params, x, batch))
  }
  if(inherits(params, "svatrain")) {
    return(svabaaddon(params, x))
  }
  if(inherits(params, "meancenter")) {
    return(meancenteraddon(params, x, batch))
  }
  if(inherits(params, "standardize")) {
    return(standardizeaddon(params, x, batch))
  }
  if(inherits(params, "ratioa")) {
    return(ratioaaddon(params, x, batch))
  }
  if(inherits(params, "ratiog")) {
    return(ratiogaddon(params, x, batch))
  }
  if(inherits(params, "noba")) {
    return(nobaaddon(params, x, batch))
  }

}
