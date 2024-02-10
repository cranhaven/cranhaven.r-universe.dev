#' Internal function of ffphase
#' @noRd
onestep <- function(B,pik,EPS){

  kern <- MASS::Null(B)
  if(length(kern) == 0){
    return(NULL)
  }
  N <- length(pik)
  u = kern[,1]

  l1=min(pmax((1-pik)/u,-pik/u))
  l2=min(pmax((pik-1)/u,pik/u))

  if(stats::runif(1) < l2/(l1+l2)){
    l = l1;
  }else{
    l = -l2;
  }
  pik = pik + l*u

  return(pik);
}
