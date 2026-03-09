erodibilityRisk=function(x){

  erodClass=ifelse(x>0.6,5,ifelse(x>0.3,4,ifelse(x>0.15,3,ifelse(x>0.075,2,1))))
  return(erodClass)
}
