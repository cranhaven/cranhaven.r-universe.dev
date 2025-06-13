#==================================================================================================#
# Date: 14/04/2020
# Description:
# Function:
#==================================================================================================#
mtarNAIC = function(regimemodel){
  if (class(regimemodel) != 'regime_model') {
    stop('regimemodel must be an object of type (regime_model)')
  }
  l = length(regimemodel$regime)
  k = nrow(regimemodel$regime[[1]]$sigma)
  nuaux = NULL
  for (lj in 1:l) {
    nuaux[lj] = length(regimemodel$regime[[lj]]$beta[[1]][1,])
  }
  nu = max(nuaux)
  pj = qj = dj = vector(length = l)
  for (lj in 1:l) {
    pj[lj] = length(regimemodel$regime[[lj]]$phi)
    qj[lj] = length(regimemodel$regime[[lj]]$beta)
    dj[lj] = length(regimemodel$regime[[lj]]$delta)
  }
  etaj = 1 + pj*k + qj*nu + dj
  Nj = c(regimemodel$Nj)
  logLikj = as.numeric(regimemodel$logLikj)
  AICj = as.numeric(Nj*logLikj + 2*k*etaj,nrow = 1,row.names = NULL)
  NAIC = sum(AICj)/sum(Nj)
  message('NAIC=',round(NAIC,4),'\n')
  return(list(AICj = AICj,NAIC = NAIC))
}
