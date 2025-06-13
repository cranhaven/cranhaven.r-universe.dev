#==================================================================================================#
# Date: 14/04/2020
# Description:
# Function:
#==================================================================================================#
lists_ind = function(r,Zt,l,...){
  N = length(Zt)
  rj = matrix(nrow = 2,ncol = l)
  if (l == 1) {
    rj[,1] = c(-Inf,Inf)
  }else{
    rj[,1] = c(-Inf,r[1])
    rj[,l] = c(rev(r)[1],Inf)
  }
  if (l > 2) {for (i2 in 2:{l - 1}) {rj[,i2] = c(r[i2 - 1],r[i2])}}
  # indicator variable for the regime
  if (any(is.na(Zt))) {
    posZt = (1:N)[!is.na(Zt)]
  }else{
    posZt = 1:N
  }
  Ind = c()
  for (j in 1:l) {
    for (w in posZt) {
      if (Zt[w] > rj[1,j] & Zt[w] <= rj[2,j]) {
        Ind[w] = j
      }
    }
  }
  return(list(Ind = Ind))
}
tsregime = function(Yt, Zt = NULL, Xt = NULL, r = NULL){
  list_result = vector('list')
  if (!is.null(r)) {
    if (!is.numeric(r)) {stop('r must be a numeric vector')}
    l = length(r) + 1
    if (l > 2) {for (i in 1:{l - 2}) {
      if (r[i] >= r[i + 1]) {stop('r[i] must be smaller than r[i+1]')}}
    }
    list_result$l = l
    if (is.null(Zt)) {
      stop('Zt must be enter with threshold value')
    }
  }
  if (!is.numeric(Yt)) {
    stop('Yt must be a real matrix of dimension Nxk')
  }
  if (!is.matrix(Yt)) {Yt = as.matrix(Yt)}
  if (!is.null(Zt)) {
    if (!is.numeric(Zt)) {stop('Zt must be a real matrix of dimension Nx1')}
    if (!is.matrix(Zt)) {Zt = as.matrix(Zt)}
    if (nrow(Zt) != nrow(Yt)) {stop('Zt and Yt number of rows must match')}
  }else{l = 1}
  if (!is.null(Xt)) {
    if (!is.numeric(Xt)) {stop('Xt must be a real matrix of dimension Nx(nu+1)')}
    if (!is.matrix(Xt)) {Xt = as.matrix(Xt)}
    if (nrow(Xt) != nrow(Yt)) {stop('Xt and Yt number of rows must match')}
    nu = ncol(Xt)
    list_result$nu = nu
  }
  k = ncol(Yt)
  N = nrow(Yt)
  list_result$Yt = Yt
  list_result$Zt = Zt
  if (!is.null(Xt)) {
    list_result$Xt = Xt
  }
  if (any(is.na(Yt)) | any(is.na(Zt)) | any(is.na(Xt))) {
    message('Yt, Zt and Xt admit NA values use mtarmissing for estimation \n')
  }
  if (!is.null(r)) {
    list_result$r = r
    # Compute which regime belongs observations
    list_result$Ind = lists_ind(r,Zt,l)
    Table_r = data.frame('N_reg' = c(table(list_result$Ind)),
                         'Prop_reg' = 100*c(prop.table(table(list_result$Ind))))
    rownames(Table_r) = paste('Regim',1:l)
    list_result$Summary_r = Table_r
  }
  list_result$N = N
  list_result$k = k
  class(list_result) = 'tsregime'
  return(list_result)
}
