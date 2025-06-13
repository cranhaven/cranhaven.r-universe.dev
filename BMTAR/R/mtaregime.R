#==================================================================================================#
# Date: 14/04/2020
# Coments:
#-> k was taken by default by Sigma dimensions
# Function:
# Function to repeat matrix
#==================================================================================================#
repM = function(M,r){lapply(rep(0,r),function(x){x*M})}
mtaregime = function(orders = list(p = 1,q = 0,d = 0), cs = NULL,
                    Phi, Beta = NULL, Delta = NULL, Sigma){
  if (is.numeric(Sigma) & !is.matrix(Sigma)) {Sigma = as.matrix(Sigma)}
  if (is.numeric(cs) & !is.matrix(cs)) {cs = as.matrix(cs)}
  if (!is.list(orders)) {
    stop('orders must be a list with names p (Not NULL), q or d')
  }else if (!any(names(orders) %in% c('p','q','d'))) {
    stop('orders must be a list with names p (Not NULL), q or d')
  }
  if (is.null(orders$p)) {stop('orders must have orders$p a positive integer')}
  p = orders$p
  q = ifelse(is.null(orders$q),0,orders$q)
  d = ifelse(is.null(orders$d),0,orders$d)
  # Validation of values
  ## structural parameters
  if (!{round(p) == p & p >= 0}) {stop('p must be a positive integer')}
  if (!{round(q) == q & q >= 0}) {stop('q must be a positive integer or 0')}
  if (!{round(d) == d & d >= 0}) {stop('d must be a positive integer or 0')}
  # set matrix dimensions
  k = ncol(Sigma)
  ## non-structural parameters
  if (!is.list(Phi)) {
    stop('Phi must be a list of real matrix of dimension kxk')
  }else{
    for (i in 1:length(Phi)) {
      if (is.numeric(Phi[[i]]) & !is.matrix(Phi[[i]])) {Phi[[i]] = as.matrix(Phi[[i]])}
      vl = all((is.numeric(Phi[[i]]) & {dim(Phi[[i]]) == c(k,k)}))
      if (!vl) {stop('Phi must be a list of real matrix of dimension kxk')}
      if (!is.matrix(Phi[[i]])) {stop('Phi[[i]] must be a matrix type object')}
      if (substr(names(Phi[i]),1,3) != 'phi' | !{as.numeric(substr(names(Phi[i]),4,4)) %in% c(1:p)}) {
        stop('names  in the list Phi must be \'phii\' with a integer i in 1:p')
      }
    }
    if (max(as.numeric(sapply(names(Phi),substr,4,4))) != p) {
      stop('p and Phi max order must match')
    }
  }
  if (!is.null(Beta)) {
    if (!is.list(Beta)) {
      stop('Beta must be a list of real matrix of dimension kxnu')
    } else{
      if (is.matrix(Beta[[1]])) {nu = ncol(Beta[[1]])
      }else{stop('Beta must be a list of real matrix of dimension kxnu')}
      for (i in 1:length(Beta)) {
        if (is.numeric(Beta[[i]]) & !is.matrix(Beta[[i]])) {Beta[[i]] = as.matrix(Beta[[i]])}
        if (!is.matrix(Beta[[i]])) {stop('Beta[[i]] must be a matrix type object')}
        vl = all(is.numeric(Beta[[i]]) & {dim(Beta[[i]]) == c(k,nu)})
        if (!vl) {stop('Beta must be a list of real matrix of dimension kxnu')}
        if (substr(names(Beta[i]),1,4) != 'beta' | !{
          as.numeric(substr(names(Beta[i]),5,5)) %in% c(1:q)}) {
          stop('names  in the list Beta must be \'betai\' with a integer i in 1:q')
        }
      }
      if (max(as.numeric(sapply(names(Beta),substr,5,5))) != q) {
        stop('q and Beta max order must match')
      }
    }
  }else{
    nu = 0
    if (q > 0) {stop('q and Beta max order must match')}}
  if (!is.null(Delta)) {
    if (!is.list(Delta)) {
      stop('Delta must be a list of real matrix of dimension kx1')
    } else{
      for (i in 1:length(Delta)) {
        if (is.numeric(Delta[[i]]) & !is.matrix(Delta[[i]])) {Delta[[i]] = as.matrix(Delta[[i]])}
        vl = all(is.numeric(Delta[[i]]) & {dim(Delta[[i]]) == c(k,1)})
        if (!vl) {stop('Delta must be a list of real matrix of dimension kx1')}
        if (!is.matrix(Delta[[i]])) {stop('Delta[[i]] must be a matrix type object')}
        if (substr(names(Delta[i]),1,5) != 'delta' | !{
          as.numeric(substr(names(Delta[i]),6,6)) %in% c(1:d)}) {
          stop('names  in the list Delta must be \'deltai\' with a integer i in 1:q')
        }
      }
      if (max(as.numeric(sapply(names(Delta),substr,6,6))) != d) {
        stop('d and Delta max order must match')
      }
    }
  } else if (d > 0) {
    stop('Delta must be a list of real matrix of dimension kx1')
  }
  if (!is.null(cs)) {
    if (!is.matrix(cs)) {
      stop('cs must be a matrix type object')
    }else{
      vl = all(is.numeric(cs) & {dim(cs) == c(k,1)})
      if (!vl) {stop('cs must be a real matrix of dimension kx1')}
    }
  }
  if (!is.matrix(Sigma)) {
    stop('Sigma must be a matrix type object')
  }else if (is.numeric(Sigma)) {
    vl = all(dim(Sigma) == c(k,k))
    if (!vl) {stop('Sigma must be a real positive matrix of dimension kxk')}
    vl = all(eigen(Sigma)$values >= 0)
    if (!vl) {stop('Sigma must be a real positive matrix of dimension kxk')}
  }else{
    stop('Sigma must be a real positive matrix of dimension kxk')
  }
  # Create a list of regimes
  Ri = vector('list')
  if (is.numeric(cs) & !is.matrix(cs)) {Sigma = as.matrix(cs)}
  if (!is.null(cs)) {
    cs = cs
  }else{
    cs = rep(0,k)
  }
  Ri$cs = cs
  Ri$phi = vector('list', p)
  names(Ri$phi) = paste0('phi',1:p)
  Ri$phi[names(Phi)] = Phi
  Ri$phi[names(Ri$phi)[!(names(Ri$phi) %in% names(Phi))]] = repM(matrix(0,k,k),
                                                             sum(!(names(Ri$phi) %in% names(Phi))))
  if (q != 0) {
    Ri$beta = vector('list', q)
    names(Ri$beta) = paste0('beta',1:q)
    Ri$beta[names(Beta)] = Beta
    Ri$beta[names(Ri$beta)[!(names(Ri$beta) %in%
                               names(Beta))]] = repM(matrix(0,k,nu),
                                                     sum(!(names(Ri$beta) %in% names(Beta))))
  }
  if (d != 0) {
    Ri$delta = vector('list', d)
    names(Ri$delta) = paste0('delta',1:d)
    Ri$delta[names(Delta)] = Delta
    Ri$delta[names(Ri$delta)[!(names(Ri$delta) %in%
                                 names(Delta))]] = repM(matrix(0,k,1),
                                                        sum(!(names(Ri$delta) %in% names(Delta))))
  }
  Ri$sigma = Sigma
  # creation of object type regime
  class(Ri) = 'regime'
  return(Ri)
}
