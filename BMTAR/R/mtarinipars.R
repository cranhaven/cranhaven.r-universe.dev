#==================================================================================================#
# Date: 14/04/2020
# Description: Functions for checking prior and other parameters for estimation
# Function:
#==================================================================================================#
mtarinipars = function(tsregime_obj,
                       list_model = list(pars = list(l = 2, orders = list(pj = c(1,1), qj = c(0,0), dj = c(0,0)),
                                                     r = NULL, Sigma = NULL),
                                         orders = NULL,l0_min = NULL,l0_max = NULL),
                       method = NULL, theta_prior = NULL, sigma_prior = NULL, gamma_prior = NULL,
                       r_prior = NULL){
  if (!inherits(tsregime_obj, 'tsregime')) {
    stop('tsregime_obj must be a tsregime object')
  }
  k = tsregime_obj$k
  nu = tsregime_obj$nu
  if (is.null(nu)) {nu = 0}
  if (!is.list(list_model)) {
    stop('list_model must be a list type object with pars, orders, l0_min or l0_max')
    message('If pars are unknown use mtarnumreg with l0_max maximum number of regimes','\n')
  }else{
    if (sum(names(list_model) %in% c('pars','orders','l0_min','l0_max')) == 0) {
      stop('list_model must be a list type object with pars, orders, l0_min or l0_max')
      message('If pars are unknown use mtarnumreg with l0_max maximum number of regimes','\n')
    }else{
      if (!is.null(list_model$orders)) {
        if (!is.list(list_model$orders)) {stop('list_model$orders must be a list type object with names pj(Not NULL),qj,dj')
        }else{
          if (is.null(list_model$orders$pj)) {stop('list_model$orders must have orders$pj a positive integer')}
          if (is.null(list_model$orders$qj)) {
            list_model$orders$qj = list_model$orders$pj*0
          }else{
            if (any(list_model$orders$qj != 0) & is.null(tsregime_obj$Xt)) {
              stop('For qj > 0 covariate process Xt must be in tsregime_obj')
            }
          }
          if (is.null(list_model$orders$dj)) {
            list_model$orders$dj = list_model$orders$pj*0
          }else{
            if (any(list_model$orders$dj != 0) & is.null(tsregime_obj$Zt)) {
              stop('For dj > 0 threshold process Zt must be in tsregime_obj')
            }
          }
        }
      }
      if (is.null(list_model$l0_max)) {
        if (!is.null(list_model$pars)) {
          if (!is.list(list_model$pars)) {
            stop('list_model$pars must be a list type object with l,Sigma,r or orders')
          }else{
            if (sum(names(list_model$pars) %in% c('l','Sigma','r','orders')) == 0) {
              stop('list_model$pars must be a list type object with l,Sigma,r or orders')
            }else{
              if (!is.null(list_model$pars$orders)) {
                if (!is.list(list_model$pars$orders)) {stop('list_model$pars$orders must be a list type object with names pj(Not NULL),qj,dj')
                }else{
                  if (is.null(list_model$pars$orders$pj)) {stop('list_model$pars$orders must have orders$pj a positive integer')}
                  if (is.null(list_model$pars$orders$qj)) {
                    list_model$pars$orders$qj = list_model$pars$orders$pj*0
                  }else{
                    if (any(list_model$pars$orders$qj != 0) & is.null(tsregime_obj$Xt)) {
                      stop('For qj > 0 covariate process Xt must be in tsregime_obj')
                    }
                  }
                  if (is.null(list_model$pars$orders$dj)) {
                    list_model$pars$orders$dj = list_model$pars$orders$pj*0
                  }else{
                    if (any(list_model$pars$orders$dj != 0) & is.null(tsregime_obj$Zt)) {
                      stop('For dj > 0 threshold process Zt must be in tsregime_obj')
                    }
                  }
                }
              }
              l = list_model$pars$l
              if (round(l) != l & l <= 0) {stop('l must be an integer greater than 0')}
              if (l > 4) {stop('l must be less than 4')}
            }
          }
        }
      }else{
        if (!is.null(list_model$pars) & is.list(list_model$pars)) {
          if ('l' %in% names(list_model$pars)) {
            stop('If l is known, l0_min and l0_max is not necesary')
          }
        }
        l = list_model$l0_max
        l1 = list_model$l0_min
        if (!is.null(l1)) {
          if (round(l1) != l1 & l1 <= 0) {stop('l0_min must be an integer greater than 0')}
          if (round(l) != l & l < l1) {stop('l0_max must be an integer greater than l0_min')}
        }else{
          if (round(l) != l & l <= 1) {stop('l0_max must be an integer greater than 1')}
        }
        if (l > 4) {stop('l0_max must be less than 4')}
        if (is.null(method)) {stop('For l unknown method must be KUO or SSVS')}
        if (!is.null(r_prior)) {
          if (!is.list(r_prior)) {
            stop('r_prior must be a list type object with names za, zb or val_rmh')
          }else{
            if (!all(names(r_prior) %in% c('za','zb','val_rmh'))) {
              stop('r_prior must be a list type object with names za, zb or val_rmh')
            }
            if (!is.null(r_prior$za)) {
              if (is.numeric(r_prior$za) & length(r_prior$za) == 1) {
                if (!{r_prior$za > 0 & r_prior$za < 1}) {
                  stop('r_prior$za must be between 0 and 1. Note: suggestion not less than 0.2')
                }
              }else{stop('r_prior$za must be a real number between 0 and 1')}
            }else{r_prior$za = NULL}
            if (!is.null(r_prior$zb)) {
              if (is.numeric(r_prior$zb) & length(r_prior$zb) == 1) {
                if (!{r_prior$zb > 0 & r_prior$zb < 1}) {
                  stop('r_prior$zb must be between 0 and 1. Note: suggestion not less than 0.8')
                }
              }else{stop('r_prior$zb must be a real number between 0 and 1')}
            }else{r_prior$zb = NULL}
            if (!is.null(r_prior$val_rmh)) {
              if (is.numeric(r_prior$val_rmh) & length(r_prior$val_rmh) == 1 & abs(r_prior$val_rmh) > 0 & abs(r_prior$val_rmh) < 1) {
              }else{stop('abs(r_prior$val_rmh) must be a real number between 0 and 1')}
            }else{
              r_prior$val_rmh = 0.00375
            }
          }
        }else{
          r_prior$za = NULL
          r_prior$zb = NULL
          r_prior$val_rmh = 0.00375
        }
        message('If pars are unknown use mtarnumreg with l0_max maximum number of regimes','\n')
        listf = list(tsregime_obj = tsregime_obj, l0_min = list_model$l0_min,l0_max = list_model$l0_max,method = method,init = list(r = r_prior))
        class(listf) = 'regime_inipars'
        return(listf)
      }
    }
  }
  if (!all(is.null(r_prior$zb),is.null(r_prior$za))) {
    if (r_prior$za >= r_prior$zb) {
      stop('za must be less than zb')
    }
  }
  # list_model
  #orders
  if (is.null(list_model$l0_min) & is.null(list_model$l0_max)) {
    if (!is.null(list_model$pars$orders)) {
      orders = list_model$pars$orders
      if (!is.null(method)) {message('For orders known, method is not necesary','\n')}
      method = 'ns'
    }else{
      if (!is.null(list_model$orders)) {orders = list_model$orders}else{
        stop('For orders unknown they must be enter maximum orders in list_model$orders')
      }
      if (is.null(method)) {stop('For orders unknown method must be KUO or SSVS')}
    }
  }else{
    orders = list(pj = NULL,qj = NULL,dj = NULL)
    message('For l unknown, orders are not necesary')
  }
  if (!is.list(orders) | length(orders) != 3) {
    stop('orders must be a list with names pj (Not NULL), qj or dj')
  }else if (!{all(names(orders) %in% c('pj','qj','dj'))}) {
    stop('orders must be a list with names pj (Not NULL), qj or dj')
  }
  pj = orders$pj
  qj = orders$qj
  dj = orders$dj
  if (!is.null(pj) & !is.null(dj) & !is.null(qj)) {
    if (is.vector(pj) & is.vector(qj) & is.vector(dj)) {
      if (!{length(pj) == l & length(qj) == l & length(dj) == l}) {
        stop('pj qj and dj must have length l')
      }else{
        for (lj in 1:l) {
          if (!{round(pj[lj]) == pj[lj] & pj[lj] >= 0}) {stop('pj must be a positive integer or 0 for each regime')}
          if (!{round(qj[lj]) == qj[lj] & qj[lj] >= 0}) {stop('qj must be a positive integer or 0 for each regime')}
          if (!{round(dj[lj]) == dj[lj] & dj[lj] >= 0}) {stop('dj must be a positive integer or 0 for each regime')}
          if (pj[lj] > 5) {stop('pj must be smaller or 5 for each regime')}
          if (qj[lj] > 5) {stop('qj must be smaller or 5 for each regime')}
          if (dj[lj] > 5) {stop('dj must be smaller or 5 for each regime')}
        }
      }
    }else{stop('pj qj and dj must be of numeric type')}
  }
  eta = 1 + pj*k + qj*nu + dj
  #
  # Validar Sigma
  Sigma = list_model$pars$Sigma
  if (!is.null(Sigma)) {
    if (!is.list(Sigma)) {
      stop(paste('Sigma must be a list of length l with names', paste0('R',1:l,collapse = ', '),'of real positive matrix of dimension',k,'x',k))
    }else{
      if (length(Sigma) != l) {
        stop(paste('Sigma must be a list of length l with names', paste0('R',1:l,collapse = ', '),'of real positive matrix of dimension',k,'x',k))
      }else{
        if (all(names(Sigma) %in% paste0('R',1:l))) {
          for (lj in 1:l) {
            if (is.numeric(Sigma[[paste0('R',lj)]])) {
              if (!is.matrix(Sigma[[paste0('R',lj)]])) {stop(paste0('Sigma$R',lj,' must be a matrix type object'))}
              vl = sum(dim(Sigma[[paste0('R',lj)]]) == c(k,k))
              if (vl != 2) {stop(paste0('Sigma$R',lj,' must be a real positive matrix of dimension ',k,' x ',k))}
              vl = sum(eigen(Sigma[[paste0('R',lj)]])$values >= 0)
              if (vl != k) {stop(paste0('Sigma$R',lj,' must be a real positive matrix of dimension ',k,' x ',k))}
            }else{stop(paste0('Sigma$R',lj,' must be a real positive matrix of dimension ',k,' x ',k))}
          }
        }else{stop(paste('Sigma must be a list of length l with names', paste0('R',1:l,collapse = ', '),'of real positive matrix of dimension',k,'x',k))}
      }
    }
  }
  # r
  r = list_model$pars$r
  if (is.null(r)) {
    if (l > 1 & is.null(tsregime_obj$Zt)) {stop('Threshold process Zt must be enter')}
    if (!is.null(r_prior)) {
      if (!is.list(r_prior)) {
        stop('r_prior must be a list type object with names za, zb or val_rmh')
      }else{
        if (sum(names(r_prior) %in% c('za','zb','val_rmh')) == 0) {
          stop('r_prior must be a list type object with names za, zb or val_rmh')
        }
        if (!is.null(r_prior$za)) {
          if (is.numeric(r_prior$za) & length(r_prior$za) == 1) {
            if (!{r_prior$za > 0 & r_prior$za < 1}) {
              stop('r_prior$za must be between 0 and 1. Note: suggestion not less than 0.2')
            }
          }else{stop('r_prior$za must be a real number between 0 and 1')}
        }else{r_prior$za = NULL}
        if (!is.null(r_prior$zb)) {
          if (is.numeric(r_prior$zb) & length(r_prior$zb) == 1) {
            if (!{r_prior$zb > 0 & r_prior$zb < 1}) {
              stop('r_prior$zb must be between 0 and 1. Note: suggestion not less than 0.8')
            }
          }else{stop('r_prior$zb must be a real number between 0 and 1')}
        }else{r_prior$zb = NULL}

        if (!is.null(r_prior$val_rmh)) {
          if (is.numeric(r_prior$val_rmh) & length(r_prior$val_rmh) == 1 & abs(r_prior$val_rmh) > 0 & abs(r_prior$val_rmh) < 1) {
          }else{stop('abs(r_prior$val_rmh) must be a real number between 0 and 1')}
        }else{
          r_prior$val_rmh = 0.00375
        }
      }
    }else{
      r_prior$za = NULL
      r_prior$zb = NULL
      r_prior$val_rmh = 0.00375
    }
  }else{
    if (l == 1) {stop('One regime must not have threshold')
    }else{
      if (length(r) != {l - 1}) {stop('r known must be of length l-1')}
      if (is.null(tsregime_obj$Zt)) {stop('Zt must be enter with threshold value')}
    }
  }
  # validar iniciales
  #THETA
  if (!is.null(theta_prior)) {
    if (length(theta_prior) <= l) {
      if (sum(names(theta_prior) %in% paste0('R',1:l)) != 0) {
        for (lj in 1:l) {
          if (!is.null(theta_prior[[paste0('R',lj)]])) {
            if (!is.list(theta_prior[[paste0('R',lj)]])) {
              stop(paste0('theta_prior$R',lj,' must be a list type object with names theta0j or cov0j'))
            }else{
              if (sum(c('theta0j','cov0j') %in% names(theta_prior[[paste0('R',lj)]])) == 0) {
                stop(paste0('theta_prior$R',lj,' must be a list type object with names theta0j or cov0j'))
              }else{
                if (!is.null(theta_prior[[paste0('R',lj)]]$theta0j)) {
                  if (is.numeric(theta_prior[[paste0('R',lj)]]$theta0j)) {
                    if (!is.matrix(theta_prior[[paste0('R',lj)]]$theta0j)) {stop(paste0('theta_prior$R',lj,'$theta0j must be a matrix type object'))}
                    vl = sum(dim(theta_prior[[paste0('R',lj)]]$theta0j) == c(k*eta[lj],1))
                    if (vl != 2) {stop(paste0('theta_prior$R',lj,'$theta0j must be a matrix of dimension ',(k*eta[lj]),' x 1'))}
                  }else{stop(paste0('theta_prior$R',lj,'$theta0j must be a real positive matrix of dimension',(k*eta[lj]),'x 1'))}
                }else{
                  theta_prior[[paste0('R',lj)]]$theta0j = rep(0,k*eta[lj])
                }
                if (!is.null(theta_prior[[paste0('R',lj)]]$cov0j)) {
                  if (is.numeric(theta_prior[[paste0('R',lj)]]$cov0j)) {
                    if (!is.matrix(theta_prior[[paste0('R',lj)]]$cov0j)) {stop(paste0('theta_prior$R',lj,'$cov0j must be a matrix type object'))}
                    vl = sum(dim(theta_prior[[paste0('R',lj)]]$cov0j) == c(k*eta[lj],k*eta[lj]))
                    if (vl != 2) {stop(paste0('theta_prior$R',lj,'$cov0j must be a matrix of dimension ',(k*eta[lj]),' x ',k*eta[lj]))}
                    vl = sum(eigen(theta_prior[[paste0('R',lj)]]$cov0j)$values >= 0)
                    if (vl != (k*eta[lj])) {stop(paste0('theta_prior$R',lj,'$cov0j must be a real positive matrix of dimension ',k*eta[lj],' x' ,k*eta[lj]))}
                  }
                }else{
                  theta_prior[[paste0('R',lj)]]$cov0j = diag(k*eta[lj])
                }
              }
              if (method == "KUO" & !all(names(theta_prior[[paste0('R',lj)]]) %in% c('theta0j','cov0j'))) {
                stop(paste0('theta_prior$R',lj,' must be a list type object with names theta0j or cov0j only'))}
              if (method == "SSVS" & !all(names(theta_prior[[paste0('R',lj)]]) %in% c('theta0j','cov0j','Cij','Tauij','R'))) {
                stop(paste0('theta_prior$R',lj,' must be a list type object with names theta0j, cov0j, Cij, Tauij or R'))
              }else{
                Cij = theta_prior[[paste0('R',lj)]]$Cij
                Tauij = theta_prior[[paste0('R',lj)]]$Tauij
                R = theta_prior[[paste0('R',lj)]]$R
                if (!is.null(Cij)) {
                  if (length(Cij) != k*eta[lj]) {stop(paste0('theta_prior$R',lj,'$Cij must be a vector of length ',k*eta[lj]))}
                }else{
                  theta_prior[[paste0('R',lj)]]$Cij = rep(25,k*eta[lj])
                }
                if (!is.null(Tauij)) {
                  if (length(Tauij) != k*eta[lj]) {stop(paste0('theta_prior$R',lj,'$Tauij must be a vector of length ',k*eta[lj]))}
                }else{
                  if (l == 2) {theta_prior[[paste0('R',lj)]]$Tauij = rep(1.25,k*eta[lj])
                  }else{theta_prior[[paste0('R',lj)]]$Tauij = rep(1.5,k*eta[lj])}
                }
                if (!is.null(R)) {
                  if (is.numeric(R)) {
                    if (sum(dim(R) == c(k*eta[lj],k*eta[lj])) != 2) {stop(paste0('theta_prior$R',lj,'$R must be a matrix of dimension ',k*eta[lj]))}
                    vl = sum(eigen(R)$values >= 0)
                    if (vl != k*eta[lj]) {stop(paste0('theta_prior$R',lj,'$R must be a real positive matrix of dimension ',k*eta[lj]))}
                  }else{stop(paste0('theta_prior$R',lj,'$R must be a list with l real positive matrix of dimension ',k*eta[lj]))}
                }else{
                  theta_prior[[paste0('R',lj)]]$R = diag(k*eta[lj])
                }
              }
            }
          }
        }
      }else{
        stop(paste('theta_prior must be a list type object of length l or less with any name',paste0('R',1:l,collapse = ', ')))
      }
    }else{stop(paste('theta_prior must be a list type object of length l or less with any name',paste0('R',1:l,collapse = ', ')))}
  }else{
    theta_prior = vector('list')
    for (lj in 1:l) {
      theta_prior[[paste0('R',lj)]] = vector('list')
      theta_prior[[paste0('R',lj)]]$theta0j = rep(0,k*eta[lj])
      theta_prior[[paste0('R',lj)]]$cov0j = diag(k*eta[lj])
      if (method == 'SSVS') {
        theta_prior[[paste0('R',lj)]]$Cij = rep(25,k*eta[lj])
        if (l == 2) {theta_prior[[paste0('R',lj)]]$Tauij = rep(1.25,k*eta[lj])
        }else{theta_prior[[paste0('R',lj)]]$Tauij = rep(1.5,k*eta[lj])}
        theta_prior[[paste0('R',lj)]]$R = diag(k*eta[lj])
      }
    }
  }
  #SIGMA
  if (!is.null(sigma_prior)) {
    if (!is.list(sigma_prior)) {
      stop(paste('sigma_prior must be a list type object of length l or less with names',paste0('R',1:l,collapse = ', ')))
    }else{
      if (length(sigma_prior) <= l) {
        if (sum(names(sigma_prior) %in% paste0('R',1:l)) != 0) {
          for (lj in 1:l) {
            if (!is.null(sigma_prior[[paste0('R',lj)]])) {
              if (!is.list(sigma_prior[[paste0('R',lj)]])) {
                stop(paste0('sigma_prior$R',lj,' must be a list type object with names S0j or nu0j only'))
              }else{
                if (sum(names(sigma_prior[[paste0('R',lj)]]) %in% c('S0j','nu0j')) != 0) {
                  if (!is.null(sigma_prior[[paste0('R',lj)]]$S0j)) {
                    if (is.numeric(sigma_prior[[paste0('R',lj)]]$S0j)) {
                      if (!is.matrix(sigma_prior[[paste0('R',lj)]]$S0j)) {
                        stop(paste0('sigma_prior$R',lj,'$S0j must be a real positive matrix type object of dimension ',k,' x ',k))
                      }
                      vl = sum(dim(sigma_prior[[paste0('R',lj)]]$S0j) == c(k,k))
                      if (vl != 2) {stop(paste0('sigma_prior$R',lj,'$S0j must be a real positive matrix of dimension ',k,' x ',k))}
                      vl = sum(eigen(sigma_prior[[paste0('R',lj)]]$S0j)$values >= 0)
                      if (vl != k) {stop(paste0('sigma_prior$R',lj,'$S0j must be a real positive matrix of dimension ',k,' x ',k))}
                    }else{stop(paste0('sigma_prior$R',lj,'$S0j must be a real positive matrix of dimension ',k,' x ',k))}
                  }else{
                    sigma_prior[[paste0('R',lj)]]$S0j = diag(k)
                  }
                  if (!is.null(sigma_prior[[paste0('R',lj)]]$nu0j)) {
                    if (is.numeric(sigma_prior[[paste0('R',lj)]]$nu0j) & length(sigma_prior[[paste0('R',lj)]]$nu0j) == 1) {
                      if (!{round(sigma_prior[[paste0('R',lj)]]$nu0j) == sigma_prior[[paste0('R',lj)]]$nu0j & sigma_prior[[paste0('R',lj)]]$nu0j >= k}) {
                        stop(paste0('sigma_prior$R',lj,'$nu0j must be an integer greater or equal ',k))
                      }
                    }else{stop(paste0('sigma_prior$R',lj,'$nu0j must be an integer greater or equal ', k))}
                  }else{
                    sigma_prior[[paste0('R',lj)]]$nu0j = k
                  }
                }else{stop(paste0('sigma_prior$R',lj,' must be a list type object with names S0j or nu0j'))}
              }
            }
          }
        }else{stop(paste('sigma_prior must be a list type object of length l or less with any name',paste0('R',1:l,collapse = ', ')))}
      }else{stop(paste('sigma_prior must be a list type object of length l or less with any name',paste0('R',1:l,collapse = ', ')))}
    }
  }else{
    sigma_prior = vector('list')
    for (lj in 1:l) {
      sigma_prior[[paste0('R',lj)]] = vector('list')
      sigma_prior[[paste0('R',lj)]]$S0j = diag(k)
      sigma_prior[[paste0('R',lj)]]$nu0j = k
    }
  }
  #GAMMA
  if (method %in% c('KUO','SSVS')) {
    if (!is.null(gamma_prior)) {
      if (!is.list(gamma_prior)) {stop(paste('gamma_prior must be a list type object of length l or less with any name',paste0('R',1:l,collapse = ', ')))
      }else{
        if (sum(names(gamma_prior) %in% paste0('R',1:l)) != 0) {
          if (length(gamma_prior) > l) {stop(paste('gamma_prior must be a list type object of length l or less with any name',paste0('R',1:l,collapse = ', ')))
          }else{
            for (lj in 1:l) {
              if (!is.null(gamma_prior[[paste0('R',lj)]])) {
                if (length(gamma_prior[[paste0('R',lj)]]) != k*eta[lj]) {stop(paste0('gamma_prior$R',lj,' must be a vector of length ',k*eta[lj]))}
                if (sum(gamma_prior[[paste0('R',lj)]] >= 0 & gamma_prior[[paste0('R',lj)]] <= 1) != k*eta[lj]) {
                  stop(paste0('gamma_prior$R',lj,' values must be between 0 and 1'))}
              }else{
                gamma_prior[[paste0('R',lj)]] = rep(0.5,k*eta[lj])
              }
            }
          }
        }else{
          stop(paste('gamma_prior must be a list type object of length l or less with any name',paste0('R',1:l,collapse = ', ')))
        }
      }
    }else{
      gamma_prior = vector('list')
      for (lj in 1:l) {
        gamma_prior[[paste0('R',lj)]] = rep(0.5,k*eta[lj])
      }
    }
  }
  # exits
  if (method %in% c('KUO','SSVS')) {
    listf = list(tsregime_obj = tsregime_obj, pars = list_model$pars, orders = list_model$orders,method = method,
                 init = list(r = r_prior, Theta = theta_prior, Sigma = sigma_prior, Gamma = gamma_prior))
  }else{
    if (!is.null(Sigma) & is.null(r)) {
      listf = list(tsregime_obj = tsregime_obj, pars = list_model$pars,init = list(r = r_prior, Theta = theta_prior))
    }else if (is.null(Sigma) & !is.null(r)) {
      listf = list(tsregime_obj = tsregime_obj, pars = list_model$pars,init = list(Theta = theta_prior, Sigma = sigma_prior))
    }else if (is.null(Sigma) & is.null(r)) {
      listf = list(tsregime_obj = tsregime_obj, pars = list_model$pars,init = list(r = r_prior, Theta = theta_prior, Sigma = sigma_prior))
    }else if (!is.null(Sigma) & !is.null(r)) {
      listf = list(tsregime_obj = tsregime_obj, pars = list_model$pars,init = list(Theta = theta_prior))
    }
  }
  class(listf) = 'regime_inipars'
  return(listf)
}


