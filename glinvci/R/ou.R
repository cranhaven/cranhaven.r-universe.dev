#' Parameterisation functions of Ornstein-Uhlenbeck model
#'
#' \code{oupar} is a function that maps from the Ornstein-Uhlenbeck model
#' parameters to the Gaussian parametersation.
#'
#' By multivariate Ornstein-Uhlenbeck process, we mean
#'     \deqn{dx(t) = -H(x(t) - \theta)dt + \Sigma_x dW(t)}
#' where \eqn{H} is a \eqn{k}-by-\eqn{k} matrix with real entries,
#' \eqn{\theta} is any real \eqn{k}-vector, \eqn{\Sigma_x} is a
#' lower-triangular matrix, \eqn{W(t)} is the Brownian motion process.
#' The parameters of this model is \eqn{(H,\theta,\Sigma_x)},
#' therefore \eqn{k^2+k+k(k+1)/2} dimensional.
#'
#' This package uses parameterisation \eqn{(H,\theta,\Sigma_x')}, where
#' \eqn{H} and \eqn{\theta} is the same as above defined, and \eqn{\Sigma_x'}
#' is the lower-triangular part of \eqn{\Sigma_x}, except that, only on diagonal
#' entries, \eqn{\Sigma_x'=log(\Sigma_x)}. The use of logarithm is for
#' eliminating multiple local maxima in the log-likelihood.
#'
#' The \code{par} arguemnt is the concatenation of column-major-flattened
#' \eqn{H}, \eqn{\theta}, and the column-major-flattened lower-triangular part
#' of \eqn{\Sigma_x'}.
#'
#' @param par     A numeric vector containing the joint vector of the
#'                Ornstein-Uhlenbeck drift matrix, long-term mean,
#'                and volitality matrix, which is a lower-triangular
#'                Cholesky factor.
#' @param t       Branch length of the currently processing node.
#' @param ...     Unused in these functions. Their existence is needed because
#'                \code{\link{lik.glinv}} etc. always pass us four arguments.
#'                See \code{\link{lik.glinv}} for details.
#' @return        \code{oupar} returns the a vector of concatenated \eqn{(\Phi, w, V')},
#'                where \eqn{V'} is the lower triangular part of \eqn{V}. \code{oujac}
#'                returns the Jacobian matrix of \code{oupar}. \code{ouhess} returns
#'                a list of three 3D arrays, named \code{Phi}, \code{w}, \code{V} respectively inside the list, in which
#'                \code{ouhess(...)$Phi[m,i,j]} contains
#'                the cross second-order partial derivative of \eqn{\Phi_m} (here we treat the matrix \eqn{\Phi} as a
#'                column-major-flattened vector) with respect to the \eqn{i}-th and\eqn{j}-th user parameters;
#'                and \code{ouhess(...)$w[m,i,j]} and \code{((parhess[[i]])(...))$V[m,i,j]}
#'                analogously contains second-order derivative of \eqn{w_m} and \eqn{V'_m}.
#' 
#' @export
oupar = function (par, t, ...) {
  ## L == k*k+k+k*(k+1)/2 => 3(k*k) + 3k - 2*L == 0
  ## outfmt="vector"
  opts = list(...)
  if (is.null(opts[['outfmt']])) outfmt = 'vector'
  if (is.numeric(par)) {
    k     = sqrt(9+24*length(par))/6-1/2
    H     = par[1L:(k*k)]
    theta = par[(k*k+1L):(k*k+k)]
    sig_x = par[(k*k+k+1L):length(par)]
  } else if (is.list(par)) {
    k     = length(theta)
    H     = c(par[['H']])
    theta = c(par[['theta']])
    sig_x = c(par[['sig_x']])
    if (! ((is.numeric(H)     && length(H) == k*k) ||
            is.numeric(theta) && length(theta) == k||
            is.numeric(sig_x) && length(sig_x) == (k*(k+1L))%/%2L))
      stop("Invalid argument: `par` is malformed")
  } else  stop('`par` must be either a list or a numeric vector')
  if      (identical(outfmt, 'vector')) vecfmt=T
  else if (identical(outfmt, 'list'))   vecfmt=F
  else    stop('outfmt must be one of "vector" or "list"')
  ;                      mode(H)       = 'double'
  ;                      mode(t)       = 'double'
  ;                      mode(theta)   = 'double'
  ;                      mode(sig_x)   = 'double'
  ;                      mode(k)       = 'integer'
  V = numeric((k*(k+1L))%/%2L);mode(V) = 'double'
  w = matrix(0,k);       mode(w)       = 'double'
  Phi = matrix(0,k,k);   mode(Phi)     = 'double'
  P = matrix(0,k,k);     mode(P)       = 'complex'
  invP = matrix(0,k,k);  mode(invP)    = 'complex'
  Lambda = complex(k);   mode(Lambda)  = 'complex'
  info = integer(1);     mode(info)    = 'integer'
  lwsp = 18L*k*k+19L+1L; mode(lwsp)    = 'integer'
  lzwsp = 10L*k*k;       mode(lzwsp)   = 'integer'
  wsp = double(lwsp);    mode(wsp)     = 'double'
  zwsp = complex(lzwsp); mode(zwsp)    = 'complex'
  eigavail = 0;          mode(eigavail)= 'integer'
  res = .C(d0geouvwphi_, H,k,t,theta,sig_x,V,w,Phi,P,invP,Lambda,wsp,lwsp,zwsp,lzwsp,eigavail,info, NAOK=T)
  if (res[[17L]] != 0)   stop('Cannot eigen-decompose `H`')
  if (vecfmt)            return( unlist(res[c(8L,7L,6L)]) )
  else                   return( { R=res[c(8L,7L,6L)]; names(R)=c('Phi','w','V'); R } )
}

#' Jacobian of matrix of the Ornstein-Uhlenbeck model
#' 
#' \code{oujac} accepts the same arguments as \code{oupar} and returns the
#' Jacobian matrix of \code{oupar}.
#' 
#' @rdname oupar
#' @export
oujac = function (par, t, ...) {
  if (is.numeric(par)) {
    k     = as.integer(sqrt(9+24*length(par))/6-1/2)
    H     = par[1L:(k*k)]
    theta = par[(k*k+1L):(k*k+k)]
    sig_x = par[(k*k+k+1L):length(par)]
  } else if (is.list(par)) {
    k     = length(theta)
    H     = c(par[['H']])
    theta = c(par[['theta']])
    sig_x = c(par[['sig_x']])
    if (! ((is.numeric(H)     && length(H) == k*k) ||
            is.numeric(theta) && length(theta) == k||
            is.numeric(sig_x) && length(sig_x) == (k*(k+1L))%/%2L))
        stop("Invalid argument: `par` is malformed")
  } else  stop('`par` must be either a list or a numeric vector')
  ;                                       mode(t)       = 'double'
  ;                                       mode(k)       = 'integer'
  hts = c(H,theta,sig_x);                 mode(hts)     = 'double'
  P = matrix(0,k,k);                      mode(P)       = 'complex'
  invP = matrix(0,k,k);                   mode(invP)    = 'complex'
  Lambda = complex(k);                    mode(Lambda)  = 'complex'
  info = integer(1);                      mode(info)    = 'integer'
  lzwsp = max(12L*k*k, 6L*k*k+4L*k^4L);   mode(lzwsp)   = 'integer'
  zwsp = complex(lzwsp);                  mode(zwsp)    = 'complex'
  lwsp = 18L*k*k+as.integer(4*k^4)+30L;   mode(lwsp)    = 'integer'
  wsp = double(lwsp);                     mode(wsp)     = 'double'
  eigavail = 0;                           mode(eigavail)= 'integer'
  jac   = matrix(0,k*k+k+(k*(k+1L))%/%2L,k*k+k+(k*(k+1L))%/%2L); mode(jac)   = 'double'
  ougejacres = .C(ougejac_, t,k,hts,P,invP,Lambda,wsp,lwsp,zwsp,lzwsp,eigavail,jac,info,  NAOK=T)
  if (ougejacres[[13]] != 0)   stop('Cannot eigen-decompose `H`')
  r = ougejacres[[12]]
  r
}


#' Jacobian of matrix of the Ornstein-Uhlenbeck model
#' 
#' \code{ouhess} accepts the same arguments as \code{oupar}
#' and returns all the second derivatives \code{oupar}. The returned
#' values are consistent with the format required by \code{\link{glinv}}.
#' 
#' @rdname oupar
#' @export
ouhess = function (par, t, ...) {
  k = as.integer(sqrt(9+24*length(par))/6-1/2)
  npar = length(par)
  mode(par) = 'double'
  mode(t)   = 'double'
  sig = .C(lnunchol_, as.double(par[((k*k)+k+1L):npar]), k,
           double(k*k), k*k, matrix(0.,k,k), 0L, NAOK=T)[[5]]
  eig = eigen(matrix(par[1L:(k*k)],k,k))
  P = as.complex(eig[['vectors']])
  invP = as.complex(solve(eig[['vectors']]))
  Lambda = as.complex(eig[['values']])
  if (any(Mod(Lambda)>=12*log(10))) {
    biggest = Lambda[which.max(Mod(Lambda))]
    warning(sprintf('ouhess: the closed form spectral formula for the Hessian\'s drift-matrix entries could potentially be numerically quite inaccurate because some eigenvalues of the drift matrix is big (=%s, the %d-th eigenvalue). This means that after expontiation, we are computing with numbers like this extreme: exp(%s)=%s and exp(-(%s))=%s. Finite difference might be slightly more stable in these cases but it is more important to note that this statistically means that the phylogeny is almost irrelevant on the evolution in such directions.',
                    as.character(biggest), which.max(Mod(Lambda)),
                    as.character(biggest), as.character(exp(biggest)),
                    as.character(biggest), as.character(exp(-biggest))))
  }
  res = .C(hphiha_, t,
           k, P,invP,Lambda,
           array(0.0, c(k*k,k*k,k*k)),
           complex(k^6+2L*(k*k)+3), as.integer(k^6+2L*(k*k)+3),
           0L)
  if (res[[9]] != 0) stop('Error executing hphiha_()')
  hphiha = res[[6]]
  r = list('V' = {
    hv = array(0., c((k*(k+1L))%/%2L, npar, npar))
    hvhares = .C(hvha_, t, sig, par[1L:(k*k)],
             k, P, invP, Lambda,
             array(0., c((k*(k+1L))%/%2L,k*k,k*k)),
             double(2L*(k*k)), 2L*(k*k),
             complex(k^6+4*(k*k)+3*k),as.integer(k^6)+4L*(k*k)+3L*k,
             1L,0L)
    if (hvhares[[14]] != 0) stop('Error executing hvha_()')
    hv[1L:((k*(k+1L))%/%2L),1L:(k*k),1L:(k*k)] = hvhares[[8]]
    hvdadlres = .C(hvdadl_, t, par[1L:(k*k)],
              k, par[((k*k)+k+1L):npar],
              P,invP,Lambda, array(0., c((k*(k+1L))%/%2L,k*k,(k*(k+1L))%/%2L)),
              double(4L*(k*k)), 4L*(k*k),
              complex(k^4+2*k*k),as.integer(k^4)+2L*k*k,
              0L, NAOK=T)
    if (hvdadlres[[13]] != 0) stop('Error executing hvdadl_()')
    hv[1L:((k*(k+1L))%/%2L),(k*k+k+1L):npar,1L:(k*k)] =
        aperm(hv[1L:((k*(k+1L))%/%2L),1L:(k*k),(k*k+k+1L):npar] <- hvdadlres[[8]], c(1L,3L,2L))
    hv[1L:((k*(k+1L))%/%2L),(k*k+k+1L):npar,(k*k+k+1L):npar] =
        .C(hvhl_, t, k, par[((k*k)+k+1L):npar],
           P, invP, Lambda,
           double(4L*k*k), 4L*k*k,
           complex(2L*k*k), 2L*k*k,
           array(0.0, c((k*(k+1L))%/%2L,(k*(k+1L))%/%2L,(k*(k+1L))%/%2L)), NAOK=T)[[11]]
    hv
  },
  'w' = {
    hw = array(0., c(k,npar,npar))
    hw[1L:k,1L:(k*k),(k*k+1L):(k*k+k)] =
        aperm(hw[1L:k,(k*k+1L):(k*k+k),1L:(k*k)]<-
                  .C(hwdthetada_, k,
                     .C(dphida_, t, k,
                        P,invP,Lambda,
                        matrix(0.,k*k,k*k),
                        complex(as.integer(k^4)+k*k+2L),as.integer(k^4)+k*k+2L)[[6]],
                     array(0., c(k,k,k*k)))[[3]],
              perm=c(1L,3L,2L))
    hw[1L:k,1L:(k*k),1L:(k*k)] =
        .C(hwha_, k, hphiha, par[((k*k)+1L):(k*k+k)],
           array(0., c(k,k*k,k*k)))[[4]]
    hw
  },
  'Phi' = {
    hphi = array(0., c(k*k,npar,npar))
    hphi[1L:(k*k),1L:(k*k),1L:(k*k)] = hphiha[,,]
    hphi
  })
  r
}

#' Get the number of parameters of the unrestricted OU model
#' 
#' \code{nparams_ou} returns the number of parameters of the unrestricted OU model. For the restricted
#' models, including Brownian motion, see \code{\link{parameter_restriction}} for details.
#' 
#' @param k    An Integer. The total number of dimensions of the multivariate traits.
#' @return     A numerical scalar, which is the number of parameters of the the unrestricted OU model.
#' @export
nparams_ou = function (k) k*k+k+(k*(k+1L))%/%2L
