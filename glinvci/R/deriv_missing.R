#' Handling missing data and lost traits in Ornstein-Uhlenbeck processes
#'
#' \code{ou_haltlost} and \code{ou_zaplost} handles lost traits and missing data.
#' Each of them wraps the function \code{\link{oupar}} and returns
#' a new function that accepts the same arguments and output the same form of result,
#' but takes into account lost traits and missing data. \code{dou_haltlost} and
#' \code{dou_zaplost} wraps the Jacobian function \code{\link{oujac}}, and
#' \code{hou_haltlost} and \code{hou_zaplost} wraps the Hessian function
#' \code{\link{ouhess}}.
#'
#' \subsection{What is missing traits and lost traits}{
#' A `missing' trait refers to a trait value whose data is missing due to data
#' collection problems. Fundamentally, they evolves in the same manner as other
#' traits. An \code{NA} entry in the data is deemed `missing'. On the other hand,
#' a lost trait is a trait dimension which had ceased to exists during the
#' evolutionary process. An \code{NaN} entry in the data indicates a `lost' trait.
#' }
#' 
#' \subsection{Each nodes has their own missing-ness tags}{
#' Each trait dimension of each nodes, either internal or tip, are tagged with
#' one of the three labels: \code{MISSING}, \code{LOST}, and \code{OK}.
#' If the data contains an \code{NA} in the \eqn{p}-th dimension of the \eqn{i}-th tip
#' then \eqn{X_pi} is tagged \code{MISSING}. No other tags of any other nodes and dimensions
#' are changed in the case of missing-ness. On the other hands, the \eqn{p}-th dimension of
#' any node \eqn{j}, regardless of whether or not it is an internal node or a tips, is
#' tagged \code{LOST} if and only if the \eqn{p}-th dimension of all tips inside
#' the clade started at \eqn{j} are \code{NaN}. Any entry that is neither tagged
#' \code{LOST} nor \code{MISSING} are tagged \code{OK}.
#' 
#' This corresponds to the biological intuition that, if a value is missing only due
#' to data collection problems, the missingness should not influence the random walk
#' process way up the phylogenetic tree; and this is obviously not true if the trait
#' had ceased to exists instead.
#' }
#'
#' \subsection{Handling of missing data and lost traits}{
#' \code{ou_haltlost} and \code{ou_zaplost} handles missing data in the same way: they
#' simply marginalises the unobserved dimensions in the joint Gaussian distributions of
#' tip data.
#'
#' For lost traits, \code{ou_haltlost} assumes the followings:
#' \enumerate{
#'   \item In the entire branch leading to the earliest node \eqn{j} whose \eqn{p}-th dimension
#'         is tagged \code{LOST}, the lost trait dimension does not evolve at all.
#'   \item In the entire same branch, the magnitude of the \eqn{p}-th dimension at \eqn{j}'s
#'         mother node has no influence on other dimensions, in any instantaneous moments during
#'         the evolution in the branch, neither through the linear combination with the drift
#'         matrix nor the Wiener process covariance; in other words, the SDE governing the 
#'         non-lost dimensions' random walk is invariant of \eqn{j}'s mother nodes' \eqn{p}-th dimension.
#' }
#' Therefore, \code{ou_haltlost} first set the \eqn{p}-th row and column of both of \eqn{H_j}
#' and the \eqn{p}-th row of \eqn{Sigma_x} to zero and marginalise out the degenerate Gaussian
#' dimension.
#'
#' On the other hands, \code{ou_zaplost} does not assume the lost trait to stop evolving
#' immediately at moment when the branch leading to \eqn{j} starts, but, instead, simply
#' marginalise out the lost, non-degenerate Gaussian dimensions. This method is the same as
#' the one that is used in the \code{PCMBase} package.
#' }
#'
#' \subsection{Usage in combination with parameter restrictions}{
#' Without paramter restriction, the following is an example usage in a call to the
#' \code{\link{glinv}} function. It constructs a \code{\link{glinv}} model object
#' which is capable of handling missing data and lost traits.
#' \preformatted{
#'         mod.full = glinv(tree, x0, my_data,
#'                          parfns  = haltlost(oupar),
#'                          pardims = nparams_ou(k),
#'                          parjacs = dhaltlost(oujac),
#'                          parhess = hhaltlost(ouhess))
#' }
#' Note that we have the same naming convention that functions wrappers whose
#' nams have prefix \code{d} wraps the Jacobians, while prefix \code{d} wraps
#' the Hessians.
#' 
#' If parameter restriction is needed, then \code{*ou_*lost} should called
#' \emph{before any reparameterisation/restriction functions} because it
#' expects the passed-in function \code{parfn} to accept the full \eqn{H}
#' matrix, rather than only the diagonal or lower-triangular part of it.
#' Example:
#' \preformatted{
#'         f = haltlost(oupar)
#'         g = dhaltlost(oujac)
#'         h = hhaltlost(oujac)
#'         mod.full = glinv(tree, x0, my_data,
#'                          parfns  = ou_spdH(f),
#'                          pardims = nparams_ou_spdH(k),
#'                          parjacs = dou_spdH(g),
#'                          parhess = ou_spdH(h,g))
#' }
#' }
#' 
#' @param parfn  A function that maps from the user-parametrisation to the underlying Gaussian parameters.
#'                Each of them returns a vector of concatenated \eqn{(\Phi, w, V')}, where \eqn{V'} is the lower triangular
#'                part of \eqn{V}, and accepts four arguments: a vector of parameters whose length is specified
#'                by the \code{pardims} argument to the \code{glinv_gauss} function, the branch length leading to the currently processing node, 
#'                a vector of factors with three levels indicating which dimensions are missing or lost in the mother of
#'                the current node, and a vector of factors with the same three levels indicating missingness of the current
#'                node.
#' @param jacfn  A function that accepts the same arguments as \code{parfn} and returns the Jacobian
#'               of \code{parfn}.
#' @param hessfn A function that accepts the same arguments as \code{parfns} and returns a list of three 3D arrays,
#'                named \code{Phi}, \code{w}, \code{V} respectively inside the list. \code{((hessfn)(...))$Phi[m,i,j]}
#'                contains the cross second-order partial derivative of \eqn{\Phi_m} (here we treat the matrix
#'                \eqn{\Phi} as a column-major-flattened vector) with respect to the \eqn{i}-th and\eqn{j}-th parameters
#'                in the joint \eqn{(H,\theta,\Sigma_x)} vector, and
#'                \code{((hessfn)(...))$w[m,i,j]} and \code{((hessfn)(...))$V[m,i,j]}
#'                analogously contains second-order derivative of \eqn{w_m} and \eqn{V'_m}.
#' @return       \code{ou_haltlost} and \code{ou_zaplost} returns a wrapped versions of `parfn`, which accepts the same arguments
#'               and outputs in the same format. \code{dou_haltlost} and \code{dou_zaplost}, analogously, wraps \code{jacfn}.
#'               \code{hou_zaplost} and \code{hou_zaplost} wraps \code{hessfn}.
#' @export
ou_haltlost = function (parfn) {
  simple_zap = ou_zaplost(parfn)
  function (par, t, misstags_mother, misstags_me, ...) {
    k = sqrt(9+24*length(par))/6-1/2
    H                           = par[1L:(k*k)]
    dim(H)                      = c(k,k)
    whichlost_me= which(misstags_me=='LOST')
    H[whichlost_me,]            = 0.0
    H[,misstags_mother=='LOST'] = 0.0
    par[1L:(k*k)]    = c(H)
    ## Set missing _row_ of sigma_x to zero.
    sigma_x_fulldim_mask = matrix(F,k,k)
    sigma_x_fulldim_mask[whichlost_me,] = T
    par[k*k+k+which(sigma_x_fulldim_mask[lower.tri(sigma_x_fulldim_mask, diag=T)])] = 0.
    sigma_x_ninf_mask = matrix(F,k,k)
    for (i in whichlost_me)
      sigma_x_ninf_mask[i,i] = T
    par[k*k+k+which(sigma_x_ninf_mask[lower.tri(sigma_x_ninf_mask, diag=T)])] = -Inf
    simple_zap(par, t, misstags_mother, misstags_me, ...)
  }
}

#' @rdname ou_haltlost
#' @export
dou_haltlost = function (jacfn) {
  simple_dzap = dou_zaplost(jacfn)
  function (par, t, misstags_mother, misstags_me, ...) {
    k = as.integer(sqrt(9+24*length(par))/6-1/2)
    H                           = par[1L:(k*k)]
    dim(H)                      = c(k,k)
    whichlost_me= which(misstags_me=='LOST')
    maskzero = matrix(F,k,k)
    maskzero[whichlost_me,]            = T
    maskzero[,misstags_mother=='LOST'] = T
    whichzero_H = which(c(maskzero))
    ## Missing row of sigma_x
    sigma_x_fulldim_mask = matrix(F,k,k)
    sigma_x_fulldim_mask[whichlost_me,] = T
    whichzero_sigx = k*k+k+which(sigma_x_fulldim_mask[lower.tri(sigma_x_fulldim_mask, diag=T)])
    whichzero = c(whichzero_H, whichzero_sigx)
    par[whichzero]                     = 0.
    sigma_x_ninf_mask = matrix(F,k,k)
    for (i in whichlost_me)
      sigma_x_ninf_mask[i,i] = T
    whichninf = k*k+k+which(sigma_x_ninf_mask[lower.tri(sigma_x_ninf_mask, diag=T)])
    par[whichninf] = -Inf
    jacob = simple_dzap(par, t, misstags_mother, misstags_me, ...)
    jacob[,c(whichzero,whichninf)]                  = 0.
    jacob
  }
}

#' @rdname ou_haltlost
#' @export
hou_haltlost = function (hessfn) {
  simple_hzap = hou_zaplost(hessfn)
  function (par, t, misstags_mother, misstags_me, ...) {
    k = as.integer(sqrt(9+24*length(par))/6-1/2)
    H                           = par[1L:(k*k)]
    dim(H)                      = c(k,k)
    whichlost_me = which(misstags_me=='LOST')
    maskzero_H = matrix(F,k,k)
    maskzero_H[whichlost_me,]            = T
    maskzero_H[,misstags_mother=='LOST'] = T
    whichzero_H = which(c(maskzero_H))
    sigma_x_fulldim_mask = matrix(F,k,k)
    sigma_x_fulldim_mask[whichlost_me,] = T
    whichzero_sigx = k*k+k+which(sigma_x_fulldim_mask[lower.tri(sigma_x_fulldim_mask, diag=T)])
    whichzero = c(whichzero_H, whichzero_sigx)
    par[whichzero]                     = 0.
    sigma_x_ninf_mask = matrix(F,k,k)
    for (i in whichlost_me)
      sigma_x_ninf_mask[i,i] = T
    whichninf = k*k+k+which(sigma_x_ninf_mask[lower.tri(sigma_x_ninf_mask, diag=T)])
    par[whichninf] = -Inf
    res = simple_hzap(par, t, misstags_mother, misstags_me, ...)
    lapply(res, function (x) {
      x[,c(whichzero,whichninf),] = 0.
      x[,,c(whichzero,whichninf)] = 0.
      x
    })
  }
}

#' @rdname ou_haltlost
#' @export
ou_zaplost = function (parfn) {
  function (par, t, misstags_mother, misstags_me, ...) {
    y = parfn(par, t, misstags_mother, misstags_me, ...)
    k = as.integer(sqrt(9+24*length(par))/6-1/2)
    Phi = y[1:(k*k)]
    dim(Phi) = c(k,k)
    avail_me  = which(misstags_me    =='OK')
    avail_mom = which(misstags_mother=='OK')
    Phi_      = Phi[avail_me, avail_mom, drop=F]
    w_        = y[(k*k+1L):(k*k+k)][avail_me,drop=F]
    V_        = matrix(0,k,k)
    V_[lower.tri(V_,diag=T)] =y[(k*k+k+1L):length(y)]
    V_        = V_[avail_me,avail_me,drop=F]
    c(Phi_,w_,V_[lower.tri(V_,diag=T)])
  }
}

#' @rdname ou_haltlost
#' @export
dou_zaplost = function (jacfn) {
  function (par, t, misstags_mother, misstags_me, ...) {
    Jac = jacfn(par, t, misstags_mother, misstags_me, ...)
    k = as.integer(sqrt(9+24*length(par))/6-1/2)
    avail_me  = misstags_me    =='OK'
    avail_mom = misstags_mother=='OK'
    ku = sum(avail_me)
    kv = sum(avail_mom)
    ku2half  = (ku*(ku+1L))%/%2L
    gausslen = ku*kv+ku+ku2half
    npar = length(par)
    res = matrix(0., gausslen, npar)
    nrowJ = nrow(Jac)
    Phi  = matrix(0., k,k)
    PhiMask = matrix(F,k,k)
    PhiMask[avail_me,avail_mom] = T
    w    = matrix(0., k)
    V    = matrix(0., k,k)
    VMask= lower.tri(V,diag=T)
    VkuMask = VMask
    VkuMask[!avail_me,] = F
    VkuMask[,!avail_me] = F
    for (i in seq_len(npar)) {
      Phi[,]  = Jac[1L:(k*k),i]
      w[,]    = Jac[(k*k+1L):(k*k+k),i]
      V[VMask]= Jac[(k*k+k+1L):nrowJ,i]
      res[1L:(ku*kv),i] = Phi[PhiMask]
      res[(ku*kv+1L):(ku*kv+ku),i] = w[avail_me]
      res[(ku*kv+ku+1L):gausslen,i]= V[VkuMask]
    }
    res
  }
}

#' @rdname ou_haltlost
#' @export
hou_zaplost = function (hessfn) {
  hessfn
  function (par, t, misstags_mother, misstags_me, ...) {
    hess_orig = hessfn(par, t, misstags_mother, misstags_me, ...)
    avail_me  = which(misstags_me    =='OK')
    avail_mom = which(misstags_mother=='OK')
    hess_orig[['w']]   = hess_orig[['w']][avail_me,,,drop=F]
    Phi_mask  = matrix(F, length(misstags_me), length(misstags_mother))
    Phi_mask[avail_me,avail_mom]  = T
    hess_orig[['Phi']] = hess_orig[['Phi']][c(Phi_mask),,,drop=F]
    V_mask                    = matrix(F, length(misstags_me), length(misstags_me))
    V_mask[avail_me,avail_me] = T
    V_mask                    = V_mask[lower.tri(V_mask,diag=T)]
    hess_orig[['V']] = hess_orig[['V']][c(V_mask),,,drop=F]
    hess_orig
  }
}

