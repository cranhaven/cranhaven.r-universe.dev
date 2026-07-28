## Invert a Hessian matrix and warn if it is not symmetric positive definite
hessian_inv_warn = function (X, matrix_name = 'The matrix', extra_info_nonpd = '', extra_info_noninvt = '') {
  tryCatch({
    L = chol(X)
    chol2inv(L)
  }, error = function (e_) {
    tryCatch({
      Y = solve(X)
      warning(sprintf('%s is numerically non-positive-definite. %s', matrix_name, extra_info_nonpd))
      Y
    }, error = function (f_) {
      stop(sprintf('%s is numerically non-invertible. %s', matrix_name, extra_info_noninvt))
    })
  })
}


list_set_default = function (L, defaults) {
  default_names = names(defaults)
  L_names       = names(L)
  for (i in seq_along(defaults))
    if (!(default_names[[i]] %in% L_names))
      L[[default_names[[i]]]] = defaults[[i]]
  L
}

## Returns: a tree in which edge table ordered depth-first
## Throws error if: root is not numbered n+1; or found an edge length that is zero.
fix_tree = function (tr) {
  if (0L != length(which(abs(tr$edge.length) < 10e-8) -> whichzero)) {
    stop('Branch(es) that lead to node no. ', capture.output(str(tr$edge[whichzero,2], give.head=F)), ' have zero or extremely small length. Check tree$edge.length. Multifurcating tree is supported by glinvci but zero-length branch length is not allowed because it covariance matrices will vanish.')
  }
  replace(tr, 'edge', list({
    if (! ape::is.rooted(tr)) stop('Non-rooted trees are not supported')
    ord = integer(nrow(tr$edge->ecpy)->nr)
    S   = integer(nr)
    if (max(tr$edge) != (length(tr$tip.label)->nt)+tr$Nnode || min(tr$edge) != 1L)
      stop('Trees which has non-consecutive or negative node numbers are not supported.')
    if ((.Call(Rgetroot, t(tr$edge))->S[1L->p]->rt) != nt+1L)
      stop('Trees whose root\'s node number isn\'t number of tip plus one is not supported.')
    i = 0L
    while (p>=1L) {
      if(length(which(ecpy[,1L]==S[p])->e)<=0L) {p=p-1L; next}
      S[(p=p+1L)] = ecpy[e[1L]->ord[(i+1L->i)],2L]
      ecpy[e[1L],] = NaN
    }
    tr$edge[ord,]
  }))
}
