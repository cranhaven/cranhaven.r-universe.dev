# Get a missing-ness matrix from X, each column is a trait vector.
trait_missingness = function (x) {
  if (!is.matrix(x)) dim(x) = length(x)
  TM = matrix(1, dim(x)[1], dim(x)[2]) # See `enum missingness`
  TM[is.na(x)]  = 2
  TM[is.nan(x)] = 0
  mode(TM) = 'integer'
  TM
}

tag_missing = function (mod, X) {
  if (!inherits(mod, 'glinv_gauss')) stop("Invalid argument: mod")
  M = .Call(Rtagmiss, mod$ctree, length(unique(c(mod$apetree$edge))),
            trait_missingness(X))
  d = dim(M)
  N = factor(M, levels=c(0,1,2), labels=c('LOST','OK','MISSING'))
  dim(N) = d
  N
}

tag_regimes = function (mod, roots, theroot=.Call(Rgetroot,t(mod$rawmod$apetree$edge))) {
  if (!inherits(mod, 'glinv_gauss')) stop("Invalid argument: mod")
  if (!is.numeric(roots))
    stop(sprintf(
      "`roots` must be numeric and contain node numbers but I've received an object of class `%s`",
      class(roots)[1]))
  if (!(length(roots) == length(unique(roots))))
    stop(sprintf("Two regimes cannot be simultaneously started at the same node", class(roots)[1]))
  M = .Call(Rtagreg, mod$ctree, length(unique(c(mod$apetree$edge))),
            as.integer(c(roots,-1)))
  M[theroot] = NA
  M
}

tag_parfns = function (regtags, regime) {
  tags = integer(length(regtags))
  for (i in seq_along(regtags))
    tags[i] = if (is.na(regtags[i])) NA else regime[[regtags[i]]]['fn']
  tags
}
