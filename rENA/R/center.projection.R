### centering for projection

center.projection = function(lws, rotation) {
  if(is.null(rotation) || is.null(rotation$center.vec)) {
    stop("Supplied value for `rotation` does not have a center vector");
  }
  mean_ <- rotation$center.vec;

  centered.lws <- t(lws) - mean_;

  return( t(centered.lws) );
}

# og_lws = as.matrix(set.new$line.weights)
# set.new$rotation$center.vec = colMeans(og_lws)
#
#
#
# test = center.projection(lws = og_lws,set.new)
# centered_og = rENA:::center_data_c(as.matrix(set.new$line.weights))
# #
# View(test == centered_og)   ### DIFFERENCE IN ROUNDING
# View(round(test,3) == round(centered_og,3))
