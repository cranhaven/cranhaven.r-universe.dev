# Ellipsoidal scaling version
lws.positions.sq <- function(enaset) {
  points = as.matrix(enaset$points)
  weights = as.matrix(enaset$line.weights)
  positions = lws_lsq_positions(weights, points, ncol(points));

  node.positions = positions$nodes;
  rownames(node.positions) = enaset$enadata$codes;

  return(list("node.positions" = node.positions, "centroids" = positions$centroids))
}

lws.positions.sq.R6 <- function(enaset) {
  if( enaset$function.params$center.align.to.origin ) {
    non_zero_rows <- rowSums(as.matrix(enaset$line.weights)) != 0
    positions = lws_lsq_positions(enaset$line.weights[non_zero_rows,], enaset$points.rotated[non_zero_rows,], ncol(enaset$points.rotated));
    mean_centroids = colMeans(positions$centroids);
    centroids = enaset$points.rotated;

    non_zero_row_centroids = rowSums(as.matrix(centroids))!=0;
    centroids[non_zero_row_centroids,] = t(t(positions$centroids) - mean_centroids)
    positions$centroids = centroids;
    positions$nodes = t(t(positions$nodes)-mean_centroids)
  }
  else {
    positions = lws_lsq_positions(enaset$line.weights, enaset$points.rotated, ncol(enaset$points.rotated));
  }

  node.positions = positions$nodes;
  rownames(node.positions) = enaset$enadata$codes;

  return(list("node.positions" = node.positions, "centroids" = positions$centroids))

}
