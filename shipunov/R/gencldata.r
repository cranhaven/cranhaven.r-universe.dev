Gen.cl.data <- function(type=c("blobs", "moons", "circles"), N=100, noise=NULL,
 shuffle=TRUE, bdim=2, bcenters=3, bnoise=1, bbox=c(-10, 10), cfactor=0.8) {
 if (!type %in% c("blobs", "moons", "circles")) stop("Unknown 'type'")
 if (type=="blobs") {
  bcenters <- runif(n=bdim*bcenters, min=bbox[1], max=bbox[2])
  bcenters <- matrix(bcenters, ncol=bdim)
  if ((length(bnoise) != 1) & (length(bnoise) != nrow(bcenters))) {
   stop("'bnoise' must be 1 or the same length as the number of clusters")
  }
  labels <- sample(nrow(bcenters), size=N, replace=TRUE)
  starting.points <- matrix(rnorm(N*bdim), ncol=bdim)
  if (length(bnoise) == 1) {
   points <- starting.points * bnoise
  } else {
   points <- starting.points * bnoise[labels]
  }
  points <- points + bcenters[labels, ]
 }
 if (type=="moons") {
  n.out <- N %/% 2
  n.in <- N - n.out
  points <- matrix(c(
   cos(seq(from=0, to=pi, length.out=n.out)),  # outer circle x
   1 - cos(seq(from=0, to=pi, length.out=n.in)), # inner circle x
   sin(seq(from=0, to=pi, length.out=n.out)), # outer circle y
   1 - sin(seq(from=0, to=pi, length.out=n.in)) - 0.5 # inner circle y
   ), ncol=2)
  labels <- c(rep(1, n.out), rep(2, n.in))
 }
 if (type=="circles") {
  if (cfactor >= 1 | cfactor < 0) stop("'cfactor' must be between 0 and 1")
  n.out <- N %/% 2
  n.in <- N - n.out
  c.out <- seq(0, 2*pi, length.out=n.out)
  c.in <- seq(0, 2*pi, length.out=n.in)
  outer.circ.x <- cos(c.out)
  outer.circ.y <- sin(c.out)
  inner.circ.x <- cos(c.in) * cfactor
  inner.circ.y <- sin(c.in) * cfactor
  points <- cbind(c(outer.circ.x, inner.circ.x), c(outer.circ.y, inner.circ.y))
  labels <- c(rep(1, n.out), rep(2, n.in))
 }
 if (!is.null(noise)) {
  points <- apply(points, 2, function(.x) .x + rnorm(N, sd=noise))
 }
 res <- list(samples=points, labels=labels)
 if (shuffle) {
  shuf <- sample(N)
  res <- list(samples=res$samples[shuf, ], labels=res$labels[shuf])
 }
 return(res)
}
