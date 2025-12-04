
# Helper function to fit folded density

# mat : matrix of MCMC output with 1 column per chain
densFold0 <- function(mat, bw, from=NA, to=NA,...)  {

  # deal with folding for probability and non-negative values
  # use these values if folding is not needed:
  if(is.na(from))
    from <- min(mat) - 3*bw
  if(is.na(to))
    to <- max(mat) + 3*bw
  mult <- 1
  xx <- mat

  if (min(mat) >= 0 && min(mat) < 2 * bw) {  # it's non-negative
    from <- 0
    xx <- rbind(mat, -mat)
    mult <- 2
  }
  if (min(mat) >= 0 && max(mat) <= 1 &&
        (min(mat) < 2 * bw || 1 - max(mat) < 2 * bw)) { # it's a probability
    to <- min(to, 1)
    xx <- rbind(mat, -mat, 2-mat)
    mult <- 3
  }

  # fit density to each column
  n <- 512
  dens <- apply(xx, 2, function(x) density(x, bw=bw, from=from, to=to, n=n)$y)

  return(list(
    x = seq(from, to, length.out=n),
    y = dens * mult))
}
# ..........................................................

