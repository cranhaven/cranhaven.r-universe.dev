
# x1, x2: Either vectors containing the torus coordinates, or matrices with the same dimensions whose rows contain different points on the torus
# l: Side lengths
# DistTorus <- function(x1, x2, l) {
  # d2xy <- pmin((x1 - x2)^2, (x1 + l - x2)^2, (x1 - l - x2)^2)
  # sqrt(sum(d2xy))
# }
DistTorus <- function(x1, x2, l) {

  if (!is.matrix(x1)) {
    x1 <- matrix(x1, nrow=1)
  }
  if (!is.matrix(x2)) {
    x2 <- matrix(x2, nrow=1)
  }

  if (length(x1) > length(x2)) {
    tmp <- x1
    x1 <- x2
    x2 <- tmp
  }
  x1 <- matrix(x1, nrow(x2), ncol(x2), byrow=TRUE)
  d2xy <- pmin(abs(x1 - x2), l - abs(x1 - x2))
  sqrt(rowSums(d2xy^2))
}

# local({
  # x <- c(0, 0)
  # y <- c(1, 0)
  # z <- c(0.5, 0.5)
  # l <- c(1, 1)
  # expect_equal(DistTorus(x, y, l), 0)
  # expect_equal(DistTorus(x, z, l), sqrt(1/2))
  # expect_equal(unname(DistTorus(cbind(x, y, z), cbind(z, y, x), l)), c(sqrt(1/2), 0, sqrt(1/2)))
# })



# Connect x and y on an existing plot
Connect <- function(x1, x2, l) {
  x1 <- c(x1)
  x2 <- c(x2)
  l <- c(l)
  mat <- cbind((x1 - x2)^2, (x1 + l - x2)^2, (x1 - l - x2)^2)
  case <- apply(mat, 1, which.min)

  res <- lapply(seq_along(case), function(i) {
    
    if (case[i] == 1) {
      rep(c(x1[i], x2[i]), 2)
    } else if (case[i] == 2) {
      c(x1[i] + l[i], x2[i], x1[i], x2[i] - l[i])
    } else if (case[i] == 3) {
      c(x1[i] - l[i], x2[i], x1[i], x2[i] + l[i])
    }
  })
  lines(res[[1]][1:2], res[[2]][1:2])
  lines(res[[1]][3:4], res[[2]][3:4])
}


# # any number x modulo [0, divisor]
# Mod <- function(x, divisor) {
  # x - floor(x / divisor) * divisor
# }

# expect_equal(Mod(1.5, 1), 0.5)
# expect_equal(Mod(-1.5, 1), 0.5)
# expect_equal(Mod(1.5, 2), 1.5)
# expect_equal(Mod(-1.5, 2), 0.5)


GetSampler <- function(l, type=c('uniform', 'regular')) {
  type <- match.arg(type)
  if (type == 'uniform') {
    function(n) {
      cbind(runif(n) * l[1], runif(n) * l[2])
    }
  } else if (type == 'regular') {
    function(n) {
      m <- floor(sqrt(n))
      as.matrix(expand.grid(seq_len(m) / m * l[1], seq_len(m) / m * l[2]))
    }
  }
}

# x: The data matrix
# l: A vector containing the side lengths of the torus
ModTorus <- function(x, l) {
  x %% matrix(l, nrow(x), ncol(x), byrow=TRUE)
}


# Plot a tiling of the data, repeating for multiple times
# x: The data on [0, l[1]] \times [0, l[2]]
# col: Color for the data
# times: How many times to tile in each direction
# clipRec: If not NULL, this needs to be a matrix whose two columns are the lower-left and upper-right corners of a clipping box
# ...: Passed to plot()
Tile <- function(x, col, l=rep(1, ncol(x)), times=1, clipRec=cbind(0, l), ...) {
  stopifnot(times >= 0)
  mult <- (-times):times
  res <- plyr::alply(expand.grid(l1 = mult * l[1],
                                 l2 = mult * l[2]), 
                     1, function(dat) {
    x[, 1] <- x[, 1] + dat$l1
    x[, 2] <- x[, 2] + dat$l2
    x
  })
  mat <- do.call(rbind, res)
  col <- rep(col, times=2 * times + 1)
  plot(mat[, 1], mat[, 2], col=col, ...)
  if (!is.null(clipRec)) {
    rect(clipRec[1, 1], clipRec[2, 1], clipRec[1, 2], clipRec[2, 2])
  }
}


# Tile the dataframe
TileDat <- function(dat, l, torusCols, times=1) {
  stopifnot(times >= 0)
  dat <- as.data.frame(dat)

  mult <- (-times):times
  res <- plyr::alply(expand.grid(l1 = mult * l[1],
                                 l2 = mult * l[2]), 
                     1, function(lmult) {
    d <- dat
    d[torusCols] <- lapply(seq_along(torusCols), function(i) {
      coli <- torusCols[i]
      lmult[[i]] + d[[coli]]
    })
    # browser()
    d
  })
  do.call(rbind, res)
}

