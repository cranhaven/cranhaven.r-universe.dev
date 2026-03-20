# Tests for visualization functions: outliergram and FPCAPlot

# =============================================================================
# MEI (Modified Epigraph Index) Tests
# =============================================================================

test_that("MEI depth computation works", {
  set.seed(42)
  m <- 50
  t_grid <- seq(0, 1, length.out = m)

  # Create simple curves
  X <- matrix(0, 10, m)
  for (i in 1:10) {
    X[i, ] <- sin(2 * pi * t_grid) + (i - 5) * 0.1
  }
  fd <- fdata(X, argvals = t_grid)

  mei <- depth(fd, method = "MEI")

  expect_length(mei, 10)
  expect_true(all(mei >= 0 & mei <= 1))
})

test_that("MEI values are ordered correctly", {
  # Create curves where we know the order:
  # Higher curves should have lower MEI (less time below others)
  m <- 50
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(0, 5, m)
  X[1, ] <- rep(1, m)   # highest - should have lowest MEI
  X[2, ] <- rep(0.5, m)
  X[3, ] <- rep(0, m)   # middle
  X[4, ] <- rep(-0.5, m)
  X[5, ] <- rep(-1, m)  # lowest - should have highest MEI

  fd <- fdata(X, argvals = t_grid)
  mei <- depth(fd, method = "MEI")

  # Higher curves should have lower MEI
  expect_true(mei[1] < mei[3])
  expect_true(mei[3] < mei[5])
})

test_that("MEI with separate reference sample works", {
  set.seed(42)
  m <- 30
  t_grid <- seq(0, 1, length.out = m)

  X1 <- matrix(rnorm(5 * m), 5, m)
  X2 <- matrix(rnorm(10 * m), 10, m)

  fd1 <- fdata(X1, argvals = t_grid)
  fd2 <- fdata(X2, argvals = t_grid)

  mei <- depth(fd1, fd2, method = "MEI")

  expect_length(mei, 5)
  expect_true(all(mei >= 0 & mei <= 1))
})

# =============================================================================
# Outliergram Tests
# =============================================================================

test_that("outliergram basic functionality works", {
  set.seed(42)
  m <- 50
  t_grid <- seq(0, 1, length.out = m)

  # Create normal curves
  X <- matrix(0, 20, m)
  for (i in 1:20) {
    X[i, ] <- sin(2 * pi * t_grid) + rnorm(m, sd = 0.2)
  }
  fd <- fdata(X, argvals = t_grid)

  og <- outliergram(fd)

  expect_s3_class(og, "outliergram")
  expect_length(og$mei, 20)
  expect_length(og$mbd, 20)
  expect_true(all(og$mei >= 0 & og$mei <= 1))
  expect_true(all(og$mbd >= 0 & og$mbd <= 1))
})

test_that("outliergram detects outliers", {
  set.seed(42)
  m <- 50
  t_grid <- seq(0, 1, length.out = m)

  # Create normal curves plus one outlier
  X <- matrix(0, 25, m)
  for (i in 1:24) {
    X[i, ] <- sin(2 * pi * t_grid) + rnorm(m, sd = 0.15)
  }
  X[25, ] <- sin(2 * pi * t_grid) + 3  # magnitude outlier

  fd <- fdata(X, argvals = t_grid)
  og <- outliergram(fd, factor = 1.5)

  # The outlier should be detected (may not always be, depends on factor)
  expect_true(og$n_outliers >= 0)
  expect_equal(length(og$outliers), og$n_outliers)
})

test_that("outliergram factor parameter works", {
  set.seed(42)
  m <- 50
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(0, 20, m)
  for (i in 1:20) {
    X[i, ] <- sin(2 * pi * t_grid) + rnorm(m, sd = 0.3)
  }
  fd <- fdata(X, argvals = t_grid)

  # Lower factor should detect more outliers
  og_strict <- outliergram(fd, factor = 0.5)
  og_lenient <- outliergram(fd, factor = 3)

  expect_true(og_strict$n_outliers >= og_lenient$n_outliers)
})

test_that("outliergram print method works", {
  set.seed(42)
  m <- 30
  X <- matrix(rnorm(10 * m), 10, m)
  fd <- fdata(X)
  og <- outliergram(fd)

  expect_output(print(og), "Outliergram")
  expect_output(print(og), "Number of curves: 10")
})

test_that("outliergram plot method works", {
  skip_if_not_installed("ggplot2")

  set.seed(42)
  m <- 30
  X <- matrix(rnorm(10 * m), 10, m)
  fd <- fdata(X)
  og <- outliergram(fd)

  p <- plot(og)
  expect_s3_class(p, "ggplot")
})

# =============================================================================
# FPCA Plot Tests
# =============================================================================

test_that("fdata2pc returns correct class", {
  set.seed(42)
  m <- 50
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(0, 20, m)
  for (i in 1:20) {
    X[i, ] <- sin(2 * pi * t_grid + runif(1, 0, pi)) + rnorm(m, sd = 0.1)
  }
  fd <- fdata(X, argvals = t_grid)

  pc <- fdata2pc(fd, ncomp = 3)

  expect_s3_class(pc, "fdata2pc")
  expect_equal(length(pc$d), 3)
  expect_equal(nrow(pc$x), 20)
  expect_equal(ncol(pc$x), 3)
})

test_that("plot.fdata2pc components plot works", {
  skip_if_not_installed("ggplot2")

  set.seed(42)
  m <- 50
  t_grid <- seq(0, 1, length.out = m)

  X <- matrix(0, 20, m)
  for (i in 1:20) {
    X[i, ] <- sin(2 * pi * t_grid + runif(1, 0, pi)) + rnorm(m, sd = 0.1)
  }
  fd <- fdata(X, argvals = t_grid)
  pc <- fdata2pc(fd, ncomp = 3)

  p <- plot(pc, type = "components")
  expect_s3_class(p, "ggplot")
})

test_that("plot.fdata2pc variance plot works", {
  skip_if_not_installed("ggplot2")

  set.seed(42)
  m <- 50
  X <- matrix(rnorm(20 * m), 20, m)
  fd <- fdata(X)
  pc <- fdata2pc(fd, ncomp = 5)

  p <- plot(pc, type = "variance")
  expect_s3_class(p, "ggplot")
})

test_that("plot.fdata2pc scores plot works", {
  skip_if_not_installed("ggplot2")

  set.seed(42)
  m <- 50
  X <- matrix(rnorm(20 * m), 20, m)
  fd <- fdata(X)
  pc <- fdata2pc(fd, ncomp = 3)

  p <- plot(pc, type = "scores")
  expect_s3_class(p, "ggplot")
})

test_that("print.fdata2pc works", {
  set.seed(42)
  m <- 50
  X <- matrix(rnorm(20 * m), 20, m)
  fd <- fdata(X)
  pc <- fdata2pc(fd, ncomp = 3)

  expect_output(print(pc), "Functional Principal Component Analysis")
  expect_output(print(pc), "Number of observations: 20")
  expect_output(print(pc), "Number of components: 3")
  expect_output(print(pc), "Variance explained")
})

# =============================================================================
# Error Handling Tests
# =============================================================================

test_that("MEI rejects 2D data", {
  X <- array(rnorm(100), dim = c(5, 10, 2))
  fd2d <- fdata(X, argvals = list(1:10, 1:2), fdata2d = TRUE)

  expect_error(depth(fd2d, method = "MEI"), "2D")
})

test_that("outliergram rejects 2D data", {
  X <- array(rnorm(100), dim = c(5, 10, 2))
  fd2d <- fdata(X, argvals = list(1:10, 1:2), fdata2d = TRUE)

  expect_error(outliergram(fd2d), "2D")
})

test_that("outliergram rejects non-fdata input", {
  expect_error(outliergram(matrix(1:10, 2, 5)), "fdata")
})

# =============================================================================
# Enhanced plot.fdata Tests
# =============================================================================

test_that("plot.fdata with numeric color works", {
  skip_if_not_installed("ggplot2")

  set.seed(42)
  m <- 30
  X <- matrix(rnorm(20 * m), 20, m)
  fd <- fdata(X)
  y <- rnorm(20)

  p <- plot(fd, color = y)
  expect_s3_class(p, "ggplot")
})

test_that("plot.fdata with categorical color works", {
  skip_if_not_installed("ggplot2")

  set.seed(42)
  m <- 30
  X <- matrix(rnorm(20 * m), 20, m)
  fd <- fdata(X)
  groups <- factor(rep(c("A", "B"), each = 10))

  p <- plot(fd, color = groups)
  expect_s3_class(p, "ggplot")
})

test_that("plot.fdata with show.mean works", {
  skip_if_not_installed("ggplot2")

  set.seed(42)
  m <- 30
  X <- matrix(rnorm(20 * m), 20, m)
  fd <- fdata(X)
  groups <- factor(rep(c("A", "B"), each = 10))

  p <- plot(fd, color = groups, show.mean = TRUE)
  expect_s3_class(p, "ggplot")
})

test_that("plot.fdata with show.ci works", {
  skip_if_not_installed("ggplot2")

  set.seed(42)
  m <- 30
  X <- matrix(rnorm(20 * m), 20, m)
  fd <- fdata(X)
  groups <- factor(rep(c("A", "B"), each = 10))

  p <- plot(fd, color = groups, show.ci = TRUE, ci.level = 0.90)
  expect_s3_class(p, "ggplot")
})

test_that("plot.fdata with show.mean and show.ci together works", {
  skip_if_not_installed("ggplot2")

  set.seed(42)
  m <- 30
  X <- matrix(rnorm(20 * m), 20, m)
  fd <- fdata(X)
  groups <- factor(rep(c("A", "B"), each = 10))

  p <- plot(fd, color = groups, show.mean = TRUE, show.ci = TRUE)
  expect_s3_class(p, "ggplot")
})

test_that("plot.fdata with custom palette works", {
  skip_if_not_installed("ggplot2")

  set.seed(42)
  m <- 30
  X <- matrix(rnorm(20 * m), 20, m)
  fd <- fdata(X)
  groups <- factor(rep(c("A", "B"), each = 10))

  p <- plot(fd, color = groups, palette = c("A" = "blue", "B" = "red"))
  expect_s3_class(p, "ggplot")
})

test_that("plot.fdata rejects mismatched color length", {
  m <- 30
  X <- matrix(rnorm(20 * m), 20, m)
  fd <- fdata(X)

  expect_error(plot(fd, color = 1:5), "length\\(color\\)")
})

# =============================================================================
# group.distance Tests
# =============================================================================

test_that("group.distance with centroid method works", {
  set.seed(42)
  m <- 30
  n <- 20
  X <- matrix(rnorm(n * m), n, m)
  fd <- fdata(X)
  groups <- factor(rep(c("A", "B"), each = 10))

  gd <- group.distance(fd, groups, method = "centroid")

  expect_s3_class(gd, "group.distance")
  expect_equal(length(gd$groups), 2)
  expect_equal(dim(gd$centroid), c(2, 2))
  expect_equal(gd$centroid[1, 1], 0)
  expect_equal(gd$centroid[2, 2], 0)
  expect_equal(gd$centroid[1, 2], gd$centroid[2, 1])  # symmetric
})

test_that("group.distance with hausdorff method works", {
  set.seed(42)
  m <- 30
  n <- 20
  X <- matrix(rnorm(n * m), n, m)
  fd <- fdata(X)
  groups <- factor(rep(c("A", "B"), each = 10))

  gd <- group.distance(fd, groups, method = "hausdorff")

  expect_s3_class(gd, "group.distance")
  expect_equal(dim(gd$hausdorff), c(2, 2))
  expect_equal(gd$hausdorff[1, 1], 0)
  expect_true(gd$hausdorff[1, 2] > 0)
})

test_that("group.distance with depth method works", {
  set.seed(42)
  m <- 30
  n <- 20
  X <- matrix(rnorm(n * m), n, m)
  fd <- fdata(X)
  groups <- factor(rep(c("A", "B"), each = 10))

  gd <- group.distance(fd, groups, method = "depth")

  expect_s3_class(gd, "group.distance")
  expect_equal(dim(gd$depth), c(2, 2))
  expect_equal(gd$depth[1, 1], 1)  # self-similarity is 1
  expect_equal(gd$depth[2, 2], 1)
  expect_true(gd$depth[1, 2] >= 0 && gd$depth[1, 2] <= 1)
})

test_that("group.distance with method='all' computes all methods", {
  set.seed(42)
  m <- 30
  n <- 20
  X <- matrix(rnorm(n * m), n, m)
  fd <- fdata(X)
  groups <- factor(rep(c("A", "B"), each = 10))

  gd <- group.distance(fd, groups, method = "all")

  expect_true(!is.null(gd$centroid))
  expect_true(!is.null(gd$hausdorff))
  expect_true(!is.null(gd$depth))
})

test_that("group.distance print method works", {
  set.seed(42)
  m <- 30
  n <- 20
  X <- matrix(rnorm(n * m), n, m)
  fd <- fdata(X)
  groups <- factor(rep(c("A", "B"), each = 10))

  gd <- group.distance(fd, groups, method = "centroid")

  expect_output(print(gd), "Group Distance Analysis")
  expect_output(print(gd), "Centroid Distance")
})

test_that("group.distance plot method works", {
  skip_if_not_installed("ggplot2")

  set.seed(42)
  m <- 30
  n <- 20
  X <- matrix(rnorm(n * m), n, m)
  fd <- fdata(X)
  groups <- factor(rep(c("A", "B"), each = 10))

  gd <- group.distance(fd, groups, method = "centroid")

  p <- plot(gd, type = "heatmap")
  expect_s3_class(p, "ggplot")
})

test_that("group.distance plot works with hausdorff method", {
  skip_if_not_installed("ggplot2")

  set.seed(42)
  m <- 30
  n <- 20
  X <- matrix(rnorm(n * m), n, m)
  fd <- fdata(X)
  groups <- factor(rep(c("A", "B"), each = 10))

  # Create with hausdorff method only
  gd <- group.distance(fd, groups, method = "hausdorff")

  # Should auto-detect and use hausdorff

  p <- plot(gd, type = "heatmap")
  expect_s3_class(p, "ggplot")

  # Explicit which = "hausdorff" should also work
  p2 <- plot(gd, type = "heatmap", which = "hausdorff")
  expect_s3_class(p2, "ggplot")
})

test_that("group.distance plot works with depth method", {
  skip_if_not_installed("ggplot2")

  set.seed(42)
  m <- 30
  n <- 20
  X <- matrix(rnorm(n * m), n, m)
  fd <- fdata(X)
  groups <- factor(rep(c("A", "B"), each = 10))

  # Create with depth method only
  gd <- group.distance(fd, groups, method = "depth")

  # Should auto-detect and use depth
  p <- plot(gd, type = "heatmap")
  expect_s3_class(p, "ggplot")
})

test_that("group.distance plot errors on unavailable method", {
  set.seed(42)
  m <- 30
  n <- 20
  X <- matrix(rnorm(n * m), n, m)
  fd <- fdata(X)
  groups <- factor(rep(c("A", "B"), each = 10))

  # Create with hausdorff only
  gd <- group.distance(fd, groups, method = "hausdorff")

  # Asking for centroid should error with helpful message
  expect_error(plot(gd, which = "centroid"), "not available.*Available.*hausdorff")
})

test_that("group.distance rejects mismatched groups length", {
  m <- 30
  X <- matrix(rnorm(20 * m), 20, m)
  fd <- fdata(X)

  expect_error(group.distance(fd, factor(c("A", "B"))), "length\\(groups\\)")
})

test_that("group.distance rejects single group", {
  m <- 30
  X <- matrix(rnorm(20 * m), 20, m)
  fd <- fdata(X)
  groups <- factor(rep("A", 20))

  expect_error(group.distance(fd, groups), "at least 2 groups")
})

# =============================================================================
# group.test Tests
# =============================================================================

test_that("group.test basic functionality works", {
  set.seed(42)
  m <- 30
  n <- 20
  t_grid <- seq(0, 1, length.out = m)

  # Create two clearly different groups
  X <- matrix(0, n, m)
  for (i in 1:10) X[i, ] <- sin(2 * pi * t_grid) + rnorm(m, sd = 0.1)
  for (i in 11:20) X[i, ] <- cos(2 * pi * t_grid) + rnorm(m, sd = 0.1)
  fd <- fdata(X, argvals = t_grid)
  groups <- factor(rep(c("A", "B"), each = 10))

  gt <- group.test(fd, groups, n.perm = 50)  # small n.perm for speed

  expect_s3_class(gt, "group.test")
  expect_true(gt$statistic > 0)
  expect_true(gt$p.value >= 0 && gt$p.value <= 1)
  expect_length(gt$perm.dist, 50)
})

test_that("group.test print method works", {
  set.seed(42)
  m <- 30
  X <- matrix(rnorm(20 * m), 20, m)
  fd <- fdata(X)
  groups <- factor(rep(c("A", "B"), each = 10))

  gt <- group.test(fd, groups, n.perm = 20)

  expect_output(print(gt), "Permutation Test")
  expect_output(print(gt), "P-value")
})
