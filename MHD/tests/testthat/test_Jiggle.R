test_that('Jiggle() works', {
  scenarios <- list(
    c(n = 4, m = 2), 
    c(n = 6, m = 3), 
    c(n = 8, m = 4), 
    c(n = 20, m = 3))

  for (l in scenarios) {
    n <- unname(l['n'])
    m <- unname(l['m'])

    X <- Jiggle(n, m)

    # Grid points lie on a sphere
    expect_equal(t(apply(X, 1, Normalize)), X)
    expect_equal(nrow(X), n)

    Y <- Jiggle(n, m, 'coordUnif')
    expect_equal(nrow(Y), n)
    if (n == m * 2) {
      expect_equal(apply(Y, 1, function(x) max(abs(x))), rep(1, n))
    }
    # if (m == 3) {
      # print(plotly::plot_ly(x=~Y[, 1], y=~Y[, 2], z=~Y[, 3]))
    # }
  }
})
