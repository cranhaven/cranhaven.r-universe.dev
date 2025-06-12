dist1 <- matrix(data = c(0, 1, 2, 1, 0, 1, 2, 1, 0),
                nrow = 3,
                byrow = TRUE,
                dimnames = list(1:3, 1:3))
dist2 <- matrix(data = c(0, 1, 1, 0),
                nrow = 2,
                dimnames = list(c("a", "b"), c("a", "b")))
strata_dist <- create_dist_matrix(dist1, dist2)

test_that("Combining distance matrices works", {
  expect_equal(strata_dist,
               matrix(c(0:2, 1:3, 1, 0:2, 1:2, 2:0, 3:1, 1:3, 0:2, 2, 1, 2, 1, 0, 1, 3:1, 2:0),
                      byrow = TRUE, nrow = 6,
                      dimnames = list(paste(rep(1:3, 2), rep(c("a", "b"), each = 3), sep = ":"),
                                      paste(rep(1:3, 2), rep(c("a", "b"), each = 3), sep = ":"))))
})
