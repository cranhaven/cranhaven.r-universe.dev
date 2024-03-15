# test_that("gcluster.run works", {
#     v <- 17
#     r <- gcluster.run(2 + 3, gsummary("test.fixedbin+v+qwe"), 3 + 4, 4 + 5, gsummary("test.sparse-v"), gsummary("test.rects*v"))
#     expect_regression(list(r[[1]]$retv, r[[2]]$retv, r[[3]]$retv, r[[4]]$retv, r[[5]]$retv, r[[6]]$retv), "gcluster.run")
# })

# test_that("gcluster.run works (2)", {
#     v <- 17
#     r <- gcluster.run(2 + 3, gsummary("test.fixedbin+v+qwe"), 3 + 4, 4 + 5, gsummary("test.sparse-v"), gsummary("test.rects*v"), max.jobs = 2)
#     expect_regression(list(r[[1]]$retv, r[[2]]$retv, r[[3]]$retv, r[[4]]$retv, r[[5]]$retv, r[[6]]$retv), "gcluster.run.2")
# })
