library(pipenostics)
all_paths <- list(
  c(12, 13, 11, 8, 4, 1),  # hereinafter indexes of acceptor nodes
  c(12, 13, 11, 8, 4, 2),
  c(12, 13, 11, 8, 6, 5,  3),
  c(12, 13, 11, 8, 6, 7),
  c(12, 13, 11, 8, 6, 9),
  c(12, 13, 11, 10),
  c(12, 13, 14, 15),
  c(12, 13, 16, 17),
  c(12, 13, 16, 18, 20, 19),
  c(12, 13, 16, 18, 20, 21),
  c(12, 13, 16, 18, 22, 24),
  c(12, 13, 16, 18, 22, 25),
  c(12, 13, 16, 18, 20, 23, 26)
)

test_that("*flowls* errs in listing paths without execution parallelization", {
  path <- with(m325testbench, {flowls(sender, acceptor)})
  for (i in union(seq_along(path), seq_along(all_paths))) expect_equal(path[[i]], all_paths[[i]])
})

test_that("*flowls* errs in listing paths utilizing parallel execution (if possible)", {
  path <- with(m325testbench, {flowls(sender, acceptor, use_cluster = !nzchar(Sys.getenv("_R_CHECK_LIMIT_CORES_", "")))})
  for (i in union(seq_along(path), seq_along(all_paths))) expect_equal(path[[i]], all_paths[[i]])
})

rm(all_paths)
