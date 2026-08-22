test_that("attach message uses tidyverse-style sections", {
  msg <- tol_attach_message("rtreeoflife")

  expect_match(msg, "Attaching rtreeoflife", fixed = TRUE)
  expect_match(msg, "species index", fixed = TRUE)
  expect_match(msg, "selective download", fixed = TRUE)
  expect_match(msg, "tidy FASTA", fixed = TRUE)
  expect_match(msg, "visualisation", fixed = TRUE)
  expect_match(msg, "https://treeoflife.kew.org/", fixed = TRUE)
})
