`%is%` <- expect_equal


test_that("i_rle", {

  target <- lapply(1:10, \(x) list(length=x, value=x))

  result <- (
    as.list(
      i_rle(
        i_concat(
          i_apply(
            icount(10),
            \(x) i_rep(x, each=x))))))

  result %is% target

  inv_target <- lapply(1:10, \(x) rep(x, x)) |> c(recursive = TRUE)
  inv_result <- as.numeric(i_rleinv(result))

  inv_result %is% inv_target

})
