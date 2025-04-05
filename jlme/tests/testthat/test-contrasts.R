stop_julia()

test_that("custom contrast detection works", {

  x <- data.frame(v = c("a", "b", "c"))

  expect_false(is_fct_custom_contrast(x$v))
  x$v <- as.factor(x$v)
  expect_false(is_fct_custom_contrast(x$v))

  contrasts(x$v) <- contr.treatment(3)
  expect_true(is_fct_custom_contrast(x$v))
  contrasts(x$v) <- contr.helmert(3)
  expect_true(is_fct_custom_contrast(x$v))

})

test_that("formula construction works", {

  x <- data.frame(v = factor(c("a", "b", "c")))
  contrasts(x$v) <- contr.helmert(3)
  colnames(contrasts(x$v)) <- c("helm1", "helm2")

  strsquish <- function(x) gsub("\\s", "", x)

  expect_identical(strsquish(construct_contrasts(x, "v", format = FALSE)), strsquish('
      Dict(
        :v => HypothesisCoding(
          [ -1/2  1/2   0;
            -1/6 -1/6 1/3 ];
          levels=["a", "b", "c"],
          labels=["helm1", "helm2"]
        )
      )
    '))

})
