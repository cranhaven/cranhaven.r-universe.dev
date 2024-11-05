context("faux_options")

user_opts <- faux_options("sep", "verbose", "plot", "connection")
on.exit(faux_options(user_opts))

faux_options(list(sep = "_", 
                  verbose = TRUE, 
                  plot = TRUE, 
                  connection = stdin()))

test_that("default", {
  o <- faux_options()
  expect_equal(names(o), c("connection", "plot", "sep", "verbose"))
  expect_equal(o$plot, TRUE)
  expect_equal(o$sep, "_")
  expect_equal(o$verbose, TRUE)
  expect_equal(class(o$connection), c("terminal", "connection"))
})

test_that("set", {
  user_sep <- faux_options("sep")
  on.exit(faux_options(sep = user_sep))
  
  faux_options(sep = ".")
  expect_equal(faux_options("sep"), ".")
  
  faux_options(list(sep = "|", verbose = FALSE))
  expect_equal(faux_options("sep"), "|")
  expect_equal(faux_options("verbose"), FALSE)
  expect_equal(getOption("faux.sep"), "|")
  expect_equal(getOption("faux.verbose"), FALSE)
  
  faux_options(sep = "x", plot = TRUE)
  expect_equal(faux_options("sep"), "x")
  expect_equal(faux_options("plot"), TRUE)
  
  faux_options(list(sep = "_", verbose = TRUE, plot = FALSE))
  expect_equal(faux_options("sep"), "_")
  expect_equal(faux_options("verbose"), TRUE)
})

test_that("get", {
  expect_equal(faux_options("sep"), "_")
  expect_equal(faux_options("sep", "verbose"), 
               list(sep = "_", verbose = TRUE))
  expect_equal(faux_options(c("sep", "verbose")), 
               list(sep = "_", verbose = TRUE))
})

test_that("error", {
  err = "Format lists with names like list(sep = '.', verbose = FALSE)"
  expect_error(faux_options(list("sep", "verbose")), err, fixed = TRUE)
})
