context("test-fix_name_labels")

# error ----
test_that("error", {
  expect_error(fix_name_labels(NULL), "x must be a vector or list")
  expect_error(fix_name_labels(base::list), "x must be a vector or list")
  expect_error(fix_name_labels(list("A"), " ", NULL), "invalid 'replacement' argument")
})

# default  ----
test_that("default", {
  # unnamed list
  source <- list("A", "B", "C")
  target <- list(A = "A", B = "B", C = "C")
  test <- fix_name_labels(source)
  expect_equal(test, target)
  
  # unnamed vector
  source <- c("A", "B", "C")
  target <- list(A = "A", B = "B", C = "C")
  test <- fix_name_labels(source)
  expect_equal(test, target)
  
  # named list
  source <- list(A = "A cond", B = "B cond", C = "C cond")
  test <- fix_name_labels(source)
  expect_equal(test, source)
  
  # unnamed list with illegal characters
  source <- list("full.stop", " space ", "under_score", "plus+", "dash-", 
                 "tab\t", "line\nbreak")
  target <- list(full.stop = "full.stop", 
                 .space. = " space ", 
                 under.score = "under_score", 
                 plus. = "plus+", 
                 dash. = "dash-", 
                 tab. = "tab\t", 
                 line.break = "line\nbreak")
  test <- fix_name_labels(source)
  expect_equal(test, target)
  
  source <- list("._+- abc123()[]*&%$@!")
  target <- list(".....abc123.........." = "._+- abc123()[]*&%$@!")
  test <- fix_name_labels(source)
  expect_equal(test, target)
})

# pattern ----
test_that("pattern", {
  source <- list("._+- abc123()[]*&%$@!")
  target <- list("._+- abc123()[].&%$@." = "._+- abc123()[]*&%$@!")
  test <- fix_name_labels(source, pattern = "(\\*|\\!)")
  expect_equal(test, target)
  
  source <- list("replace don't replace")
  target <- list(". don't replace" = "replace don't replace")
  test <- fix_name_labels(source, pattern = "^replace")
  expect_equal(test, target)
  
  target <- list(A = "A", B = "B")
  test <- fix_name_labels(list("A", "B"), NULL)
  expect_equal(test, target)
})

# replacement ----
test_that("replacement", {
  source <- list("full.stop", " space ", "under_score", "plus+", "dash-", 
                 "tab\t", "line\nbreak")
  target <- list("full_stop" = "full.stop", 
                 "_space_" = " space ", 
                 "under_score" = "under_score", 
                 "plus_" = "plus+", 
                 "dash_" = "dash-", 
                 "tab_" = "tab\t", 
                 "line_break" = "line\nbreak")
  test <- fix_name_labels(source, replacement = "_")
  expect_equal(test, target)
})

# args ----
test_that("args", {
  # named arguments in order 
  source <- list("abc 123")
  target <- list("123" = "abc 123")
  test <- fix_name_labels(x = source, pattern = "(\\W|_|abc)", replacement = "")
  expect_equal(test, target)
  
  # different order
  test <- fix_name_labels(pattern = "(\\W|_|abc)", replacement = "", x = source)
  expect_equal(test, target)
})

# data.frame ----
test_that("data.frame", {
  # fixes column names of a data frame, but converts to a list
  # not intended behaviour but maybe useful?
  df <- data.frame(A_1 = 1:3, 
                   B_2 = c("one", "two", "three"), 
                   C_3 = c(T, F, T), 
                   row.names = c("D_1", "E_2", "F_3"),
                  stringsAsFactors = FALSE)
  target <- list(A.1 = 1:3, 
                 B.2 = c("one", "two", "three"), 
                 C.3 = c(T, F, T))
  test <- fix_name_labels(df)
  expect_equal(test, target)
  
  faux_options(sep = ".")
  on.exit(faux_options(sep = "_")) # reset sep
  test2 <- fix_name_labels(target)
  target2 <- as.list(df)
  expect_equal(test2, target2)
})
