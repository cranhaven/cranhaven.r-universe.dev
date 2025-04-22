context("Helper")

test_that("CSEnvir and Funs", {
  env = globalenv()
  
  createCSEnvir(carstats[, .(Cylinders, Displacement, Horsepower)]
                , strPreds = "Displacement", strResps = "Horsepower", strGroups = "Cylinders"
                , env = env
                )
  # init cs.* functions (only necessary for local call)
  createCSFunctions(env = env)
  res = cs.in.dataset()
  expect_data_table(res, nrows = 406, ncols = 3)
  
  createCSEnvir(carstats[, .(Displacement, Horsepower)]
                , strPreds = "Displacement", strResps = "Horsepower"
                , lstScriptVars = list(a = 4, b = "Text")
                , env = env
                )
  expect_list(cs.in.scriptvars())
  expect_equal(cs.in.scriptvars("a"), 4)
  expect_equal(cs.in.scriptvars("b"), "Text")
})

test_that("Matching Names", {
  # test names
  # Names from CS should not have surrounding backticks
  names.org = c("Bla", "Bl up", " Hal lo", "1Welt", "Backticks", ".Fine", ".!", "..")
  names.valid = make.names(names.org, unique = TRUE)
  # replace .. in valid with characters "XYZ" because of regexp in back replacement
  names.valid[length(names.valid)] = "XYZ"
  # test text
  # In text backticks mask invalid names, hence we have to replace only backticked names
  text.org = "`Bl up` = Bla + `Backticks`"
  text.valid = "Bl.up = Bla + Backticks"
  text.backticks = "`Bl up` = `Bla` + `Backticks`"
  
  # generate lookup table
  dtTable = getMatchingNames(names.org)
  expect_data_table(dtTable, types = "character", any.missing = FALSE, nrows = length(names.org), ncols = 2)
  expect_set_equal(names(dtTable), c("original", "valid"))
  expect_equal(dtTable$original, names.org)
  expect_equal(dtTable$valid, names.valid)

  # error on backticks
  expect_error(getMatchingNames(c("b", "b`")))
  expect_error(getMatchingNames(c("b", "`b`")))
  expect_error(getMatchingNames(c("b", "`b")))
  
  # error on duplicates
  expect_error(getMatchingNames(c("a", "a")))
  
  # replace names
  res = setMatchingNames(names.org[5:1], dtTable)
  expect_equal(res, names.valid[5:1])
  res = setMatchingNames(names.valid[4:1], dtTable, to.original = TRUE)
  expect_equal(res, names.org[4:1])
  res = setMatchingNames(c(names.valid[c(2, 4)], "Dingsda"), dtTable, to.original = TRUE)
  expect_equal(res, c(names.org[c(2, 4)], "Dingsda"))
  res = setMatchingNames(c(names.valid[c(6, 4)], "Dingsda"), dtTable, to.original = TRUE)
  expect_equal(res, c(names.org[c(6, 4)], "Dingsda"))
  
  # replace names in text
  res = setMatchingNames(text.org, dtTable, in.text = TRUE)
  expect_equal(res, text.valid)
  res = setMatchingNames(rep(text.org, 2), dtTable, in.text = TRUE)
  expect_equal(res, rep(text.valid, 2))
  # to original
  expect_error(setMatchingNames(text.org, dtTable, to.original = TRUE, in.text = TRUE))
  res = setMatchingNames(text.valid, dtTable, to.original = TRUE, in.text = TRUE)
  expect_equal(res, text.backticks)
  
  # error on names not available
  expect_error(setMatchingNames(names.valid, dtTable))
})
