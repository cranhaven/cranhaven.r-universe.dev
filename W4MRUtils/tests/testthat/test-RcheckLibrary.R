
test_that("Testing check_err", {
  testthat::expect_error(
    check_err("Some error"),
    regex = "\n- - - - - - - - -\nSome error\n- - - - - - - - -\n",
    fixed = TRUE
  )
})

test_that("Testing match2 variable metadata", {
  match2 <- function(...) {
    W4MRUtils::match2(..., "variable")
  }
  dm <- data.frame(a = strsplit("abc", ""), b = 2:4, c = 3:5)
  dm2 <- data.frame(a = strsplit("abcdefgh", ""), b = 2:9, c = 3:10)
  eight_var_1 <- data.frame(
    variable = strsplit("xyzwvtsr", ""),
    a = 1:8,
    b = 2:9
  )
  two_var_1 <- data.frame(variable = strsplit("abc", ""), a = 1:3, b = 2:4)
  two_var_2 <- data.frame(variable = c("a", "b", 7), b = 1:3, a = 2:4)
  three_var_1 <- data.frame(variable = 5:7, b = 1:3, a = 2:4, c = 3:5)
  testthat::expect_identical(match2(dm, two_var_1), NULL)
  testthat::expect_identical(
    match2(dm, two_var_2),
    c(
      "\nData\n      matrix and ",
      "variable",
      " metadata do not\n       match regarding ",
      "variable", " identifiers.",
      "\n    The ",
      "following identifiers found in the data matrix\n",
      "    do not appear in the ",
      "variable",
      " metadata file:\n",
      "    ",
      "c",
      "\n",
      "\n    The ",
      "following identifiers found in the ",
      "variable",
      " metadata file\n",
      "    do not appear in the data matrix:\n",
      "    ",
      "7",
      "\n",
      "\nPlease check your data.\n"
    )
  )
  testthat::expect_identical(
    match2(dm, three_var_1),
    c(
      "\nData\n      matrix and ",
      "variable",
      " metadata do not\n       match regarding ",
      "variable",
      " identifiers.",
      "\n    The ",
      "following identifiers found in the data matrix\n",
      "    do not appear in the ",
      "variable",
      " metadata file:\n",
      "    ",
      "a\n    b\n    c",
      "\n",
      "\n    The ",
      "following identifiers found in the ",
      "variable",
      " metadata file\n",
      "    do not appear in the data matrix:\n",
      "    ",
      "5\n    6\n    7",
      "\n",
      "\nPlease check your data.\n"
    )
  )
  testthat::expect_identical(
    match2(dm2, eight_var_1),
    c(
      "\nData\n      matrix and ",
      "variable",
      " metadata do not\n       match regarding ",
      "variable",
      " identifiers.",
      "\n    For example, the ",
      "following identifiers found in the data matrix\n",
      "    do not appear in the ",
      "variable",
      " metadata file:\n",
      "    ",
      "a\n    b\n    c",
      "\n",
      "\n    For example, the ",
      "following identifiers found in the ",
      "variable",
      " metadata file\n",
      "    do not appear in the data matrix:\n",
      "    ",
      "x\n    y\n    z",
      "\n",
      "\nPlease check your data.\n"
    )
  )
})

test_that("Testing match3", {
  var_meta <- data.frame(
    variable = strsplit("xyzwvtsr", ""),
    a = 1:8,
    b = 2:9
  )
  match3 <- function(...) {
    W4MRUtils::match3(..., var_meta)
  }
  dm <- data.frame(a = strsplit("xyz", ""), b = 2:4, c = 3:5)
  dm2 <- data.frame(a = strsplit("abcdefgh", ""), b = 2:9, c = 3:10)
  dm3 <- data.frame(a = strsplit("xyzwvtsr", ""), b = 2:9, c = 3:10)
  eight_samp_1 <- data.frame(
    variable = strsplit("xyzwvtsr", ""),
    a = 1:8,
    b = 2:9
  )
  two_samp_1 <- data.frame(variable = strsplit("abc", ""), a = 1:3, b = 2:4)
  two_samp_2 <- data.frame(variable = c("a", "b", 7), b = 1:3, a = 2:4)
  three_samp_1 <- data.frame(variable = 5:7, b = 1:3, a = 2:4, c = 3:5)
  base_err <- paste(
    "\nData matrix and sample metadata do not match",
    "regarding sample identifiers."
  )
  err_both <- paste(
    "\nData matrix and variable metadata do not match",
    "regarding variable identifiers."
  )
  testthat::expect_identical(
    match3(dm, two_samp_1),
    c(
      base_err,
      "\n    The ",
      "following identifiers found in the sample metadata file\n",
      "    do not appear in the data matrix:\n",
      "    ",
      "a",
      "\n",
      err_both,
      "\n    For example, the ",
      "following identifiers found in the variable metadata file\n",
      "    do not appear in the data matrix:\n",
      "    ",
      "w\n    v\n    t",
      "\n",
      "\nPlease check your data.\n"
    )
  )
  testthat::expect_identical(
    match3(dm, two_samp_2),
    c(
      base_err,
      "\n    The ",
      "following identifiers found in the data matrix\n",
      "    do not appear in the sample metadata file:\n",
      "    ",
      "c",
      "\n",
      "\n    The ",
      "following identifiers found in the sample metadata file\n",
      "    do not appear in the data matrix:\n",
      "    ",
      "a\n    7",
      "\n",
      err_both,
      "\n    For example, the ",
      "following identifiers found in the variable metadata file\n",
      "    do not appear in the data matrix:\n",
      "    ",
      "w\n    v\n    t",
      "\n",
      "\nPlease check your data.\n"
    )
  )
  testthat::expect_identical(
    match3(dm, three_samp_1),
    c(
      base_err,
      "\n    The ",
      "following identifiers found in the data matrix\n",
      "    do not appear in the sample metadata file:\n",
      "    ",
      "b\n    c",
      "\n",
      "\n    The ",
      "following identifiers found in the sample metadata file\n",
      "    do not appear in the data matrix:\n",
      "    ",
      "5\n    6\n    7",
      "\n",
      err_both,
      "\n    For example, the ",
      "following identifiers found in the variable metadata file\n",
      "    do not appear in the data matrix:\n",
      "    ",
      "w\n    v\n    t",
      "\n",
      "\nPlease check your data.\n"
    )
  )
  testthat::expect_identical(
    match3(dm2, eight_samp_1),
    c(
      base_err,
      "\n    The ",
      "following identifiers found in the data matrix\n",
      "    do not appear in the sample metadata file:\n",
      "    ",
      "b\n    c",
      "\n",
      "\n    For example, the ",
      "following identifiers found in the sample metadata file\n",
      "    do not appear in the data matrix:\n",
      "    ",
      "x\n    y\n    z",
      "\n",
      err_both,
      "\n    For example, the ",
      "following identifiers found in the data matrix\n",
      "    do not appear in the variable metadata file:\n",
      "    ",
      "a\n    b\n    c",
      "\n",
      "\n    For example, the ",
      "following identifiers found in the variable metadata file\n",
      "    do not appear in the data matrix:\n",
      "    ",
      "x\n    y\n    z",
      "\n",
      "\nPlease check your data.\n"
    )
  )
  testthat::expect_identical(
    match3(dm3, eight_samp_1),
    c(
      base_err,
      "\n    The ",
      "following identifiers found in the data matrix\n",
      "    do not appear in the sample metadata file:\n",
      "    ",
      "b\n    c",
      "\n",
      "\n    For example, the ",
      "following identifiers found in the sample metadata file\n",
      "    do not appear in the data matrix:\n",
      "    ",
      "x\n    y\n    z",
      "\n",
      "\nPlease check your data.\n"
    )
  )
})
