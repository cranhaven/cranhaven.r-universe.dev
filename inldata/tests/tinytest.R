if (requireNamespace("tinytest", quietly = TRUE)) {
  Sys.setenv(R_TESTS = "")
  tinytest::test_package("inldata")
}
