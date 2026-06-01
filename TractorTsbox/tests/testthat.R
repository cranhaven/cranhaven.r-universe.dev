# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/tests.html
# * https://testthat.r-lib.org/reference/test_package.html#special-files

library("testthat") # nolint unused_import_linter
library("TractorTsbox") # nolint unused_import_linter

test_check("TractorTsbox")
