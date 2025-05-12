library(testthat)

# when running manually line by line:
# devtools::load_all(".")

rm(list=ls(all.names = TRUE))


test_that("checkOnTest", {
    expect_false(checkOnTest())
}) # EOT


assign("get_settings_from_dogPack_package_root", TRUE, pos=.GlobalEnv)
#
# the existence of this object causes all subsequent calls to
# updateSettings(),
# autoUpS() and
# getstn()
# as defined in the package 'dogPack'
# to source the settings.R file from the root of the installed package
# 'dogPack' instead of the user-defined settings.R file.
# This is necessary for testing, as otherwise, i.e. when testing in a
# **remote runner**, the non-existence of the system variable leading to the
# place where the user-defined settings.R file is living will cause an error.
#
# function 'checkOnTest()' is checking for the existence of this object


test_that("checkOnTest", {
    expect_true(checkOnTest())
}) # EOT

# from now on, only the settings-file from the **root of package 'dogPack'**
# will be sourced.



test_that("dogPack_demo_tellFavouriteColor", {
    expect_identical(dogPack_demo_tellFavouriteColor(), "blue")
}) # EOT
