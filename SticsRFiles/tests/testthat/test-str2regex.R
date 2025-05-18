test_that("regex escaping works", {
  expect_equal(str2regex("myfile.ext"), "myfile\\.ext")
  expect_equal(str2regex("myfile+.ext"), "myfile\\+\\.ext")
  expect_equal(str2regex("mydir*"), "mydir\\*")
  expect_equal(str2regex("*mydir"), "\\*mydir")
  expect_equal(str2regex("my.file.ext"), "my\\.file\\.ext")
  expect_equal(str2regex("**mydir**"), "\\*\\*mydir\\*\\*")
})

test_that("no regex escaping", {
  expect_equal(str2regex("mydir-"), "mydir-")
  expect_equal(str2regex("-mydir"), "-mydir")
  expect_equal(str2regex("mydir_"), "mydir_")
  expect_equal(str2regex("mydir&"), "mydir&")
})
