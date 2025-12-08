test_that("file_replace works", {
  p = tempInstance()
  # print(p)
  # safety check
  expect_true(length(list.files(p, pattern = "html")) == 1)
  expect_error(file_replace())
  expect_error(file_replace("foo.bar"))
  # dummy file
  f = file.path(p, "dummy.txt")
  expect_error(file_replace(f, c(1,2), c(1,2)))
  expect_error(file_replace(f, "a", c(1)))
  writeLines("DummyText To Test.\n The End.", f)
  t = readLines(f, warn = FALSE)
  g = grepl("End.", t)
  expect_true(any(g))
  file_replace(f, "End.", "Beginning.")
  t = readLines(f, warn = FALSE)
  g = grepl("End.", t)
  expect_false(any(g))
  expect_true(length(all(grepl("Beginning.", t))) == 1)
})
test_that("list_api_files works", {
  p = tempInstance()
  expect_error(list_api_files())
  a = list_api_files(p)
  expect_true(length(a) == 1)
  # mess it up
  file.remove(a)
  expect_error(list_api_files(p))
  unlink(p, recursive = TRUE)
})

test_that("help shows messages", {
  expect_message(help())
})
