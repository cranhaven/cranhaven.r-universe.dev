test_that("download media for europeana_search_api method is OK!", {
  skip_on_cran()
  test_dir <- tempdir()
  resp <- query_search_api("arioch", rows = 2)
  download_dir <- download_media(resp,
                                 type_ = "IMAGE",
                                 download_dir = test_dir)
  expect_true(file.exists(download_dir))
  dat <- tidy_search_items(resp)
  expect_true(
    all(gsub( ".*/", "",
              dat[type == "IMAGE", id]) %in% list.files(path = download_dir))
  )
})

test_that("download media for cursored_search method is OK!", {
  skip_on_cran()
  test_dir <- tempdir()
  query <- "(when:17 OR when:16 OR when:15 OR when:14) AND what:painting"
  resp <- tidy_cursored_search(query, max_items = 3)
  download_dir <- download_media(resp,
                                 type_ = "IMAGE",
                                 download_dir = test_dir)
  expect_true(file.exists(download_dir))
  dat <- resp$data
  expect_true(
    all(gsub( ".*/", "",
              dat[type == "IMAGE", id]) %in% list.files(path = download_dir))
  )
})
