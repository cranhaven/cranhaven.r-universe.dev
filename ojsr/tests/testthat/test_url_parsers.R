context("url parsing")
library(ojsr)


test_that("result is a dataframe with the same number of rows as the input", {

  url_sample_test <- c(
    'https://publicaciones.sociales.uba.ar/index.php/psicologiasocial/',
    'http://fundacionmenteclara.org.ar/revista/index.php/RCA/article/download/43/54',
    'http://fundacionmenteclara.org.ar/revista/index.php/RCA/issue/view/2018-Vol3-2'
  )

  ojs_base_url <- ojsr::parse_base_url(input_url = url_sample_test)
  ojs_oai_url <- ojsr::parse_oai_url(input_url = url_sample_test)


  expect_is(ojs_base_url,"character")
  expect_equal(length(url_sample_test), length(ojs_base_url))
  expect_equal(length(url_sample_test), length(ojs_oai_url))
})
