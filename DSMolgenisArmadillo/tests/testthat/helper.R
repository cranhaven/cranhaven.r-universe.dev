library(mockery)
library(tibble)

cookies <- tibble::tibble(
  domain = "#HttpOnly_example.org",
  name = "JSESSIONID",
  value = "abcde"
)
handle <- httr::handle(url = "http://example.org:8080")
connection <- methods::new("ArmadilloConnection",
  handle = handle,
  cookies = cookies,
  token = "token"
)
