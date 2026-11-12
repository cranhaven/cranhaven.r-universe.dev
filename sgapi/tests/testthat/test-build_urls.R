test_that("URL query strings build correctly", {

expect_equal(build_url_query_string(), "")
expect_equal(build_url_query_string(field1 = "str_option"), "?field1=str_option")
expect_equal(build_url_query_string(field1 = "str_option1", field2 = "str_option2"), "?field1=str_option1&field2=str_option2")
expect_equal(build_url_query_string(field1 = 3.14, field2 = c(1, 2, 3)), "?field1=3.14&field2=1,2,3")
expect_equal(build_url_query_string(prefix = "/query?", sep = "|", value_sep = ":", field1 = 42, field2 = c(1.1, 1.2, 1.3)), "/query?field1=42|field2=1.1:1.2:1.3")
})

