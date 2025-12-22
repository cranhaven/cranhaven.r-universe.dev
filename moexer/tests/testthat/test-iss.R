test_that("Parsing a JSON ISS response works", {
    local_edition(2)
    parsed_query <- with_mock(
        `httr::GET` = function(...) readRDS('../testdata/iss/securities-SBER-json.rds'),
        query_iss('securities/SBER', debug_output = FALSE)
    )
    ref_parsed_query <- readRDS('../testdata/iss/securities-SBER-parsed_query.rds')
    expect_equal(parsed_query, ref_parsed_query)
})
