fake_httr_get <- function(url, ...) {
    secid <- str_match(url, 'iss\\/securities\\/(.+)\\.json')[1,2]
    if (!is.na(secid)) {
        return(readRDS(glue('../testdata/candles/securities-{secid}-json.rds')))
    }
    secid <- str_match(url, '.+securities\\/(.+)\\/candles.+')[1,2]
    if (!is.na(secid)) {
        return(readRDS(glue('../testdata/candles/candles-{secid}-json.rds')))
    }
    secid <- str_match(url, '.+securities\\/(.+)\\/candleborders.+')[1,2]
    return(readRDS(glue('../testdata/candles/candleborders-{secid}-json.rds')))
}


test_that("Fetching candles works", {
    local_edition(2)
    parsed_query <- with_mock(
        `httr::GET` = function(url, ...) fake_httr_get(url),
        get_candles(secid = c('XXXX', 'SBER', 'YYYY', 'FXGD'), from = '2020-01-01', debug_output = FALSE)
    )
    ref_parsed_query <- readRDS('../testdata/candles/candles-SBER_FXGD.rds')
    expect_equal(parsed_query, ref_parsed_query)
})


test_that("Specifying wrong symbols only returns an empty tibble", {
    local_edition(2)
    parsed_query <- with_mock(
        `httr::GET` = function(url, ...) fake_httr_get(url),
        get_candles(secid = c('XXXX', 'YYYY'), from = '2020-01-01', debug_output = FALSE)
    )
    ref_parsed_query <- tibble() |> append_class('MoexCandles')
    expect_equal(parsed_query, ref_parsed_query)
})


test_that("Getting candle borders works", {
    local_edition(2)
    parsed_query <- with_mock(
        `httr::GET` = function(url, ...) fake_httr_get(url),
        get_candle_borders(secid = c('FXGD', 'XXXX'), debug_output = FALSE)
    )
    ref_parsed_query <- readRDS('../testdata/candles/candleborders-FXGD.rds')
    expect_equal(parsed_query, ref_parsed_query)
})
