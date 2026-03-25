test_that("get_analytics function works", {

    skip_if_no_cred()
    skip_if_offline()

    expect_error(get_analytics())
    expect_error(get_analytics(return_type = 'ABC'))
    expect_null(
        get_analytics(
            dx %.d% 'XEX93uLsAm2',
            startDate = '2019-01-01',
            endDate = '2019-01-01'
        )
    )
    expect_no_error(
        get_analytics(
            dx %.d% 'XEX93uLsAm2',
            pe %.d% 'LAST_MONTH'
        )
    )
})
