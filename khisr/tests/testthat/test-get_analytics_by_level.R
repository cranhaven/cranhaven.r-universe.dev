test_that("get_analytics_by_level function works", {

    skip_if_no_cred()
    skip_if_offline()

    expect_error(get_analytics_by_level())
    expect_error(get_analytics_by_level(element_ids = 12345))
    expect_error(
        get_analytics_by_level(element_ids = '12345')
    )
    expect_error(
        get_analytics_by_level(element_ids = '12345',
                               start_date = '12345')
    )
    expect_error(
        get_analytics_by_level(element_ids = '12345',
                               start_date = 12345)
    )

    expect_error(
        get_analytics_by_level(element_ids = '12345',
                               start_date = '20-01-2000')
    )

    expect_error(
        get_analytics_by_level(element_ids = '12345',
                               start_date = '2000-01-20')
    )

    expect_error(
        get_analytics_by_level(element_ids = '12345',
                               start_date = '2000-01-20',
                               end_date = '12345')
    )

    expect_error(
        get_analytics_by_level(element_ids = '12345',
                               start_date = '2000-01-20',
                               level = '122')
    )

    expect_error(
        get_analytics_by_level(element_ids = '12345',
                               start_date = '2000-01-20',
                               level = 12)
    )

    expect_error(
        get_analytics_by_level(element_ids = '12345',
                               start_date = '2000-01-20',
                               level = 1,
                               org_ids = 12)
    )

    expect_null(
        get_analytics_by_level(element_ids = 'cXe64Yk0QMY',
                               start_date = '2023-01-01',
                               end_date = '2023-01-01')
    )

    expect_no_error(
        get_analytics_by_level(element_ids = 'cXe64Yk0QMY',
                               start_date = '2023-01-01',
                               end_date = '2023-02-01',
                               level = 2,
                               org_ids = c('jkG3zaihdSs', 'qKzosKQPl6G'))
    )
})
