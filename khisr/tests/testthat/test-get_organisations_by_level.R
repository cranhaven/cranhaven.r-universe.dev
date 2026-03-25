test_that("get_organisations_by_level function works", {

    skip_if_no_cred()
    skip_if_offline()

    expect_error(get_organisations_by_level(level = 1.1))
    expect_error(get_organisations_by_level(level = NULL))
    expect_error(get_organisations_by_level(level = NA))
    expect_error(get_organisations_by_level(level = c(1,2)))
    expect_error(get_organisations_by_level(level = 10))

    expect_error(get_organisations_by_level(org_ids = 123))
    expect_error(get_organisations_by_level(org_ids = ''))

    expect_warning(get_organisations_by_level(org_ids = '1234'))

    expect_no_error(get_organisations_by_level())
    expect_no_error(get_organisations_by_level(level = 3))
})
