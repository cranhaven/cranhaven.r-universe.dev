test_that("Profile class works", {

    a <- init_Profile(
        id = 'id',
        username = 'username',
        email = 'email',
        phone_number = 'phone_number',
        display_name = 'display_name',
        first_name = 'first_name',
        last_name = 'last_name'
    )

    expect_s3_class(a, 'Profile')

    expect_equal(a$get_id(), 'id')

    expect_equal(a$get_username(), 'username')

    expect_equal(a$get_email(), 'email')

    expect_equal(a$get_phone_number(), 'phone_number')

    expect_equal(a$get_display_name(), 'display_name')

    expect_equal(a$get_first_name(), 'first_name')

    expect_equal(a$get_last_name(), 'last_name')

    a$clear()

    expect_null(a$get_id())

    expect_null(a$get_username())

    expect_null(a$get_email())

    expect_null(a$get_phone_number())

    expect_null(a$get_display_name())

    expect_null(a$get_first_name())

    expect_null(a$get_last_name())
})
