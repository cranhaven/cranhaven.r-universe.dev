test_that("inputs are checked when creating AuthCred", {

    expect_snapshot(
        init_AuthCred(
            username = c('home', 'weed'),
            password = NULL,
            base_url = NULL
        ),
        error = TRUE
    )

    expect_snapshot(
        init_AuthCred(
            username = 123,
            password = NULL,
            base_url = NULL
        ),
        error = TRUE
    )

    expect_snapshot(
        init_AuthCred(
            username = NULL,
            password = c('123','abc'),
            base_url = NULL
        ),
        error = TRUE
    )

    expect_snapshot(
        init_AuthCred(
            username = NULL,
            password = 123,
            base_url = NULL
        ),
        error = TRUE
    )

    expect_snapshot(
        init_AuthCred(
            username = NULL,
            password = NULL,
            base_url = c('home', 'weed')
        ),
        error = TRUE
    )

    expect_snapshot(
        init_AuthCred(
            username = NULL,
            password = NULL,
            base_url = 123
        ),
        error = TRUE
    )

    a <- init_AuthCred(
        username = 'username',
        password = NULL,
        base_url = NULL
    )
    expect_s3_class(a, "AuthCred")
})


test_that("AuthCred username can be modified and cleared", {
    a <- init_AuthCred(username = 'AAA')
    expect_equal(a$get_username(), "AAA")

    a$set_username('BBB')
    expect_equal(a$get_username(), "BBB")

    a$set_username(NULL)
    expect_null(a$get_username())
})

test_that("AuthCred password can be modified and cleared", {
    a <- init_AuthCred(password = 'AAA')
    expect_equal(a$get_password(), "AAA")

    a$set_password('BBB')
    expect_equal(a$get_password(), "BBB")

    a$set_password(NULL)
    expect_null(a$get_password())

    a$set_password('CCC')
    expect_equal(a$get_password(), "CCC")

    a$clear_password()
    expect_null(a$get_password())
})

test_that("AuthCred base url can be modified and cleared", {
    a <- init_AuthCred(base_url = 'AAA')
    expect_equal(a$get_base_url(), "AAA")

    a$set_base_url('BBB')
    expect_equal(a$get_base_url(), "BBB")

    a$set_base_url(NULL)
    expect_null(a$get_base_url())

    a$set_base_url('CCC')
    expect_equal(a$get_base_url(), "CCC")

    a$set_base_url(NULL)
    expect_null(a$get_base_url())
})

test_that("AuthCred has_cred can return Correctly", {
    a <- init_AuthCred(
        username = 'username',
        password = 'password',
        base_url = 'base_url'
    )

    expect_true(a$has_cred())

    a$set_username(NULL)

    expect_false(a$has_cred())

    a$set_username('AAA')

    expect_true(a$has_cred())

    a$set_password(NULL)

    expect_false(a$has_cred())

    a$set_password('BBB')

    expect_true(a$has_cred())

    a$set_base_url(NULL)

    expect_false(a$has_cred())
})

test_that("AuthCred has_valid_cred can return Correctly", {
    a <- init_AuthCred()

    expect_false(a$has_valid_cred())

    expect_null(a$get_profile())

    a$set_profile(init_Profile())

    expect_true(a$has_valid_cred())

    expect_s3_class(a$get_profile(), 'Profile')
})
