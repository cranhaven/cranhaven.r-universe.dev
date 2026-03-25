test_that("khis_cred works correctly using configuration file", {

    skip_if_offline()

    expect_error(khis_cred(), class = 'khis_missing_credentials')

    expect_error(
        khis_cred(config_path = 'creds.json', username = 'username'),
        class = 'khis_multiple_credentials'
    )

    expect_error(
        khis_cred(config_path = 'creds.json', password = 'password'),
        class = 'khis_multiple_credentials'
    )

    expect_error(
        khis_cred(config_path ='does-not-exist.json'),
        class = 'khis_invalid_config_path'
    )

    expect_error(khis_cred(
        config_path = system.file("extdata", "empty_cred_conf.json", package = "khisr")),
        class = 'khis_invalid_config_path'
    )

    expect_error(
        khis_cred(config_path = system.file("extdata", "blank_cred_conf.json", package = "khisr")),
        class = 'khis_invalid_credentials'
    )

    expect_error(
        khis_cred(config_path = '{ "credentials": {}}'),
        class = 'khis_invalid_credentials'
    )

    # expect_error(
    #     khis_cred(
    #         config_path =  system.file("extdata", "no_url_cred_conf.json", package = "khisr")),
    #     class = 'khis_missing_base_url'
    # )

    skip_if_server_error()

    expect_no_error(
        khis_cred(
            config_path = system.file("extdata", "valid_cred_conf.json", package = "khisr"))
    )

    expect_true(khis_has_cred())

    expect_equal(khis_username(), 'dodoma')

    khis_cred_clear()

    expect_false(khis_has_cred())

    expect_error(
        khis_cred(username = 'username2',
                  password = 'password2',
                  base_url = NULL
        )
    )

    expect_no_error(
        khis_cred(username = 'dodoma',
                  password = 'Ytrewq!23456',
                  base_url="https://test.hiskenya.org/api"
        )
    )

    expect_true(khis_has_cred())

    expect_equal(khis_username(), 'dodoma')

    khis_cred_clear()

    expect_error(
        khis_cred(username = 'username2'),
        class = 'khis_invalid_credentials'
    )
})

test_that("req_auth_khis_basic works correctly", {

    expect_error(
        httr2::request('https://example.com') %>% req_auth_khis_basic(),
        class = 'khis_missing_credentials'
    )

    skip_if_server_error()

    khis_cred(
        config_path = system.file("extdata", "valid_cred_conf.json", package = "khisr"))

    expect_no_error(httr2::request('https://example.com') %>% req_auth_khis_basic())

    khis_cred_clear()
})
