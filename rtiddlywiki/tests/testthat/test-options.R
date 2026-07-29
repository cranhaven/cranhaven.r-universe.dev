test_that("test options", {
    original_options <- tw_options()
    options <- tw_options(host = "http://127.0.0.1:8081")
    expect_equal(as.character(options$host), "http://127.0.0.1:8081")
    # tw_options(host = original_options$host)
    # tw_reset()
    # options <- tw_options()
    #
    # expect_equal(as.character(options$host), original_options$host)

})

