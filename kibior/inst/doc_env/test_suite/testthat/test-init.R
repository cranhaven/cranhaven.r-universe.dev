
context("Initialize")

testthat::setup({
    # remove indices if they exist
    remove_all_indices()
})

testthat::teardown({
    # remove indices if they exist
    remove_all_indices()
})


# start initialize namespace ----

test_that("kibior::initialize, nominal case", {
    expect_equal(kc$host, trimws(Sys.getenv("KIBIOR_BUILD_ES_ENDPOINT")))
    expect_true(kc$cluster_status %in% c("green", "yellow"))
})

test_that("kibior::initialize, no host", {
    expect_error(Kibior$new())
    expect_error(Kibior$new(""))
})

test_that("kibior::initialize, wrong host", {
    expect_error(Kibior$new("nope"))
})

test_that("kibior::initialize, host param, w/wo credentials", {
    # only works when username is not null
    if(!purrr::is_null(kc$user)){
        expect_error(Kibior$new(host = kc$user, port = kc$port, verbose = TRUE))
    } else {
        # no user/pwd given, cannot assert ES is secure or not.
        # if not secure, see previous tests
        # if it is, this test + the next one are taken into account
        expect_true(TRUE)
    }
})

test_that("kibior::initialize, host param and credentials", {
    # only works when username is not null
    if(!purrr::is_null(kc$user)){
        # try wrong user/pwd couple
        expect_error(Kibior$new(host = kc$host, port = kc$port, user = "nope", pwd = "nope"))
    } else {
        # no user/pwd given, cannot assert ES is secure or not.
        # if not secure, see previous tests
        # if it is, this test + the last one are taken into account
        expect_true(TRUE)
    }
})

test_that("kibior::initialize, wrong host message error", {
    expect_error(Kibior$new(verbose = TRUE), info = "Failed to connect to localhost port 9200: Connection refused")
})

test_that("kibior::initialize, wrong host arg type", {
    expect_error(Kibior$new(host = list()))
})

test_that("kibior::initialize, wrong port arg type", {
    expect_error(Kibior$new(port = list()))
})

test_that("kibior::initialize, wrong user arg type", {
    expect_error(Kibior$new(user = list()))
})

test_that("kibior::initialize, wrong pwd arg type", {
    expect_error(Kibior$new(pwd = list()))
})

test_that("kibior::initialize, wrong verbose arg type", {
    expect_error(Kibior$new(verbose = list()))
})


# end initialize namespace 



# start print namespace ----

test_that("kibior::print, nominal case", {
    expected_output <- paste0("KibioR client: 
  - host: ", kc$host, " 
  - port: ", kc$port, " 
  - verbose: no 
  - print result: yes 
  - print progressbar: no ")
    expect_output(print(kc), expected_output)
})

test_that("kibior::print, nominal case, args change", {
    kc_new <- Kibior$new(host = kc$host, user = kc$user, pwd = kc$pwd, verbose = TRUE)
    msg <- paste0("KibioR client: 
  - host: ", kc$host, " 
  - port: ", kc$port, " ")
    if(!purrr::is_null(kc$user)){
        msg <- paste0(msg, "
  - username: ", kc$user, " 
  - password: ", kc$pwd, " ")
    }
    msg <- paste0(msg, "
  - verbose: yes 
  - print result: yes 
  - print progressbar: yes ")
    expect_output(print(kc_new), msg)
})

# end print namespace 
