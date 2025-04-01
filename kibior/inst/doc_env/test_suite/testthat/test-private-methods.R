

context("Private methods - utils")



# helper class to test private members
# inherits from Kibior class
# makes the privates public for testing
PrivateMethodsTester <- R6Class(
    inherit = Kibior,
    public = list(
        test_round = function(x, nb_decimal = 1L){
            private$round(x, nb_decimal = nb_decimal)
        }
    )
)
#
instantiate <- function(){
    do.call(PrivateMethodsTester$new, .kibior_get_elastic_var_from_env())
}


# start round ----

test_that("kibior::round, wrong args", {
    private_round <- instantiate()$test_round
    # nb
    expect_error(private_round())
    expect_error(private_round(NULL))
    expect_error(private_round(c()))
    expect_error(private_round("aaa"))
    expect_error(private_round(c("aaa", "bbb")))
    expect_error(private_round(list()))
    # nb_decimal
    expect_error(private_round(1.1234, nb_decimal = NULL))
    expect_error(private_round(1.1234, nb_decimal = c()))
    expect_error(private_round(1.1234, nb_decimal = "aaa"))
    expect_error(private_round(1.1234, nb_decimal = c("aaa", "bbb")))
    expect_error(private_round(1.1234, nb_decimal = list()))
    expect_error(private_round(1.1234, nb_decimal = -2))
})

test_that("kibior::round, nominal case, round decimal", {
    private_round <- instantiate()$test_round
    #
    expect_equal(typeof(private_round(1.12314)), "double")
    expect_identical(private_round(1.12314, nb_decimal = 0), 1)
    expect_identical(private_round(1.12314, nb_decimal = 1), 1.1)
    expect_identical(private_round(1.12314, nb_decimal = 2), 1.12)
    # based on 1L
    expect_identical(private_round(1.12314), 1.1)
})

# end round

