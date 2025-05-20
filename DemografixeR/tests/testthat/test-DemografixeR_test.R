################################### agify() ####################################

test_that("agify", {
  expect_error(agify())
  skip_on_cran()
  expect_true(is.integer(agify("Maria")))
  expect_true(is.integer(agify("Maria", country_id = "ES")))
  expect_true(is.data.frame(agify("Maria", simplify = FALSE)))
  expect_true(is.data.frame(agify("Maria", country_id = "US", simplify = FALSE)))
  expect_true(is.data.frame(agify("Maria", simplify = FALSE, meta = TRUE)))
  expect_true(is.data.frame(agify("Maria", country_id = "US", simplify = FALSE, meta = TRUE)))
})

################################# genderize() ##################################
test_that("genderize", {
  expect_error(genderize())
  skip_on_cran()
  expect_true(is.character(genderize("Maria")))
  expect_true(is.character(genderize("Maria", country_id = "US")))
  expect_true(is.data.frame(genderize("Maria", simplify = FALSE)))
  expect_true(is.data.frame(genderize("Maria", country_id = "US", simplify = FALSE)))
  expect_true(is.data.frame(genderize("Maria", simplify = FALSE, meta = TRUE)))
  expect_true(is.data.frame(genderize("Maria", country_id = "US", simplify = FALSE, meta = TRUE)))
})

################################ nationalize() #################################
test_that("nationalize", {
  expect_error(nationalize())
  skip_on_cran()
  expect_true(is.character(nationalize("Maria", sliced = TRUE)))
  expect_true(length(nationalize("Maria", sliced = FALSE))>1)
  expect_true(is.data.frame(nationalize("Maria", simplify = FALSE, sliced=TRUE)))
  expect_true(nrow(nationalize("Maria", simplify = FALSE, sliced=FALSE))>1)
  expect_true(is.data.frame(nationalize("Maria", simplify = FALSE, meta = TRUE)))
  expect_true(is.data.frame(nationalize("Maria", simplify = FALSE, meta = TRUE, sliced=FALSE)))
})

############################ supported_countries() #############################
test_that("supported_countries", {
  expect_error(supported_countries())
  skip_on_cran()
  expect_true(is.data.frame(supported_countries(type="genderize")))
  expect_true(is.data.frame(supported_countries(type="agify")))
  expect_true(is.data.frame(supported_countries(type="nationalize")))
})

################################ save_api_key() ################################
test_that("save_api_key", {
  expect_error(save_api_key())

  expect_true(save_api_key(key = "test"))
})

################################ get_api_key() #################################
test_that("get_api_key", {
  expect_true(is.null(get_api_key()) || is.character(get_api_key()))
})

############################### remove_api_key() ###############################
test_that("remove_api_key", {

  expect_output(remove_api_key())

  expect_null(remove_api_key(verbose = FALSE))
})

################################### add_id() ###################################
test_that("add_id", {
  expect_error(add_id())
  expect_true(is.data.frame(add_id(data.frame(a="test"), 2)))
  expect_null(add_id(data.frame(), 2))
})

################################# null_to_na() #################################
test_that("null_to_na", {
  expect_error(null_to_na())
  expect_true(is.na(null_to_na(NULL)))
  expect_true(null_to_na(2)=="2")
})

################################ api_request() #################################
test_that("api_request", {
  expect_error(api_request())
  skip_on_cran()
  expect_true(is.data.frame(api_request(x="Maria", type="gender")))
  expect_true(is.data.frame(api_request(x=c("Maria", "Ben"), type="gender")))
  expect_true(is.data.frame(api_request(x="Maria",country_id = "US", type="gender")))
  expect_true(is.data.frame(api_request(x=c("Maria", "Ben"),country_id = "US", type="gender")))
  expect_true(is.data.frame(api_request(x="Maria", type="nationality", sliced = FALSE)))
  expect_true(is.data.frame(api_request(x=c("Maria", "Ben"), type="nationality", sliced = FALSE)))
})

################################# sequencer() ##################################
test_that("sequencer", {
  expect_error(sequencer())
  skip_on_cran()
  expect_true(is.data.frame(sequencer(input = data.frame(x=c("Ben"),
                                                         country_id=c("NO COUNTRY"),
                                                         id=1,
                                                         stringsAsFactors = FALSE),
                                      type = "gender")))
})

############################ country_distributor() #############################
test_that("country_distributor", {
  expect_error(country_distributor())
  skip_on_cran()
  expect_true(is.data.frame(country_distributor(x = "Ben", type = "gender", "US")))
  expect_error(country_distributor(x = "Ben", type = "gender", c("ES","US")))
})

################################# rbind_fill() #################################
test_that("rbind_fill", {
  expect_error(rbind_fill())
  expect_true(is.data.frame(rbind_fill(list(data.frame(a=1, b=2), data.frame(b=2, c=3)))))
})

################################ fill_df_nas() #################################
test_that("fill_df_nas", {
  expect_error(fill_df_nas())
})

