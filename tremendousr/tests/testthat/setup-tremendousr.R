library("vcr") # *Required* as vcr is set up on loading
invisible(vcr::vcr_configure(
  dir = vcr::vcr_test_path("fixtures"),
  filter_request_headers = list("Authorization" = "<<MY-BEARER-TOKEN>>")
))
vcr::check_cassette_names()

virtual_visa_id <- "Q24BD9EZ332JT"
amazon_id <- "VW9JLMPRL9N7"
