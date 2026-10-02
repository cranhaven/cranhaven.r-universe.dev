vcr::use_cassette("voting_reverse_geocode_returns_information_in_different_formats_1", {
  test_that("Voting reverse geocode returns information in default format", {
    expect_snapshot(vote_rev(x = 12.58515, y = 55.68324))
  })
})

vcr::use_cassette("voting_reverse_geocode_returns_information_in_different_formats_2", {
  test_that("Voting reverse geocode returns information as dataframe", {
    expect_snapshot(vote_rev(
      x = 12.58515,
      y = 55.68324,
      as_df = TRUE
    ))
  })
})

vcr::use_cassette("voting_reverse_geocode_returns_information_in_different_formats_3", {
  test_that("Voting reverse geocode returns information as list", {
    expect_snapshot(vote_rev(
      x = 12.58515,
      y = 55.68324,
      as_list = TRUE
    ))
  })
})

vcr::use_cassette("afstemningsomraader_autocomplete_returns_the_right_answer_1", {
  test_that("Afstemningsomraader autocomplete returns the right answer for 'Dok'", {
    expect_snapshot(vote_ac(input = "Dok"))
  })
})

vcr::use_cassette("afstemningsomraader_autocomplete_returns_the_right_answer_2", {
  test_that("Afstemningsomraader autocomplete returns the right answer for 'Æbel'", {
    expect_snapshot(vote_ac(input = "Æbel"))
  })
})

vcr::use_cassette("afstemningsomraader_autocomplete_returns_the_right_answer_3", {
  test_that("Afstemningsomraader autocomplete returns the right answer for 'Røddi'", {
    expect_snapshot(vote_ac(input = "Røddi"))
  })
})

vcr::use_cassette("afstemningsomraader_autocomplete_returns_the_right_answer_4", {
  test_that("Afstemningsomraader autocomplete returns the right answer for 'brønder'", {
    expect_snapshot(vote_ac(input = "brønder"))
  })
})
