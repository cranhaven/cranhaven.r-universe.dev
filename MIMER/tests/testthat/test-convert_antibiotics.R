test_that("Convert NDC code to Antimicrobial", {

  test_df <- data.frame(ndc = c("67457033950","63323039923"))
  test_df$ndc_to_antimicrobial <- as.character(ndc_to_antimicrobial(test_df$ndc))

  expected_df <- data.frame(ndc = c("67457033950","63323039923") ,
                            ndc_to_antimicrobial = c("VAM","AMP"))

  expect_equal(data.frame(test_df),expected_df)
})

test_that("Check NDC code is Antimicrobial or Not", {

  test_df <- data.frame(ndc = c("67457033950","565343522"))
  test_df$ndc_is_antimicrobial <- ndc_is_antimicrobial(test_df$ndc)

  expected_df <- data.frame(ndc = c("67457033950","565343522") ,
                            ndc_is_antimicrobial = c(TRUE,FALSE))

  expect_equal(data.frame(test_df),expected_df)
})





