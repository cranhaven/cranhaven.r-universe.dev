test_that("Antibiotic Check : DataFrame with Medicine", {
  test_df <- clean_antibiotics(data.frame(drug = c("Amoxicilln","Amoxicillin","Paracetamol") ), drug_col=drug)
  expected_df <- data.frame(drug = c("Amoxicilln","Amoxicillin","Paracetamol") ,
                            abx_name=c("Amoxicillin","Amoxicillin",NA),
                            synonyms= c("Amoxicillin","Amoxicillin",NA),
                            is_abx=c(TRUE,TRUE,FALSE))
  expect_equal(test_df,expected_df)
})


test_that("Antibiotic Check : Character Vector with Medicine", {
  test_df <- clean_antibiotics( c("Amoxicilln","Amoxicillin","Paracetamol"))
  expected_df <- c("Amoxicillin","Amoxicillin",NA)
  expect_equal(test_df,expected_df)
})

test_that("Custom synonyms check : character vector", {
  test_str <- clean_antibiotics("some_new_name_for_ciprofloxacin",
                                custom_synonyms = c("Ciprofloxacin" = "some_new_name_for_ciprofloxacin"))
  expected_str <- "Ciprofloxacin"
  expect_equal(test_str, expected_str)

  # without the custom rule
  test_str <- clean_antibiotics("some_new_name_for_ciprofloxacin")
  expected_str <- NA_character_
  expect_equal(test_str, expected_str)
})

test_that("Custom synonyms check : dataframe", {
  test_df <- data.frame(drug = "some_new_name_for_ciprofloxacin")
  test_str <- clean_antibiotics(test_df, drug_col = drug,
                                custom_synonyms = c("Ciprofloxacin" = "some_new_name_for_ciprofloxacin"))
  expected_str <- data.frame(drug = "some_new_name_for_ciprofloxacin",
                             abx_name = "Ciprofloxacin",
                             synonyms = "some_new_name_for_ciprofloxacin",
                             is_abx = TRUE)
  expect_equal(test_str, expected_str)

  # without the custom rule
  test_str <- clean_antibiotics(test_df, drug_col = drug)
  expected_str <- data.frame(drug = "some_new_name_for_ciprofloxacin",
                             abx_name = NA_character_,
                             synonyms = NA_character_,
                             is_abx = FALSE)
  expect_equal(test_str, expected_str)
})
