context("Load data")

test_that("load_data() returns data frame (not tibble)", {
  # create sample data
  df <- data.frame(x = 1, y = 2)
  file_path <- tempfile()
  write.csv(df, file_path, row.names = FALSE)

  expect_equal(class(load_data(file_path, "test.csv")), "data.frame")
})


test_that("clean string() removes spaces, hyphens, underscores and transforms
          to lowercase", {
            expect_equal(clean_string("H-_ djkL"), "hdjkl")
            expect_equal(clean_string("Animal-ID"), "animalid")
            expect_equal(clean_string(" Animal ID"), "animalid")
          })

test_that("guess_match()", {
  crit_cols <- c("animal_id", "day", "tumour_volume", "treatment")
  df_names <- c("Day ", "Animal-ID", "Tumour Volume", "Month")
  guesses <-
    list(
      "animal_id" = "Animal-ID",
      "day" = "Day ",
      "tumour_volume" = "Tumour Volume",
      "treatment" = ""
    )
  expect_equal(guess_match(df_names, crit_cols), guesses)
})
