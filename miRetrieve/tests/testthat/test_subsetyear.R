library(miRetrieve)
library(testthat)

# Call column "Year_" to also check if the correct column can be set with
# subset_year()
toy_df <- data.frame("Year_" = seq(2010,2021))

subset <- subset_year(toy_df,
            col.year = Year_,
            start = 2013, end = 2019)

test_that("Tests subsetting data.frames for years", {
    expect_equal(nrow(subset), 7)
})
