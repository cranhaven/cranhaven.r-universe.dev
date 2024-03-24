
# ---- This code builds the mtcars_lab dataset ---------------------------------

# library(labelled)
# library(usethis)
#
# mtcars_lab <- mtcars
#
# mtcars_lab <-
#     data.frame(car = rownames(mtcars_lab)) |>
#     cbind(mtcars_lab)
#
# rownames(mtcars_lab) <- NULL
#
# mtcars_lab$car       <- factor(mtcars_lab$car, levels = sort(unique(mtcars_lab$car)), labels = sort(unique(mtcars_lab$car)))
# mtcars_lab$am        <- factor(mtcars_lab$vs, levels = c(1, 0), labels = c("Automatic", "Manual"))
# mtcars_lab$cyl       <- as.integer(mtcars_lab$cyl)
# mtcars_lab$gear      <- as.integer(mtcars_lab$gear)
# mtcars_lab$carb      <- as.integer(mtcars_lab$carb)
# mtcars_lab$above_avg <- mtcars_lab$qsec > mean(mtcars_lab$qsec)
#
# # Setting up vs as a labelled values column.
# mtcars_lab <-
#     set_value_labels(mtcars_lab, vs = c(`V-shaped` = 0, Straight = 1))
#
# # Add some NAs to it
# mtcars_lab$qsec[c(3, 7, 10)] <- NA
#
# # Add a new column that is entirely NA
# mtcars_lab$`this column all na` <- NA
#
# # Add a new column that is a list column
# mtcars_lab$list_col <- replicate(nrow(mtcars_lab), list(sample(month.abb, size = 3)))
#
# # These var labels are set last because the per-column editing above would remove them if
# # they had been set earlier.
# mtcars_lab <-
#     set_variable_labels(mtcars_lab,
#                         car = "Car makes and models (1973-74 models)",
#                         mpg = "Mileage (miles per gallon)",
#                         cyl = "Number of cylinders the car has",
#                         disp = "Displacement (cubic inches)",
#                         hp = "Gross horsepower",
#                         drat = "Rear axle ratio",
#                         wt = "Weight (1000s of pounds)",
#                         qsec = "Time to complete a 1/4 mile lap (seconds)",
#                         vs = "Engine shape",
#                         am = "Transmission",
#                         gear = "Number of forward gears",
#                         carb = "Number of carburetors",
#                         above_avg = "Faster than the average 1/4 mile lap times in this dataset",
#                         `this column all na` = "This column is entirely NA, and also named poorly",
#                         list_col = "This is a column containing lists"
#     )
#
# use_data(mtcars_lab, overwrite = TRUE)



# ---- This documents mtcars_lab -----------------------------------------------

#' Labelled version of mtcars for testing `siftr`
#'
#' This is `mtcars` with value labels, variable labels (in `vs` only), some transformation
#' to factor (`car` and `am`), an added Logical column (`above_avg`), an added column
#' of all `NA`s with a non-syntactic name (`this column all na`), and an added column
#' of lists (`list_col`). I also added some `NA` values to `qsec`.
#'
#' @format A dataframe with 15 columns and 32 rows.
#'
#' @examples
#' head(mtcars_lab)
#' lapply(mtcars_lab, typeof)
#'
#' @source `mtcars`
#' @md
"mtcars_lab"
