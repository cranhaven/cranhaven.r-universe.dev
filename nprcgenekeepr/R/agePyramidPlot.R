#' Form age pyramid plot
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#'
#' @return The return value of par("mar") when the function was called.
#'
#' @param males integer vector with the number of males in age groups
#' corresponding to the position in the vector
#' @param females integer vector with the number of females in age groups
#' corresponding to the position in the vector
#' @param ageLabels character vector of labels for the categories represented
#' by each pair of bars. There should be a label for each lx or rx value,
#' even if empty. If labels is a matrix or data frame, the first two columns
#' will be used for the left and right category labels respectively.
#' @param mcol color for the male (left) side of the plot
#' @param fcol color for the female (right) side of the plot
#' @param laxlab label for the male (left) side of the plot
#' @param raxlab label for the female (right) side of the plot
#' @param gap numeric value for one half of the space between the two sets
#' of bars for the \code{ageLabels} in user units
#' @param currentDate POSIXct date object indicating the date corresponding to
#' the date the pedigree census occurred.
#' @importFrom lubridate year month day
#' @importFrom stringi stri_c
#' @importFrom plotrix pyramid.plot
#' @noRd
agePyramidPlot <- function(males,
                           females,
                           ageLabels,
                           mcol,
                           fcol,
                           laxlab,
                           raxlab,
                           gap,
                           currentDate) {
  pyramid.plot(
    lx = males,
    rx = females,
    labels = ageLabels,
    main = stri_c(
      "Total on ",
      year(currentDate),
      "-",
      month(currentDate, label = TRUE),
      "-",
      day(currentDate),
      ": ",
      sum(c(males, females))
    ),
    top.labels = c(stri_c("Male = ", sum(males)), "Age", stri_c(
      "Female = ",
      sum(females)
    )),
    lxcol = mcol,
    rxcol = fcol,
    laxlab = laxlab,
    raxlab = raxlab,
    gap = gap,
    # TODO for chimpanzees (PT == species)
    # set gap to 40,
    # laxlab to seq(0, 100, by = 10),
    # raxlab to seq(0, 100, by = 10),
    unit = "Number of Animals",
    show.values = TRUE,
    ndig = 0L
  )
}
