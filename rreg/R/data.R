##' hfdata as sample data for institutions
##'
##' hfdata is just a randomly created dataset to show how this package works.
##' The centre names are derived from names of towns on the north-eastern part of Borneo.
##'
##' @format  \code{hfdata} consist of several columns:
##' \itemize{
##'   \item inst: The institution names and "Sabah" is the region name
##'   \item id: The identification number of the centres
##'   \item 2003-2007: The measurement collected yearly based
##'   \item case1: Cases normally distributed with mean=60 and SD=30
##'   \item case2: Cases normally distributed with mean=20 and SD=2
##'   \item extt: Variable with extreme values
##'   \item ll: Lower limit for case2
##'   \item up: Upper limit for case2
##' }
##'

"hfdata"


##' yrdata is sample data for trends
##'
##' yrdata is just a sample data to use in example for "regline" function.
##'
##' @format \code{yrdata} consist of these variables:
##' \itemize{
##'   \item year: List of different years
##'   \item var: Variable to be grouped
##'   \item N: Number of n for each group
##'   \item sum: Total for each year
##'   \item pros: Percentage for each group
##' }

"yrdata"
