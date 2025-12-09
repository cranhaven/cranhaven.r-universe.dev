# See:
# https://stackoverflow.com/questions/57618769/how-to-automatically-load-functions-into-namespace-of-an-r-package
# https://github.com/sckott/analogsea/issues/32#issue-43772852
# https://github.com/tidyverse/tibble/blob/master/R/pipe.R

### * dplyr::groups

#' @importFrom dplyr groups
#' @export
dplyr::groups

### * dplyr::select

#' @importFrom dplyr select
#' @export
dplyr::select

### * coda::as.mcmc.list

#' @importFrom coda as.mcmc.list
#' @export
coda::as.mcmc.list

### * coda::varnames

#' @importFrom coda varnames
#' @export
coda::varnames

### * magrittr pipe operator

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`
