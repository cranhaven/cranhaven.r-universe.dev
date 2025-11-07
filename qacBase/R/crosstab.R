#' @title Two-way frequency table
#' @description This function creates a two way frequency table.
#' @param data data frame
#' @param rowvar row factor (unquoted)
#' @param colvar column factor (unquoted)
#' @param type statistics to print. Options are \code{"freq"},
#' \code{"percent"}, \code{"rowpercent"}, or \code{"colpercent"}
#' for frequencies, cell percents, row percents, or column percents).
#' @param total logical. if TRUE, includes total percents.
#' @param na.rm logical. if TRUE, deletes cases with missing values.
#' @param digits number of decimal digits to report for percents.
#' @param chisquare logical. If \code{TRUE} perform a chi-square test
#' of independence
#' @param plot logical. If \code{TRUE} generate stacked bar chart.
#' @return If \code{plot=TRUE}, return a ggplot2 graph.
#' Otherwise the function return a list with 6 components:
#' \itemize{
#' \item{\code{table}}{ (table). Table of frequencies or percents}
#' \item{\code{type}}{ (character). Type of table to print}
#' \item{\code{total}}{ (logical). If \code{TRUE}, print row and or column totals}
#' \item{\code{digits}}{ (numeric). number of digits to print}
#' \item{\code{rowname}}{ (character). Row variable name}
#' \item{\code{colname}}{ (character). Column variable name}
#' \item{\code{chisquare}}{ (character). If \code{chisquare=TRUE}, contains
#' the results of the Chi-square test. \code{NULL} otherwise.}
#' }
#'
#' @details Given a data frame, a row factor, a column factor, and a
#' type (frequencies, cell percents, row percents, or column percents)
#' the function provides the requested cross-tabulation.
#'
#' If \code{na.rm = FALSE}, a level labeled \code{<NA>} added. If
#' \code{total = TRUE}, a level labeled \code{Total} is added. If
#' \code{chisquare = TRUE}, a chi-square test of independence is
#' performed.
#'
#' @seealso \link{print.crosstab}, \link{plot.crosstab}
#' @examples
#' # print frequencies
#' crosstab(mtcars, cyl, gear)
#'
#' # print cell percents
#' crosstab(cardata, vehicle_size, driven_wheels)
#' crosstab(cardata, vehicle_size, driven_wheels,
#' plot=TRUE)
#' crosstab(cardata, driven_wheels, vehicle_size,
#' type="colpercent", plot=TRUE, chisquare=TRUE)
#' @rdname crosstab
#' @export

crosstab <- function(data, rowvar, colvar,
                     type=c("freq", "percent",
                            "rowpercent", "colpercent"),
                     total=TRUE,
                     na.rm=TRUE,
                     digits=2,
                     chisquare=FALSE,
                     plot=FALSE){

  rowvar <- deparse(substitute(rowvar))
  colvar <- deparse(substitute(colvar))
  type <- match.arg(type)
  if (na.rm){
    useNA <- "no"
  } else {
    useNA <- "always"
  }
  tb <- table(data[[rowvar]], data[[colvar]], useNA=useNA)
  names(dimnames(tb)) <- c(rowvar, colvar)

  # chisquare test
  x2 <- NULL
  if (chisquare){
    x2 <- suppressWarnings(stats::chisq.test(tb))
    x2 <- paste0("Chi-square = ", round(x2$statistic,2),
                ", df = ", x2$parameter,
                ", p = ", format.pval(x2$p.value, digits=2))
  }

  # results
  tb <- switch(type,
                "freq" = tb,
                "percent"= prop.table(tb),
                "rowpercent" = prop.table(tb, 1),
                "colpercent" = prop.table(tb, 2)
                )

  # package results
  results <- list(table = tb,
                  type=type,
                  total=total,
                  digits=digits,
                  rowname = rowvar,
                  colname = colvar,
                  chisquare = x2)


  class(results) <- c("crosstab", "list")
  if (plot){
    return(plot(results))
  } else{
    return(results)
  }
  return(results)
}
