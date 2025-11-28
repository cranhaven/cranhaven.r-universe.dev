#' Iterate Over a Grid of All Possible Quantiles and Calculate Probabilities
#'
#' This function queries the probabilistic space of a pbox object to calculate probabilities
#' associated with specific marginal or conditional distributions on a quantile grid. It supports conditional
#' probability calculations as well.
#'
#' @name grid_pbox
#' @export
#' @param pbx An object of class \code{pbox} from which to query the probabilistic space.
#' @param mj A character vector specifying the variables to query.
#' @param co A character vector specifying the variables to query
#' @param probs A numeric vector of quantiles to calculate probabilities for (default: seq(0, 1, 0.1)).
#' @param ... Additional parameters passed to \code{qpbox}.
#' @return A data.table containing estimated probabilities for each combination of quantiles and distributions queried.
#' @examples
#'   data("SEAex")
#'   pbx <- set_pbox(SEAex)
#'   grid_pbox(pbx, mj = c("Vietnam", "Malaysia"))
#' @importFrom data.table setDT
#' @importFrom stats quantile

setGeneric("grid_pbox",
           def = function(pbx, mj = character(), co = NULL, probs = seq(0, 1, 0.1), ...) {
             standardGeneric("grid_pbox")
           })


#' @rdname grid_pbox
#' @description
#' This method processes the \code{pbox} object to compute probabilities based on the specified marginal
#' and conditional parameters. It handles both simple probability calculations and complex queries involving
#' joint and conditional distributions, with an option for bootstrap confidence interval estimation.
#'
#' @param pbx An object of class \code{pbox} from which to query the probabilistic space.
#' @param mj A character vector specifying the variables to query.
#' @param co A character vector specifying the variables to query
#' @param probs A numeric vector of quantiles to calculate probabilities for (default: seq(0, 1, 0.1)).
#' @param ... Additional parameters passed to \code{qpbox}.
#' @return A data.table containing estimated probabilities for each combination of quantiles and distributions queried.

setMethod("grid_pbox", signature = "pbox",
          definition = function(pbx, mj = character(), co = NULL, probs = seq(0, 1, 0.1), ...) {
            if (!is.character(mj)) {
              stop("Expecting 'mj' to be a vector of variable names!")
            }

            # Validate co to be NULL or a character vector
            if (!is.null(co) && !is.character(co)) {
              stop("Expecting 'co' to be NULL or a vector of variable names!")
            }
            cols <- c(mj, co)
            qframe <- as.data.frame(apply(pbx@data[, ..cols], 2, quantile, probs = probs))
            qgrid <- expand.grid(as.list(qframe))
            setDT(qgrid)
            generate_string <- function(row) {
              string <- paste(names(row), row, sep = ":", collapse = " & ")
              return(string)
            }
            if (is.null(co)) {
              allCombo <- apply(qgrid, 1, generate_string)
              qgrid$probs <- lapply(allCombo, function(x) {
                qpbox(pbx, mj = x, ...)
              })
            } else {
              mjCombo <- apply(qgrid[, ..mj], 1, generate_string)
              coCombo <- apply(qgrid[, ..co], 1, generate_string)
              allCombo <- cbind.data.frame(mjCombo, coCombo)
              qgrid$probs <- apply(allCombo, 1, function(x) {
                qpbox(pbx, mj = x["mjCombo"], co = x["coCombo"], ...)
              })
            }
            return(qgrid)
          })
