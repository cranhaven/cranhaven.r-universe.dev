#' @title Plot Logistic Load Shape
#'
#' @description Plot method of \code{lslog} object



#' @param x An object of class \code{lslog}, created by
#' \code{\link{lslog}} function.
#'
#' @param case A numeric value from \{1, 2, 3\} to select
#' the type of plot. See "Details".
#'
#'
#' @param col Color of the plots. Can be numeric or
#' text or mixed as in \code{\link{col}}. For
#' \code{length(col) < 2}, a default second color is used.
#'
#'
#' @param scatter Logical. Scatter plot if \code{TRUE},
#' line plot if \code{FALSE}.
#'
#' @param legend Logical indicating if legend to be displayed.
#'
#' @param leg_pos A text value for position/location
#' of the legend. Default is \code{topright}.
#' See \code{\link{legend}} for full list
#' of keywords.
#'
#'
#' @param ... \code{NULL}. Used for S3 generic/method consistency.
#'
#'
#' @details If \code{scatter = FALSE}
#' then per unit load duration curve for \code{case = 1},
#' per unit load for \code{case = 2}, actual load (in original
#' unit) for \code{case = 3}. If \code{scatter = TRUE}
#' then  per unit scatter plot for \code{case = 1 or 2},
#' actual load scatter plot for \code{case = 3}.
#'
#'
#' @return \code{NULL}.
#'
#' @examples
#' loads <- ercot[ercot$Year == 2019, ]$COAST
#' loads <- ercot[ercot$Year == 2019, ]$COAST
#' log_loadshape <- lslog(loads, target_lf = 0.5,
#'                        inf_pos = 0.3, k = 0.8)
#' # --------------
#' plot(log_loadshape, col = c(2, 4))
#' plot(log_loadshape, case = 2, col = c(2, 4))
#' plot(log_loadshape, case = 3,
#'      col = c("salmon", "deepskyblue"),
#'      leg_pos = "topleft")
#'
#'
#'
#'
#' @method plot lslog
#' @export
plot.lslog <- function(x, case = 1, col = c(1, 2),
                       scatter = FALSE, legend = TRUE,
                       leg_pos = "topright",
                       ... = NULL)
{

  obj <- x

  case_1 <- case == 1 # load duration pu
  case_2 <- case == 2 # load pu
  case_3 <- case == 3 # load

  if(length(col) < 2){
    col <- c(col, 2)
    warning("length(col) < 2, default second color used")
  }




  if(case_1){
    base <- obj$df$x_ordered_pu
    derived <- obj$df$y_ordered_pu2

    ln_ttl <- "Base and Derived Load Shape Line Plot"
    ln_sbttl <- "Ordered, Per Unit Values"

    sc_ttl <- "Base and Derived Load Shape Scatter Plot"
    sc_sbttl <- "Per Unit Values"
  }

  if(case_2){
    base <- obj$df$x_pu
    derived <- obj$df$y_pu2

    ln_ttl <- "Base and Derived Load Shape Line Plot"
    ln_sbttl <- "Per Unit Values"

    sc_ttl <- "Base and Derived Load Shape Scatter Plot"
    sc_sbttl <- "Per Unit Values"
  }

  if(case_3){
    base <- obj$df$x
    derived <- obj$df$y2

    ln_ttl <- "Base and Derived Load Shape Line Plot"
    ln_sbttl <- "Actaul Values"

    sc_ttl <- "Base and Derived Load Shape Scatter Plot"
    sc_sbttl <- "Actual Values"
  }


  base_lftxt <- paste("Base LF: ", round(obj$base_load_factor, 4))
  derived_lftxt <- paste("Derived LF: ",
                         round(obj$derived_load_factor, 4))
  target_lftxt <- paste("Target LF: ",
                        round(obj$target_load_factor, 4))



  max_val <- max(max(base), max(derived)) * 1.12
  min_val <- min(min(base), min(derived)) * 0.88



  if(scatter){
    graphics::plot(derived ~ base, xlim = c(min_val, max_val),
                   cex = 0.8,
         ylim = c(min_val, max_val), col = col[1], pch = 16,
         xlab = "Base Vlaues", ylab = "Derived Values")
    graphics::abline(0, 1, lwd = 1.2, col = col[2])
    graphics::grid(col = 1)

    graphics::mtext(side = 3, sc_ttl, font = 2,
                    cex = 1, line = 1)
    graphics::mtext(side = 3,
                    sc_sbttl, font = 2, cex = 0.8, line = 0.3)




  }else{
    tmp_lwd <- ifelse(case == 1, 2, 0.7)
    graphics::plot(base, type = "l",
                   col = col[1], lwd = tmp_lwd,
         ylim = c(min_val, max_val), ylab = "Values")


    graphics::lines(derived, type = "l",
                    col = col[2], lwd = tmp_lwd,
          ylim = c(min_val, max_val), lty = 1)

    graphics::grid(col = 1)
    graphics::mtext(side = 3, ln_ttl, font = 2,
                    cex = 1, line = 1)
    graphics::mtext(side = 3, ln_sbttl,
                    font=2, cex = 0.8, line = 0.3)
    if(legend){
      graphics::legend(leg_pos, c("Base", "Derived"),
             col = c(col[1], col[2]), border = 1,
             bty = "n", lwd = 3)}


  }

}
