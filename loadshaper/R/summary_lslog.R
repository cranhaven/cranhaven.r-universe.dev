
#' @title Summary of Logistic Load Shape
#'
#'
#' @description Print method for summarizing
#' \code{lslog} object
#'
#'
#' @param object An object of class \code{lslog},
#' created by the function \code{\link{lslog}}.
#'
#' @param ... \code{NULL}. Used for S3 generic/method consistency.
#'
#'
#' @return \code{NULL}. Prints the summary of the derived
#' load shape using linear method.
#'
#'
#'
#'
#' @seealso \code{\link{lslog}}
#'
#'
#' @note Same as \code{\link{print.lslog}}
#'
#'
#'
#' @examples
#' # ---------------------
#' loads <- ercot[ercot$Year == 2019, ]$COAST
#' logistic_loadshape <- lslog(loads, target_lf = 0.5, k = 0.5)
#' summary(logistic_loadshape)
#' # ---------------------
#' loads2 <- ercot[ercot$Year == 2019, ]$ERCOT
#' logistic_loadshape2 <- lslog(loads2, target_lf = 0.75, k = 1)
#' summary(logistic_loadshape2)
#'
#'
#'

#' @method summary lslog
#' @export

summary.lslog <- function(object, ... = NULL)

{
  obj <- object
  linespace <- "------------------------------------"

  line11 <- paste("Steepness factor :", obj$k)
  line12 <- paste("Inflection position :", obj$inf_pos)
  line13 <- paste("L parameter (solved) :", obj$L)



  line2 <- paste("Maximum Multilier :", obj$max_mult)
  line3 <- paste("Minimum Multilier :", obj$min_mult)

  line4 <- paste("Base Shape Load Factor :",
                 obj$base_load_factor)
  line5 <- paste("Target Load Factor :",
                 obj$target_load_factor)
  line6 <- paste("Derived Shape Load Factor :",
                 obj$derived_load_factor)

  line7 <- paste("Base Shape Peak :",
                 obj$base_max)
  line8 <- paste("Target Peak :", obj$target_max)
  line9 <- paste("Derived Shape Peak :", obj$derived_max)

  line10 <- paste("Base Shape Min :", obj$base_min)
  line11 <- paste("Derived Shape Min :", obj$derived_min)


  base_smry <- rbind(line11, line12, line13, linespace,
                     line2, line3, linespace,
                     line4, line5, line6, linespace,
                     line7, line8, line9, linespace,
                     line10, line11, linespace)


  # dec_flag
  if(obj$dec_flag){
    line_dec_1 <- "Multipliers (obj$mult) do not"
    line_dec_2 <- "yield  decreasing obj$df$y_ordered_pu;"
    line_dec_3 <- "Re-orderdered in  obj$df$y_ordered_pu2 which "
    line_dec_4 <- "is used in obj$df$y_pu2 and obj$df$y2."

    dec_smry <- data.frame(rbind(line_dec_1, line_dec_2,
                                 line_dec_3, line_dec_4))

  }else{
    dec_smry <- NULL
  }




  # lf_flag
  if(obj$lf_flag){
    line_lf_1 <- "Derived load factor differs from target"
    line_lf_2 <- "by more than 1%;"
    lf_smry <- data.frame(rbind(line_lf_1, line_lf_2))

  }else{
    lf_smry <- NULL
  }


  # min_pu_flag
  if(obj$min_pu_flag){
    line_min_1 <- "Algorithm yielded negative multipliers;"
    line_min_2 <- "resulted in negative values in the derived"
    line_min_3 <- "load shape."
    min_smry <- data.frame(rbind(line_min_1, line_min_2,
                                 line_min_3))
  }else{
    min_smry <- NULL
  }


  tmp <- data.frame(rbind(base_smry))
  tmp <- format(tmp, justify = "left")
  names(tmp) <- NULL
  rownames(tmp) <- NULL
  print(tmp, row.names = FALSE)


  linespace <- "===================================="
  tmp <- data.frame(rbind(linespace, dec_smry))
  tmp <- format(tmp, justify = "left")
  names(tmp) <- NULL
  rownames(tmp) <- NULL
  print(tmp, row.names = FALSE)



  if(obj$lf_flag){
    linespace <- "===================================="
    tmp <- data.frame(rbind(linespace, lf_smry))
    tmp <- format(tmp, justify = "left")
    names(tmp) <- NULL
    rownames(tmp) <- NULL
    print(tmp, row.names = FALSE)
  }



  if(obj$min_pu_flag){
    linespace <- "===================================="
    tmp <- data.frame(rbind(linespace, min_smry))
    tmp <- format(tmp, justify = "left")
    names(tmp) <- NULL
    rownames(tmp) <- NULL
    print(tmp, row.names = FALSE)
  }
}


