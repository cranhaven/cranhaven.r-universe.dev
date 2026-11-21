#' @title LDA_table
#'
#' @description show table with activities and clonogenic survival from limiting
#'   dilution assay (LDA) data.
#'
#' @param x numeric data.frame or matrix with at least three columns (cells,
#'   wells, positive wells, group (optional))
#' @param ref_class name of reference class for calculation of SF values
#' @param uncertainty method for calculating the uncertainty bands of survival
#'   fractions ("act" (default) for combining the activity confidence intervals;
#'   "ep" for error propagation via first order Taylor series expansion.)
#'
#' @return table
#'
#' @examples
#' x <- data.frame("cells" = c(10,50,100,250,10,50,100,250),
#'                 "wells" = rep(25,8),
#'                 "positive" = c(2,5,10,20,1,2,6,11),
#'                 "group" = c(rep("A",4),rep("B",4)))
#' LDA_table(x,ref_class = "A")
#' @export
#'
LDA_table <- function(x,ref_class = "unknown",uncertainty = "act"){
  if (ncol(x)==3){
    act <- LDA_activity_single(x = x)
    show_LDA <- list(
      "activity^-1 [N]" = round(act$act,digits = 3),
      "confidence interval" = round(act$CI,digits = 3),
      "cooperativity coefficient b" = round(act$est[2,1],digits = 3),
      "p-value cooperativity" = round(act$p.lin.Model,digits = 3))
    return(show_LDA)
  }
  colnames(x)[1:4] <- c("cells","wells","positive","group")
  if (!is.numeric(x$cells) | !is.numeric(x$wells) | !is.numeric(x$positive)){
    stop("error: all elements of x must be numeric")
  }
  if (ref_class == "unknown"){
    ref_class <- unique(x$group)[1]
    if (ref_class != 0){
      warning(paste0("warning: reference class for survival analysis is ",
                     ref_class,
                     ", not 0. - specify ref_class or reorder data!"))
    } else {
      message(" reference class is 0")
    }
  }
  x <- rbind(subset.data.frame(x = x,subset = x$group==ref_class),
             subset.data.frame(x = x,subset = x$group!=ref_class))
  act <- LDA_activity(x[,1:4])
  sf <- LDA_survival(x[,1:4])
  show_LDA <- data.frame("treatment" = unique(x$group),
                         "act" = NA,
                         "act.CI.lb" = NA,
                         "act.CI.ub" = NA,
                         "b" = NA,
                         "b.pvalue" = NA,
                         "SF" = NA,
                         "SF.CI.lb" = NA,
                         "SF.CI.ub" = NA)
  a <- act[[1]]
  show_LDA$treatment[1] <- ref_class
  show_LDA$act[1] <- a$act
  show_LDA$act.CI.lb[1] <- a$CI[1]
  show_LDA$act.CI.ub[1] <- a$CI[2]
  show_LDA$b[1] <- a$est[2,1]
  show_LDA$"b.pvalue"[1] <- a$p.lin.Model
  for (i in 2:length(act)){
    a <- act[[i]]
    show_LDA$treatment[i] <- a$treatment
    show_LDA$act[i] <- a$act
    show_LDA$act.CI.lb[i] <- a$CI[1]
    show_LDA$act.CI.ub[i] <- a$CI[2]
    show_LDA$b[i] <- a$est[2,1]
    show_LDA$"b.pvalue"[i] <- a$p.lin.Model
    s <- sf[[i-1]]
    show_LDA$SF[i] <- s$sf
    switch(uncertainty,
           "act" = {
             show_LDA$SF.CI.lb[i] <- s$CI.act[1]
             show_LDA$SF.CI.ub[i] <- s$CI.act[2]
           },
           "ep" = {
             show_LDA$SF.CI.lb[i] <- s$CI.ep[1]
             show_LDA$SF.CI.ub[i] <- s$CI.ep[2]
           })
  }
  return(show_LDA)
}
