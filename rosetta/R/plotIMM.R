

#' Makes plot of Index of Moderated Mediation of gemm object
#'
#' @param x   object moderatedMediationSem
#' @param ... optional
#'
#' @return simple slope plots for each mediator and simple slopes parameter estimates
#' @export
#'
plotIMM <- function(x,...) {

  data <- x$intermediate$data
  xmmod <- x$input$xmmod
  mymod <- x$input$mymod
  xvar <- x$input$xvar
  yvar <- x$input$yvar
  mvars <- x$input$mvars
  parEst <- x$intermediate$parameterEstimates
  xmoderator <- x$input$data[,xmmod]
  ymoderator <- x$input$data[,mymod]
  xdichotomous <- x$intermediate$xdichotomous
  ydichotomous <- x$intermediate$ydichotomous

  if ((!length(xmmod)) & (!length(mymod)))
    return(cat("No plots can be given, because no moderators have been specified"))

  ## test if moderator exists for x=m path and if it is dichotomous factor
  if (length(xmmod)) {
    if (length(unique(xmoderator)) == 2) xmodLevels <- c(0,1)
    if (is.factor(xmoderator)) {
      if (length(levels(xmoderator)) > 2) {
        stop("This function can not yet plot moderation with a moderator (x-m path) that is a factor with more than two levels.");
      }
      else {
        xmodLevels <- levels(xmoderator);
        data[,xmmod] <- as.numeric(xmoderator) - 1;
     }
    }
    res <-
      prepPlotIMM(data=data, xvar=xvar, yvar = yvar, mod = xmmod, mvars = mvars, parEst = parEst,
                 vdichotomous = xdichotomous, modLevels = xmodLevels, path = "x-m")
  }

  ## test if moderator exists for m=y path and if it is dichotomous factor

  if (length(mymod)) {
    if (length(unique(ymoderator)) == 2) ymodLevels <- c(0,1)
    if (is.factor(ymoderator)) {
      if (length(levels(ymoderator)) > 2) {
        stop("This function can not yet plot moderation with a moderator (x-y path) that is a factor with more than two levels.")}
      else {
        ymodLevels <- levels(ymoderator);
        data[,mymod] <- as.numeric(ymoderator) - 1;
      }
    }

    res <-
      prepPlotIMM(data=data, xvar=xvar, yvar = yvar, mod = mymod, mvars = mvars, parEst = parEst,
                 vdichotomous = ydichotomous, modLevels = ymodLevels, path = "m-y")
  }

  #invisible()
  return(res)

}  # end function



