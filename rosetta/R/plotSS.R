

#' Makes simple slope plots of gemm object
#'
#' @param x   object moderatedMediationSem
#' @param ... optional
#'
#' @return simple slope plots for each mediator and simple slopes parameter estimates
#' @export
#'
plotSS <- function(x,...) {

  data <- x$intermediate$data
  xmmod <- x$input$xmmod
  mymod <- x$input$mymod
  xvar <- x$input$xvar
  yvar <- x$input$yvar
  mvars <- x$input$mvars
  xmoderator <- x$input$data[,xmmod]
  ymoderator <- x$input$data[,mymod]
  parEst <- x$intermediate$parameterEstimates
  predLevels <- x$intermediate$predLevels
  xdichotomous <- x$intermediate$xdichotomous
  ydichotomous <- x$intermediate$ydichotomous

  if ((!length(xmmod)) & (!length(mymod))) {
    stop("No plots can be given, because no moderators have been specified");
  }

  xquant <- stats::quantile(data[,xvar],c(.16,.84), na.rm = TRUE)
  if (xquant[1] == xquant[2])  {
    if (!is.null(predLevels))  {
      xquant <- c(0,1)
    } else {
      stop("The 16th and 84th precentile of the predictor are the same. No plots are made.");
    }
  }

  yquant <- stats::quantile(data[,yvar], c(.16,.84), na.rm = TRUE)

  ## test if moderator exists for x=m path

  if (length(xmmod)) {
    if (length(unique(xmoderator)) == 2) xmodLevels <- c(0,1)
    if (is.factor(xmoderator)) xmodLevels <- levels(xmoderator)

    res <-
      prepPlotSS(data=data, xvar=xvar, yvar = yvar, mod = xmmod, mvars = mvars, parEst = parEst,
                 vdichotomous = xdichotomous, modLevels = xmodLevels, predLevels = predLevels,
                 xquant = xquant, yquant = yquant, path = "x-m")
  }

  ## test if moderator exists for m=y path

  if (length(mymod)) {
    if (length(unique(ymoderator)) == 2) {
      ymodLevels <- c(0,1);
    }
    if (is.factor(ymoderator)) {
      ymodLevels <- levels(ymoderator);
    }
    res <-
      prepPlotSS(data=data, xvar=xvar, yvar = yvar, mod = mymod, mvars = mvars, parEst = parEst,
                 vdichotomous = ydichotomous, modLevels = ymodLevels,predLevels = predLevels,
                 xquant = xquant, yquant = yquant, path = "m-y");
  }

  return(res);

}  # end function



