#' Makes simple slope plots
#'
#' @param data data frame containg the variables of the model
#' @param xvar predictor variable name
#' @param yvar depedendent variable name
#' @param mod moderator name
#' @param mvars vector of mediators names
#' @param parEst parameter estimates from lavaan results
#' @param vdichotomous indicates whether moderator is dichotomous (TRUE)
#' @param modLevels levels of dichotomous moderator
#' @param predLevels levels of dichotomous moderator
#' @param xquant quantiles of x
#' @param yquant quantiles of y
#' @param path which path is used
#' @import ggplot2
#' @return empty, directly plots all simple slopes and all indices of mediation
#' @export

prepPlotSS <- function(data,xvar,yvar,mod, mvars, parEst, vdichotomous,
                       modLevels, predLevels = NULL, xquant, yquant, path = NULL) {


    if (vdichotomous) {
       modquant <- c(0,1)
       ifelse(is.null(modLevels), legendLabel <- c(0,1), legendLabel <- modLevels)
    } else {
       modquant <- stats::quantile(data[,mod], c(.16,.84), na.rm = TRUE)
       legendLabel <- c("16th percentile", "84th percentile")
    }

    if (path == "x-m") {
        vorw <- "w"
        inter <- "im"
        modmed <- "modmedx"
    } else {
          vorw <- "v"
          inter <- "iy"
          modmed <- "modmedm"
      }

  ind <- subset(parEst, grepl("ind", parEst$label))[,c("ci.lower","est","ci.upper")]
  vw <- subset(parEst, grepl(vorw, parEst$label))[,c("ci.lower","est","ci.upper")]
  int <- subset(parEst, grepl(inter, parEst$label))[,c("ci.lower","est","ci.upper")]
  mm <- subset(parEst, grepl(modmed, parEst$label))[,c("ci.lower","est","ci.upper")]

  bw <- subset(parEst, grepl("bw", parEst$label))[,c("ci.lower","est","ci.upper")]
  gw <- subset(parEst, grepl("gw", parEst$label))[,c("ci.lower","est","ci.upper")]

  N <- dim(data)[1]

  if (vorw == "v") bw <- (matrix(as.numeric(vw), nrow=length(mvars), ncol = 3, byrow = TRUE ))


  # initialize data for mediated simple slopes

  plotDat2 <- data.frame(yv=numeric(), xv=numeric(), mov=numeric(),mev=factor())
  xv <-  c(xquant[1],xquant[1],xquant[2],xquant[2])
  mov <- c(modquant,modquant)

  # loop over mediators

  for (i in seq_along(mvars)) {

      d1 <-  matrix(as.numeric(ind[i,]), nrow = 2, ncol=3, byrow = TRUE)
      d2 <-  (modquant %o% as.numeric(mm[i,]))
      yIom2 <- d1+ d2

    pred1 <-  as.numeric(modquant) %o%  as.numeric(bw[i,])  + yIom2*xquant[1]
    pred2 <-  as.numeric(modquant) %o%  as.numeric(bw[i,])  + yIom2*xquant[2]
    pred <- rbind(pred1, pred2)
    plotDat1 <- data.frame(cbind(pred, xv = xv))
    plotDat1$mov <- as.factor(round(mov,1))
    plotDat1$mev <- as.factor(rep(mvars[i],4))

    plotDat2 <- rbind(plotDat2,plotDat1)

     }  # loop mvars


    names(plotDat2) <- c("lwr",yvar,"upr", xvar, mod, "mediator")
    ymin <- min(c(plotDat2$yvar, plotDat2$lwr,plotDat2$upr, yquant))
    ymax <- max(c(plotDat2$yvar, plotDat2$lwr,plotDat2$upr, yquant))
    if (!is.null(predLevels)) {
      plotDat2[,xvar] <- as.factor(plotDat2[,xvar])
      levels(plotDat2[,xvar]) <- predLevels
    }

    plot_simpleSlopes <-
      ggplot2::ggplot(plotDat2,
                      ggplot2::aes_string(x=xvar,
                                          y=yvar,
                                          group= mod,
                                          colour=mod)) +
      ggplot2::geom_point() +
      ggplot2::geom_line() +
      ggplot2::geom_ribbon(ggplot2::aes_string(ymin='lwr',
                                               ymax='upr'),
                           alpha=.3,
                           linetype=0) +
      ggplot2::ylim(ymin,ymax) +
      ggplot2::theme(plot.title = ggplot2::element_text(lineheight=.4, face="italic")) +
      ggplot2::ggtitle(paste0("Simple slopes in ", path , " path for indirect effect ")) +
      ggplot2::scale_colour_discrete(name  = mod, labels=legendLabel) +
      ggplot2::facet_grid(rows=ggplot2::vars(plotDat2$mediator))

  return(plot_simpleSlopes)

} # end function





