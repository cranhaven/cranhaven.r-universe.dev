#' Makes Index of Mediated Moderated plots
#'
#' @param data data frame containg the variables of the model
#' @param xvar predictor variable name
#' @param yvar depedendent variable name
#' @param mod moderator name
#' @param mvars vector of mediators names
#' @param parEst parameter estimates from lavaan results
#' @param vdichotomous indicates whether moderator is dichotomous (TRUE)
#' @param modLevels levels of dichotomous moderator
#' @param path which path is used
#' @import ggplot2
#' @return empty, directly plots all simple slopes and all indices of mediation
#' @export

prepPlotIMM <- function(data,xvar,yvar,mod, mvars, parEst, vdichotomous,
                              modLevels, path = NULL) {

  xquant <- stats::quantile(data[,xvar], c(.16,.84), na.rm = TRUE)
  yquant <- stats::quantile(data[,yvar], c(.16,.84), na.rm = TRUE)

  # compute simple slopes

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



  # initialize data for index mediated moderation

  plotData <- data.frame(X1 = numeric(),X2 = numeric(),X3 = numeric(),
                         moderator = numeric(),
                         mediator = factor())
  moderator <- as.numeric(data[,mod])


  # loop over mediators

  for (i in seq_along(mvars)) {

   # index of moderated mediation

        d1 <-  matrix(as.numeric(ind[i,]), nrow = N, ncol=3, byrow = TRUE)
        d2 <- (moderator %o% as.numeric(mm[i,]))
        yIom <- d1+ d2
      mediator <- rep(mvars[i],nrow(data))
      plotDat0 <- data.frame(yIom,moderator,mediator);
      plotData <- rbind(plotData,plotDat0)

     }  # loop mvars


  names(plotData) <- c("IMM_lwr",'IMM',"IMM_upr", mod, "mediator")
  ymin <- min(plotData$IMM,plotData$IMM_lwr,plotData$IMM_upr, yquant, na.rm = TRUE)
  ymax <- max(plotData$IMM,plotData$IMM_lwr,plotData$IMM_upr, yquant, na.rm = TRUE)


  if (!vdichotomous) {

      plot_indexOfmediation <-
        ggplot2::ggplot(plotData, ggplot2::aes_string(x=mod,y="IMM",colour = "mediator")) +
        ggplot2::geom_line(ggplot2::aes(colour = mediator, group = mediator)) +
        ggplot2::coord_cartesian(ylim=c(ymin, ymax)) +
        ggplot2::ggtitle("Index of moderated mediation") +
        ggplot2::xlab(paste0("Moderator: ",mod)) +
        ggplot2::geom_ribbon(ggplot2::aes_string(ymin="IMM_lwr", ymax="IMM_upr"),
                             alpha=.3, linetype=0)

      #print(plot_indexOfmediation)
  }

  if (vdichotomous) {

    pd <- position_dodge(0.1)

    plotData[,mod] <- as.factor(plotData[,mod])
    levels(plotData[,mod]) <- legendLabel

    plot_indexOfmediation <-
      ggplot2::ggplot(plotData, ggplot2::aes_string(x=mod,y="IMM",colour = "mediator")) +
      ggplot2::geom_point(ggplot2::aes(colour = mediator), position = pd, size=2) +
      ggplot2::geom_errorbar(ggplot2::aes_string(ymin="IMM_lwr", ymax="IMM_upr"),
                             width=0.2, size=0.5, position = pd) +
      ggplot2::coord_cartesian(ylim=c(ymin, ymax)) +
      ggplot2::ggtitle("Index of moderated mediation") +
      ggplot2::xlab(paste0("Moderator: ",mod))

    #print(plot_indexOfmediation)
  }


  return(plot_indexOfmediation)

} # end function





