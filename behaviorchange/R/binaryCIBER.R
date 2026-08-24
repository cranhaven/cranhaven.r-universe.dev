#' @export
#' @param comparisonColors Colors to use for the two groups in a
#' binary CIBER plot with one (dichotomous) target.
#' @param categoryLabels Labels for the two values of the target.
#' @param dotAlpha The alpha (opaqueness, i.e. inverse transparency) of the
#' dots representing data points.
#' @examples \donttest{### With a binary target
#' data(BBC_pp17.1);
#' behaviorchange::binaryCIBER(data=BBC_pp17.1,
#'                             determinants=c('epGeneralBeliefs_loudnessPreference',
#'                                            'epGeneralBeliefs_loudnessGenre',
#'                                            'epGeneralBeliefs_loudnessTooMuch',
#'                                            'epGeneralBeliefs_priceFoam',
#'                                            'epGeneralBeliefs_priceSilicon',
#'                                            'epGeneralBeliefs_priceCustom'),
#'                             targets=c('epPossession'),
#'                             categoryLabels = c('no',
#'                                                'yes'));
#'
#' }
#' @rdname CIBER
binaryCIBER <- function(data,
                        determinants,
                        targets,
                        conf.level = list(means = .9999,
                                          associations = .95),
                        subQuestions = NULL,
                        leftAnchors = rep("Lo", length(determinants)),
                        rightAnchors = rep("Hi", length(determinants)),
                        outputFile = NULL,
                        outputWidth = NULL,
                        outputHeight = NULL,
                        outputUnits = "in",
                        outputParams = list(),
                        orderBy = NULL,
                        decreasing = NULL,
                        numberSubQuestions = FALSE,
                        comparisonColors=viridis::viridis(2, end=.5),
                        categoryLabels=NULL,
                        generateColors = list(means = c("red", "blue", "green"),
                                              associations = c("red", "grey", "green")),
                        strokeColors = viridis::viridis(length(targets)),
                        vLines = c(-0.8, 0, 0.8),
                        vLineColors = "grey",
                        titlePrefix = "Means and associations (d) with",
                        titleVarLabels = NULL,
                        titleSuffix = "",
                        fullColorRange = NULL,
                        associationsAlpha = .5,
                        dotAlpha = .33,
                        returnPlotOnly = TRUE,
                        drawPlot = TRUE,
                        baseSize = .8,
                        dotSize = 2.5 * baseSize,
                        baseFontSize=10*baseSize,
                        theme=ggplot2::theme_bw(base_size=baseFontSize),
                        xbreaks=NULL,
                        ...) {

  if (!all(c(determinants, targets) %in% names(data))) {
    stop("Not all variables names you passed in arguments ",
         "'determinants' or 'targets' are in the dataset!\n",
         "Specifically, ",
         ufs::vecTxtQ(c(determinants, targets)[!(c(determinants, targets) %in% names(data))]),
         " is or are not in the provided dataset.");
  }

  res <- list(input = as.list(environment()),
              intermediate = list(),
              output = list());

  if (is.null(subQuestions)) subQuestions <- determinants;

  ### Check whether none of the determinants is constant for one
  ### of the target values; if so, kick them out now and issue a
  ### warning.
  constantDeterminants <- c();
  constantTargets <- c();
  for (currentDeterminant in determinants) {
    for (currentTarget in targets) {
      constantTable <-
        table(data[, currentDeterminant],
              data[, currentTarget]);
      if ((all(constantTable[, 1] == 0)) | (all(constantTable[, 2] == 0))) {
        constantDeterminants <-
          c(constantDeterminants,
            currentDeterminant);
        constantTargets <-
          c(constantTargets,
            currentTarget);
      }
    }
  }
  if (length(constantDeterminants) > 0) {
    determinants <- setdiff(determinants,
                            constantDeterminants);
    if (length(determinants) > 0) {
      warning(paste0("One or more (sub-)determinants has a constant value for one or more targets. ",
                     "These have been removed from the (sub-)determinant list. These are the following ",
                     "(sub-)determinant and target combinations: ",
                     vecTxt(paste0("(sub-)determinant '", constantDeterminants, "' with target '",
                                   constantTargets, "'")), "."));
    } else {
      stop("All determinants are constant for one or more targets!");
    }
  }

  ### Extract relevant subdatasets
  res$intermediate$determinantsDat <- data[, determinants];
  res$intermediate$dat <- data[, c(determinants, targets)];

  if (!inherits(data, 'data.frame')) {
    stop("After having extracted the determinants (",
         ufs::vecTxtQ(determinants), ") and the targets (",
         ufs::vecTxtQ(targets), ") from the provided data frame, '",
         deparse(substitute(data)),
         "', the class of the remaining object is no longer ",
         "'data.frame', but instead '", class(data), "'.");
  }

  res$output$determinantsN <- sum(stats::complete.cases(res$intermediate$determinantsDat));
  res$output$associationsN <- sum(stats::complete.cases(res$intermediate$dat));

  if (!all(sapply(res$intermediate$determinantsDat, is.numeric))) {
    notNumericVars <-
      names(res$intermediate$determinantsDat)[!sapply(res$intermediate$determinantsDat, is.numeric)];
    stop("Not all determinants are numeric! Specifically, ",
         ufs::vecTxtQ(notNumericVars), " are not numeric variables.");
  }

  ### For the scores, the max and min need to be determined from the data
  res$intermediate$fullColorRange <-
    ufs::ifelseObj(is.null(fullColorRange),
                   range(res$intermediate$determinantsDat, na.rm = TRUE),
                   fullColorRange);

  ### These will be used to determine the breaks in the plot with
  ### the scores
  res$intermediate$uniqueValues <-
    sort(unique(stats::na.omit(unlist(res$intermediate$determinantsDat))));

  ### If only one of the sorting arguments is set, set the other
  ### one on the basis of the defaults; otherwise, store the
  ### passed arguments for use later on.
  if (!is.null(orderBy) && is.null(decreasing)) {
    res$intermediate$decreasing <- TRUE;
  } else if (is.null(orderBy) && !is.null(decreasing)) {
    res$intermediate$orderBy <- TRUE;
  } else {
    res$intermediate$decreasing <- decreasing;
    res$intermediate$orderBy <- orderBy;
  }

  ### Turn 'decreasing' around, because ggplot places the 'first' values
  ### at the bottom and the last ones at the top
  if (!is.null(res$intermediate$decreasing)) {
    decreasing <- res$intermediate$decreasing <- !res$intermediate$decreasing;
  }

  if (is.null(orderBy)) {
    ### Invert order, because ggplot starts from the bottom on the y axis.
    res$intermediate$sortOrder <- rev(1:length(determinants));
  } else if (ufs::isTrue(orderBy)) {
    res$intermediate$sortOrder <- order(colMeans(data[, determinants], na.rm=TRUE),
                                        decreasing=res$intermediate$decreasing);
  } else if (orderBy %IN% (targets)) {
    tryCatch({

      assocMatrix <-
        ufs::associationMatrix(data,
                               x=determinants,
                               y=orderBy);

      dataToSort <-
        assocMatrix$output$raw$es[,1];

      # sortedAssocMatrix <-
      #   sort(dataToSort,
      #        decreasing=res$intermediate$decreasing);

      res$intermediate$sortOrder <-
        order(dataToSort);
        #sortedAssocMatrix$intermediate$sorting$order;

    }, error = function(errorMsg) {
      stop("When trying to call associationMatrix to get the sorting order, ",
           "the data frame no longer has class 'numeric', but instead '",
           class(data), "'.");
    });
  } else {
    stop("In argument 'orderBy' either pass TRUE (to order by ",
         "(sub)determinants), or the name of one of the target ",
         "variables (e.g. determinants such as attitude, motivational ",
         "constructs such as intention, behavioral proxies or ",
         "behavioral measures).");
  }

  ### Get confidence intervals (we may re-sort later)
  res$intermediate$meansDat <-
    ufs::varsToDiamondPlotDf(data,
                             items = determinants,
                             conf.level=conf.level$means);

  if (length(unique(c(targets, determinants))) < 2) {
    stop("Something is wrong with the arguments provided ",
         "as determinants (", ufs::vecTxtQ(determinants),
         ") or targets (", ufs::vecTxtQ(targets),
         "): together, they seem to contain less than ",
         "two different elements (i.e. variable names).");
  }

  if (getOption('ufs.debug', FALSE)) {
    message(paste0("\nDebugging message:\n",
                   "\nnames(res$intermediate$dat) = ",
                   ufs::vecTxtQ(names(res$intermediate$dat)),
                   "\ndeterminants = ", ufs::vecTxtQ(determinants),
                   "\ntargets = ", ufs::vecTxtQ(targets),
                   "\n"));
  }

  ### Get confidence intervals for effect sizes
  res$intermediate$assocDat <- sapply(targets, function(currentTarget) {
    return(ufs::associationsToDiamondPlotDf(res$intermediate$dat,
                                            determinants,
                                            currentTarget,
                                            esMetric = 'd'));
  }, simplify=FALSE);
  names(res$intermediate$assocDat) <- targets;

  ### Get R squared values
  tryCatch(
    res$intermediate$Rsq <- lapply(targets, function(currentTarget) {
      return(suppressMessages({
        ### Run and store lm objects; code lifted from rosetta::logRegr()
        glmRes <-
          stats::glm(formula=stats::formula(paste(currentTarget,
                                                  '~',
                                                  paste(determinants,
                                                        collapse=" + "))),
                     data=res$intermediate$dat,
                     family=stats::binomial(link='logit'));
        CoxSnellRsq <-
          1 - exp((glmRes$deviance -
                     glmRes$null.deviance) /
                    length(glmRes$fitted.values));
        NagelkerkeRsq <-
          CoxSnellRsq /
          (1 - exp(-(glmRes$null.deviance /
                       length(glmRes$fitted.values))));
        return(list(CoxSnellRsq=CoxSnellRsq,
                    NagelkerkeRsq=NagelkerkeRsq));
    }))}), error=function(e) {
      stop("Encountered an error when trying to compute the R square of targets ",
           ufs::vecTxtQ(targets), " predicted by ", ufs::vecTxtQ(determinants),
           ".");
    });

  res$intermediate$meansDat <-
    res$intermediate$meansDat[res$intermediate$sortOrder, ];
  res$intermediate$assocDat <-
    sapply(res$intermediate$assocDat, function(x) {
      return(x[res$intermediate$sortOrder, ]);
    }, simplify=FALSE);

  ### Get extreme values from association dataframe
  scale_extreme <- 0;
  color_extreme <- 0;
  for (i in targets) {

    scale_extreme <- max(abs(c(scale_extreme,
                               unlist(res$intermediate$assocDat[[i]][, 'lo']),
                               unlist(res$intermediate$assocDat[[i]][, 'hi']))));
    color_extreme <- max(c(color_extreme,
                           abs(unlist(res$intermediate$assocDat[[i]][, 'es']))));

    ### Set to 2 if lower than 2
    scale_extreme <- max(scale_extreme, 2);
    color_extreme <- max(color_extreme, 2);
  }

  ### Sort determinant names
  determinants <- determinants[res$intermediate$sortOrder];

  sortedSubQuestions <- subQuestions[res$intermediate$sortOrder];

  if (numberSubQuestions) {
    sortedSubQuestions <- paste0(length(sortedSubQuestions):1,
                                 ". ",
                                 sortedSubQuestions);
  }

  if (is.null(leftAnchors) || is.null(rightAnchors)) {
    res$intermediate$biAxisDiamondPlot <-
      ufs::biAxisDiamondPlot(data, items = determinants,
                             subQuestions = sortedSubQuestions,
                             leftAnchors = rep("lo", length(sortedSubQuestions)),
                             rightAnchors = rep("hi", length(sortedSubQuestions)),
                             generateColors = generateColors$means,
                             fullColorRange = res$intermediate$fullColorRange,
                             conf.level = conf.level$means,
                             drawPlot = FALSE,
                             dataAlpha = dotAlpha,
                             returnPlotOnly = FALSE,
                             dotSize = dotSize,
                             baseFontSize = baseFontSize,
                             theme = theme,
                             jitterHeight = .3,
                             xbreaks=xbreaks,
                             ...);
  } else {
    res$intermediate$biAxisDiamondPlot <-
      ufs::biAxisDiamondPlot(data, items = determinants,
                             subQuestions = sortedSubQuestions,
                             leftAnchors = leftAnchors[res$intermediate$sortOrder],
                             rightAnchors = rightAnchors[res$intermediate$sortOrder],
                             generateColors = generateColors$means,
                             fullColorRange = res$intermediate$fullColorRange,
                             conf.level = conf.level$means,
                             drawPlot = FALSE,
                             dataAlpha = dotAlpha,
                             returnPlotOnly = FALSE,
                             dotSize = dotSize,
                             baseFontSize = baseFontSize,
                             theme = theme,
                             jitterHeight = .3,
                             xbreaks=xbreaks,
                             ...);
  }

  if (length(targets)==1) {
    if (is.null(leftAnchors) || is.null(rightAnchors)) {
      labels <-
        sortedSubQuestions;
    } else {
      labels <-
        paste0(sortedSubQuestions,
               "\n[",
               leftAnchors[res$intermediate$sortOrder],
               " | ",
               rightAnchors[res$intermediate$sortOrder],
               "]");
    }
    res$intermediate$meansComparisonDiamondPlot <-
      ufs::meansComparisonDiamondPlot(data, items=determinants,
                                      compareBy=targets,
                                      labels = labels,
                                      xbreaks = xbreaks,
                                      dataAlpha = dotAlpha,
                                      conf.level=conf.level$means,
                                      comparisonColors=comparisonColors,
                                      legend.position="left",
                                      theme=theme) +
      ggplot2::theme(legend.position = "left") +
      ggplot2::scale_fill_discrete(labels = categoryLabels) +
      ggplot2::scale_color_discrete(labels = categoryLabels);

    res$intermediate$meansPlot <-
      res$intermediate$meansComparisonDiamondPlot;
    builtMeansPlot <-
      ggplot2::ggplot_build(res$intermediate$meansComparisonDiamondPlot);
  } else {
    res$intermediate$meansPlot <-
      res$intermediate$biAxisDiamondPlot$output$plot;
    builtMeansPlot <-
      ggplot2::ggplot_build(res$intermediate$biAxisDiamondPlot$intermediate$meansPlot);
  }

  yMajor <- builtMeansPlot$layout$panel_ranges[[1]]$y.major_source;

  ### Note to self: this changed in ggplot2 3.0; used to be stored in
  ###   builtMeansPlot$layout$panel_ranges[[1]]$y.range
  yRange <- range(builtMeansPlot$layout$panel_scales_y[[1]]$range$range);

  if (length(targets)==1) {
    strokeColors <- "#000000";
  } else if (is.null(strokeColors)) {
    strokeColors <- viridis::viridis(length(targets));
  }
  names(strokeColors) <- targets;

  res$intermediate$fullAssocDat <-
    do.call(rbind,
            res$intermediate$assocDat);

  res$intermediate$allEffectsizes <-
    unlist(res$intermediate$fullAssocDat[, 1:3]);

  # res$intermediate$scaleMin.raw <-
  #   min(res$intermediate$allEffectsizes);
  #
  # res$intermediate$scaleMax.raw <-
  #   max(res$intermediate$allEffectsizes);
  #
  # res$intermediate$scaleMin.clean <-
  #   floor(res$intermediate$scaleMin.raw * 2) / 2;
  #
  # res$intermediate$scaleMax.clean <-
  #   ceiling(res$intermediate$scaleMax.raw * 2) / 2;

  res$intermediate$scaleMin.clean <- -1 * scale_extreme;
  res$intermediate$scaleMax.clean <- scale_extreme;

  limits <- c(res$intermediate$scaleMin.clean,
              res$intermediate$scaleMax.clean);

  colorLimits <- c(-1 * color_extreme, color_extreme);

  res$intermediate$assocLayers <-
    sapply(names(res$intermediate$assocDat),
           function(currentTarget) {
             return(ufs::diamondPlot(res$intermediate$assocDat[[currentTarget]],
                                     ciCols=c('lo', 'es', 'hi'),
                                     yLabels = subQuestions[res$intermediate$sortOrder],
                                     generateColors=generateColors$associations,
                                     fullColorRange = colorLimits,
                                     alpha = associationsAlpha,
                                     lineColor=strokeColors[currentTarget],
                                     size=1, theme=theme,
                                     returnLayerOnly = TRUE, ...));
           }, simplify=FALSE);

  if (!is.null(vLines) && !is.null(vLineColors)) {
    if (!(length(vLineColors) == length(vLines))) {
      vLineColors <- rep_len(vLineColors, length(vLines));
    }
    vLineLayer <-
      ggplot2::geom_vline(xintercept = vLines,
                          color = vLineColors);
  } else {
    vLineLayer <- NULL;
  }

  res$intermediate$assocPlot <- ggplot2::ggplot() +
    vLineLayer +
    res$intermediate$assocLayers +
    theme +
    ggplot2::xlab(paste0(round(100 * conf.level$associations, 2), '% CIs of associations')) +
    ggplot2::scale_x_continuous(limits=limits) +
    ggplot2::scale_y_continuous(breaks=yMajor) +
    ggplot2::theme(axis.ticks.y=ggplot2::element_blank(),
                   panel.grid.minor.y = ggplot2::element_blank(),
                   axis.text.y=ggplot2::element_blank(),
                   axis.title.y=ggplot2::element_blank());

  builtAssocPlot <-
    ggplot2::ggplot_build(res$intermediate$assocPlot);
  builtAssocPlot$layout$panel_ranges[[1]]$y.range <- yRange;
  builtAssocPlot$layout$panel_ranges[[1]]$y.major <-
    builtMeansPlot$layout$panel_ranges[[1]]$y.major;


  if (is.null(titleVarLabels)) titleVarLabels <- targets;

  titleGrobs <- list(grid::textGrob(label = paste0(titlePrefix, " "),
                                    x = grid::unit(0.2, "lines"),
                                    y = grid::unit(0.8, "lines"),
                                    hjust = 0, vjust = 0));
  currentXpos <- sum(grid::unit(0.2, "lines"),
                     grid::grobWidth(titleGrobs[[1]]));
  newGrob <- grid::textGrob(label = paste0(titleVarLabels[1], " (R\U00B2 = ",
                                           ufs::formatR(res$intermediate$Rsq[[1]]$CoxSnellRsq),
                                           " | ",
                                           ufs::formatR(res$intermediate$Rsq[[1]]$NagelkerkeRsq), ")"),
                            x = currentXpos,
                            y = grid::unit(.8, "lines"),
                            hjust = 0, vjust = 0,
                            gp = grid::gpar(col = strokeColors[targets[1]]));
  titleGrobs <- c(titleGrobs, list(newGrob));
  currentXpos <- sum(currentXpos,
                     grid::grobWidth(titleGrobs[[2]]));

  if (length(targets) > 1) {
    for (i in 2:length(targets)) {
      prefixGrob <-
        grid::textGrob(label = ifelse(i == length(targets), " & ", ", "),
                       x = currentXpos,
                       y = grid::unit(0.8, "lines"),
                       hjust = 0, vjust = 0,
                       gp = grid::gpar(col = "#000000"));
      currentXpos <- sum(currentXpos,
                         grid::grobWidth(prefixGrob));
      newGrob <-
        grid::textGrob(label = paste0(titleVarLabels[i], " (R\U00B2 = ",
                                      ufs::formatR(res$intermediate$Rsq[[i]]$CoxSnellRsq),
                                      " | ",
                                      ufs::formatR(res$intermediate$Rsq[[i]]$NagelkerkeRsq), ")"),
                       x = currentXpos,
                       y = grid::unit(0.8, "lines"),
                       hjust = 0, vjust = 0,
                       gp = grid::gpar(col = strokeColors[targets[i]]));
      currentXpos <- sum(currentXpos, grid::grobWidth(newGrob));
      titleGrobs <- c(titleGrobs, list(prefixGrob, newGrob));
    }
  }
  titleGrobs <- c(titleGrobs,
                  list(grid::textGrob(label = paste0(" ", titleSuffix),
                                      x = currentXpos,
                                      y = grid::unit(0.8, "lines"),
                                      hjust = 0, vjust = 0)));

  titleGrob <- do.call(grid::grobTree,
                       c(list(gp = grid::gpar(fontsize = 1.2*baseFontSize,
                                              fontface = "bold")),
                         titleGrobs));

  if (length(targets)==1) {
    res$output$plot <-
      gtable::gtable_add_cols(ggplot2::ggplotGrob(res$intermediate$meansPlot),
                              grid::unit(1, "null"));
  } else {
    res$output$plot <-
      gtable::gtable_add_cols(res$intermediate$meansPlot,
                              grid::unit(1, "null"));
  }

  res$output$plot <-
    gtable::gtable_add_grob(res$output$plot,
                            ggplot2::ggplot_gtable(builtAssocPlot),
                            t=1,
                            b=length(res$output$plot$heights),
                            l=length(res$output$plot$widths));

  res$output$plot <-
    gridExtra::arrangeGrob(res$output$plot,
                           top = titleGrob,
                           padding = grid::unit(1.25, "line"));

  ### Default sizes ; first compute in centimeters, then convert to inches
  attr(res$output$plot, 'height') <- baseSize + 1.25 * baseSize * max(length(determinants), 1.5);
  attr(res$output$plot, 'width') <- 21 - 3;
  attr(res$output$plot, 'height') <- attr(res$output$plot, 'height') / 2.54;
  attr(res$output$plot, 'width') <- attr(res$output$plot, 'width') / 2.54;

  if (drawPlot) {
    grid::grid.newpage();
    grid::grid.draw(res$output$plot);
  }

  if (!is.null(outputFile)) {
    if ((nchar(dirname(outputFile)) == 0) | (!dir.exists(dirname(outputFile)))) {
      warning("The directory specified to save the the outputFile to ('",
              dirname(outputFile),
              "') does not exist, so not saving the plot!");
    } else {
      if (is.null(outputWidth)) {
        outputWidth <- attr(res$output$plot, 'width');
        outputUnits <- "in";
      }
      if (is.null(outputHeight)) {
        outputHeight <- attr(res$output$plot, 'height');
        outputUnits <- "in";
      }
      if (is.null(outputUnits)) {
        outputUnits <- "in";
      }
      do.call(ggplot2::ggsave,
              c(list(file=outputFile,
                     plot=res$output$plot,
                     width = outputWidth,
                     height = outputHeight,
                     units = outputUnits),
                outputParams));
    }
  }

  invisible(ufs::ifelseObj(returnPlotOnly, res$output$plot, res));

}
