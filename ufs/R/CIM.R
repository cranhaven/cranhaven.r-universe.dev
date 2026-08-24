#' Conceptual Independence Matrix
#'
#' @param data The dataframe containing the variables.
#' @param scales The scales: a named list of character vectors,
#' where the character vectors specify the variable names, and the
#' names of each character vector specifies the relevant scale.
#' @param conf.level The confidence level for the confidence intervals.
#' @param colors The colors used for the factors. The default uses the
#' discrete viridis() palette, which is optimized for perceptual
#' uniformity, maintaining its properties when printed in grayscale,
#' and designed for colourblind readers. A vector can also be supplied;
#' the colors must be valid arguments to [colorRamp()] (and therefore,
#' to [col2rgb()]).
#' @param outputFile The file to write the output to.
#' @param outputWidth,outputHeight,outputUnits The width, height,
#' and units for the output file.
#' @param faMethod The method to pass on to [psych::fa()].
#' @param n.iter The number of iterations to pass on to [psych::fa()].
#' @param n.repeatOnWarning How often to repeat on warnings (in the
#' hopes of getting a run without warnings).
#' @param warningTolerance How many warnings are accepted.
#' @param silentRepeatOnWarning Whether to be chatty or silent when
#' repeating after warnings.
#' @param showWarnings Whether to show the warnings.
#' @param skipRegex A character vector of length 2 containing two
#' regular expressions; if the two scales both match one or both
#' of those regular expressions, that cell is skipped.
#' @param headingLevel The level for the heading; especially useful when
#' knitting an Rmd partial.
#' @param printAbbreviations Whether to print a table with the abbreviations
#' that are used.
#' @param drawPlot Whether to draw the plot or only return it.
#' @param returnPlotOnly Whether to return the plot only, or the entire object.
#' @param x The object to print.
#' @param quiet Whether to be quiet or chatty.
#' @param echoPartial Whether to `echo` the code in the Rmd partial.
#' @param partialFile Can be used to override the Rmd partial file.
#' @param ... Additional arguments are passed on the respective default methods.
#'
#' @return A [ggplot2::ggplot()] plot.
#' @rdname CIM
#' @examples ### Load dataset `bfi`, originally from psychTools package
#' data(bfi, package= 'ufs');
#'
#' ### Specify scales
#' bfiScales <-
#'   list(Agreeableness     = paste0("Agreeableness_item_", 1:5),
#'        Conscientiousness = paste0("Conscientiousness_item_", 1:5),
#'        Extraversion      = paste0("Extraversion_item_", 1:5),
#'        Neuroticism       = paste0("Neuroticism_item_", 1:5),
#'        Openness          = paste0("Openness_item_", 1:5));
#'
#' names(bfi) <- c(unlist(bfiScales),
#'                 c('gender', 'education', 'age'));
#'
#' ### Only select first two and the first three items to
#' ### keep it quick; just pass the full 'bfiScales'
#' ### object to run for all five the full scales
#' \donttest{
#' CIM(bfi,
#'     scales=lapply(bfiScales, head, 3)[1:2],
#'     n.iter=10);
#' }
#' @export
CIM <- function(data,
                scales,
                conf.level=.95,
                colors = c("#440154FF", "#7AD151FF"), #viridis::viridis(2, end = .8),
                outputFile = NULL,
                outputWidth = 100,
                outputHeight = 100,
                outputUnits = "cm",
                faMethod = "minres",
                n.iter = 100,
                n.repeatOnWarning = 50,
                warningTolerance = 2,
                silentRepeatOnWarning = FALSE,
                showWarnings = FALSE,
                skipRegex = NULL,
                headingLevel=2,
                printAbbreviations = TRUE,
                drawPlot=TRUE,
                returnPlotOnly=TRUE) {

  res <- list(input = as.list(environment()),
              intermediate = list(),
              output = list());

  ###--------------------------------------------------------------------------
  ### Uses three suggested packages; check their presence
  ###--------------------------------------------------------------------------

  if (!requireNamespace("psych", quietly = TRUE)) {
    message("To do the exploratory factor analysis, the \"psych\" package is required. ",
            "Please install it using `install.packages('psych');`.");
    return(invisible(FALSE));
  }

  if (!requireNamespace("GPArotation", quietly = TRUE)) {
    message("To do the exploratory factor analysis, the \"GPArotation\" package is required. ",
            "Please install it using `install.packages('GPArotation');`.");
    return(invisible(FALSE));

  } else {
    ### This is just to satisfy R CHECK - otherwise we're not "allowed" to
    ### check whether GPArotation exists, and `psych` doesn't handle that
    ### elegantly itself.
    a <- GPArotation::box20
  }

  if (!requireNamespace("lavaan", quietly = TRUE)) {
    message("To do the confirmatory factor analysis, the \"lavaan\" package is required. ",
            "Please install it using `install.packages('lavaan');`.");
    return(invisible(FALSE));
  }

  ###--------------------------------------------------------------------------
  ### Helper functions
  ###--------------------------------------------------------------------------

  ### Generate tableGrob with headers for the
  ### table with factor loadings (see
  ### https://stackoverflow.com/questions/33214671/merging-table-header-cells-using-tablegrob
  ### and https://github.com/baptiste/gridextra/wiki/tableGrob
  makeHeaderTable <- function(vector, startCol=2, colSpan=3) {
    nCol <- length(vector);
    tmpDf <- t(matrix(1:nCol));
    colnames(tmpDf) <- vector;
    rownames(tmpDf) <- 'rowName';
    headerTable <- gridExtra::tableGrob(tmpDf);
    headerTable$grobs[[1]] <- grid::textGrob('');
    rm(tmpDf);
    headerTable <- headerTable[1, ];

    ### Compute where each column should start and end
    startCols <- seq(startCol, colSpan * nCol + startCol - 1, by=colSpan);
    endCols <- startCols + colSpan - 1;

    headerTable$layout[3:nrow(headerTable$layout), c("l", "r")] <-
      list(startCols, endCols);
    return(headerTable);
  }

  headerTable <- makeHeaderTable(c('Factor 1', 'Factor 2'));

  ###--------------------------------------------------------------------------
  ### Prepare input and object to return
  ###--------------------------------------------------------------------------

  ### Create objects for output
  grobsList <- list();
  factorLoadingCIs <- list();
  ciSummaryList <- list();
  errorsList <- list();
  warningsList <- list();
  messagesList <- list();
  dfList <- list();

  dataframeName <- deparse(substitute(data));

  abbrScaleNames <- abbreviate(names(scales));
  abbrScales <-
    lapply(seq_along(scales), function(i) {
      return(paste0(abbrScaleNames[i], 1:length(scales[[i]])));
    });
  names(abbrScales) <- abbrScaleNames;
  names(abbrScaleNames) <- names(scales);
  abbreviationLegend <-
    data.frame(scale = rep(names(scales),
                           times=unlist(lapply(scales, length))),
               abbreviatedScale = rep(names(abbrScales),
                                      times=unlist(lapply(abbrScales, length))),
               variable = unlist(scales),
               abbreviatedVariable = unlist(abbrScales),
               stringsAsFactors = FALSE);
  row.names(abbreviationLegend) <- NULL;

  ###--------------------------------------------------------------------------
  ### Reminders for the future
  ###--------------------------------------------------------------------------

  ### Reminder: maybe change order of variables

  ### Reminder: compare variances of scales (homogeneity)

  ###--------------------------------------------------------------------------
  ### The loop
  ###--------------------------------------------------------------------------

  res$intermediate <- list(efas = list(),
                           cfa1s = list(),
                           cfa2s = list(),
                           cfas = list(),
                           factorLoadingPlots = list(),
                           faDfs = list(),
                           scales = scales,
                           abbrScales = abbrScales,
                           abbrScaleNames = abbrScaleNames,
                           abbreviationLegend = abbreviationLegend);

  if ((!isTRUE(getOption('knitr.in.progress'))) && interactive()) {
    ### Initialize progress bar
    pb <- utils::txtProgressBar(style=3,
                                min = 1,
                                max = length(scales) ^ 2);
  }

  #on.exit({cat("\nPrematurely interrupted during factor analyses!\n");});

  for (rowVar in names(scales)) {

    res$intermediate$efas[[rowVar]] <- list();
    res$intermediate$cfa1s[[rowVar]] <- list();
    res$intermediate$cfa2s[[rowVar]] <- list();
    res$intermediate$cfas[[rowVar]] <- list();
    res$intermediate$factorLoadingPlots[[rowVar]] <- list();
    res$intermediate$faDfs[[rowVar]] <- list();

    ### Get index of this row
    rowIndex <- which(names(scales) == rowVar);

    ### Generate object to store the columns of this row in
    grobsList[[rowIndex]] <- list();
    errorsList[[rowIndex]] <- list();
    warningsList[[rowIndex]] <- list();
    messagesList[[rowIndex]] <- list();
    ciSummaryList[[rowIndex]] <- list();
    factorLoadingCIs[[rowIndex]] <- list();

    for (colVar in names(scales)) {

      ### Get index of this column
      colIndex <- which(names(scales) == colVar);
      ### Generate object to store results for this cell in
      grobsList[[rowIndex]][[colIndex]] <- list();
      errorsList[[rowIndex]][[colIndex]] <- list(efa = character(),
                                                 cfa1 = character(),
                                                 cfa2 = character());
      warningsList[[rowIndex]][[colIndex]] <- list(efa = character(),
                                                   cfa1 = character(),
                                                   cfa2 = character());
      messagesList[[rowIndex]][[colIndex]] <- list(efa = character(),
                                                   cfa1 = character(),
                                                   cfa2 = character());
      ciSummaryList[[rowIndex]][[colIndex]] <- list();

      if ((!isTRUE(getOption('knitr.in.progress'))) && interactive()) {
        progressIndex <- ((rowIndex - 1) * length(scales)) + colIndex;
        ### Update progress bar
        utils::setTxtProgressBar(pb,
                                 value=progressIndex);
      }

      if (rowVar == colVar) {
        ### On the main diagonal
        grobsList[[rowIndex]][[colIndex]] <-
          grid::textGrob("");

      } else {
        ### On the upper or lower diagonal

        ### Continue with next variable if we match the regexes that
        ### specify when to skip a combination
        if (!is.null(skipRegex) &&
            (any(grepl(skipRegex[1],
                       x = c(colVar, rowVar))) &&
             any(grepl(skipRegex[2],
                       x = c(colVar, rowVar))))) {
          grobsList[[rowIndex]][[colIndex]] <-
            grid::textGrob(paste0("Skipping this cell!\n(", rowVar,
                                  " & ", colVar, ")"));

        ### Otherwise, do the factor analyses
        } else {

          if (rowIndex < colIndex) {

            ### Get a convenient list of the items in both scales
            vars <- c(scales[[rowVar]],
                      scales[[colVar]]);

            ### Get both measurement models
            oneFactor <- paste0(' variable =~ ',
                                paste0(vars, collapse=" + "));
            twoFactor <- paste0(
              ' ',
              'var1', #rowVar,
              ' =~ ',
              paste0(scales[[rowVar]], collapse=" + "),
              '\n',
              ' ',
              'var', #colVar,
              ' =~ ',
              paste0(scales[[colVar]], collapse=" + ")
            );

            if (ufs::opts$get('debug')) {
              cat0("\n\nOne factor CFA model: ", oneFactor, "\n",
                   "Two factor CFA model: ", twoFactor, "\n\n");
            }

            ### Check the extremely useful explanation at
            ### https://cran.r-project.org/web/packages/tryCatchLog/vignettes/tryCatchLog-intro.html

            ### First do an EFA
            abbrVarsDat <- data[, rev(vars)];
            abbrVarNames <- c(abbrScales[[abbrScaleNames[rowVar]]],
                              abbrScales[[abbrScaleNames[colVar]]]);
            names(abbrVarsDat) <- abbrVarNames;

            ### Store final factor analysis
            res$intermediate$efas[[rowVar]][[colVar]] <-
              fa_failsafe(abbrVarsDat,
                          nfactors = 2,
                          fm = faMethod,
                          p = 1-conf.level,
                          n.iter=n.iter,
                          n.repeatOnWarning = n.repeatOnWarning,
                          warningTolerance = warningTolerance,
                          silentRepeatOnWarning = silentRepeatOnWarning,
                          showWarnings = showWarnings);

            warningsList[[rowIndex]][[colIndex]]$efa <-
              res$intermediate$efas[[rowVar]][[colVar]]$warnings;
            errorsList[[rowIndex]][[colIndex]]$efa <-
              res$intermediate$efas[[rowVar]][[colVar]]$errors;
            res$intermediate$efas[[rowVar]][[colVar]] <-
              efa <-
              res$intermediate$efas[[rowVar]][[colVar]]$fa;

            if (ufs::opts$get('debug')) {
              cat("\n\nDid EFA. Moving on to one-factor CFA.\n\n");
            }

            ### Set warnings to appear as messages (first save original value)
            oldWarn <- getOption("warn", 0);
            options(warn=1);

            ### First the CFA for one factor
            tryCatch(
              withCallingHandlers(
                moreCapturedWarnings <-
                  utils::capture.output(
                    {
                      warningsList[[rowIndex]][[colIndex]]$cfa1 <-
                        oneFactorCFA <-
                        lavaan::cfa(oneFactor, data=data);
                    },
                    type="message"
                  ),
                warning = function(w) {
                  options(ufs.CIM.warnings = c(getOption("ufs.CIM.warnings"),
                                               w$message));
                  invokeRestart("muffleWarning");
                }
              ),
              error = function(e) {
                options(ufs.CIM.errors = c(getOption("ufs.CIM.errors"),
                                           e$message));
                stop("Error encountered in one factor CFA: ", e$message);
              }
            );

            res$intermediate$cfa1s[[rowVar]][[colVar]] <-
              oneFactorCFA;

            if (ufs::opts$get('debug')) {
              cat("\n\nDid one-factor CFA. Moving on to two-factor CFA.\n\n");
            }

            ### Then the CFA for two factors
            tryCatch(
              withCallingHandlers(
                moreCapturedWarnings <-
                  utils::capture.output(
                    {
                      warningsList[[rowIndex]][[colIndex]]$cfa2 <-
                        twoFactorCFA <-
                        lavaan::cfa(twoFactor, data=data);
                    },
                    type="message"
                  ),
                warning = function(w) {
                  options(ufs.CIM.warnings = c(getOption("ufs.CIM.warnings"),
                                               w$message));
                  invokeRestart("muffleWarning");
                }
              ),
              error = function(e) {
                options(ufs.CIM.errors = c(getOption("ufs.CIM.errors"),
                                           e$message));
                stop("Error encountered in two factor CFA: ", e$message);
              }
            );

            res$intermediate$cfa2s[[rowVar]][[colVar]] <-
              twoFactorCFA;

            if (ufs::opts$get('debug')) {
              cat("\n\nDid two-factor CFA.\n");
              cat0("  Class of 'oneFactorCFA' is: ", class(oneFactorCFA), "\n");
              cat0("  Class of 'twoFactorCFA' is: ", class(twoFactorCFA), "\n\n");
            }

            ### Then run and compare the two CFAs
            if (("lavaan" %in% class(oneFactorCFA)) && ('lavaan' %in% class(twoFactorCFA))) {
              res$intermediate$cfas[[rowVar]][[colVar]] <-
                cfa <-
                lavaan::anova(oneFactorCFA, twoFactorCFA);

              ### In cfa$Chisq, the first number is the chi square of the
              ### TWO-factor model, and the second one of the ONE-factor model!
              bestFittingModel <- ifelse(cfa$Chisq[1] < cfa$Chisq[2],
                                         '2-factor', '1-factor');

              fitText <- paste0(bestFittingModel, ' fits better; ',
                                'Chisq[',
                                cfa[2, 'Df diff'],
                                ']=', round(cfa[2, 'Chisq diff'], 2),
                                ", ",
                                ufs::formatPvalue(cfa[2, 'Pr(>Chisq)']));

              ### Set options back to original setting
              options(warn=oldWarn);

            } else {
              res$intermediate$cfas[[rowVar]][[colVar]] <-
                cfa <-
                NA;
              bestFittingModel <- "not computed";
              fitText <- "Not computed which model fits better."
            }

            ### Make dataframe with factor loading confidence intervals
            if (n.iter > 1) {
              if ('psych' %in% class(efa)) {

                factorLoadingCIs[[rowVar]][[colVar]] <-
                  ufs::faConfInt(res$intermediate$efas[[rowVar]][[colVar]]);
                loadingCIs <-
                  factorLoadingCIs[[rowVar]][[colVar]];

                ciSummaryList[[rowIndex]][[colIndex]] <-
                  (loadingCIs[[1]]$hi < loadingCIs[[2]]$lo) |
                  (loadingCIs[[2]]$hi < loadingCIs[[1]]$lo);

                faDf <- matrix(unlist(factorLoadingCIs[[rowVar]][[colVar]]),
                               ncol=6);

              } else {
                faDf <- matrix(rep(NA, 6*ncol(abbrVarsDat)),
                               ncol=6);

              }
              colnames(faDf) <- c(rep(c('lo', 'est', 'hi'), 2));

          } else {

            faDf <-
              as.data.frame(
                unclass(
                  res$intermediate$efas[[rowVar]][[colVar]]$loadings
                )
              );

          }

          ### Get abbreviated scale names
          abbr <- abbreviate(names(scales));

          ### Set row and column names
          rownames(faDf) <- c(abbrScales[[abbrScaleNames[rowVar]]],
                              abbrScales[[abbrScaleNames[colVar]]]);
          # paste0(abbr[rowVar], 1:length(scales[[rowVar]])),
          # paste0(abbr[colVar], 1:length(scales[[colVar]])));

          faDfReordered <- faDf[order(rownames(faDf)),
          ];
          res$intermediate$faDfs[[rowVar]][[colVar]] <-
            list(faDf_raw = faDf,
                 faDf = faDfReordered,
                 faDf_rounded = round(faDfReordered, 2));

          if (ufs::opts$get('debug')) {
            cat0("\n\nJust stored this dataframe to create a gTable later on:\n\n");
            print(res$intermediate$faDfs[[rowVar]][[colVar]]$faDf_rounded);
            cat0("\n\n");
          }

            ###------------------------------------------------------------------
            ###------------------------------------------------------------------
            ### To do: add explained variance per factor
            ###------------------------------------------------------------------
            ###------------------------------------------------------------------

          } else {

            ### Get everything from the top diagonal

            res$intermediate$efas[[rowVar]][[colVar]] <-
              res$intermediate$efas[[colVar]][[rowVar]];
            res$intermediate$cfa1s[[rowVar]][[colVar]] <-
              res$intermediate$cfa1s[[colVar]][[rowVar]];
            res$intermediate$cfa2s[[rowVar]][[colVar]] <-
              res$intermediate$cfa2s[[colVar]][[rowVar]];
            res$intermediate$cfas[[rowVar]][[colVar]] <-
              res$intermediate$cfas[[colVar]][[rowVar]];

            factorLoadingCIs[[rowVar]][[colVar]] <-
              factorLoadingCIs[[colVar]][[rowVar]];
            ciSummaryList[[rowIndex]][[colIndex]] <-
              ciSummaryList[[colIndex]][[rowIndex]];
            res$intermediate$faDfs[[rowVar]][[colVar]] <-
              res$intermediate$faDfs[[colVar]][[rowVar]];

          }

          if (rowIndex < colIndex) {
            ### Upper diagonal

            if ('psych' %in% class(res$intermediate$efas[[rowVar]][[colVar]])) {
              titleString <-
                sort(c(rowVar, colVar));
              titleString <- paste0(titleString[1], ' & ', titleString[2], "\n",
                                    fitText);

              if (n.iter > 1) {

                grobsList[[rowIndex]][[colIndex]] <-
                  res$intermediate$factorLoadingPlots[[rowVar]][[colVar]] <-
                  factorLoadingDiamondCIplot(res$intermediate$efas[[rowVar]][[colVar]],
                                             colors=colors,
                                             sortAlphabetically=TRUE) +
                  #faDfDiamondCIplot(faDf, xlab=NULL) +
                  ggplot2::ggtitle(titleString);
                #           textGrob(paste0("Upper diag:\n", rowVar,
                #                           " and ", colVar));

              } else {

                grobsList[[rowIndex]][[colIndex]] <-
                  res$intermediate$factorLoadingPlots[[rowVar]][[colVar]] <-
                    factorLoadingHeatmap(
                      res$intermediate$efas[[rowVar]][[colVar]],
                      sortAlphabetically=TRUE
                    );

              }

            } else {
              grobsList[[rowIndex]][[colIndex]] <-
                grid::textGrob('Not possible');
            }

          } else {
            ### Lower diagonal

            ### Note: we get the factor loadings that were generated by the
            ### upper diagonal commands, to retain consistency with the
            ### diamond plot

            grobsList[[rowIndex]][[colIndex]] <-
              gridExtra::tableGrob(res$intermediate$faDfs[[colVar]][[rowVar]]$faDf_rounded);

            if (ufs::opts$get('debug')) {
              cat0("\n\nJust created a tableGrob for dataframe:\n\n");
              print(res$intermediate$faDfs[[colVar]][[rowVar]]$faDf_rounded);
              cat0("\n\n");
            }

            if (n.iter > 1) {

              grobsList[[rowIndex]][[colIndex]] <-
                gridExtra::gtable_combine(headerTable,
                                          grobsList[[rowIndex]][[colIndex]],
                                          along=2);

              prevWidths <- grobsList[[rowIndex]][[colIndex]]$widths;

              ### Add variable names
              titleString <-
                sort(c(rowVar, colVar));
              titleString <- paste0(titleString[1], ' & ', titleString[2]);
              grobsList[[rowIndex]][[colIndex]] <-
                gridExtra::gtable_combine(
                  makeHeaderTable(
                    titleString,
                    colSpan=ncol(res$intermediate$faDfs[[colVar]][[rowVar]]$faDf_rounded)
                  ),
                  grobsList[[rowIndex]][[colIndex]],
                  along=2
                );

              ### Set widths (again, based on
              ### https://github.com/baptiste/gridextra/wiki/tableGrob
              grobsList[[rowIndex]][[colIndex]]$widths <-
                grid::unit(rep(.95 * (1/ncol(grobsList[[rowIndex]][[colIndex]])),
                               ncol(grobsList[[rowIndex]][[colIndex]])),
                           "npc");
            }
          }
        }
      }
      ### Finally, construct content; for diagonal, omega etc; for
      ### upper diagonal, factorLoadingPlots; for lower diagnoal, numbers

    }
  }

  if ((!isTRUE(getOption('knitr.in.progress'))) && interactive()) {
    ### Close progress indicator;
    close(pb);
  }

  on.exit();

  CIM <- gridExtra::arrangeGrob(grobs=unlist(grobsList,
                                             recursive=FALSE),
                                ncol=length(grobsList));

  if (returnPlotOnly && (!isTRUE(getOption('knitr.in.progress')))) {
    res <- CIM;
  } else {
    res$output <- list(CIM = CIM);
    res$intermediate$grobsList <- grobsList;
    res$intermediate$factorLoadingCIs <- factorLoadingCIs;
    res$intermediate$ciSummaryList <- ciSummaryList;
    res$intermediate$abbreviationLegend <- abbreviationLegend;
    res$intermediate$errorsList <- errorsList;
    res$intermediate$warningsList <- warningsList;
    res$intermediate$messagesList <- messagesList;
    class(res) <-
      "CIM";
  }

  if (printAbbreviations && (!isTRUE(getOption('knitr.in.progress')))) {
    cat0("\n\nUsed scales and items:\n\n");
    print(abbreviationLegend);
  }

  ### Return and/or save conceptual independence matrix
  if (drawPlot && (!isTRUE(getOption('knitr.in.progress')))) {
    if (ufs::opts$get('debug')) {
      cat("\n\nDebugging message from the `ufs::CIM` function: going to print the CIM using `grid.draw`!\n\n");
    }
    grid::grid.newpage();
    grid::grid.draw(CIM)
  }
  if (is.null(outputFile)) {
    if (isTRUE(getOption('knitr.in.progress'))) {
      return(res);
    } else {
      return(invisible(res));
    }
  } else {
    ggplot2::ggsave(file=outputFile,
                    plot=CIM,
                    width = outputWidth,
                    height = outputHeight,
                    units = outputUnits);
    if (isTRUE(getOption('knitr.in.progress'))) {
      return(res);
    } else {
      return(invisible(res));
    }
  }

}

#' @rdname CIM
#' @export
CIM_partial <- function(x,
                        headingLevel = x$input$headingLevel,
                        quiet=TRUE,
                        echoPartial = FALSE,
                        partialFile = NULL,
                        ...) {

  ### Get filename
  if ((!is.null(partialFile)) && file.exists(partialFile)) {
    rmdPartialFilename <-
      partialFile;
  } else {
    rmdPartialFilename <-
      system.file("partials", "_CIM_partial.Rmd", package="ufs");
  }

  if (!file.exists(rmdPartialFilename)) {
    stop(
      "The file with the RMarkdown partial specified to ",
      "`ufs::scaleStructure_partial()` was not found (this file ",
      "was '", rmdPartialFilename, "')!"
    );
  }

  rmdpartials::partial(rmdPartialFilename);

}

#' @rdname CIM
#' @method knit_print CIM
#' @importFrom knitr knit_print
#' @export
knit_print.CIM <- function(x,
                           headingLevel = x$input$headingLevel,
                           quiet=TRUE,
                           echoPartial = FALSE,
                           partialFile = NULL,
                           ...) {

  CIM_partial(x = x,
              headingLevel = headingLevel,
              quiet = quiet,
              echoPartial = echoPartial,
              partialFile = partialFile,
              ...);

}
