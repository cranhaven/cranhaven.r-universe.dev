#' Conduct reliability analyses with output similar to jamovi and SPSS
#'
#' The `reliability()` analysis is the only one most users will need. It tries
#' to apply best practices by, as much as possible, complementing point
#' estimates with confidence intervals.
#'
#' The `rosettaReliability` object that is returned has
#' its own `print()` method, that, when using `knitr`, will use
#' the `rmdpartials` package to insert an RMarkdown partial. That partial is
#' created using
#' `rosettaReliability_partial()`, which is also called by a specific
#' `knit_print()` method.
#'
#' @param data The data frame
#' @param items The items (if omitted, all columns are used)
#' @param scaleStructure Whether to include scale-level estimates using
#' [ufs::scaleStructure()]
#' @param descriptives Whether to include mean and standard deviation eastimates
#' and their confidence intervals
#' @param itemLevel Whether to include item-level internal consistency estimates
#' @param scatterMatrix,scatterMatrixArgs Whether to produce a scatter matrix,
#' and the arguments to pass to the [scatterMatrix()] function.
#' @param digits The number of digits to round the result to
#' @param conf.level The confidence level of confidence intervals
#' @param itemLabels Optionally, labels to use for the items (optionally, named,
#' with the names corresponding to the `items`; otherwise, the order of the
#' labels has to match the order of the items)
#' @param itemOmittedCorsWithRest,itemOmittedCorsWithTotal Whether to include
#' each item's correlations with, respectively, the scale with that item
#' omitted, or the full scale.
#' @param alphaOmittedCIs Whether to include the
#' confidence intervals for the Coefficient Alpha estimates with the item
#' omitted.
#' @param omegaFromMBESS,omegaFromPsych Whether to include omega from `MBESS`
#' and/or `psych`
#' @param ordinal Wheher to set `poly=TRUE` when calling
#' [ufs::scaleStructure()], which will compute the polychoric correlation
#' matrix to provide the scale estimates assuming ordinal-level items. Note
#' that this may throw a variety of errors from within the `psych` package if
#' the data are somehow not what `psych` expects
#' @param headingLevel The number of hashes to print in front of the headings
#' when printing while knitting
#' @param x The object to print
#' @param forceKnitrOutput Force knitr output
#' @param printPlots Whether to print plots (can be used to suppress plots,
#' which can be useful sometimes)
#' @param quiet Passed on to [knitr::knit()] whether it should b
#'  chatty (`FALSE`) or quiet (`TRUE`).
#' @param echoPartial Whether to show the executed code in the R Markdown
#' partial (`TRUE`) or not (`FALSE`).
#' @param partialFile This can be used to specify a custom partial file. The
#' file will have object `x` available.
#' @param ... Any additional arguments are passed to [ufs::scaleStructure()] by
#' `reliability`, to the default print method by `print.reliability`, and to
#' [rmdpartials::partial()] when knitting an RMarkdown partial.
#'
#' @return An object with all results
#' @rdname rosettaReliability
#' @aliases reliability,print.reliability
#' @export
#'
#' @examples ### These examples aren't run during tests
#' ### because they can take quite long
#' \dontrun{
#' ### Simple example with only main reliability results
#' data(pp15, package="rosetta");
#' rosetta::reliability(
#'   pp15,
#'   c(
#'     "highDose_AttGeneral_good",
#'     "highDose_AttGeneral_prettig",
#'     "highDose_AttGeneral_slim",
#'     "highDose_AttGeneral_gezond",
#'     "highDose_AttGeneral_spannend"
#'   )
#' );
#'
#' ### More extensive example with an RMarkdown partial that
#' ### displays in the viewer
#' rosetta::rosettaReliability_partial(
#'   rosetta::reliability(
#'     attitude,
#'     descriptives = TRUE,
#'     itemLevel = TRUE,
#'     scatterMatrix = TRUE
#'   )
#' );
#' }
#'
reliability <- function(data,
                        items = NULL,
                        scaleStructure = TRUE,
                        descriptives = FALSE,
                        itemLevel = FALSE,
                        scatterMatrix = FALSE,
                        scatterMatrixArgs = list(progress=FALSE),
                        digits = 2,
                        conf.level = .95,
                        itemLabels = NULL,
                        itemOmittedCorsWithRest = FALSE,
                        itemOmittedCorsWithTotal = FALSE,
                        alphaOmittedCIs = FALSE,
                        omegaFromMBESS = FALSE,
                        omegaFromPsych = TRUE,
                        ordinal = FALSE,
                        headingLevel = 3,
                        ...) {

  if (omegaFromMBESS && !requireNamespace("MBESS", quietly = TRUE)) {
    stop("If you order omega from MBESS, you need to have it installed, too!");
  }

  if (omegaFromPsych) {
    suppressWarnings(suppressMessages({
      if (!requireNamespace("GPArotation", quietly=TRUE)) {
        stop("To compute omega with the `psych` package, you need to have ",
             "package {GPArotation} installed. You can install it with:\n\n",
             "install.packages('GPArotation');");
      }
    }));
  }

  if (!is.data.frame(data)) {
    stop("As `data`, you did not pass a data frame, but an object of ",
         "class ", vecTxt(class(data), useQuote = "`"), ". You ",
         "can try converting this object to a data frame using ",
         "`as.data.frame()`.");
  }

  if (is.null(items)) {
    items <- names(data);
  } else {
    wrongItems <- which (!(items %in% names(data)));
    if (length(wrongItems) > 0) {
      stop("In `items`, you specified one or more variable/column ",
           "names that do not exist in the data frame you passed in ",
           "`data`: ", vecTxt(items[wrongItems], useQuote = "`"), ".");
    }
  }

  if (is.null(itemLabels)) {
    itemLabels <- items;
  } else {
    if (length(itemLabels) != length(items)) {
      stop("The vector with item labels (", vecTxtQ(itemLabels),
           ") does not have the same length ",
           "as the vector with items(", vecTxtQ(items),
           ")!");
    }
  }

  if (is.null(names(itemLabels))) {
    names(itemLabels) <- items;
  } else {
    if (!(all(names(itemLabels) %in% items))) {
      stop("You passed a named vector with item labels, but not all ",
           "names correspond to items that you passed in `items`!");
    }
  }

  res <-
    list(
      digits = digits,
      items = items,
      itemLabels = itemLabels,
      conf.level = conf.level,
      headingLevel = headingLevel,
      data = data[, items]
    );

  res$data <-
    res$data[
      stats::complete.cases(res$data),
    ];

  if (scaleStructure) {
    res$scaleStructure <-
      ufs::scaleStructure(data = res$data,
                          items = items,
                          digits = digits,
                          poly = ordinal,
                          headingLevel = headingLevel,
                          conf.level = conf.level,
                          omega.psych_nfactors = 1,
                          omega.psych_flip = FALSE,
                          ...);
  }

  if (scatterMatrix) {
    res$scatterMatrix <-
      do.call(
        ufs::scatterMatrix,
        c(list(dat=res$data,
               items=items,
               itemLabels = itemLabels,
               conf.level = conf.level),
          scatterMatrixArgs)
      );
  }

  ### Create a vector with the full scale for potential
  ### item-total correlations
  res$scaleVector <-
    rowMeans(res$data,
             na.rm=TRUE);

  if (descriptives) {
    res$scaleDescriptiveCIs <-
      descriptiveCIs(
        data = data.frame(scale = res$scaleVector),
        items = "scale",
        digits = digits,
        conf.level = conf.level
      );
  }

  if (itemLevel) {

    if (descriptives) {
      res$itemLevelDescriptiveCIs <-
        descriptiveCIs(
          data = res$data,
          items = items,
          itemLabels = itemLabels,
          digits = digits,
          conf.level = conf.level
        );
    }

    ### Item-total correlations
    res$itemTotalCorrelations <-
      do.call(
        rbind,
        lapply(
          items,
          function(i) {
            res <- stats::cor.test(
              res$data[, i],
              res$scaleVector,
              conf.level = conf.level
            );
            return(
              data.frame(
                itemTotal_r.ci.lo = res$conf.int[1],
                itemTotal_r.point = res$estimate,
                itemTotal_r.ci.hi = res$conf.int[2],
                drop=FALSE
              )
            );
          }
        )
      );
    row.names(res$itemTotalCorrelations) <-
      items;

    ### Create dataframes omitting each item
    res$dataOmittingItems <-
      lapply(
        items,
        function(i) {
          return(res$data[, names(res$data) != i,
                          drop=FALSE]);
        }
      );
    names(res$dataOmittingItems) <-
      items;

    ### Also create scale vectors omitting items
    res$scaleVectorsOmittingItems <-
      lapply(
        res$dataOmittingItems,
        function(i) {
          return(rowMeans(i, na.rm=TRUE));
        }
      );
    names(res$scaleVectorsOmittingItems) <-
      items;

    ### Use scale vectors omitting items to compute item-rest
    ### correlations
    res$itemRestCorrelations <-
      do.call(
        rbind,
        lapply(
          items,
          function(i) {
            res <- stats::cor.test(
              res$data[, i],
              res$scaleVectorsOmittingItems[[i]],
              conf.level = conf.level
            );
            return(
              data.frame(
                itemRest_r.ci.lo = res$conf.int[1],
                itemRest_r.point = res$estimate,
                itemRest_r.ci.hi = res$conf.int[2]
              )
            );
          }
        )
      );
    row.names(res$itemRestCorrelations) <- items;

    if (length(items) > 2) {
      ### Compute alpha omitting items
      res$alphaOmittingItems <-
        lapply(
          res$dataOmittingItems,
          function(i) {
            res <- list();
            invisible(
              utils::capture.output(
                suppressMessages(
                  suppressWarnings(
                    res$alphaFromPsychObject <-
                      psych::alpha(
                        i,
                        delete=FALSE,
                        check.keys=FALSE,
                        warnings=FALSE)
                  )
                )
              )
            );
            res$alphaFromPsych <- res$alphaFromPsychObject$total$raw_alpha;
            if (alphaOmittedCIs) {
              invisible(utils::capture.output(
                res$alphaFromPsych_ci <-
                  psych::alpha.ci(
                    alpha = res$alphaFromPsych,
                    n.obs = nrow(i),
                    n.var = length(items),
                    p.val = 1 - conf.level
                  )
              ));
            }
            return(res);
          }
        );
      names(res$alphaOmittingItems) <-
        items;

      ### Compute omega from `psych` omitting items
      if (omegaFromPsych) {
        res$omegaFromPsychOmittingItems <-
          lapply(
            res$dataOmittingItems,
            function(i) {
              res <- list();
              invisible(
                utils::capture.output(
                  suppressMessages(
                    suppressWarnings(
                      res$omegaFromPsychObject <-
                        psych::omega(
                          i,
                          nfactors = 1,
                          flip=FALSE,
                          plot=FALSE
                        )
                    )
                  )
                )
              );
              res$omegaFromPsych <- res$omegaFromPsychObject$omega.tot;
              return(res);
            }
          );
        names(res$omegaFromPsychOmittingItems) <-
          items;
      }

      ### Compute omega from `MBESS` omitting items
      if (omegaFromMBESS) {
        res$omegaFromMBESSOmittingItems <-
          lapply(
            res$dataOmittingItems,
            function(i) {
              res <- list();
              res$omegaFromMBESSObject <-
                MBESS::ci.reliability(
                  i,
                  type="omega",
                  conf.level = conf.level
                );
              res$omegaFromMBESS <-
                res$omegaFromMBESSObject$est;
              return(res);
            }
          );
        names(res$omegaFromMBESSOmittingItems) <-
          items;
      }

      if (itemOmittedCorsWithRest && itemOmittedCorsWithTotal) {
        res$itemOmitted_correlations <-
          cbind(res$itemRestCorrelations,
                res$itemTotalCorrelations);
      } else if (itemOmittedCorsWithRest) {
        res$itemOmitted_correlations <-
          res$itemRestCorrelations;
      } else if (itemOmittedCorsWithTotal) {
        res$itemOmitted_correlations <-
          res$itemTotalCorrelations;
      } else {
        res$itemOmitted_correlations <- NULL;
      }

      res$itemOmitted_internalConsistency <-
        do.call(
          rbind,
          lapply(
            items,
            function(i) {
              if (alphaOmittedCIs) {
                subResult <- data.frame(
                  alpha.ci.lo =
                    res$alphaOmittingItems[[i]]$alphaFromPsych_ci$lower.ci,
                  alpha.point =
                    res$alphaOmittingItems[[i]]$alphaFromPsych,
                  alpha.ci.hi =
                    res$alphaOmittingItems[[i]]$alphaFromPsych_ci$upper.ci
                );
              } else {
                subResult <- data.frame(
                  alpha.point =
                    res$alphaOmittingItems[[i]]$alphaFromPsych
                );
              }
              if (omegaFromPsych) {
                subResult <-
                  cbind(
                    subResult,
                    data.frame(
                      omega.point_fromPsych =
                        res$omegaFromPsychOmittingItems[[i]]$omegaFromPsych
                    )
                  );
              }
              if (omegaFromMBESS) {
                subResult <-
                  cbind(
                    subResult,
                    data.frame(
                      omega.point_fromMBESS =
                        res$omegaFromMBESSOmittingItems[[i]]$omegaFromMBESS
                    )
                  );
              }
              return(subResult);
            }
          )
        );
      row.names(res$itemOmitted_internalConsistency) <-
        items;
    }
  } else {
    res$alphaOmittingItems <- NULL;
    res$omegaFromMBESSOmittingItems <- NULL;
    res$omegaFromPsychOmittingItems <- NULL;
    res$itemOmitted_correlations <- NULL;
    res$itemOmitted_internalConsistency <- NULL;
  }

  class(res) <-
    "rosettaReliability";

  return(res);

}

###-----------------------------------------------------------------------------

#' @rdname rosettaReliability
#' @export
rosettaReliability_partial <- function(x,
                                       digits = x$digits,
                                       headingLevel = x$headingLevel,
                                       printPlots = TRUE,
                                       echoPartial = FALSE,
                                       partialFile = NULL,
                                       quiet=TRUE,
                                       ...) {

  ### Get filename
  if (!is.null(partialFile) && file.exists(partialFile)) {
    rmdPartialFilename <-
      partialFile;
  } else {
    rmdPartialFilename <-
      system.file("partials", "_rosettaReliability_partial.Rmd", package="rosetta");
  }

  rmdpartials::partial(rmdPartialFilename);

}

###-----------------------------------------------------------------------------

#' @rdname rosettaReliability
#' @method knit_print rosettaReliability
#' @importFrom knitr knit_print
#' @export
knit_print.rosettaReliability <- function(x,
                                          digits = x$digits,
                                          headingLevel = x$headingLevel,
                                          printPlots = TRUE,
                                          echoPartial = FALSE,
                                          partialFile = NULL,
                                          quiet=TRUE,
                                          ...) {
  rosettaReliability_partial(x = x,
                             headingLevel = headingLevel,
                             quiet = quiet,
                             echoPartial = echoPartial,
                             partialFile = partialFile,
                             printPlots = printPlots,
                             ...);
}

###-----------------------------------------------------------------------------

#' @export
#' @rdname rosettaReliability
print.rosettaReliability <- function(x,
                                     digits = x$digits,
                                     headingLevel = x$headingLevel,
                                     forceKnitrOutput = FALSE,
                                     printPlots = TRUE,
                                     ...) {

  if (isTRUE(getOption('knitr.in.progress')) || forceKnitrOutput) {

    rosettaReliability_partial(x = x,
                               digits = digits,
                               headingLevel = headingLevel,
                               printPlots = printPlots,
                               ...);

  } else {
    if (!is.null(x$scaleStructure)) {
      print(x$scaleStructure, ...);
    }
    if (!is.null(x$scaleDescriptiveCIs)) {
      cat0(
        "\nScale descriptives\n\n"
      );
      print(x$scaleDescriptiveCIs);
      cat0("\n");
    }
    if (!is.null(x$itemLevelDescriptiveCIs)) {
      cat0(
        "\nItem-level descriptives\n\n"
      );
      print(x$itemLevelDescriptiveCIs);
      cat0("\n");
    }
    if (!is.null(x$itemOmitted_correlations)) {
      cat0(
        "\nCorrelations of items with scale\n\n"
      );
      row.names(x$itemOmitted_correlations) <-
        x$itemLabels[row.names(x$itemOmitted_correlations)];
      print(x$itemOmitted_correlations,
            digits = digits);
      cat(paste0("\nNote: these are ", round(100*x$conf.level, 2),
                 "% confidence intervals."));
    }
    if (!is.null(x$itemOmitted_internalConsistency)) {
      cat("\nInternal consistency estimates with items omitted\n\n");
      row.names(x$itemOmitted_internalConsistency) <-
        x$itemLabels[row.names(x$itemOmitted_internalConsistency)];
      print(x$itemOmitted_internalConsistency,
            digits = digits);
      cat(paste0("\nNote: these are ", round(100*x$conf.level, 2),
                 "% confidence intervals."));
    }
    if (!is.null(x$scatterMatrix) && printPlots) {
      print(x$scatterMatrix$output$scatterMatrix);
    }
    return(invisible(x));
  }
}
