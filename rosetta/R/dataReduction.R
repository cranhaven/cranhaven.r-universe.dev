#' Factor analysis or principal component analysis
#'
#' This is a wrapper for the `psych` functions [psych::pca()] and [psych::fa()]
#' to produce output that it similar to the output produced by jamovi.
#'
#' The code in these functions uses parts of the code in jamovi, written
#' by Jonathon Love and Ravi Selker.
#'
#' @param data The data frame that contains the `items`.
#' @param nfactors The number of factors to extract, or '`eigen`' to
#' extract all factors with an eigen value higher than the number
#' specified in `kaiser`. In the future, `parallel` can be specified
#' here to extract the number of factors suggested by parallel analysis.
#' @param items The items to analyse; if not specified, all variables
#' in `data` will be used.
#' @param rotate Which rotation to use; see [psych::fa()] for all
#' options. The most common options are '`none`' to not rotate at all;
#' '`varimax`' for an orthogonal rotation (assuming/imposing that the
#' components or factors are not correlated); or '`oblimin`' for an
#' oblique rotation (allowing the components/factors to correlate).
#' @param covar Whether to analyse the correlation matrix (`FALSE`) or
#' the covariance matrix (`TRUE`).
#' @param na.rm Whether to first remove all cases with missing values.
#' @param kaiser The minimum eigenvalue when applying the Kaiser criterion (see
#' `nfactors`).
#' @param loadings Whether to display the component or factor loadings.
#' @param summary Whether to display the factor or component summary.
#' @param correlations Whether to display the correlations between
#' factors of components.
#' @param modelFit Whether to display the model fit Only for EFA).
#' @param eigenValues Whether to display the eigen values.
#' @param screePlot Whether to display the scree plot.
#' @param residuals Whether to display the matrix with residuals.
#' @param itemLabels Optionally, labels to use for the items (optionally, named,
#' with the names corresponding to the `items`; otherwise, the order of the
#' labels has to match the order of the items)
#' @param colorLoadings Whether, when producing an Rmd partial (i.e. when
#' calling the command while knitting) to colour the cells using
#' [kableExtra::kable_styling()].
#' @param fm The method to use for the factor analysis: '`fm`' for Minimum
#' Residuals; '`ml`' for Maximum Likelihood; and '`pa`' for Principal Factor.
#' @param digits The number of digits to round to.
#' @param headingLevel The number of hashes to print in front of the headings
#' when printing while knitting
#' @param x The object to print.
#' @param forceKnitrOutput Force knitr output.
#' @param quiet Passed on to [knitr::knit()] whether it should b
#'  chatty (`FALSE`) or quiet (`TRUE`).
#' @param echoPartial Whether to show the executed code in the R Markdown
#' partial (`TRUE`) or not (`FALSE`).
#' @param partialFile This can be used to specify a custom partial file. The
#' file will have object `x` available.
#' @param ... Any additional arguments are passed to [psych::fa()],
#' [psych::pca()], to the default print method by the print method, and to
#' [rmdpartials::partial()] when knitting an RMarkdown partial.
#'
#' @return An object with the object resulting from the call to the
#' `psych` functions and some extracted information that will be printed.
#'
#' @rdname dataReduction
#' @aliases dataReduction factorAnalysis principalComponentAnalysis
#'
#' @examples ### Load example dataset
#' data("pp15", package="rosetta");
#'
#' ### Get variable names with expected
#' ### effects of a high dose of MDMA
#' items <-
#'   grep(
#'     "highDose_AttBeliefs_",
#'     names(pp15),
#'     value=TRUE
#'   );
#'
#' ### Do a factor analysis
#' rosetta::factorAnalysis(
#'   data = pp15,
#'   items = items,
#'   nfactors = "eigen",
#'   scree = TRUE
#' );
#'
#' if (FALSE) {
#'   ### To get more output, show the
#'   ### output as Rmd Partial in the viewer,
#'   ### and color/size the factor loadings
#'   rosetta::rosettaDataReduction_partial(
#'     rosetta::factorAnalysis(
#'       data = pp15,
#'       items = items,
#'       nfactors = "eigen",
#'       summary = TRUE,
#'       correlations = TRUE,
#'       colorLoadings = TRUE
#'     )
#'   );
#' }
#'
#' @export
factorAnalysis <- function(data,
                           nfactors,
                           items = names(data),
                           rotate = "oblimin",
                           covar = FALSE,
                           na.rm = TRUE,
                           kaiser = 1,
                           loadings = TRUE,
                           summary = FALSE,
                           correlations = FALSE,
                           modelFit = FALSE,
                           eigenValues = FALSE,
                           screePlot = FALSE,
                           residuals = FALSE,
                           itemLabels = items,
                           colorLoadings = FALSE,
                           fm = "minres",
                           digits = 2,
                           headingLevel = 3,
                           ...) {
  return(
    do.call(
      dataReduction,
      c(as.list(environment()),
        list(dataReductionMethod = "factorAnalysis")
      )
    )
  );
}

###-----------------------------------------------------------------------------

#' @rdname dataReduction
#' @export
principalComponentAnalysis <- function(data,
                                       items,
                                       nfactors,
                                       rotate = "oblimin",
                                       covar = FALSE,
                                       na.rm = TRUE,
                                       kaiser = 1,
                                       loadings = TRUE,
                                       summary = FALSE,
                                       correlations = FALSE,
                                       eigenValues = FALSE,
                                       screePlot = FALSE,
                                       residuals = FALSE,
                                       itemLabels = items,
                                       colorLoadings = FALSE,
                                       digits = 2,
                                       headingLevel = 3,
                                       ...) {
  return(
    do.call(
      dataReduction,
      c(as.list(environment()),
        list(modelFit = FALSE,
             dataReductionMethod = "principalComponentAnalysis")
      )
    )
  );
}

###-----------------------------------------------------------------------------

dataReduction <- function(data,
                          items = names(data),
                          nfactors = NULL,
                          rotate = "oblimin",
                          covar = FALSE,
                          na.rm = TRUE,
                          kaiser = 1,
                          loadings = TRUE,
                          summary = FALSE,
                          correlations = FALSE,
                          modelFit = FALSE,
                          eigenValues = FALSE,
                          screePlot = FALSE,
                          residuals = FALSE,
                          itemLabels = items,
                          colorLoadings = FALSE,
                          fm = "minres",
                          digits = 2,
                          headingLevel = 3,
                          ...,
                          dataReductionMethod) {

  if (dataReductionMethod == "principalComponentAnalysis") {
    factorName <- "component";
    FactorName <- "Component";
    factorsName <- "components";
  } else {
    factorName <- "factor";
    FactorName <- "Factor";
    factorsName <- "factors";
  }

  if (!(rotate == "none")) {
    suppressWarnings(suppressMessages({
      if (!requireNamespace("GPArotation", quietly=TRUE)) {
        stop("To rotate the ", factorsName, ", you need to have ",
             "package {GPArotation} installed. You can install it with:\n\n",
             "install.packages('GPArotation');");
      }
    }));
  }

  if (is.null(nfactors)) {
    stop("You must specify how many ", factorsName, " you want to ",
         "extract in argument `nfactors`! Please pass a number as value of ",
         "argument `nfactors`, or pass 'eigen' to use the Kaiser ",
         "criterion (the default in SPSS, i.e., select factors with ",
         "eigenvalues over 1, or, more accurately, the value of the `kaiser` ",
         "argument) or 'parallel' to use parallel analysis (the ",
         "default in jamovi; as yet, this functionality hasn't been ",
         "added yet.");
  }

  if (!is.data.frame(data)) {
    stop("As argument `data`, you must pass a data frame!");
  }

  if (!all (items %in% names(data))) {
    stop("Not all variables you passed in argument `items` exist in ",
         "data frame `data`!");
  }

  if (is.null(names(itemLabels))) {
    names(itemLabels) <- items;
  }

  data <- data[, items];

  if (na.rm) {
    data <- data[stats::complete.cases(data), ];
  }

  res <- list(input = as.list(environment()),
              intermediate = list(),
              output = list());

  ### Get PCA eigen values
  res$intermediate$eigen <- eigen(stats::cor(data))$values;

  ### Get EFA eigen values
  if (dataReductionMethod != "principalComponentAnalysis") {
    suppressMessages(suppressWarnings(
      res$intermediate$fa.eigen <-
        psych::fa(
          r = data,
          nfactors = 1,
          rotate = rotate,
          covar = covar,
          fm = fm,
          ...
        )
    ));
    res$intermediate$commonFactorEigenvalues <-
      res$intermediate$fa.eigen$values;
  }

  if (!is.numeric(nfactors)) {
    if (nfactors == "parallel") {
      stop("Selecting the number of factors automatically through parallel ",
           "analysis is not yet supported!");
    } else {

      if (dataReductionMethod == "principalComponentAnalysis") {
        res$intermediate$nfactors <-
          sum(res$intermediate$eigen > kaiser);
      } else {
        res$intermediate$nfactors <-
          sum(res$intermediate$commonFactorEigenvalues > kaiser);
      }
    }
  } else {
    res$intermediate$nfactors <-
      nfactors;
  }

  ### Potentially add a helper function based on
  ### .parallel in https://github.com/jamovi/jmv/blob/master/R/pca.b.R
  ### to also enable 'parallel'.

  if (dataReductionMethod == "principalComponentAnalysis") {
    suppressMessages(suppressWarnings(
      res$intermediate$object <- psych::pca(
        r = data,
        nfactors = res$intermediate$nfactors,
        rotate = rotate,
        covar = covar,
        ...
      )
    ));
  } else {
    suppressMessages(suppressWarnings(
      res$intermediate$object <- psych::fa(
        r = data,
        nfactors = res$intermediate$nfactors,
        rotate = rotate,
        covar = covar,
        fm = fm,
        ...
      )
    ));
  }

  ### Loadings
  res$output$loadings <-
    as.data.frame(cbind(
      res$intermediate$object$loadings,
      res$intermediate$object$uniquenesses
    ));
  names(res$output$loadings) <-
    c(paste0(FactorName,
             " ",
             1:(ncol(res$output$loadings)-1)),
      "Uniqueness"
    );
  row.names(res$output$loadings) <-
    itemLabels[row.names(res$output$loadings)];

  ### Potentially color loadings


  ### Summary
  res$output$summary <-
    t(res$intermediate$object$Vaccounted);

  if (ncol(res$output$summary) == 2) {
    ### 1 factor
    names(res$output$summary) <-
      c("SS Loadings", "% of Variance");
  } else {
    res$output$summary <-
      as.data.frame(res$output$summary[, 1:3]);
    names(res$output$summary) <-
      c("SS Loadings", "% of Variance", "Cumulative %");
  }
  rownames(res$output$summary) <-
    paste0(FactorName, " ", 1:nrow(res$output$summary));

  ### Correlations
  res$output$correlations <-
    as.data.frame(res$intermediate$object$r.scores);
  row.names(res$output$correlations) <-
    names(res$output$correlations) <-
    paste0(FactorName, " ", 1:nrow(res$output$correlations));

  ### Model fit
  if (dataReductionMethod == "factorAnalysis") {
    res$output$modelFit <-
      data.frame(rmsea = res$intermediate$object$RMSEA['RMSEA'],
                 rmsea.lo = res$intermediate$object$RMSEA['lower'],
                 rmsea.hi = res$intermediate$object$RMSEA['upper'],
                 tli = res$intermediate$object$TLI,
                 bic = res$intermediate$object$BIC,
                 chisq = res$intermediate$object$STATISTIC,
                 df = res$intermediate$object$dof,
                 pval = res$intermediate$object$PVAL);
    res$intermediate$rmsea_conf.level <- res$intermediate$object$RMSEA['confidence'];
    names(res$output$modelFit) <-
      c("RMSEA (point estimate)",
        paste0("RMSEA (",
               round(100*res$intermediate$rmsea_conf.level, 2),
               "% CI lower bound)"),
        paste0("RMSEA (",
               round(100*res$intermediate$rmsea_conf.level, 2),
               "% CI upper bound)"),
        "TLI",
        "BIC",
        "Model test: ChiSquare",
        "Model test: Degrees of freedom",
        "Model test: p-value"
      );
    row.names(res$output$modelFit) <- NULL;
  }

  ### Eigen values
  if (dataReductionMethod == "principalComponentAnalysis") {
    res$output$eigenValues <-
      data.frame(seq_along(res$intermediate$eigen),
                 res$intermediate$eigen);
  } else {
    res$output$eigenValues <-
      data.frame(seq_along(res$intermediate$eigen),
                 res$intermediate$commonFactorEigenvalues);
  }
  names(res$output$eigenValues) <-
    c(FactorName,
      "Eigenvalue");

  ### Scree plot
  res$output$screePlot <-
    ggplot2::ggplot(
      data = data.frame(res$output$eigenValues),
      mapping = ggplot2::aes_string(x = FactorName,
                                    y = "Eigenvalue")) +
    ggplot2::geom_point(size=5) +
    ggplot2::geom_line(size=1.5) +
    ggplot2::theme_minimal();

  ### Residuals
  if (residuals) {
    res$output$residuals <-
      res$intermediate$object$residual;
    colnames(res$output$residuals) <- seq_along(items);
    row.names(res$output$residuals) <-
      paste0(seq_along(items), ": ", itemLabels[items]);
  }

  ### Some settings/labels for convenience
  res$output$analysisName <-
    ifelse(FactorName == "Factor",
           "Exploratory Factor Analysis (EFA)",
           "Principal Component Analysis (PCA)");

  res$output$rotation <-
    paste0(res$input$rotate, " rotation");
  res$output$rotation <-
    paste0(toupper(substring(res$output$rotation, 1, 1)),
           substring(res$output$rotation, 2));

  res$output$extractionMethod <-
    ifelse(FactorName == "Factor",
           res$input$fm,
           "pca");

  extractionMethods <-
    c(minres = "Minimum Residuals",
      uls = "Minimum Residuals",
      ols = "Ordinary Least Squares",
      wls = "Weighted Least Squares",
      gls = "Generalized Weighted Least Squares",
      pa = "Principal Factor Solution",
      ml = "Maximum Likelihood",
      minchi = "Minimize Sample Size Weighted Chi Squared",
      minrank = "Minimum Rank Factor Analysis",
      old.min = "Old implementation of OLS (see `?psych::fa` help page)",
      alpha = "Alpha Factor Analysis",
      pca = "Principal Component Analysis");

  res$output$extractionMethod <-
    extractionMethods[res$output$extractionMethod];

  res$output$n <- nrow(res$input$data);

  class(res) <-
    "rosettaDataReduction";

  return(res);

}

###-----------------------------------------------------------------------------

#' @rdname dataReduction
#' @export
rosettaDataReduction_partial <- function(x,
                                         digits = x$input$digits,
                                         headingLevel = x$input$headingLevel,
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
      system.file("partials", "_rosettaDataReduction_partial.Rmd",
                  package="rosetta");
  }

  rmdpartials::partial(rmdPartialFilename);

}

###-----------------------------------------------------------------------------

#' @rdname dataReduction
#' @method knit_print rosettaDataReduction
#' @importFrom knitr knit_print
#' @export
knit_print.rosettaDataReduction <- function(x,
                                            digits = x$input$digits,
                                            headingLevel = x$input$headingLevel,
                                            echoPartial = FALSE,
                                            partialFile = NULL,
                                            quiet=TRUE,
                                            ...) {
  rosettaDataReduction_partial(x = x,
                               headingLevel = headingLevel,
                               quiet = quiet,
                               echoPartial = echoPartial,
                               partialFile = partialFile,
                               ...);
}

###-----------------------------------------------------------------------------

dataReduction_loadingColoring <- function(loadingDf,
                                          digits) {

  loadingDf[, 1:(ncol(loadingDf) - 1)] <-
    data.frame(
      lapply(
        loadingDf[, 1:(ncol(loadingDf) - 1),
                          drop=FALSE],
        function(column) {
          kableExtra::cell_spec(
            round(column, digits),
            bold = T,
            color = kableExtra::spec_color(
              abs(column),
              end = 0.9,
              direction = -1,
              scale_from = c(0, 1)
            ),
            font_size = kableExtra::spec_font_size(
              abs(column),
              scale_from = c(-1, 1)
            )
          )
        }
      )
    );

  return(loadingDf);

}

###-----------------------------------------------------------------------------

#' @rdname dataReduction
#' @export
print.rosettaDataReduction <- function(x,
                                       digits = x$input$digits,
                                       headingLevel = x$input$headingLevel,
                                       forceKnitrOutput = FALSE,
                                       ...) {

  if (isTRUE(getOption('knitr.in.progress')) || forceKnitrOutput) {

    rosettaDataReduction_partial(x = x,
                                 digits = digits,
                                 headingLevel = headingLevel,
                                 ...);

  } else {

    FactorName <- x$input$FactorName;

    cat0("\n", x$output$analysisName, "\n");
    cat0("\n  Extraction method: ", x$output$extractionMethod);
    cat0("\n  Rotation :         ", x$output$rotation);
    cat0("\n  Sample size :      ", x$output$n, "\n\n");

    if (x$input$loadings) {
      cat0(FactorName, " loadings\n\n");
      print(round(x$output$loadings, digits=x$input$digits));

      if (((pkgdown_rdname() %in% "factorAnalysis") ||
           (pkgdown_rdname() %in% "dataReduction"))
           ||
          in_pkgdown_example()
          ) {

        ### Do nothing, actually

      } else {

        if (x$input$colorLoadings) {

          x$output$loadings <-
            dataReduction_loadingColoring(
              x$output$loadings,
              digits = x$input$digits
            );

        }

        ufs::kblXtra(
          x$output$loadings,
          digits = digits,
          viewer = TRUE
        );

      }

    }

    if (x$input$summary) {
      cat0(FactorName, " summary\n\n");
      roundedSummary <-
        round(x$output$summary, digits=x$input$digits);
      if (ncol(roundedSummary) > 2) {
        roundedSummary[, 2:3] <- 100*roundedSummary[, 2:3];
      } else {
        roundedSummary[, 2] <- 100*roundedSummary[, 2];
      }
      print(roundedSummary);
    }

    if (x$input$correlations) {
      cat0(FactorName, " correlations\n\n");
      print(round(x$output$correlations, digits=x$input$digits));
    }

    if (x$input$modelFit) {
      modelFit <- x$output$modelFit;
      names(modelFit) <-
        c("RMSEA", "CI, LB", "CI, UB", "TLI", "BIC", "ChiSq", "Df", "p");
      modelFit[, 1:4] <-
        round(modelFit[, 1:4], digits+2);
      modelFit[, 5:6] <-
        round(modelFit[, 5:6], max(digits-2, 0));
      modelFit[, 7] <-
        round(modelFit[, 7]);
      modelFit[, 8] <-
        formatPvalue(modelFit[, 8], includeP = FALSE, spaces=FALSE);
      cat0("\nModel fit\n\n");
      print(modelFit);
    }

    if (x$input$eigenValues) {
      cat0("\nInitial eigen values\n\n");
      print(round(x$output$eigenValues, digits=x$input$digits));
    }

    if (x$input$screePlot) {
      grid::grid.newpage();
      grid::grid.draw(x$output$screePlot);
    }

    if (x$input$residuals) {
      cat0("\nResiduals\n\n");
      print(round(x$output$residuals, digits=x$input$digits));
    }

    return(invisible(x));

  }

}
