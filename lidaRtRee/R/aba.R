# package lidaRtRee
# Copyright INRAE
# Author(s): Jean-Matthieu Monnet
# Licence: GPL-3
#-------------------------------------------------------------------------------
#' Calibrates and validates area-based models
#'
#' The function can first apply a Box-Cox transformation to the dependent variable,
#' in order to normalize its distribution, or a log transformation to the whole
#' dataset. Then it uses \code{\link[leaps]{regsubsets}} to find the 20 linear
#' regressions with the best adjusted-R2 among combinations of at most \code{nmax}
#' independent variables. Each model can then be tested regarding the following
#' linear model assumptions are checked:
#' \itemize{
#' \item tests performed by \code{\link[gvlma]{gvlma}}
#' \item the variance inflation factor is below 5 (models with two or more
#' independent variables)
#' \item no partial p.value of variables in the model is below 0.05
#' }
#' The model with the highest adjusted-R2 among those fulfilling the required
#' conditions is selected. A leave-one-out cross validation (LOO CV) is performed
#' by fitting the model coefficients using all observations except one and applying
#' the resulting model to predict the value for the remaining observation. In
#' case a transformation was performed beforehand, a bias correction is applied.
#' LOO CV statistics are then computed.
#'
#' @param variable vector. dependent variable values
#' @param predictors data.frame. independent variables (columns: metrics,
#' lines: observations). Row names are used for the output predicted values
#' @param transform string. transformation to be applied to data (\code{"none"},
#' \code{"boxcox"}: Box-Cox transformation applied only to the dependent variable,
#' \code{"log"}: log transformation applied to both dependent and independent
#' variables)
#' @param nmax numeric. maximum number of independent variables in the model
#' @param test vector. which tests should be satisfied by the models, one to
#' three in \code{"partial_p"}, \code{"vif"}, \code{"gvlma"}
#' @param xy data.frame or matrix of easting and northing coordinates of
#' observations: not used in the function but exported in the result for use in
#'  further inference functions
#' @param threshold vector of length two. minimum and maximum values of threshold
#'  to apply to predicted values. Non-finite values are replaced by the minimum. 
#'  Values are replaced after computation of the variance of residuals used for
#'  bias correction
#' @param ... other parameters to be passed to \code{\link[leaps]{regsubsets}},
#'
#' @seealso \code{\link{aba_combine_strata}} for combining models calibrated
#' on different strata, \code{\link{aba_plot}} for plotting model
#' cross-validation results, \code{\link[leaps]{regsubsets}} for variable selection,
#'  \code{\link{lma_check}} for linear model assumptions check,
#'  \code{\link{boxcox_itr_bias_cor}} for reverse Box-Cox transformation with bias
#'   correction.
#' @examples
#  # load Quatre Montagnes dataset
#' data(quatre_montagnes)
#' # build ABA model for basal area, with all metrics as predictors
#' model_aba <- aba_build_model(quatre_montagnes$G_m2_ha, quatre_montagnes[, 9:76],
#'   transform = "boxcox", nmax = 2
#' )
#' # summary of regression model
#' summary(model_aba$model)
#' # validation statistics
#' model_aba$stats
#' # observed and predicted values
#' summary(model_aba$values)
#'
#' # plot field values VS predictions in cross-validation
#' aba_plot(model_aba, main = "Basal area")
#' @return a list with three elements
#' \itemize{
#' \item \code{model}: list with one regression model (output from
#'  \code{\link[stats]{lm}}),
#' \item \code{stats}: model statistics (root mean square error estimated in
#' leave-one-out cross validation, coefficient of variation of rmse, p-value of
#' wilcoxon test of observed and predicted values, p-value of t-test of observed
#' and predicted values, p-value of anova of observed and predicted values,
#' correlation of observed and predicted values, R2 of observed and predicted
#' values, variance of regression residuals)
#' \item \code{values}: data.frame with observed and values predicted in
#' cross-validation.
#' }
#' @export
#'
aba_build_model <-
  function(variable,
           predictors,
           transform = "none",
           nmax = 3,
           test = c("partial_p", "vif", "gvlma"),
           xy = NULL,
           threshold = NULL,
           ...) {
    # build data.frame with dependent variable and predictors
    df <- data.frame(dep_var = variable, predictors)
    row.names(df) <- row.names(predictors)
    # remove lines where dependent variable is negative or missing
    dummy <- which(!is.na(df$dep_var) & df$dep_var > 0)
    if (length(dummy) < nrow(df)) {
      message("Negative or NA observations of dependent variable have been removed")
    }
    df <- df[dummy,]
    #
    # column to remove from data.frame to leave only independent variables
    var_out_indices <- 1
    #
    #-----------------------------------------------------------------------------
    # variable transformation
    # no transformation
    if (transform == "none") {
      df_transform <- df
      lambda <- NA
    }
    # Box-Cox transformation of the dependent variable
    if (transform == "boxcox") {
      # selecting the best Box-Cox parameter for normalization of the dependent
      # variable distribution
      lambda <- car::powerTransform(df[, 1])$lambda
      # create a copy of data
      df_transform <- df
      # apply Box-Cox transformation to dependent variable
      df_transform[, 1] <- boxcox_tr(df_transform[, 1], lambda)
    }
    # log transformation of all data
    if (transform == "log") {
      df_transform <- log(df)
      lambda <- NA
      # check that no non-finite values have been created
      test_finite <- apply(df_transform, 2, function(x) {
        all(is.finite(x))
      })
      # remove variable with non-finite values if present
      if (!all(test_finite)) {
        warning(
          paste(
            "Removed variables because of non-finite values after log transform: ",
            paste(names(df_transform[which(!test_finite)]))
          )
        )
      }
      # add variables with non-finite values to vector of columns to be removed
      # before model building
      var_out_indices <- c(var_out_indices, which(!test_finite))
    }
    #
    #-----------------------------------------------------------------------------
    # selection of 20 combinations of variables with best adj-R2
    var_combi <-
      suppressWarnings(
        leaps::regsubsets(
          y = df_transform[, 1],
          x = df_transform[,-var_out_indices],
          nbest = 20,
          nvmax = nmax - 1,
          method = "exhaustive",
          really.big = TRUE,
          ...
        )
      )
    #-----------------------------------------------------------------------------
    # check linear model assumptions
    # matrix of booleans indicating formulas
    fomula_bool <- summary(var_combi)$which
    # check all formulas for modeling hypotheses
    test_lin <- apply(fomula_bool, 1, function(x) {
      formule <-
        stats::as.formula(paste("dep_var~",
                                paste(colnames(fomula_bool)[which(x == TRUE)[-1]], collapse = "+")))
      lma_check(formule,
                df_transform,
                max.pvalue = 0.05,
                max.vif = 5)
    })
    # bind results
    test_lin <- do.call(rbind, test_lin)
    # removal of models which do not satisfy hypotheses
    if (length(test) > 0) {
      test_lin <- test_lin[which(apply(test_lin[, test], 1, all)),]
    }
    #-----------------------------------------------------------------------------
    # output the best remaining model
    if (dim(test_lin)[1] > 0) {
      # keep only best adjR2
      ir <- which(test_lin$adjR2 == max(test_lin$adjR2))[1]
      nbobs <- nrow(df)
      formule <- as.character(test_lin$formula[ir])
      # create result data.frame with model information
      modeles <-
        data.frame(
          n = nbobs,
          formula = formule,
          adjR2 = test_lin$adjR2[ir],
          transform = transform,
          lambda = lambda
        )
      # build formula
      formule <- stats::as.formula(paste0("dep_var ~ ", formule))
      # save global regression model
      regr <- eval(parse(
        text = paste0(
          "stats::lm(",
          formule[2],
          "~",
          formule[3],
          ",data = df_transform)"
        )
      ))
      #---------------------------------------------------------------------------
      # leave one out cross validation
      # create vectors of outputs (predictions, residuals variance)
      prediction <- var_res <- rep(NA, nbobs)
      # Leave-One-Out Cross Validation
      for (i in 1:nbobs)
      {
        # fit lm with formula and n-1 values
        dummy <-
          stats::lm(formula = formule, data = df_transform[-i,])
        # predict on remaining value
        prediction[i] <- stats::predict.lm(dummy, df_transform[i,])
        # record residuals variance for model of prediction i
        var_res[i] <- sum(dummy$residuals ^ 2) / dummy$df.residual
      }
      #---------------------------------------------------------------------------
      # apply back-transformation if required
      if (transform == "boxcox") {
        # bias correction using the variance of residuals [Tiefelsdorf
        # (January 2013): A Note on the Reverse Box-Cox Transformation]
        prediction <- boxcox_itr_bias_cor(prediction, lambda, var_res)
      }
      if (transform == "log") {
        # bias correction as in [Bouvier et al.: Generalizing...]
        prediction <- exp(prediction) * exp(var_res / 2)
      }
      #
      # threshold for predicted values
      if (!is.null(threshold)) {
        prediction <- pmin(prediction, threshold[2])
        prediction <- pmax(prediction, threshold[1])
        prediction[!is.finite(prediction)] <- threshold[1]
      }
      #
      #---------------------------------------------------------------------------
      # validation statistics
      # vector of dependent variable
      dummy <- df[, 1]
      # compute root mean square error
      modeles$rmse <- (sum((prediction - df[, 1]) ^ 2) / nbobs) ^ 0.5
      # compute coefficient of variation of rmse
      modeles$cvrmse <- modeles$rmse / mean(df[, 1])
      # compute p-value of wilcoxon test of observed and predicted values
      modeles$pwil <- stats::wilcox.test(prediction, df[, 1],
                                         paired = TRUE)$p.value
      # compute p-value of t-test of observed and predicted values
      modeles$pttest <-
        stats::t.test(prediction, df[, 1], paired = TRUE)$p.value
      # compute p-value of anova of observed and predicted values
      modeles$paov <- stats::anova(stats::lm(c(prediction, df[, 1]) ~
                                               as.factor(c(
                                                 rep(0, nbobs),
                                                 rep(1, nbobs)
                                               ))))[5][1, 1]
      # compute correlation of observed and predicted values
      modeles$cor <-
        stats::cor(df[, 1], prediction, use = "pairwise.complete.obs")
      # compute R2 of observed and predicted values
      modeles$looR2 <-
        1 - sum((prediction - df[, 1]) ^ 2) / sum((df[, 1] - mean(df[, 1])) ^ 2)
      # compute variance of regression residuals
      modeles$var_res <- sum(regr$residuals ^ 2) / regr$df.residual
      # row.names
      # rename first column in case data.frame
      row.names(modeles)[1] <- "dep_var"
      #
      # prepare output list
      output <- list(
        "model" = regr,
        "stats" = modeles,
        "values" = data.frame(
          field = df[, 1],
          predicted = prediction,
          residual = df[, 1] - prediction,
          row.names = row.names(df)
        )
      )
      # add observation coordinates if provided
      if (!is.null(xy)) {
        output$values$x <- xy[, 1]
        output$values$y <- xy[, 2]
      }
      return(output)
    } else {
      return(NULL)
    }
  }

#-------------------------------------------------------------------------------
#' Checks linear model assumptions of a multiple regression model
#'
#' The performed tests are:
#' \itemize{
#' \item partial p.values calculated by \code{\link[stats]{lm}} are all below a given value
#' \item tests implemented by \code{\link[gvlma]{gvlma}}
#' \item variance inflation factors calculated by \code{\link[car]{vif}} are all below a given value
#' }
#'
#' @param formule formula. model to be evaluated
#' @param df data.frame. data to evaluate the model
#' @param max.pvalue numeric. maximum p-value of variables included in the model
#' @param max.vif numeric. maximum variance inflation factor of variables included in the model
#' @return a one line data.frame with 5 columns.
#' \itemize{
#' \item a string: evaluated formula
#' \item a numeric: the adjusted R squared of the model
#' \item a boolean: do all variables in the model have a partial p-value < \code{max.pvalue}
#' \item a boolean: are all tests implemented by \code{\link[gvlma]{gvlma}} false
#' \item a boolean: is the variance inflation factor computed with \code{\link[car]{vif}} of all variables < \code{max.vif}
#' }
#' @examples
#' # load Quatre Montagnes dataset
#' data(quatre_montagnes)
#' # fit lm model
#' model <- lm(G_m2_ha ~ zmax + zq95, data = quatre_montagnes)
#' lma_check(eval(model$call[[2]]), quatre_montagnes)
#' # trying with Box-Cox transformation of dependent variable
#' # and other independent variables
#' model <- lm(boxcox_tr(G_m2_ha, -0.14) ~ Tree_meanH + Tree_density + zpcum7, data = quatre_montagnes)
#' lma_check(eval(model$call[[2]]), quatre_montagnes)
#' @export
#'
lma_check <- function(formule,
                      df,
                      max.pvalue = 0.05,
                      max.vif = 5) {
  # build multiple regression model
  reg <- stats::lm(formule, data = df)
  # check linear model assumptions with gvlma
  dummy <- gvlma::gvlma(reg, df, alphalevel = 0.1)
  # select tests results
  testlma <- c(
    dummy$GlobalTest$GlobalStat4$Decision,
    dummy$GlobalTest$DirectionalStat1$Decision,
    dummy$GlobalTest$DirectionalStat2$Decision,
    dummy$GlobalTest$DirectionalStat3$Decision,
    dummy$GlobalTest$DirectionalStat4$Decision
  )
  # invert test results
  testlma <- abs(testlma - 1)
  # compute variance inflation factor if more than one variable in the model
  if (length(strsplit(as.character(formule)[3], "\\+")[[1]]) > 1) {
    testvif <- max(car::vif(reg))
  } else {
    testvif <- 0
  }
  # prepare regression model summary
  reg <- summary(reg)
  # output result data.frame with test results
  data.frame(
    "formula" = as.character(formule)[[3]],
    "adjR2" = reg$adj.r.squared,
    "partial_p" = (max(reg$coefficients[-1, 4]) < max.pvalue),
    "gvlma" = all(as.logical(testlma)),
    "vif" = testvif < max.vif
  )
}

#-------------------------------------------------------------------------------
#' Box-Cox Transformation
#'
#' @param x vector or raster. values to be transformed.
#' @param lambda numeric. parameter of Box-Cox transformation
#' @return a vector or raster of transformed values
#' @seealso \code{\link{boxcox_itr}} inverse Box-Cox transformation,
#' \code{\link{boxcox_itr_bias_cor}} inverse Box-Cox transformation with bias correction.
#' @examples
#' x <- 1:10
#' boxcox_tr(x, -2)
#' boxcox_tr(x, 0)
#' boxcox_tr(x, 0.5)
#' boxcox_tr(x, 2)
#'
#' # plot functions
#' curve(boxcox_tr(x, 1.5), 1, 5,
#'   main = "Box Cox transform", xlab = "x",
#'   ylab = "Boxcox(x, lambda)", col = "red"
#' )
#' curve(boxcox_tr(x, -2), 1, 5, col = "green", add = TRUE)
#' curve(boxcox_tr(x, 0), 1, 5, col = "blue", add = TRUE)
#' curve(boxcox_tr(x, 0.5), 1, 5, col = "black", add = TRUE)
#' curve(boxcox_tr(x, 1), 1, 5, col = "pink", add = TRUE)
#' legend("topleft",
#'   legend = rev(c(-2, 0, 0.5, 1, 1.5, "lambda")),
#'   col = rev(c("green", "blue", "black", "pink", "red", NA)), lty = 1
#' )
#' @export
boxcox_tr <- function(x, lambda) {
  # count_neg <- ifelse(inherits(x, "SpatRaster"),
  #                     sum(terra::values(x < 0), na.rm = TRUE),
  #                     sum(x < 0, na.rm = TRUE))
  # if (count_neg > 0) {
  #   x[x < 0] <- NA
  #   warning(paste0(count_neg, " negative value(s) set to NA"))
  # }
  if (lambda != 0) {
    (x ^ lambda - 1) / lambda
  } else {
    log(x)
  }
}

#-------------------------------------------------------------------------------
#' Inverse Box-Cox transformation
#'
#' @param x vector or raster values to be transformed
#' @param lambda numeric. parameter of Box-Cox transformation
#' @return a vector or raster of transformed values
#' @seealso \code{\link{boxcox_tr}} Box-Cox transformation,
#' \code{\link{boxcox_itr_bias_cor}} inverse Box-Cox transformation with bias
#' correction.
#' @examples
#' x <- 1:10
#' boxcox_itr(x, 0)
#' boxcox_itr(x, 0.5)
#' boxcox_itr(x, 2)
#' boxcox_itr(boxcox_tr(x, 2), 2)
#'
#' # plot functions
#' curve(boxcox_itr(x, 0), 0, 3,
#'   col = "blue", main = "inverse Box Cox transf.",
#'   xlab = "x", ylab = "inverse Boxcox(x, lambda)"
#' )
#' curve(boxcox_itr(x, 1.5), 0, 3, col = "red", add = TRUE)
#' curve(boxcox_itr(x, 0.5), 0, 3, col = "black", add = TRUE)
#' curve(boxcox_itr(x, 1), 0, 3, col = "pink", add = TRUE)
#' legend("topleft",
#'   legend = c("lambda", 0, 0.5, 1, 1.5),
#'   col = c(NA, "blue", "black", "pink", "red"), lty = 1
#' )
#' @export
boxcox_itr <- function(x, lambda) {
  # count_neg <- ifelse(inherits(x, "SpatRaster"),
  #                     sum(terra::values(x < 0), na.rm = TRUE),
  #                     sum(x < 0, na.rm = TRUE))
  # if (count_neg > 0) {
  #   x[x < 0] <- NA
  #   warning(paste0(count_neg, " negative value(s) set to NA"))
  # }
  if (lambda != 0) {
    (lambda * x + 1) ^ (1 / lambda)
  } else {
    exp(x)
  }
}

#-------------------------------------------------------------------------------
#' Inverse Box-Cox transformation with bias correction
#'
#' Inverse Box-Cox transform with bias correction as suggested by Pu & Tiefelsdorf
#' (2015). Here `varmod` is not the local prediction variance as suggested in
#' the paper but the model residuals variance. For variance computation,
#' uses `n-p` instead of `n-1`, with `p` the number of variables in the model.
#'
#' @param x vector or raster values to be transformed
#' @param lambda numeric. parameter of Box-Cox transformation
#' @param varmod numeric. model residuals variance
#' @references Xiaojun Pu and Michael Tiefelsdorf, 2015. A variance-stabilizing
#' transformation to mitigate biased variogram estimation in heterogeneous
#' surfaces with clustered samples. \doi{10.1007/978-3-319-22786-3_24}
#' @return a vector or raster
#' @seealso \code{\link{boxcox_tr}} Box-Cox transformation,
#' \code{\link{boxcox_itr}} inverse Box-Cox transformation.
#' @examples
#' x <- 1:10
#' boxcox_itr(x, 0.3)
#' boxcox_itr_bias_cor(x, 0.3, 0)
#' boxcox_itr_bias_cor(x, 0.3, 2)
#'
#' # plot functions
#' curve(boxcox_itr(x, 0.3), 0, 3,
#'   col = "blue",
#'   main = "inverse Box Cox transf., lambda = 0.3",
#'   xlab = "x", ylab = "inverse Boxcox(x, lambda = 0.3)"
#' )
#' curve(boxcox_itr_bias_cor(x, 0.3, 1), 0, 3, col = "red", add = TRUE)
#' curve(boxcox_itr_bias_cor(x, 0.3, 2), 0, 3, col = "black", add = TRUE)
#' legend("topleft",
#'   legend = c(
#'     "residuals variance  = 2",
#'     "residuals variance  = 1", "residuals variance not accounted for"
#'   ),
#'   col = c("black", "red", "blue"), lty = 1
#' )
#' @export
boxcox_itr_bias_cor <- function(x, lambda, varmod) {
  if (lambda != 0) {
    boxcox_itr(x, lambda) * (1 + (varmod * (1 - lambda) / (2 * (lambda * x + 1) ^
                                                             2)))
  } else {
    exp(x) * exp(varmod / 2)
  }
}

#-------------------------------------------------------------------------------
#' Combines a list of ABA models into a single ABA model object
#'
#' Combines a list of models (obtained with \code{\link{aba_build_model}}) into a
#' single object. Typically used to merge stratum-specific models into one object.
#' Validation statistics are computed for the combined strata, making it easier
#' to compare prediction performance with an unstratified model.
#'
#' @param model.list list. stratum-specific models returned by
#' \code{\link{aba_build_model}}
#' @param plotsId vector. "plotsId" for ordering row names in the "values" element
#'  of the output list
#' @return a list with three elements
#' \itemize{
#' \item \code{model}: a list of regression models corresponding to each stratum
#'  (output from \code{\link[stats]{lm}}),
#' \item \code{stats}: model statistics of each stratum-specific model (as in
#' \code{\link{aba_build_model}}) plus one line corresponding to statistics for all
#' strata (COMBINED)
#' \item \code{values}: data.frame with observed and values predicted in
#' cross-validation, and information on which stratum it belongs to.
#' }
#' @seealso \code{\link{aba_build_model}} for calibrated ABA model,
#' \code{\link{aba_plot}} for plotting model cross-validation results.
#' @examples
#' # load Quatre Montagnes dataset
#' data(quatre_montagnes)
#' # initialize list of models
#' model_aba_stratified <- list()
#' # calibrate basal area prediction model for each stratum
#' for (i in levels(quatre_montagnes$stratum))
#' {
#'   subsample <- which(quatre_montagnes$stratum == i)
#'   model_aba_stratified[[i]] <-
#'     aba_build_model(quatre_montagnes[subsample, "G_m2_ha"],
#'       quatre_montagnes[subsample, 9:76],
#'       transform = "boxcox", nmax = 4,
#'       xy = quatre_montagnes[subsample, c("X", "Y")]
#'     )
#' }
#' # combine models in single object
#' model_aba_stratified <- aba_combine_strata(
#'   model_aba_stratified,
#'   quatre_montagnes$plotId
#' )
#' # display content of output list
#' model_aba_stratified$model
#' model_aba_stratified$stats
#' summary(model_aba_stratified$values)
#'
#' # plot field values VS predictions in cross-validation
#' aba_plot(model_aba_stratified)
#' @export
#'
aba_combine_strata <- function(model.list, plotsId = NULL) {
  model_combined <- list()
  # extract models into separate list
  model_combined$model <- lapply(model.list, function(x) {
    x$model
  })
  # bind statistics into single data.frame
  model_combined$stats <-
    do.call(rbind, lapply(model.list, function(x) {
      x$stats
    }))
  # bind predicted values and add stratum
  model_combined$values <-
    do.call(rbind, lapply(as.list(names(model.list)), function(x) {
      data.frame(model.list[[x]]$values, stratum = x)
    }))
  #
  # compute rmse of combined models
  rmse <-
    (sum((model_combined$values$residual) ^ 2) / nrow(model_combined$values)) ^
    0.5
  # add statistics
  model_combined$stats[nrow(model_combined$stats) + 1,] <- c(
    nrow(model_combined$values),
    NA,
    NA,
    NA,
    NA,
    rmse,
    rmse / mean(model_combined$values$field),
    stats::wilcox.test(model_combined$values$residual)$p.value,
    stats::t.test(model_combined$values$residual)$p.value,
    NA,
    stats::cor(
      model_combined$values$field,
      model_combined$values$predicted,
      use = "pairwise.complete.obs"
    ),
    1 - sum((model_combined$values$residual) ^ 2) /
      sum((
        model_combined$values$field - mean(model_combined$values$field)
      ) ^ 2),
    NA
  )
  # update names of models
  row.names(model_combined$stats) <-
    c(names(model.list), "COMBINED")
  names(model_combined$model) <- names(model.list)
  # order values same as original data
  if (!is.null(plotsId)) {
    model_combined$values <- model_combined$values[plotsId,]
  }
  #
  # convert stratum field to factor
  model_combined$values$stratum <-
    as.factor(model_combined$values$stratum)
  model_combined
}

#-------------------------------------------------------------------------------
#' Plots observed VS values predicted in leave one out cross validation of an
#' \code{\link{aba_build_model}}
#'
#' @param aba_model list. as returned by \code{\link{aba_build_model}}
#' @param disp_text boolean. indicates if points should be labeled with id
#' @param col color to be passed to \code{\link[graphics]{plot}}, default is
#' black for single models, depends on stratum in stratified models
#' @param add_legend list. parameters to be passed to \code{\link[graphics]{legend}}. In case of a stratified model, legend is automatically set up.
#' @param ... other parameters to be passed to \code{\link[graphics]{plot}},
#' \code{xlab} and \code{ylab} are automatically added
#' @examples
#' # load Quatre Montagnes dataset
#' data(quatre_montagnes)
#' # build ABA model for basal area, with three metrics as predictors
#' model_aba <- aba_build_model(quatre_montagnes$G_m2_ha,
#'                              quatre_montagnes[, c("zpcum8", "ipcumzq70", "p_hmin")],
#'   transform = "log", nmax = 2
#' )
#'
#' # plot field values VS predictions in cross-validation
#' aba_plot(model_aba, main = "Basal area")
#' @return nothing
#' @export
#'
aba_plot <-
  function(aba_model,
           disp_text = F,
           col = NULL,
           add_legend = NULL,
           ...) {
    # color
    if (is.null(col)) {
      if (nrow(aba_model$stats) > 1)
        # if stratified model, color values by stratum
      {
        col <- as.numeric(aba_model$values$stratum)
      } else {
        col <- "black"
      }
    }
    #
    # display points, symbol color is white in case text is displayed
    graphics::plot(
      aba_model[["values"]]$predicted,
      aba_model[["values"]]$field,
      asp = 1,
      ylab = "Field",
      xlab = "Predicted in LOOCV",
      col = ifelse(rep(disp_text, length(col)), "white", col),
      ...
    )
    # add 1:1 line
    graphics::abline(c(0, 1))
    # add legend if user-specified
    if (!is.null(add_legend))
      do.call(graphics::legend, add_legend)
    # add legend if stratified model
    if (is.null(add_legend) & nrow(aba_model$stats) > 1) {
      graphics::legend("topleft",
                       levels(aba_model$values$stratum),
                       fill = 1:length(levels(aba_model$values$stratum)))
    }
    # display text if required
    if (disp_text) {
      graphics::text(
        aba_model[["values"]]$predicted,
        aba_model[["values"]]$field,
        labels = row.names(aba_model[["values"]]),
        cex = 0.8,
        col = col
      )
    }
  }

#-------------------------------------------------------------------------------
#' Mapping of ABA prediction models
#'
#' Applies calibrated area-based prediction models output of
#' \code{\link{aba_build_model}} to a raster of metrics to obtain a raster of
#' predictions
#'
#' @param model_aba model returned by \code{\link{aba_build_model}} or
#' \code{\link{aba_combine_strata}}
#' @param metrics_map raster. metrics returned e.g by
#' \code{\link[lidR]{pixel_metrics}}
#' @param stratum string. indicates which layer of metrics.map contains the
#' \code{stratum} in case of a stratified \code{aba.model}. The layer should have a RAT
#' including a column with the same name (see \code{\link[terra]{is.factor}}).
#' @param add_error boolean. indicates whether errors sampled from a normal distribution
#'  N(0, sigma(residuals)) should be added to fitted values; implemented only for
#'  \code{log} transformation case
#' @param pkg raster output format. Use pkg = "terra|raster|stars" to get an output in SpatRaster, RasterLayer
#' or stars format.
#' @param ... other parameters to be passed to \code{\link[stats]{predict.lm}}, e.g. \code{interval = "prediction"},
#' @examples
#' # load data
#' data(quatre_montagnes)
#' # build model
#' model_aba <- aba_build_model(quatre_montagnes$G_m2_ha, quatre_montagnes[, 9:76],
#'   transform = "boxcox"
#' )
#' # build example raster to apply model
#' quatre_montagnes$X <- rep(1:8, 12)
#' quatre_montagnes$Y <- rep(1:12, each = 8)
#' metrics_map <- terra::rast(quatre_montagnes[, c(2, 3, 9:76)], type = "xyz")
#' predict_map <- aba_predict(model_aba, metrics_map)
#'
#' # plot map
#' terra::plot(predict_map, main = "predictions")
#' @seealso \link{aba_build_model} for model fitting and \link{aba_combine_strata}
#' for combining stratified models, \link{clean_raster} for applying spatial mask
#' and value thresholds to a raster.
#' @return a raster of predictions obtained by applying the model \code{aba_build_model}
#' to the observations in \code{metrics_map}
#' @export
#'
aba_predict <-
  function(model_aba,
           metrics_map,
           stratum = NULL,
           add_error = FALSE,
           pkg = "terra", 
           ...) {
    # convert to terra
    if(!inherits(metrics_map, "SpatRaster")) metrics_map <- convert_raster(metrics_map, "terra")
    # create factor of stratum if not existing
    if (is.null(stratum)) {
      metrics_map$stratum <- "all"
      # levels(metrics_map$stratum) <- data.frame(ID = 1, stratum = "all")
      row.names(model_aba$stats)[1] <- "all"
      model_aba$model <- list("all" = model_aba$model)
      stratum <- "stratum"
    }
    #
    r <- list()
    # retrieve levels (terra <= 1.5-34)
    levs <- terra::levels(metrics_map[[stratum]])[[1]]
    # to comply with terra >= 1.6.2 (suggestion by Robert J. Hijmans)
    if (inherits(levs, "data.frame")) levs <- levs[,2]
    # loop on strata
    for (stratum_label in levs)
    {
      variables <-
        names(model_aba$model[[stratum_label]]$coefficients)
      variables <- variables[variables != "(Intercept)"]
      # transform dependant variables if log
      if (model_aba$stats[stratum_label, "transform"] == "log")
      {
        newdata <- log(metrics_map[[variables]])
        names(newdata) <- variables
      } else {
        newdata <- metrics_map[[variables]]
      }
      # predict on all cells
      r[[stratum_label]] <- terra::predict(newdata,
                                           model_aba$model[[stratum_label]], 
                                           ...)
      
      if (model_aba$stats[stratum_label, "transform"] == "boxcox")
        # case of Box-Cox transform
      {
        # back-transform
        r[[stratum_label]] <- boxcox_itr_bias_cor(r[[stratum_label]],
                                                      model_aba$stats[stratum_label, "lambda"],
                                                      model_aba$stats[stratum_label, "var_res"])
        if (add_error == TRUE) {
          warning("Error sampling not implemented in the boxcox transformation case")
        }
      }
      if (model_aba$stats[stratum_label, "transform"] == "log")
        # case of case of log-log transform
      {
        if (add_error == TRUE)
          # sampling of errors
        {
          rastResidual <- r[[stratum_label]]
          terra::values(rastResidual) <- stats::rnorm(length(rastResidual),
                                                      0,
                                                      sqrt(model_aba$stats[stratum_label, "var_res"]))
          r[[stratum_label]] <-
            exp(r[[stratum_label]] + rastResidual)
        } else {
          # bias correction in the case of log transformation
          r[[stratum_label]] <-
            exp(r[[stratum_label]]) * exp(model_aba$stats[stratum_label, "var_res"] / 2)
        }
      }
      if (model_aba$stats[stratum_label, "transform"] == "none" & add_error == TRUE)
        warning("Error sampling not implemented in the none transformation case")
      #
      # set prediction outside of strata to NA
      # use temporary variables because "!=" operator not available
      # for categorical variables
      # dummy <- metrics_map[[stratum]] == stratum_label
      r[[stratum_label]][!(metrics_map[[stratum]] == stratum_label)] <- NA
    }
    # convert list to spatial raster collection
    r <- terra::sprc(r)
    # merge strata results
    r <- terra::merge(r)
    # convert
    if(pkg != "terra")
    {
      r <- convert_raster(r, pkg = pkg)
    }
    return(r)
  }

#-------------------------------------------------------------------------------
#' Applies thresholds and mask to a raster object
#'
#' Applies a lower and upper thresholds to the values of the input raster. If the
#'  mask input is provided, first all NA values in the raster are set to 0, then
#'   the raster in multiplied by the mask. Cells to be masked should therefore
#'   have a NA value in the mask raster object.
#'
#' @param r raster object. RasterLayer and SpatRaster are supported.
#' @param minmax vector of two numeric values. minimum and maximum thresholds to
#' apply to `r` values
#' @param mask raster object. mask to be applied (multiplication with input raster
#'  `r`)
#' @examples
#' # load data
#' data(quatre_montagnes)
#' # build model
#' model_aba <- aba_build_model(quatre_montagnes$G_m2_ha, quatre_montagnes[, 9:76],
#'   transform = "boxcox"
#' )
#' # build example raster to apply model
#' quatre_montagnes$X <- rep(1:8, 12)
#' quatre_montagnes$Y <- rep(1:12, each = 8)
#' metrics_map <- terra::rast(quatre_montagnes[, c(2, 3, 9:76)], type = "xyz")
#' predict_map <- aba_predict(model_aba, metrics_map)
#' # create raster mask
#' mask <- predict_map
#' # set values to 1 or NA
#' terra::values(mask) <- rep(c(1, 1, NA), each = 32)
#' # apply thresholds and mask
#' predict_map_clean <- clean_raster(predict_map, c(40, 70), mask)
#'
#' # plot maps
#' terra::plot(predict_map, main = "Predictions")
#' terra::plot(mask, main = "Mask", legend = FALSE)
#' terra::plot(predict_map_clean, main = "Cleaned predictions")
#' @return a raster object
#' @export
#'
clean_raster <-
  function(r,
           minmax = c(-Inf,+Inf),
           mask = NULL) {
    # if mask is present
    if (!is.null(mask)) {
      # fill NA values in rast with 0
      r[is.na(r)] <- 0
      # then apply mask
      r <- r * mask
    }
    #
    r[r <= minmax[1]] <- minmax[1]
    r[r >= minmax[2]] <- minmax[2]
    #
    return(r)
  }

#-------------------------------------------------------------------------------
#' computes inference from area-based model and predicted values
#' @param aba_model a model returned by \code{\link{aba_build_model}} or
#' \code{\link{aba_combine_strata}}
#' @param r_predictions raster of predicted values
#' @param type string vector specifying which estimators should be computed
#' (one or several in "SRS", "ED", "D", "STR", "SYNT")
#' @param r_mask raster to mask region of interest (NA values), may contain
#' post-stratification categories (should be integer, positive values)
#' @return a data frame with estimation of parameter value and standard deviation
#'  of estimation for all required estimators.
#' @export
#'
aba_inference <- function(aba_model,
                          r_predictions,
                          type = c("SRS", "ED", "D", "STR", "SYNT"),
                          r_mask = NULL) {
  inference <- list()
  # extract observations
  observations <- aba_model$values
  # extract predicted values
  # apply mask if present
  if (!is.null(r_mask)) {
    pixels <- r_predictions * (r_mask >= 0)
    # apply mask also to observations
    coord <- aba_model$values[, c("x", "y")]
    observations$mask <- terra::extract(r_mask, coord)
    observations <- observations[!is.na(observations$mask),]
  } else {
    pixels <- r_predictions
  }
  # raster pixels may contain na values
  #
  # number of observations
  n <- nrow(observations)
  # number of predictions (no NA values in raster)
  N <- sum(!is.na(terra::values(pixels)))
  # number of parameters in the model
  # TAKE MAX NUMBER OF PARAMETERS IN CASE OF STRATIFIED MODELS
  n_para <-
    length(strsplit(as.character(aba_model$stats$formula[1]), "+",
                    fixed = TRUE)[[1]]) + 1
  #
  # Simple random sampling (SRS)
  if (is.element("SRS", type)) {
    inference[["SRS"]] <- data.frame(
      mean = mean(observations$field),
      var = stats::var(observations$field)
    )
  }
  #
  # Generalized difference estimator
  if (is.element("ED", type)) {
    dummy_prediction_bias <-
      mean(observations$predicted - observations$field)
    dummy_mean <-
      mean(terra::values(pixels), na.rm = TRUE) - dummy_prediction_bias
    dummy_var <-
      sum(((observations$predicted - observations$field) -
             dummy_prediction_bias
      ) ^ 2) / (n - n_para) # /n
    inference[["ED"]] <-
      data.frame(mean = dummy_mean, var = dummy_var)
    # use n number of observations in ROI or in calibration dataset
  }
  #
  # Model-assisted difference estimator (D)
  # caution : no true correspondance between the spatial location of observations
  # (calibration step) and predictions (mapping step)
  # here pixels containing an observation are replaced by the observation, but
  #  bias is still computed by substracting the observation and the prediction
  #  at the exact location
  if (is.element("D", type)) {
    observed.pixels <-
      terra::cellFromXY(pixels, observations[, c("x", "y")])
    dummy_prediction_bias <-
      mean(observations$predicted - observations$field)
    dummy_var <-
      sum(((observations$predicted - observations$field) -
             dummy_prediction_bias
      ) ^ 2) / (n - n_para) # /n (erreur)
    inference[["D"]] <- data.frame(
      mean = (
        sum(observations$field) +
          sum(terra::values(pixels)[-observed.pixels], na.rm = TRUE)
      ) /
        N - mean(observations$predicted - observations$field),
      var = dummy_var
    )
  }
  #
  # Stratified estimator (STR)
  if (is.element("STR", type) & !is.null(r_mask)) {
    # check that there are pixels and observations in all categories
    if (setequal(stats::na.omit(unique(terra::values(r_mask))),
                 stats::na.omit(unique(observations$mask)))) {
      dummy <- list()
      # compute weights of each category
      surface <- table(terra::values(r_mask))
      surface <- surface / sum(surface)
      # for each category
      for (i in (names(surface)))
      {
        # selec observations
        temp_selec <- which(observations$mask == as.numeric(i))
        # compute mean and variance in category
        dummy[[i]] <- data.frame(
          W = surface[i],
          m = mean(observations$field[temp_selec]),
          n = length(temp_selec),
          var = stats::var(observations$field[temp_selec])
        )
      }
      dummy <- do.call(rbind, dummy)
      inference[["STR"]] <- data.frame(mean = sum(dummy$W * dummy$m),
                                       var = sum((dummy$var * dummy$W) ^ 2 / dummy$n))
    } else {
      warning(
        "Impossible to compute STR inference: categories not present in both observations and pixels"
      )
    }
  }
  # synthetic regression estimator (SYNT)
  if (is.element("SYNT", type)) {
    inference[["SYNT"]] <- data.frame(mean = mean(terra::values(pixels),
                                                  na.rm = TRUE),
                                      var = NA)
  }
  # convert list to data.frame
  inference <- do.call(rbind, inference)
  # compute sd
  inference$sd <- sqrt(inference$var)
  return(inference)
}
