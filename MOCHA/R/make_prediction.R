#' @title \code{make_prediction}
#'
#' @description \code{make_prediction} is an R helper function, part of the single-cell peak calling
#' algorithm MOCHA by (Zaim, Pebworth, et. al. 2022) that determines which genomic regions, or bins,
#' will be used to determine accessibility. The function "make_prediction" applies the
#' logistic regression model predictions and then calls peaks that exceed a given
#' threshold.
#'
#'
#' @param X an intensityMatrix output from \code{calculate_intensities}
#' @param finalModel is a matrix with coefficients and an index indicating
#'        the number of cell used to train that model
#'
#' @return the original intensityMatrix with the two intensity parameters required
#' to calculate the probability of a (+) peak, with an additional two columns
#' that include the prediction probability
#'
#' @noRd
#'

make_prediction <- function(X, finalModelObject) {

  ### Model was trained on varying
  ### cell abundances. Identify
  ### Number of cells in the sample
  ### to apply appropriate model
  cell_model <- X$numCells[1]

  ### If the number of cells < 5
  ### We do not make predictions

  if (cell_model < 5) {
    return(NULL)
  } else {

    ### Apply model fit based on the
    ### cell abundance. First Part is a LOESS
    ### Fit, final part is a linear fit,
    ### Middle part is an average between
    ### Loess & Linear fits.

    if (cell_model <= 100000) {

      ## Loess Fit
      Intercept <- stats::predict(
        finalModelObject$Loess$Intercept,
        data.frame(NumCells = cell_model)
      )
      Total <- stats::predict(
        finalModelObject$Loess$Total,
        data.frame(NumCells = cell_model)
      )
      Max <- stats::predict(
        finalModelObject$Loess$Max,
        data.frame(NumCells = cell_model)
      )
      tmpModel <- c(Intercept, Total, Max)
      tmpModel <- as.matrix(tmpModel)
    } else if (cell_model > 100000 & cell_model <= 140000) {

      ## Average of Loess & Linear Fit
      Intercept <- mean(
        stats::predict(
          finalModelObject$Loess$Intercept,
          data.frame(NumCells = cell_model)
        ),
        stats::predict(
          finalModelObject$Linear$Intercept,
          data.frame(NumCells = cell_model)
        )
      )

      Total <- mean(
        stats::predict(
          finalModelObject$Loess$Total,
          data.frame(NumCells = cell_model)
        ),
        stats::predict(
          finalModelObject$Linear$Total,
          data.frame(NumCells = cell_model)
        )
      )
      Max <- mean(
        stats::predict(
          finalModelObject$Loess$Max,
          data.frame(NumCells = cell_model)
        ),
        stats::predict(
          finalModelObject$Linear$Max,
          data.frame(NumCells = cell_model)
        )
      )

      tmpModel <- c(Intercept, Total, Max)
      tmpModel <- as.matrix(tmpModel)
    } else {
      ## Linear Fit
      Intercept <- stats::predict(finalModelObject$Linear$Intercept, data.frame(NumCells = cell_model))
      Total <- stats::predict(finalModelObject$Linear$Total, data.frame(NumCells = cell_model))
      Max <- stats::predict(finalModelObject$Linear$Max, data.frame(NumCells = cell_model))
      tmpModel <- c(Intercept, Total, Max)
      tmpModel <- as.matrix(tmpModel)
    }
  }

  ### Create Design Matrix
  designX <- X[, c("TotalIntensity", "maxIntensity")]
  designX$Intercept <- 1
  designX <- designX[, c("Intercept", "TotalIntensity", "maxIntensity")]
  colnames(designX) <- c("Intercept", "Total", "Max")

  ### Apply Prediction Model to
  ### Obtain Probability of Accessibility
  z <- as.matrix(designX) %*% tmpModel
  preds <- 1 / (1 + exp(-z))

  ### Round Predictions &
  ### Create a Prediction Strength Feature
  X$Prediction <- round(preds, 4)
  X$PredictionStrength <- X$TotalIntensity

  ### Identify cutoff based on
  ### different abundances
  newdata <- data.frame(Ncells = cell_model)
  adaptiveThreshold <- as.numeric(stats::predict(MOCHA::youden_threshold, newdata = newdata))


  ### Boolean indicating whether
  ### probability of accessibility
  ### exceeded our threshold.
  X$peak <- X$Prediction > round(adaptiveThreshold, 3)

  return(X)
}
