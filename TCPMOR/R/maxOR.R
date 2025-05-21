#' Generate simulation data
#'
#' @param n The total number of observations to generate
#' @param seed Randomized seeds for ensuring reproducible results
#'
#' @return A data frame containing age and a binary outcome variable
#' @export
#'
#' @examples
#' dataC <- createData(200)
createData <- function(n, seed = 123589) {
  set.seed(seed)
  if (n %% 2 != 0) {
    stop("n must be divisible by 2 to split evenly between case and control")
  }
  half_n <- n / 2
  case <- stats::rnorm(half_n, 49.5, 8)
  control <- stats::rnorm(half_n, 50, 10)
  age <- c(case, control)
  y <- rep(c(1, 0), c(length(case), length(control)))
  dataC <- data.frame(age = age, y = y)
  return(dataC)
}


#' Fit the data using a semi-parametric model to explore
#' the nonlinear dose-response relationship between the
#' independent variable and lnOR
#'
#' @param dataC Data frame containing age and binary outcome variables
#'
#' @return Fitted semi-parametric model object
#' @export
#'
#' @examples
#' # Generate simulated data dataC
#' dataC <- createData(200)
#'
#' # Plot the nonlinear dose-response relationship between the independent variable and lnOR
#' spm.fit <- fitSemiParamModel(dataC)
#' plot(spm.fit,ylab = "lnOR",xlab = "age",shade = FALSE)
#' summary(spm.fit)
fitSemiParamModel <- function(dataC) {
  spm.fit <- SemiPar::spm(dataC$y ~ f(dataC$age), omit.missing = TRUE, family = "binomial")
  return(spm.fit)
}


#' Finding the two cut-off points
#'
#' @param spm.fit Fitted semi-parametric model object
#' @param dataC Data frame containing age and binary outcome variables
#'
#' @return Data frame containing age, fitted lnOR, OR, se, sp, sse, and ssp
#' @export
#'
#' @examples
#' # Generate simulated data dataC
#' dataC <- createData(200)
#'
#' # Fit the semi-parametric model
#' spm.fit <- fitSemiParamModel(dataC)
#'
#' # Find two cut-off points
#' dataC <- findCutoffs(spm.fit, dataC)
findCutoffs <- function(spm.fit, dataC) {
  dataC$y0 <- stats::fitted(spm.fit)[, 2]
  OR <- se <- sp <- sse <- ssp <- rep(0, length(dataC$y0))
  dataC <- dataC[order(dataC$y0), ]

  for (i in 1:length(dataC$y0)) {
    dataC$exposure[dataC$y0 >= dataC$y0[i]] <- 1
    dataC$exposure[dataC$y0 < dataC$y0[i]] <- 0

    tableVal <- table(dataC$exposure, dataC$y)
    if (any(min(tableVal) == 0, ncol(tableVal) < 2, nrow(tableVal) < 2) == TRUE) {
      OR[i] <- 0
    } else {
      OR[i] <- round(tableVal[1, 1] * tableVal[2, 2] / (tableVal[1, 2] * tableVal[2, 1]), 2)
      sp[i] <- round(tableVal[1, 1] / sum(tableVal[, 1]), 5)
      se[i] <- round(tableVal[2, 2] / sum(tableVal[, 2]), 5)
      ssp[i] <- round(sqrt(tableVal[1, 1] * tableVal[2, 1] / (tableVal[1, 1] + tableVal[2, 1]) ^ 3), 5)
      sse[i] <- round(sqrt(tableVal[2, 2] * tableVal[1, 2] / (tableVal[1, 2] + tableVal[2, 2]) ^ 3), 5)
    }
  }

  dataC$OR <- OR
  dataC$se <- se
  dataC$sp <- sp
  dataC$sse <- sse
  dataC$ssp <- ssp
  return(dataC)
}


#' Calculate data filtering results and two cutoffs for given sensitivity and specificity threshold
#'
#' @param dataC Data frame containing columns: se, sp, age, y0, OR, y
#' @param seThreshold Sensitivity threshold
#' @param spThreshold Specificity threshold
#'
#' @return A list with two elements: filteredData(the filtered dataset) and cutoffs(the calculated two cutoffs)
#' @export
#'
#' @examples
#' # Generate simulated data dataC
#' dataC <- createData(200)
#'
#' # Fit the semi-parametric model
#' spm.fit <- fitSemiParamModel(dataC)
#'
#' # Find two cut-off points
#' dataC <- findCutoffs(spm.fit, dataC)
#'
#' # Output the two cut-off points after limiting sensitivity se and specificity sp
#' result <- calculateCutoffs(dataC)
#' cutoffs <- result$cutoffs
#' dataC2 <- result$filteredData
#' print(cutoffs)
calculateCutoffs <- function(dataC, seThreshold = 0.1, spThreshold = 0.1) {
  if (!("se" %in% names(dataC)) || !("sp" %in% names(dataC))) {
    stop("dataC must contain 'se' and 'sp' columns.")
  }
  filteredData <- subset(dataC, dataC$se > seThreshold & dataC$sp > spThreshold)
  cut.max <- max(filteredData$age[filteredData$y0 > filteredData$y0[order(filteredData$OR, decreasing = TRUE)[1]]])
  cut.min <- min(filteredData$age[filteredData$y0 > filteredData$y0[order(filteredData$OR, decreasing = TRUE)[1]]])
  return(list(filteredData = filteredData, cutoffs = c(cut.min, cut.max)))
}


#' Discretize the age variable according to the two cut-off points
#'
#' @param data Data frame with column: age
#' @param cutoffs The cut-off points of the age range
#'
#' @return A modified data frame with a new column: age_p
#' @export
#'
#' @examples
#' # Generate simulated data dataC
#' dataC <- createData(200)
#'
#' # Fit the semi-parametric model
#' spm.fit <- fitSemiParamModel(dataC)
#'
#' # Find two cut-off points
#' dataC <- findCutoffs(spm.fit, dataC)
#'
#' # Calculate the two cut-off points after limiting sensitivity se and specificity sp
#' result <- calculateCutoffs(dataC)
#' cutoffs <- result$cutoffs
#' dataC2 <- result$filteredData
#'
#' # Discretize age variable based on the two cutoffs
#' dataC2 <- discretizeAge(dataC2, cutoffs)
discretizeAge <- function(data, cutoffs) {
  data$age_p <- NA
  data$age_p[data$age >= cutoffs[1] & data$age <= cutoffs[2]] <- 1
  data$age_p[data$age < cutoffs[1] | data$age > cutoffs[2]] <- 0
  return(data)
}


#' Fit a logistic regression model and return the OR and 95% confidence interval
#'
#' @param data A data frame with columns: y, age_p
#'
#' @return A matrix of OR and 95% confidence intervals
#' @export
#'
#' @examples
#' # Generate simulated data dataC
#' dataC <- createData(200)
#'
#' # Fit the semi-parametric model
#' spm.fit <- fitSemiParamModel(dataC)
#'
#' # Find two cut-off points
#' dataC <- findCutoffs(spm.fit, dataC)
#'
#' # Calculate the two cut-off points after limiting sensitivity se and specificity sp
#' result <- calculateCutoffs(dataC)
#' cutoffs <- result$cutoffs
#' dataC2 <- result$filteredData
#'
#' # Discretize age variable based on the two cutoffs
#' dataC2 <- discretizeAge(dataC2, cutoffs)
#'
#' # Fitting logistic regression models and obtaining OR values and 95% confidence intervals
#' OR_Results <- fitLogisticRegression(dataC2)
#' print(round(OR_Results, 3))
fitLogisticRegression <- function(data) {
  model <- stats::glm(y ~ age_p, family = stats::binomial(link = "logit"), data = data)
  OR_Results <- exp(cbind(OR = stats::coef(model), stats::confint.default(model)))
  return(OR_Results)
}
