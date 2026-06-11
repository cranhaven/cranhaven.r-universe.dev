#' Extract Mean Squared Error (MSE) from Fitted Regression Model
#' 
#' The MSE, defined as the sum of the squared residuals divided by \code{n-p} 
#' (\code{n} = number of observations, \code{p} = number of regression 
#' coefficients), is an unbiased estimator for the error variance in a linear 
#' regression model. This is a convenience function that extracts the MSE from 
#' a fitted \code{\link{lm}} or \code{\link{glm}} object. The code is 
#' \code{rev(anova(model.fit)$"Mean Sq")[1]} if \code{model.fit} is a 
#' \code{\link{lm}} object and 
#' \code{sum(model.fit$residuals^2) / model.fit$df.residual} if \code{model.fit} 
#' is a \code{\link{glm}} object.
#' 
#' @param model.fit Fitted regression model returned from 
#' \code{\link[stats]{lm}} or \code{\link[stats]{glm}}.
#' @param var.estimate If \code{TRUE}, function returns a variance estimate for 
#' the error variance, defined as \code{2 * MSE^2 / (n - p)}.
#' 
#' @return If \code{var.estimate = FALSE}, numeric value indicating the MSE; if 
#' \code{var.estimate = TRUE}, named numeric vector indicating both the MSE and 
#' a variance estimate for the error variance.
#' 
#' @examples
#' # Generate 100 values: Y = 0.5 + 1.25 X + e, e ~ N(0, 1)
#' set.seed(123)
#' x <- rnorm(100)
#' y <- 0.5 + 1.25 * x + rnorm(100, sd = 1)
#' 
#' # Fit regression model using lm and using glm
#' lm.fit <- lm(y ~ x)
#' glm.fit <- glm(y ~ x)
#' 
#' # Extract MSE from lm.fit and glm.fit
#' get_mse(lm.fit)
#' get_mse(glm.fit)
#' 
#' @export
get_mse <- function(model.fit, var.estimate = FALSE) {
  
  # Extract MSE from model.fit
  class_model.fit <- class(model.fit)[1]
  if (class_model.fit == "lm") {
    mse <- rev(anova(model.fit)$"Mean Sq")[1]
  } else if (class_model.fit == "glm") {
    mse <- sum(model.fit$residuals^2) / model.fit$df.residual
  }
  
  # Get variance estimate for error variance if requested
  if (var.estimate) {
    mse.var <- 2 * mse^2 / model.fit$df.residual
    mse <- c(mse.hat = mse, mse.var = mse.var)
  }
  
  # Return mse variable
  return(mse)
  
}