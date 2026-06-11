#' Create a Clean Summary Table from a glm Object
#'
#' Formats a \code{\link[stats]{glm}} object for printing to console or
#' inputting to \code{\link[knitr]{kable}}.
#'
#'
#' @param fit Object returned from \code{\link[stats]{glm}}.
#' @param columns Character vector specifying what columns to include. Choices
#' for each element are \code{"beta"}, \code{"se"}, \code{"betaci"},
#' \code{"beta_se"}, \code{"beta_ci"} \code{"or"}, \code{"orci"}, \code{"or_ci"},
#' \code{"hr"}, \code{"hrci"}, \code{"hr_ci")}, \code{"z"}, \code{"t"}, and
#' \code{"p"}.
#' @param expand_factors Logical value for whether to include two blank rows for
#' factor variables (name of variable and reference group).
#' @param variable_labels Character vector in case you want labels other than
#' the variable names.
#' @param prep_kable Logical value for whether to prepare for
#' printing via \code{\link[knitr]{kable}}. Right now, it just adds forward
#' slashes so factor levels are indented, which only applies if there are factor
#' variables and \code{expand_factors = TRUE}.
#' @param decimals Numeric value of vector specifying number of decimal places for
#' each column.
#' @param formatp_list Arguments to pass to \code{\link[tab]{formatp}}.
#'
#'
#' @return Data frame.
#'
#'
#' @examples
#' fit <- glm(mpg ~ wt + as.factor(cyl) + hp, data = mtcars)
#' clean_glm(fit)
#' fit %>% clean_glm(prep_kable = TRUE) %>% knitr::kable()
#'
#' @export
clean_glm <- function(
  fit,
  columns = NULL,
  expand_factors = TRUE,
  variable_labels = NULL,
  prep_kable = FALSE,
  decimals = 2,
  formatp_list = NULL
) {
  
  # Error checking
  if (! "glm" %in% class(fit)) {
    stop("The input 'fit' should be an object returned by 'glm'.")
  }
  if (! is.null(columns) & 
      ! all(columns %in% c("beta", "se", "betaci", "beta_se", "beta_ci", "or", 
                           "orci", "or_ci", "hr", "hrci", "hr_ci", "z", "t", 
                           "p"))) {
    stop("Each element of 'columns' has to be one of the following: 'beta', 'se', 'betaci', 'beta_se', 'beta_ci', 'or', 'orci', 'or_ci', 'hr', 'hrci', 'hr_ci', 'z', 't', 'p'.")
  }
  if (! is.logical(expand_factors)) {
    stop("The input 'expand_factors' should be TRUE if you want to include two blank rows for factor variables (name of variable and reference group) and FALSE otherwise.")
  }
  if (! is.logical(prep_kable)) {
    stop("The input 'prep_kable' should be TRUE if you plan to print via 'kable' and FALSE otherwise.")
  }
  
  # If unspecified, figure out reasonable defaults for columns
  if (is.null(columns)) {
    
    glm.family <- fit$family$family
    glm.link <- fit$family$link
    if (glm.family == "binomial" & glm.link == "logit") {
      columns <- c("beta_se", "or_ci", "p")
    } else if (glm.family == "poisson" & glm.link == "log") {
      columns <- c("beta_se", "hr_ci", "p")
    } else {
      columns <- c("beta_se", "betaci", "p")
    }
  }
  
  # Extract info from fit
  invisible(capture.output(summary.fit <- summary(fit)))
  coef.mat <- summary.fit$coefficients
  intercept <- attr(fit$terms, "intercept") == 1
  
  # Convert decimals to variable for sprintf
  spf <- paste("%0.", decimals, "f", sep = "")
  
  # Initialize data frame
  df <- dplyr::tibble(Variable = rownames(coef.mat))
  
  # Loop through and add columns requested
  for (column in columns) {
    if (column == "beta") {
      df$`Beta` <- sprintf(spf, fit$coef)
    } else if (column == "se") {
      df$`SE` <- sprintf(spf, coef.mat[, "Std. Error"])
    } else if (column == "betaci") {
      confint.fit <- confint(fit)
      df$`95% CI` <- paste("(", sprintf(spf, confint.fit[, 1]), ", ",
                           sprintf(spf, confint.fit[, 2]), ")", sep = "")
      if (intercept) df$`95% CI`[1] <- "-"
    } else if (column == "beta_se") {
      df$`Beta (SE)` <- paste(sprintf(spf, fit$coef), " (",
                              sprintf(spf, coef.mat[, "Std. Error"]), ")",
                              sep = "")
    } else if (column == "beta_ci") {
      confint.fit <- confint(fit)
      df$`Beta (95% CI)` <- paste(sprintf(spf, fit$coef), " (",
                                  sprintf(spf, confint.fit[, 1]), ", ",
                                  sprintf(spf, confint.fit[, 2]), ")", sep = "")
      if (intercept) df$`Beta (95% CI)`[1] <- "-"
    } else if (column == "or") {
      df$`OR` <- sprintf(spf, exp(fit$coef))
      if (intercept) df$`OR`[1] <- "-"
    } else if (column == "orci") {
      confint.fit <- confint(fit)
      df$`95% CI` <- paste("(", sprintf(spf, exp(confint.fit[, 1])), ", ",
                           sprintf(spf, exp(confint.fit[, 2])), ")", sep = "")
      if (intercept) df$`95% CI`[1] <- "-"
    } else if (column == "or_ci") {
      confint.fit <- confint(fit)
      df$`OR (95% CI)` <- paste(sprintf(spf, exp(fit$coef)), " (",
                                sprintf(spf, exp(confint.fit[, 1])), ", ",
                                sprintf(spf, exp(confint.fit[, 2])), ")", sep = "")
      if (intercept) df$`OR (95% CI)`[1] <- "-"
    } else if (column == "hr") {
      df$`HR` <- sprintf(spf, exp(fit$coef))
    } else if (column == "hrci") {
      confint.fit <- confint(fit)
      df$`95% CI` <- paste(sprintf(spf, exp(confint.fit[, 1])), ", ",
                           sprintf(spf, exp(confint.fit[, 2])), sep = "")
      if (intercept) df$`95% CI`[1] <- "-"
    } else if (column == "hr_ci") {
      confint.fit <- confint(fit)
      df$`HR (95% CI)` <- paste(sprintf(spf, exp(fit$coef)), " (",
                                sprintf(spf, exp(confint.fit[, 1])), ", ",
                                sprintf(spf, exp(confint.fit[, 2])), ")", sep = "")
      if (intercept) df$`HR (95% CI)`[1] <- "-"
    } else if (column == "z") {
      df$`z` <- sprintf(spf, coef.mat[, "Estimate"] / coef.mat[, "Std. Error"])
    } else if (column == "t") {
      df$`t` <- sprintf(spf, coef.mat[, "t value"])
    } else if (column == "p") {
      df$`P` <- do.call(formatp, c(list(p = coef.mat[, ncol(coef.mat)]),
                                   formatp_list))
    }
  }
  
  # Expand factors to show variable and reference group if requested
  variable.names <- colnames(fit$model)[-1]
  if (is.null(variable_labels)) {
    variable_labels <- variable.names
  }
  variable.classes <- sapply(fit$model, class)[-1]
  factor.levels <- fit$xlevels
  if (attr(fit$terms, "intercept") == 1) {
    row.labels <- "Intercept" 
  } else {
    row.labels <- c()
  }
  if (expand_factors) {
    for (ii in 1: length(variable.classes)) {
      if (variable.classes[ii] == "factor") {
        loc <- which(substr(df$Variable, 1, nchar(variable.names[ii])) == variable.names[ii])[1]
        df <- dplyr::add_row(df, .before = loc)
        df <- dplyr::add_row(df, .before = loc)
        df[loc, -1] <- ""
        df[loc + 1, -1] <- "-"
        row.labels <- c(row.labels, variable_labels[ii],
                        paste(ifelse(prep_kable, "\\ \\ ", "  "), factor.levels[[variable.names[ii]]], sep = ""))
      } else {
        row.labels <- c(row.labels, variable_labels[ii])
      }
    }
  } else {
    row.labels <- rownames(coef.mat)
  }
  
  # Remove as.factor's
  locs <- grep("as.factor(", row.labels, fixed = TRUE)
  if (length(locs) > 0) {
    row.labels[locs] <- sapply(row.labels[locs], function(x) substr(x, 11, nchar(x) - 1))
  }
  
  # Replace Variable with row.labels
  df$Variable <- row.labels
  
  # Return object
  return(df)
  
}