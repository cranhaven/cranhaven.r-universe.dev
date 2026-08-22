########################################

# LAPOP Regression Coefficients Graph Pre-Processing

########################################

#' LAPOP Regression Coefficients Graph Pre-Processing
#'
#' This function creates a data frame which can then be input in lapop_coef() for
#' plotting regression coefficients graph using LAPOP formatting.
#'
#' @param data Survey design data from lpr_data() output.
#' @param outcome Dependent variable for the svyglm regression model. (e.g., "outcome_name"). Only one variable allowed.
#' @param xvar Vector of independent variables for the svyglm regression model (e.g., "xvar1+xvar2+xvar3" and so on). Multiple variables are allowed.
#' @param interact Interaction terms (e.g., "xvar1`*`xvar2 + xvar3`:`xvar4"). Supports `:` and `*` operators for interacting variables. Optional, default is NULL.
#' @param model Model family object for glm. Default is gaussian regression (i.e., "linear"). For a logit model, use model="binomial"
#' @param estimate Character. Graph either the coefficients (i.e., `coef`) or the change in probabilities (i.e., `contrast`). Default is "coef."
#' @param vlabs Character. Rename variable labels to be displayed in the graph produced by lapop_coef(). For instance, vlabs=c("old_varname" = "new_varname").
#' @param omit Character. Do not display coefficients for these independent variables. Default is to display all variables included in the model. To omit any variables you need to include the raw "varname" in the omit argument.
#' @param filesave Character. Path and file name with csv extension to save the dataframe output.
#' @param replace Logical. Replace the dataset output if it already exists. Default is FALSE.
#' @param level Numeric. Set confidence level in numeric values; default is 95 percent.
#'
#' @return Returns a data frame, with data formatted for visualization by lapop_coef
#'
#' @examples
#'
#' require(lapop); data(bra23)
#'
#' # Set Survey Context
#' bra23lpr <- lpr_data(bra23, wt = TRUE)
#'
#' # Example 1: Linear model
#' lpr_coef(data = bra23lpr,
#'  outcome = "ing4",
#'  xvar = "wealth+idio2",
#'  model = "linear",
#'  estimate = "coef")
#'
#' # Example 2: Logit model with contrasts
#' lpr_coef(data = bra23lpr,
#'  outcome = "fs2",
#'  xvar = "wealth+idio2",
#'  model = "binomial",
#'  estimate = "contrast")
#'
#' # Example 3: Interactive linear model
#' lpr_coef(data = bra23lpr,
#'  outcome = "ing4",
#'  xvar = "wealth+idio2",
#'  interact = "wealth*idio2",
#'  model = "linear",
#'  estimate = "coef")
#'
#' # Example 4: Interactive logit model
#' lpr_coef(data = bra23lpr,
#'          outcome = "fs2",
#'          xvar = "wealth+idio2",
#'          interact = "wealth*idio2",
#'          model = "binomial",
#'          estimate = "contrast")
#'
#'@export
#'@import dplyr
#'@import srvyr
#'@import marginaleffects
#'@importFrom stats Gamma gaussian poisson quasibinomial quasipoisson
#'
#'@author Robert Vidigal, \email{robert.vidigal@@vanderbilt.edu}

lpr_coef <- function(
    outcome  = NULL,
    xvar     = NULL,
    interact = NULL,
    model    = "linear",
    data     = NULL,
    estimate = c("coef"),
    vlabs    = NULL,
    omit     = NULL,
    filesave = NULL,
    replace  = FALSE,
    level    = 95
) {
  # Initialize an empty data frame
  coef_data <- data.frame()

  # Mapper for model family
  model <- model
  if (is.character(model)) {
    model <- switch(tolower(model),
                  "linear"        = gaussian(),
                  "gaussian"      = gaussian(),
                  "identity"      = gaussian(),
                  "binomial"      = quasibinomial(), # keep strict binomial (warns on non-integer)
                  "logit"         = quasibinomial(),
                  "logistic"      = quasibinomial(),
                  "probit"        = quasibinomial(link = "probit"),
                  "poisson"       = poisson(),
                  "quasipoisson"  = quasipoisson(),
                  "gamma"         = Gamma(link = "log"),
                  stop("Unknown model to git: ", model)
    )
  }

  # Basic checks
  if (is.null(outcome) || is.null(xvar) || is.null(model) || is.null(data)) {
    stop("Error: One or more required inputs (outcome, xvar, model, data) are NULL.")
  }

  # Process interaction terms
  if (!is.null(interact)) {
    # Normalize interaction syntax: convert * to : to avoid redundancy
    interact_clean <- gsub("\\*", ":", interact)
    interact_str   <- paste(interact_clean, collapse = "+")
    full_formula   <- paste(outcome, "~", paste(xvar, interact_str, sep = "+"))
  } else {
    full_formula   <- paste(outcome, "~", xvar)
  }

  # Fit model
  formula <- as.formula(full_formula)
  svyglm_object <- tryCatch({
    survey::svyglm(formula, design = data, family = model)
  }, error = function(e) {
    stop("Error in svyglm: ", e$message)
  })

  # Estimate type validation
  if (estimate == "") {
    stop("You need to define the type of estimate: coef or contrast")
  }

  if (estimate == "contrast") {
    # Get predictors from model
    predictors <- attr(svyglm_object$terms, "term.labels")
    if (length(predictors) == 0) {
      stop("No valid predictor variables found in the model.")
    }

    # Loop over predictors to compute marginal effects
    for (Term in predictors) {
      dataset <- svyglm_object$data
      if (!Term %in% colnames(dataset)) {
        next # Skip if term isn't in the data directly (e.g., interaction)
      }

      comparison_type <- if (is.factor(dataset[[Term]])) "reference" else "minmax"

      term_data <- tryCatch({
        term_result <- suppressWarnings(
          as.data.frame(
            avg_comparisons(
              svyglm_object,
              variables = setNames(list(comparison_type), Term)
            )
          )
        )

        term_result <- term_result %>%
          rename(
            varlabel = contrast,
            coef     = estimate,
            ub       = conf.high,
            lb       = conf.low
          ) %>%
          mutate(
            varterm   = Term,
            varlabel  = paste(Term, varlabel, sep = ": "),
            pvalue    = round(p.value, 4),
            proplabel = round(coef, 2)
          )

        term_result
      }, error = function(e) {
        data.frame(
          varterm   = NA,
          varlabel  = NA,
          coef      = NA,
          lb        = NA,
          ub        = NA,
          pvalue    = NA,
          proplabel = NA
        )
      })

      coef_data <- bind_rows(coef_data, term_data)
    }

    # Clean and relabel
    if (!is.null(omit)) {
      coef_data <- coef_data %>% filter(!varterm %in% omit)
    }
    if (!is.null(vlabs)) {
      coef_data <- coef_data %>%
        mutate(varlabel = ifelse(varterm %in% names(vlabs), vlabs[varterm], varlabel))
    }

    coef_data <- coef_data %>%
      select(varlabel, coef, lb, ub, pvalue, proplabel)

  } else {
    # Coefficient-based summary
    level <- level / 100
    coef_raw <- summary(svyglm_object)$coefficients

    if (nrow(coef_raw) == 0) {
      stop("No coefficients returned from model. Check for convergence issues or empty formula.")
    }

    coef_data <- as.data.frame(coef_raw) %>%
      mutate(Term = rownames(coef_raw)) %>%
      select(Term, everything()) %>%
      mutate(
        lb         = Estimate - qt(1 - (1 - level) / 2, df = svyglm_object$df.residual) * `Std. Error`,
        ub         = Estimate + qt(1 - (1 - level) / 2, df = svyglm_object$df.residual) * `Std. Error`,
        `Pr(>|t|)` = as.numeric(format(`Pr(>|t|)`, scientific = FALSE, digits = 3)),
        pvalue     = round(`Pr(>|t|)`, 3),
        proplabel  = round(Estimate, 3)
      )

    if (!is.null(omit)) {
      coef_data <- coef_data %>% filter(!Term %in% omit)
    }

    coef_data <- coef_data %>%
      rename(varlabel = Term, coef = Estimate) %>%
      select(varlabel, coef, lb, ub, pvalue, proplabel)

    if (!is.null(vlabs)) {
      coef_data <- coef_data %>%
        mutate(varlabel = ifelse(varlabel %in% names(vlabs), vlabs[varlabel], varlabel))
    }
  }

  # File output
  if (!is.null(filesave)) {
    if (dir.exists(filesave)) {
      stop("You provided a directory for 'filesave'. Please provide a valid file path, including the file name.")
    }
    if (file.exists(filesave) && !replace) {
      stop("File already exists. Use replace = TRUE to overwrite.")
    } else {
      write.csv(coef_data, file = filesave, row.names = FALSE)
      message("File saved to ", filesave)
    }
  } else {
    message("CSV output file not created.")
  }

  return(coef_data)
}
