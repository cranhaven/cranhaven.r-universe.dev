#' @import lme4 methods
#' @importFrom stats dnorm na.omit pchisq pnorm qnorm quantile resid summary.glm binomial predict
#' residuals residuals.glm setNames weighted.mean fitted
#' @importFrom grDevices dev.flush dev.hold devAskNewPage
#' @importFrom utils capture.output stack
#' @importFrom RColorBrewer brewer.pal


# Extending glmerMod to contain RR parameters for plot and summary functions
RRglmerMod <- setClass(
  # Set the name for the class
  "RRglmerMod",

  # Store the RR parameters in a list
  slots = c(RRparam = "list"),

  # Set the inheritance for this class
  contains = "glmerMod"
)

#' Fitting Generalized Linear Mixed-Effects Models with binary Randomized Response data
#'
#' Fit a generalized linear mixed-effects model (GLMM) with binary Randomized Response data.
#' Both fixed effects and random effects are specified via the model formula.
#' Randomize response parameters can be entered either as single values or as vectors.
#' Implemented as a wrapper for \code{\link{glmer}}. Reference: Fox, J-P, Veen, D. and Klotzke, K. (2018).
#' Generalized Linear Mixed Models for Randomized Responses. \emph{Methodology.} https://doi.org/10.1027/1614-2241/a000153
#'
#' @param formula
#' a two-sided linear formula object describing both the fixed-effects and fixed-effects part of the model,
#' with the response on the left of a ~ operator and the terms, separated by + operators, on the right.
#' Random-effects terms are distinguished by vertical bars ("|") separating expressions for design matrices from grouping factors.
#' @param link
#' a glm link function for binary outcomes. Must be a function name.
#' Available options: "RRlink.logit", "RRlink.probit", "RRlink.cloglog" and "RRlink.cauchit"
#' @param item
#' optional item identifier for long-format data.
#' @param RRmodel
#' the Randomized Response model, defined per case.
#' Available options: "DQ", "Warner", "Forced", "UQM", "Crosswise", "Triangular" and "Kuk"
#' @param p1
#' the Randomized Response parameter p1, defined per case. Must be 0 <= p1 <= 1.
#' @param p2
#' the Randomized Response parameter p2, defined per case. Must be 0 <= p2 <= 1.
#' @param data
#' a data frame containing the variables named in \code{\link{formula}} as well as the Randomized Response model and parameters.
#' If the required information cannot be found in the data frame, or if no data frame is given, then the variables are taken
#' from the environment from which RRglmer is called.
#' @param control
#' a list (of correct class, resulting from \code{\link{lmerControl}()} or \code{\link{glmerControl}()} respectively) containing control parameters,
#' including the nonlinear optimizer to be used and parameters to be passed through to the nonlinear optimizer,
#' see the \code{*lmerControl} documentation for details.
#' @param na.action
#' a function that indicates what should happen when the data contain NAs.
#' The default action (\code{\link{na.omit}}, as given by \code{getOption("na.action"))})
#' strips any observations with any missing values in any variables.
#' @param ...
#' other potential arguments to be passed to \code{\link{glmer}}.
#'
#' @return
#' An object of class RRglmerMod. Extends the class \code{glmerMod} with Randomize Response data,
#' for which many methods are available (e.g. \code{methods(class="glmerMod")}).
#' @export
#' @seealso \code{\link{lme4}}
#'
#' @examples
#' # Fit the model with fixed effects for gender, RR and pp
#' # and a random effect for age using the logit link function.
#' # The Randomized Response parameters p1, p2 and model
#' # are specified for each observation in the dataset.
#' out <- RRglmer(response ~ Gender + RR + pp + (1|age), link="RRlink.logit", RRmodel=RRmodel,
#'          p1=RRp1, p2=RRp2, data=Plagiarism, na.action = "na.omit",
#'          etastart = rep(0.01, nrow(Plagiarism)),
#'          control = glmerControl(optimizer = "Nelder_Mead", tolPwrss = 1e-03), nAGQ = 1)
#' summary(out)
RRglmer <- function (formula, item, link, RRmodel, p1, p2, data, control = glmerControl(), na.action = "na.omit", ...) {

  # Create model frame
  varnames <- all.vars(formula)
  mf <- match.call(expand.dots = FALSE)
  mf$formula <- paste(varnames[1], "~", paste(varnames[2:length(varnames)], collapse = "+"), sep = "") # model frame can't handle mixed-effect formulas
  print(mf)
  m <- match(c("formula", "data", "item", "RRmodel", "p1", "p2", "na.action"), names(mf), 0)
  mf <- mf[c(1, m)]
  mf$drop.unused.levels <- TRUE
  mf[[1]] <- quote(stats::model.frame)
  mf <- eval(mf, parent.frame())
  if(is.null(mf$'(item)'))
    mf$'(item)' <- rep("Single item", nrow(mf))
  if(is.null(mf$'(p2)'))
    mf$'(p2)' <- rep(NA, nrow(mf))

  # Translate p1, p2 to c, d for the chosen RR model
  RRparameters <- getRRparameters(mf$'(RRmodel)', mf$'(p1)', mf$'(p2)')

  # Create a dataset containing the variables of the formula and the the RR parameters
  RRdata <- data.frame(mf[,varnames], "Item" = mf$'(item)', "RRmodel" = mf$'(RRmodel)', "c" = RRparameters$c, "d" = RRparameters$d, "p1" = mf$'(p1)', "p2" = mf$'(p2)')

  # Must be a data frame
  df <- as.data.frame(data)

  # Get a reference to the correct link function by name
  glmlink <- match.fun(link)

  # Create call object with given arguments
  cl <- call("glmer", formula = formula, family = quote(RRbinomial(link = glmlink(RRdata$c, RRdata$d), c = RRdata$c, d = RRdata$d)), data = quote(df),
             control = control, na.action = na.action)

  # Make sure that additional arguments are passed
  m <- match.call(expand.dots = FALSE)
  dots <- m$...
  for (ii in 1:(length(dots))) {
    cl[names(dots[ii])] <- dots[ii]
  }

  # Evaluate the call and fit the model
  # Returns lme4::glmerMod object
  output <- eval(cl)

  # Put item identifier and RR parameters in a list
  RRparam <- list(Item = RRdata$Item, RRlink = link, RRmodel = RRdata$RRmodel, p1 = RRdata$p1, p2 = RRdata$p2, c = RRdata$c, d = RRdata$d)

  # Add a slot for the RR parameters
  outputRR <- as(output, "RRglmerMod")
  outputRR@RRparam <- RRparam

  # Return an object of class RRglmerMod
  return(outputRR)

}
