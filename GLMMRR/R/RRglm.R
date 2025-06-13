
#' Fitting Generalized Linear Models with binary Randomized Response data
#'
#' Fit a generalized linear model (GLM) with binary Randomized Response data.
#' Implemented as a wrapper for \code{\link{glm}}. Reference: Fox, J-P, Veen, D. and Klotzke, K. (2018).
#' Generalized Linear Mixed Models for Randomized Responses. \emph{Methodology.} https://doi.org/10.1027/1614-2241/a000153
#'
#' @param formula
#' a two-sided linear formula object describing the model to be fitted,
#' with the response on the left of a ~ operator and the terms, separated by + operators, on the right.
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
#' from the environment from which RRglm is called.
#' @param na.action
#' a function that indicates what should happen when the data contain NAs.
#' The default action (\code{\link{na.omit}}, as given by \code{getOption("na.action"))})
#' strips any observations with any missing values in any variables.
#' @param ...
#' other potential arguments to be passed to \code{\link{glm}}.
#'
#' @return
#' An object of class RRglm. Extends the class \code{glm} with Randomize Response data.
#' @export
#' @seealso \code{\link{glm}}
#'
#' @examples
#' # Fit the model with fixed effects for gender, RR, pp and age using the logit link function.
#' # The Randomized Response parameters p1, p2 and model
#' # are specified for each observation in the dataset.
#' out <- RRglm(response ~ Gender + RR + pp + age, link="RRlink.logit", RRmodel=RRmodel,
#'          p1=RRp1, p2=RRp2, data=Plagiarism, etastart=rep(0.01, nrow(Plagiarism)))
#' summary(out)
RRglm <- function (formula, link, item, RRmodel, p1, p2, data, na.action = "na.omit", ...) {

  # Create model frame
  mf <- match.call(expand.dots = FALSE)
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
  varnames <- all.vars(formula)
  RRdata <- data.frame(mf[,varnames], "Item" = mf$'(item)', "RRmodel" = mf$'(RRmodel)', "c" = RRparameters$c, "d" = RRparameters$d, "p1" = mf$'(p1)', "p2" = mf$'(p2)')

  # Must be a data frame
  df <- as.data.frame(data)

  # Get a reference to the correct link function by name
  glmlink <- match.fun(link)

  # Create call object with given arguments
  cl <- call("glm", formula = formula, family = quote(RRbinomial(link = glmlink(RRdata$c, RRdata$d), c = RRdata$c, d = RRdata$d)), data = quote(df), na.action = na.action)

  # Make sure that additional arguments are passed
  m <- match.call(expand.dots = FALSE)
  dots <- m$...
  for (ii in 1:(length(dots))) {
    cl[names(dots[ii])] <- dots[ii]
  }

  # Evaluate the call and fit the model
  output <- eval(cl)

  # Add item identifier and RR parameters to the output object
  output$Item <- RRdata$Item
  output$RRlink <- link
  output$RRmodel <- RRdata$RRmodel
  output$RRp1 <- RRdata$p1
  output$RRp2 <- RRdata$p2
  output$RRc <- RRdata$c
  output$RRd <- RRdata$d

  # Return an object of class RRglm
  class(output) <- c("RRglm", "glm", "lm")
  return(output)

}
