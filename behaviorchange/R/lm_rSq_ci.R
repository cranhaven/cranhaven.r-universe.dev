#' Obtaining an R squared confidence interval estimate for an lm regression
#'
#' The \code{lm_rSq_ci} function uses the base R `lm` function to conduct
#' a regression analysis and then computes the confidence interval for R squared.
#'
#'
#' @param formula The formula of the regression analysis, of the form \code{y ~
#' x1 + x2}, where y is the dependent variable and x1 and x2 are the
#' predictors.
#' @param data If the terms in the formula aren't vectors but variable names,
#' this should be the dataframe where those variables are stored.
#' @param conf.level The confidence of the confidence interval around the
#' regression coefficients.
#' @param ci.method Which method to use for the confidence
#' interval around R squared.
#' @param env The enviroment where to evaluate the formula.
#' @return The confidence interval
#'
#' @author Gjalt-Jorn Peters
#'
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@a-bc.eu>
#'
#' @keywords utilities
#' @examples
#'
#' ### Do a simple regression analysis
#' lm_rSq_ci(age ~ circumference, dat=Orange);
#'
#' @export lm_rSq_ci
lm_rSq_ci <- function(formula, data=NULL, conf.level=.95,
                      ci.method = c("widest", "r.con", "olkinfinn"),
                      env=parent.frame()) {

  dat <- data;

  ### Extract variables from formula
  variableNames <- all.vars(formula);

  ### Convert formula to a character string
  formula.as.character <-
    paste0(as.character(formula)[c(2, 1, 3)], collapse=" ");

  if (is.null(dat)) {
    ### Extract variables from formula
    variables <-
      as.character(as.list(attr(stats::terms(formula), 'variables'))[-1]);

    ### Store variablesnames only for naming in raw dataframe
    variables_namesOnly <- unlist(
      lapply(strsplit(variables, "\\$"), utils::tail, 1));

    ### Store variables in a dataframe
    dat.raw <- list();
    for (varToGet in 1:length(variables)) {
      dat.raw[[variables_namesOnly[varToGet]]] <-
        eval(parse(text=variables[varToGet]), envir=env);
    }
    ### Convert the list to a dataframe
    dat.raw <- data.frame(dat.raw);

    ### Convert variable names to bare variable names
    for (currentVariableIndex in 1:length(variables)) {
      formula.as.character <-
        gsub(variables[currentVariableIndex],
             variables_namesOnly[currentVariableIndex],
             formula.as.character, fixed=TRUE);
    }

  } else {
    ### Store variables in a dataframe
    dat.raw <- dat[, variableNames];
    variables_namesOnly <- variableNames;
  }

  formula <- formula(formula.as.character,
                                      env = environment());

  ### Run and store lm objects
  lm.raw <-
    stats::lm(formula=formula, data=dat.raw);

  ### R^2 confidence interval based on formula at
  ### http://www.danielsoper.com/statcalc3/calc.aspx?id=28
  rsq <- rsq <- summary(lm.raw)$r.squared;
  k <- k <- length(lm.raw$terms) - 1;
  n <- n <- lm.raw$df.residual + k + 1;
  rsq.se <- sqrt((4*rsq*(1-rsq)^2*(n-k-1)^2)/
                                    ((n^2-1)*(3+n)));

  rsq.t.crit <- stats::qt(p=1-(1-conf.level)/2, df=n-k-1);
  rsq.ci.olkinfinn <- c(rsq -
                                     rsq.t.crit *
                                     rsq.se,
                                   rsq +
                                     rsq.t.crit *
                                     rsq.se);

  rsq.ci.olkinfinn <- ifelse(rsq.ci.olkinfinn < 0,
                                        0, rsq.ci.olkinfinn);

  ### Confidence interval using conversion to r

  # rsq.ci.r.con <- psych::r.con(sqrt(rsq), n);
  # rsq.ci.r.con <- ifelse(rsq.ci.r.con < 0,
  #                                   0, rsq.ci.r.con) ^ 2;

  r <- sqrt(rsq);
  z <- stats::qnorm(1 - (1-conf.level)/2);
  se <- sqrt(1/((n - 3)));
  zr <- log((1 + r)/(1 - r))/2;
  LL0 <- zr - z*se;
  UL0 <- zr + z*se;
  LL <- (exp(2*LL0) - 1)/(exp(2*LL0) + 1);
  UL <- (exp(2*UL0) - 1)/(exp(2*UL0) + 1);
  rsq.ci.r.con <- c(LL^2, UL^2);

  ### End of CI using conversion to r

  if ("widest" %in% tolower(ci.method)) {
    rsq.ci <- ifelse(range(rsq.ci.r.con) >
                                  range(rsq.ci.olkinfinn),
                                rsq.ci.r.con,
                                rsq.ci.olkinfinn);
  } else if ("r.con" %in% tolower(ci.method)) {
    rsq.ci <- rsq.ci.r.con;
  } else {
    rsq.ci <- rsq.ci.olkinfinn;
  }

  rsq.ci <- sort(rsq.ci);

  if ((rsq.ci[1]) < 0) {
    rsq.ci[1] <- 0;
  }
  if ((rsq.ci[2]) > 1) {
    rsq.ci[2] <- 1;
  }

  return(rsq.ci);

}
