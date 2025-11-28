

#' Analyze moderated mediation model using SEM
#'
#' @param data data frame
#' @param xvar predictor variable, must be either numerical or dichotomous
#' @param mvars vector of names of mediator variables
#' @param yvar dependent variable, must be numerical
#' @param xmmod moderator of effect predictor on mediators, must be either numerical or dichotomous
#' @param mymod moderator of effect mediators on dependent variable, must be either numerical or dichotomous
#' @param cmvars covariates for mediators
#' @param cyvars covariates for dependent variable
#' @param estMethod estimation of standard errors method, bootstrap is default
#' @param nboot number of bootstrap samples
#'
#' @return gemm object
#' @export gemm
#'
#' @examples
#' \dontrun{
#' data("cpbExample")
#'        res <- gemm(dat = cpbExample, xvar="procJustice", mvars= c("cynicism","trust"),
#'        yvar = "CPB", nboot=500)
#' print(res)
#' }
gemm <- function(data = NULL,
                 xvar,
                 mvars,
                 yvar,
                 xmmod = NULL,
                 mymod = NULL,
                 cmvars = NULL,
                 cyvars = NULL,
                 estMethod = "bootstrap",
                 nboot = 1000) {

  res <- list(input = as.list(environment()),
              intermediate = list(),
              output = list());

  res$intermediate$dataName <- as.character(deparse(substitute(data)));

  res$intermediate$numberOfMediators <-
    nm <- length(mvars);

  res$intermediate$numberOfCovariates <-
    ncm <- length(cmvars);

  ## check if predictor is dichotomous factor
  if (is.factor(data[,xvar])) {
      if (nlevels(data[,xvar]) == 2) {
        res$intermediate$predLevels <- levels(data[,xvar])
        data[,xvar] <- as.numeric(data[,xvar])
      } else {
        return(message("Predictor is a factor with more than two levels"))
      }
  }

  ## initialize xmint and myint
  xmint <- NULL
  myint <- NULL


  ## check if there is a moderator for the x - m path
  if (!is.null(xmmod)) {
    if (length(xmmod) > 1) {
      return(message("This function can only handle one moderator for the x-m path."));
    }
    if (is.factor(data[,xmmod])) {
      if (nlevels(data[,xmmod]) > 2) {
        return(message("This function can not yet deal with categorical moderators
                       with more than two levels."));
      } else {
        res$intermediate$xdichotomous <- xdichotomous <- TRUE;
        data[,"xmodOriginal"] <- data[,xmmod];
        data[,xmmod] <- as.numeric(data[,xmmod]) - 1;
      }
    } else {
      if (length(unique(data[,xmmod])) == 2) {
        res$intermediate$xdichotomous <-  xdichotomous <- TRUE
        levels(data[,xmmod]) <- unique(data[,xmmod])
        } else {
      res$intermediate$xdichotomous <- xdichotomous <- FALSE
        }
    }

    xmint <- paste0("xmInteraction",c(1:nm));
    xmterms <- paste0(paste0("data$",xmmod,"*","data$",mvars));

    for (i in 1:nm) {
      data[,xmint[i]] <- eval(parse(text = xmterms[i]));
    }

  }

  ### check if there is a moderator for the m - y path;
  if (!is.null(mymod)) {
    if (length(mymod) > 1) {
      return("This function can only handle one moderator for the m-y path.");
    }
    if (is.factor(data[,mymod])) {
      if (nlevels(data[,mymod]) > 2) {
        return("This function can not yet deal with categorical moderators with more than two levels.");
      } else {
        res$intermediate$ydichotomous <-  ydichotomous <- TRUE;
        data[,"ymodOriginal"] <- data[,mymod];
        data[,mymod] <- as.numeric(data[,mymod]) - 1;
      }
    } else {
      if (length(unique(data[,mymod])) == 2) {
        res$intermediate$ydichotomous <-  ydichotomous <- TRUE
        levels(data[,mymod]) <- unique(data[,mymod])
      } else {
      res$intermediate$ydichotomous <-  ydichotomous <- FALSE
      }
    }

    myint <- paste0("myInteraction",c(1:nm));
    myterms <- paste0(paste0("data$",mymod,"*","data$",mvars));

    for (i in 1:nm) {
      data[,myint[i]] <- eval(parse(text = myterms[i] ));
    }
  }

  res$intermediate$data <- data

  ### Build lavaan model
  res$intermediate$model <-
    buildModMedSemModel(xvar=xvar,
                        mvars= mvars,
                        yvar = yvar,
                        xmmod = xmmod,
                        mymod = mymod,
                        cmvars = cmvars,
                        cyvars = cyvars);

  ### Run SEM
  res$intermediate$result <- result <-
    lavaan::sem(res$intermediate$model,
        data=data,
        fixed.x = FALSE,
        std.lv = TRUE,
        se = estMethod,
        bootstrap=nboot);

  ### Extract R squared values
  res$output$Rsq <-
    lavaan::inspect(res$intermediate$result, "r2");

  ### Extract parameter estimates for a and b paths
  res$intermediate$parameterEstimates <- r1 <-
    lavaan::parameterestimates(result);
  res$output$parameterEstimates.apath <-
    r1[(r1[,"lhs"] %in% mvars & r1[,"rhs"] %in% c(xvar,xmmod,xmint) & r1[,"op"] != "~~" ),-c(1:3)]
  res$output$parameterEstimates.bpath <-
    r1[(r1[,"lhs"] %in% yvar & r1[,"rhs"] %in% c(mvars,mymod,myint)),-c(1:3)]


  ### Extract parameter estimates for direct effects
  res$output$parameterEstimates.direct <-
      r1[(r1[,"lhs"] %in% yvar & r1[,"rhs"] %in% xvar),-c(1:3)]

  ### ... And for indirect effects
  a2 <- 1:nm;
  ind <- paste0("ind", a2 );
  indinter <- paste0("indinter", a2 );
  res$output$parameterEstimates.indirect.raw <-
      r1[(r1[,"lhs"] %in% c(ind,indinter, "tot")),-c(1:3)]

  ### ... And for the covariates
  covlabels <- c(paste0("d", rep(seq(mvars), ncm) ,rep(seq(cmvars), each=nm)), paste0("f",seq(cyvars)))
  res$output$parameterEstimates.covs <-
  r1[(r1[,"rhs"] %in% c(cmvars,cyvars)) & (r1[,"label"] %in% covlabels),-c(1:3)]


  ### ... And for the ratio ES
    ratio <- c(paste0("ratio", a2 ), "ratio_tot");
    es1 <-  r1[(r1[,"lhs"] %in% ratio) ,-c(1:4)]
    res$output$parameterEstimates.indirect.es_rat <- es1

 ### ... And for the completely standardized ES
    r2 <- lavaan::standardizedSolution(result)
    es2 <- r2[(r2[,"lhs"] %in% c(ind, "tot")),-c(1:3)]
    res$output$parameterEstimates.indirect.es_std <- es2


   res$output$parameterEstimates.total  <- stats::lm(data[,yvar] ~ data[,xvar], data = data)


  class(res) <- "gemm";

  return(res);

}









