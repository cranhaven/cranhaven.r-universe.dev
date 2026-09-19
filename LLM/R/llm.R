#' Create Logit Leaf Model
#'
#' This function creates the logit leaf model. It takes a dataframe with numeric
#' values as input and a corresponding vector with dependent values.
#' Decision tree parameters threshold for pruning and number of observations per
#' leaf can be set.
#'
#' @param X Dataframe containing numerical independent variables.
#' @param Y Numerical vector of dependent variable. Currently only binary classification is supported.
#' @param threshold_pruning Set confidence threshold for pruning. Default 0.25.
#' @param nbr_obs_leaf The minimum number of observations in a leaf node. Default 100.
#' @return An object of class logitleafmodel, which is a list with the following components:
#' \item{Segment Rules}{The decision rules that define segments. Use \code{\link{table.llm.html}} to visualize.}
#' \item{Coefficients}{The segment specific logistic regression coefficients. Use \code{\link{table.llm.html}} to visualize.}
#' \item{Full decision tree for segmentation}{The raw decision tree. Use \code{\link{table.llm.html}} to visualize.}
#' \item{Observations per segment}{The raw decision tree. Use \code{\link{table.llm.html}} to visualize.}
#' \item{Incidence of dependent per segment}{The raw decision tree. Use \code{\link{table.llm.html}} to visualize.}
#' @import partykit
#' @importFrom stats binomial step glm
#' @importFrom RWeka J48
#' @export
#' @references Arno De Caigny, Kristof Coussement, Koen W. De Bock, A New Hybrid Classification Algorithm for Customer Churn Prediction Based on Logistic Regression and Decision Trees, European Journal of Operational Research (2018), doi: 10.1016/j.ejor.2018.02.009.
#' @author Arno De Caigny, \email{a.de-caigny@@ieseg.fr}, Kristof Coussement, \email{k.coussement@@ieseg.fr} and Koen W. De Bock, \email{kdebock@@audencia.com}
#' @seealso \code{\link{predict.llm}}, \code{\link{table.llm.html}}, \code{\link{llm.cv}}
#' @examples
#' ## Load PimaIndiansDiabetes dataset from mlbench package
#' if (requireNamespace("mlbench", quietly = TRUE)) {
#'   library("mlbench")
#' }
#' data("PimaIndiansDiabetes")
#' ## Split in training and test (2/3 - 1/3)
#' idtrain <- c(sample(1:768,512))
#' PimaTrain <-PimaIndiansDiabetes[idtrain,]
#' Pimatest <-PimaIndiansDiabetes[-idtrain,]
#' ## Create the LLM
#' Pima.llm <- llm(X = PimaTrain[,-c(9)],Y = PimaTrain$diabetes,
#'  threshold_pruning = 0.25,nbr_obs_leaf = 100)
#'


llm <- function(X,Y,threshold_pruning=0.25 , nbr_obs_leaf=100) {
  if (threshold_pruning < 0 | threshold_pruning> 1){
    stop("Enter a valid threshold for pruning value [0,1] (threshold_pruning)")
  }
  if (nbr_obs_leaf < 1){
    stop("Enter a valid mimimum number of observations per leaf (minimum =1) (nbr_obs_leaf)")
  }
  if (nrow(X) != length(Y)){
    stop("The number of instances in (X) and (Y) should be the same")
  }
  if (nrow(X) == 0 | length(Y)== 0){
    stop("There are no instances in (X) or (Y)")
  }
  if (length(which(sapply(X, is.numeric)==FALSE))>0){
    stop("All variables in the dataframe (X) should be numeric")
  }
  if (nlevels(Y) != 2){
    stop("Only binary classification is supported at the moment")
  }
  # .list.rules function: see partykit package
  .list.rules.party <- function(x, i = NULL, ...) {
    if (is.null(i)) i <- nodeids(x, terminal = TRUE)
    if (length(i) > 1) {
      ret <- sapply(i, .list.rules.party, x = x)
      names(ret) <- if (is.character(i)) i else names(x)[i]
      return(ret)
    }
    if (is.character(i) && !is.null(names(x)))
      i <- which(names(x) %in% i)
    stopifnot(length(i) == 1 & is.numeric(i))
    stopifnot(i <= length(x) & i >= 1)
    i <- as.integer(i)
    dat <- data_party(x, i)
    if (!is.null(x$fitted)) {
      findx <- which("(fitted)" == names(dat))[1]
      fit <- dat[,findx:ncol(dat), drop = FALSE]
      dat <- dat[,-(findx:ncol(dat)), drop = FALSE]
      if (ncol(dat) == 0)
        dat <- x$data
    } else {
      fit <- NULL
      dat <- x$data
    }

    rule <- c()

    recFun <- function(node) {
      if (id_node(node) == i) return(NULL)
      kid <- sapply(kids_node(node), id_node)
      whichkid <- max(which(kid <= i))
      split <- split_node(node)
      ivar <- varid_split(split)
      svar <- names(dat)[ivar]
      index <- index_split(split)
      if (is.factor(dat[, svar])) {
        if (is.null(index))
          index <- ((1:nlevels(dat[, svar])) > breaks_split(split)) + 1
        slevels <- levels(dat[, svar])[index == whichkid]
        srule <- paste(svar, " %in% c(\"",
                       paste(slevels, collapse = "\", \"", sep = ""), "\")",
                       sep = "")
      } else {
        if (is.null(index)) index <- 1:length(kid)
        breaks <- cbind(c(-Inf, breaks_split(split)),
                        c(breaks_split(split), Inf))
        sbreak <- breaks[index == whichkid,]
        right <- right_split(split)
        srule <- c()
        if (is.finite(sbreak[1]))
          srule <- c(srule,
                     paste(svar, ifelse(right, ">", ">="), sbreak[1]))
        if (is.finite(sbreak[2]))
          srule <- c(srule,
                     paste(svar, ifelse(right, "<=", "<"), sbreak[2]))
        srule <- paste(srule, collapse = " & ")
      }
      rule <<- c(rule, srule)
      return(recFun(node[[whichkid]]))
    }
    node <- recFun(node_party(x))
    paste(rule, collapse = " & ")
  }



  # Create Decision Tree model with the parameters
  m1 <- RWeka::J48(as.factor(as.character(Y)) ~ .,
                   data = X,
                   control = RWeka::Weka_control(M = nbr_obs_leaf, C= threshold_pruning))
  # Extract the rules
  Pm1 = partykit::as.party(m1)
  Pm1_rules = .list.rules.party(Pm1)

  # Extract the nodes
  TrainPred = stats::predict(Pm1, newdata=X, type="node")

  # Create a list to store the output of the different logistic regressions
  listythelist <- vector("list",length(Pm1_rules))
  listythelist2 <- vector("list",length(Pm1_rules))
  listythelist3 <- vector("list",length(Pm1_rules))

  aa <- as.numeric()
  # Do a LR for every split
  for (l in 1:length(Pm1_rules)) {
    # Subset based on the segments of the DT
    train_ss <- X[which(TrainPred==names(Pm1_rules)[l]), ]
    y_sel <- Y[which(TrainPred==names(Pm1_rules)[l])]

    # Train a LR for each subset with forward variable selection
    # build a glm model on the training data
    LR <- stats::glm(y_sel ~ ., data=train_ss, family=stats::binomial("logit"))
    LR1 <- stats::glm(y_sel ~ 1, data=train_ss, family=stats::binomial("logit"))

    # stepwise variable selection
    listythelist[[l]] <- stats::step(LR1,direction="forward" ,scope = list(lower= LR1, upper = LR), trace = 0)
    listythelist2[[l]] <- nrow(train_ss)
    listythelist3[[l]] <- 1 - (table(y_sel)[1]/ length(y_sel))
  }
  myreturn <- list()
  myreturn[[1]] <- Pm1_rules
  myreturn[[2]] <- listythelist
  myreturn[[3]] <- m1
  myreturn[[4]] <- listythelist2
  myreturn[[5]] <- listythelist3
  class(myreturn) <- "logitleafmodel"
  names(myreturn)[[1]] <- "Segment Rules"
  names(myreturn)[[2]] <- "Coefficients"
  names(myreturn)[[3]] <- "Full decision tree for segmentation"
  names(myreturn)[[4]] <- "Observations per segment"
  names(myreturn)[[5]] <- "Incidence of dependent per segment"
  return(myreturn)
}

