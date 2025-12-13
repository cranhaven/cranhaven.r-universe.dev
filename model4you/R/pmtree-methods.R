
#' pmtree predictions
#'
#' Compute predictions from pmtree object. 
#'
#' @param object pmtree object.
#' @param newdata an optional data frame in which to look for variables with
#'    which to predict, if omitted, \code{object$data} is used.
#' @param type character denoting the type of predicted value. The terminal node 
#' is returned for \code{"node"}. If \code{type = "pass"} the model 
#' predict method is used and arguments can be passed to it via \code{predict_args}.
#' If \code{type = "coef"} the the model coefficients are returned.
#' @param predict_args If \code{type = "pass"} arguments can be passed on to the
#' model predict function.
#' @param perm an optional character vector of variable names (or integer vector 
#' of variable location in \code{newdata}). Splits of nodes 
#' with a primary split in any of these variables will be permuted (after dealing 
#' with surrogates). Note that surrogate split in the perm variables will no be 
#' permuted.
#' @param ... passed on to predict.party (e.g. \code{perm}).
#'
#' @return predictions
#' 
#' @example inst/examples/ex-pmtree-methods.R
#' 
#' @importFrom partykit predict.party
#' @export
predict.pmtree <- function(object, newdata = NULL, type = "node", 
                           predict_args = list(), perm = NULL, ...) {
  
  ## node
  if(is.numeric(perm)) perm <- names(object$data)[perm]
  node <- predict.party(object, newdata = newdata, type = "node", perm = perm, ...)
  if(type == "node") return(node)
  
  if(is.null(newdata)) newdata <- object$data
  
  ## response
  if(type %in% c("pass", "coef")) {
    trdatnodes <- object$fitted["(fitted)"]
    
    # predict outcome using respective models
    unode <- sort(unique(node))
    newdata$.node <- node
    newdata$.id <- seq_len(NROW(newdata))
    
    prfun <- function(nd, type = "pass") {
      # model
      mod <- update(object$info$model, 
                    subset = (trdatnodes == nd),
                    data = object$data)
      
      if(type == "coef") {
        data.frame(t(coef(mod)), 
                   .id = newdata$.id[newdata$.node == nd], 
                   check.names = FALSE)
      } else { # type = "pass"
        
        # prediction
        args <- c(list(object = mod,
                       newdata = newdata[newdata$.node == nd, ]),
                  predict_args)
        pred <- do.call(predict, args = args)
        data.frame(pred, .id = newdata$.id[newdata$.node == nd])
      }
    }
    pr <- lapply(unode, prfun, type = type)
    pr <- do.call(rbind, pr)
    
    ## return sorted predictions
    pred <- pr[order(pr$.id), ]
    pred$.id <- NULL
    return(pred)
    
  }
}


#' Objective function of a given pmtree
#'
#' Returns the contributions to the objective function or the 
#' sum thereof (if \code{sum = TRUE}).
#'
#' @param x pmtree object.
#' @param newdata an optional new data frame for which to compute the sum of 
#' objective functions.
#' @param weights weights.
#' @param perm the number of permutations performed (see \code{\link[partykit]{varimp}}).
#' @param sum should the sum of objective functions be computed. 
#' @param ... passed on to \code{\link[partykit]{predict.party}}.
#' 
#' Note that \code{objfun.pmtree(x, sum = TRUE)} is much faster than
#' \code{sum(objfun.pmtree(x))}.
#'
#' @return objective function or the sum thereof
#' @export
#' @examples
#' ## generate data
#' set.seed(2)
#' n <- 1000
#' trt <- factor(rep(1:2, each = n/2))
#' age <- sample(40:60, size = n, replace = TRUE)
#' eff <- -1 + I(trt == 2) + 1 * I(trt == 2) * I(age > 50)
#' expit <- function(x) 1/(1 + exp(-x))
#' success <- rbinom(n = n, size = 1, prob = expit(eff))
#' dat <- data.frame(success, trt, age)
#' 
#' ## compute base model
#' bmod1 <- glm(success ~ trt, data = dat, family = binomial)
#' 
#' ## copmute tree
#' (tr1 <- pmtree(bmod1, data = dat))
#' 
#' ## compute log-Likelihood
#' logLik(tr1)
#' objfun(tr1, newdata = dat, sum = TRUE)
#' objfun(tr1, sum = TRUE)
#' 
#' ## log-Likelihood contributions of first 
#' ## 5 observations
#' nd <- dat[1:5, ]
#' objfun(tr1, newdata = nd)
objfun.pmtree <- function(x, 
                          newdata = NULL, 
                          weights = NULL, ## TODO: check if this works
                          perm = NULL, ## TODO: implement
                          sum = FALSE, ...) {
  
  ## get models from terminal nodes
  which_node <- predict(x, type = "node", newdata = newdata,
                        perm = perm, ...)
  if(!is.null(perm) & is.null(newdata))
    newdata <- x$data
  tnodes <- unique(which_node)
  mods <- nodeapply(x, ids = tnodes, FUN = function(n) n$info$object)
  
  ## if the order does not matter, this is faster
  if(sum) {
    if(!is.null(weights) & is.null(newdata)) {
      # which_node[weights == 0] <- NA
      newdata <- x$data
    } 
    
    ## get contributions of objfun in each node
    get_objfun_node_unordered <- function(nd) {
      wn <- (which_node == nd) & !is.na(which_node)
      sum(objfun(mods[[as.character(nd)]],
                 newdata = newdata[wn, ], 
                 weights = weights[wn]))
    }
    
    ## return the unordered contributions
    return(sum(sapply(tnodes, get_objfun_node_unordered)))
    
  } else {
    
    ## FIXME: is there a better option than to get the data?
    if(is.null(newdata)) newdata <- x$data
    
    w_n_char <- as.character(which_node)
    
    ## get objective function for each observation
    get_objfun_node <- function(i) {
      objfun(mods[[w_n_char[i]]], newdata[i, ], weights = weights[i])
    }
    return(sapply(seq_len(NROW(newdata)), get_objfun_node))
  }
  
}




#' Extract log-Likelihood
#'
#' Extract sum of log-Likelihood contributions of all terminal nodes. By default the degrees
#' of freedom from the models are used but optionally degrees of freedom for splits
#' can be incorporated. 
#'
#' @param object pmtree object.
#' @param dfsplit degrees of freedom per selected split.
#' @param newdata an optional new data frame for which to compute the sum of 
#' objective functions.
#' @param weights weights.
#' @param perm the number of permutations performed (see \code{\link[partykit]{varimp}}).
#' @param ... ignored.
#' 
#' @seealso \code{\link{objfun.pmtree}} for the sum of contributions to the
#' objective function (not the same when partitioning linear models \code{\link[stats]{lm}})
#'
#' @return Returns an object of class \code{\link[stats]{logLik}}.
#' 
#' @export
logLik.pmtree <- function(object, dfsplit = 0, newdata = NULL, weights = NULL, perm = NULL, ...) {
  
  ## compute degrees of freedom
  dfs <- (length(object) - width(object)) * dfsplit
  df <- NULL
  
  if (is.null(newdata) && is.null(perm) && is.null(weights)) {
    ## get info of all terminal nodes
    ids <- nodeids(object, terminal = TRUE) 
    info <- nodeapply(object, ids = ids, function(x) x$info)
    
    ## get logLik in all terminal nodes
    ll <- lapply(info, function(x) tryCatch(logLik(x$object), error = function(err) NA))
    ndf <- sapply(ll, function(x) attr(x, "df"))
    if(!any(sapply(ndf, is.null)))  df <- sum(ndf) + dfs
    
  } else {
    if(class(object$info$model)[1] == "lm")
      stop("logLik not yet implemented for lm. Try objfun(..., sum = TRUE).") 
    
    ll <- objfun(x = object, newdata = newdata, weights = weights, perm = perm, 
                 sum = TRUE, ...)
    df <- NA
  }
  
  
  ## number of observations
  nobs <- ifelse(!is.null(weights), sum(weights),
                 ifelse(is.null(newdata), object$nobs,
                        nrow(newdata)))
  
  structure(
    sum(as.numeric(ll)),
    df = df,
    nobs = nobs,
    class = "logLik"
  )
}

#' Methods for pmtree
#'
#' Print and summary methods for pmtree objects.
#'
#' @param x object.
#' @param node node number, if any.
#' @param FUN formatinfo function.
#' @param digits number of digits.
#' @param footer should footer be included?
#' @param ... further arguments passed on to \code{\link[partykit]{print.party}}.
#'
#' @return print
#' @export
#' @importFrom partykit print.party info_node nodeapply width formatinfo_node
#' @importFrom utils capture.output
print.pmtree <- function(x, node = NULL,
                         FUN = NULL, digits = getOption("digits") - 4L,
                         footer = TRUE, ...)
{
  digits <- max(c(0, digits))
  title <- paste("Partitioned model:\n", paste(deparse(getCall(x$info$model)), 
                                               sep = "\n", collapse = "\n"),
                 "\nPartitioning variables:", deparse(x$info$zformula))
  
  if(is.null(node)) {
    header_panel <- function(party) ""
    
    footer_panel <- if(footer) function(party) {
      n <- width(party)
      n <- format(c(length(party) - n, n))
      info <- nodeapply(x, ids = nodeids(x, terminal = TRUE),
                        FUN = function(n) c(length(info_node(n)$coefficients), info_node(n)$objfun))
      k <- mean(sapply(info, "[", 1L))
      of <- format(sum(sapply(info, "[", 2L)), digits = getOption("digits"))
      
      c("", paste("Number of inner nodes:   ", n[1L]),
        paste("Number of terminal nodes:", n[2L]),
        paste("Number of parameters per node:", format(k, digits = getOption("digits"))),
        paste("Objective function: ", of, sep = ""), "")
    } else function (party) ""
    
    if(is.null(FUN)) {
      FUN <- function(x) c(sprintf(": n = %s", x$nobs), capture.output(print(x$coefficients)))
    }
    terminal_panel <- function(node) formatinfo_node(node,
                                                     default = "*", prefix = NULL, FUN = FUN)
    
    print.party(x, terminal_panel = terminal_panel,
                header_panel = header_panel, footer_panel = footer_panel, ...)
  } else {
    node <- as.integer(node)
    info <- nodeapply(x, ids = node,
                      FUN = function(n) info_node(n)[c("coefficients", "objfun", "criterion")])    
    for(i in seq_along(node)) {
      if(i == 1L) {
        cat(paste(title, "\n", collapse = ""))
      } else {
        cat("\n")
      }
      cat(sprintf("-- Node %i --\n", node[i]))
      cat("\nEstimated parameters:\n")
      print(info[[i]]$coefficients)
      cat(sprintf("\nObjective function:\n%s\n", format(info[[i]]$objfun)))
      cat("\nParameter instability tests:\n")
      print(info[[i]]$criterion)
    }
  }
  invisible(x)
}



#' @rdname print.pmtree 
#' 
#' @param object object.
#' @export
summary.pmtree <- function(object, node = NULL, ...) {
  
  ids <- if(is.null(node)) nodeids(object, terminal = TRUE) else node
  info <- nodeapply(object, ids = ids, function(x) x$info)
  
  ## coefficients
  coefs <- sapply(info, function(x) x$coefficients)
  colnames(coefs) <- paste("node", colnames(coefs))
  
  ## objective functions
  objfuns <- sapply(info, function(x) x$objfun)
  if(is.null(node)) {
    brobjf <- paste0("(", round(objfuns, 2), ")")
    sobjf <- paste(brobjf, collapse = " + ")
    objfs <- paste(sobjf, "=", round(sum(objfuns), 2))
  } else {
    objfs <- objfuns
    names(objfs) <- paste("node", ids)
  }
  
  ## call
  cl <- getCall(object$info$model)
  
  ## nobs
  nobs <- sapply(info, function(x) x$nobs)
  nullnobs <- sapply(nobs, is.null)
  if(1 %in% ids & any(nullnobs)) 
    nobs[nullnobs] <- object$nobs
  names(nobs) <- paste("node", ids)
  
  
  
  ret <- list(ids = ids, call = cl, coefs = coefs, objfs = objfs, nobs = nobs)
  class(ret) <- "summary.pmtree"
  return(ret)
}


#' @rdname print.pmtree 
#' 
#' @export
print.summary.pmtree <- function(x, digits = 4, ...) {
  cat("Stratified model for node(s)", paste(x$ids, collapse = ", "))
  cat("\n\nModel call:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), 
      "\n\n", sep = "")
  
  cat("Coefficients:\n")
  print(x$coefs, digits = digits)
  
  cat("\nNumber of obervations:\n")
  print(unlist(x$nobs))
  
  cat("\nObjective function:\n")
  if(is.character(x$objfs)) cat(x$objfs) else {
    print(x$objfs)
  }
}


#' @rdname print.pmtree 
#' 
#' @export
coef.pmtree <- function(object, node = NULL, ...) {
  
  ids <- if(is.null(node)) nodeids(object, terminal = TRUE) else node
  info <- nodeapply(object, ids = ids, function(x) x$info)
  
  ## coefficients
  t(sapply(info, function(x) x$coefficients))
}
