inSolaris <- function(){
  grepl("sunos", tolower(Sys.info()["sysname"]))
}

gx.sort.df <- function(formula, dfname){
  if (inherits(dfname, "formula")) {
    f <- dfname
    dfname <- formula
    formula <- f
  }
  if (formula[[1]] != "~") 
    stop("Formula must be one-sided.")
  formc <- as.character(formula[2])
  formc <- gsub(" ", "", formc)
  if (!is.element(substring(formc, 1, 1), c("+", "-"))) 
    formc <- paste("+", formc, sep = "")
  vars <- unlist(strsplit(formc, "[\\+\\-]"))
  vars <- vars[vars != ""]
  calllist <- list()
  pos = 1
  for (i in 1:length(vars)) {
    varsign <- substring(formc, pos, pos)
    pos <- pos + 1 + nchar(vars[i])
    if (is.factor(dfname[, vars[i]])) {
      if (varsign == "-") 
        calllist[[i]] <- -rank(dfname[, vars[i]])
      else calllist[[i]] <- rank(dfname[, vars[i]])
    }
    else {
      if (varsign == "-") 
        calllist[[i]] <- -dfname[, vars[i]]
      else calllist[[i]] <- dfname[, vars[i]]
    }
  }
  dfname[do.call("order", calllist), ]
}

#' @importFrom forcats fct_reorder
#' @noRd
recode <- function(x){
  as.integer(fct_reorder(x, seq_along(x))) - 1L
}

#' @importFrom lazyeval f_eval_rhs as.lazy lazy_eval
#' @importFrom stats terms.formula setNames
#' @noRd
getRE2 <- function(data, random, check){
  if(is.null(random)){
    return(data.frame(error = factor(seq_len(nrow(data)))))
  }
  data <- droplevels(data)
  tf <- terms.formula(random)
  factors <- rownames(attr(tf, "factors"))
  tvars <- attr(tf, "variables")
  tlabs <- attr(tf, "term.labels")
  tvars <- setNames(eval(tvars, envir = data), factors)
  if(any(vapply(tvars, is.numeric, logical(1L)))){
    warning(
      "Numeric random effects are not supported; converting to factors."
    )
  }
  rdat <- lapply(tvars, function(tvar) droplevels(as.factor(tvar)))
  if(check && any(vapply(rdat, function(x) nlevels(x) == 1L, logical(1L)))){
    stop(
      "Random effects with only one level are not allowed."
    )
  }
  if(check && any(vapply(rdat, function(x) any(table(x) == 1L), logical(1L)))){
    stop(
      "Found a random effect with a lone level."
    )
  }
  #rdat <- lapply(rdat, function(fct) factor(as.integer(fct)))
  RE <- as.data.frame(lapply(setNames(tlabs, tlabs), function(tlab){
    droplevels(lazy_eval(as.lazy(tlab), data = rdat))
  }), check.names = FALSE)
  #   group treatment group:treatment
  # 1     1         1             1:1
  # 2     1         2             1:2
  # 3     1         3             1:3
  # 4     2         1             2:1
  # 5     2         2             2:2
  # 6     2         3             2:3
  RE[["error"]] <- factor(seq_len(nrow(data))) # Adds the error effect 
  RE
}

getZ <- function(RE2){
  n <- nrow(RE2)
  E <- vapply(RE2, nlevels, integer(1L))
  Z <- NULL 
  for(i in seq_along(E)){ # Builds an indicator matrix for the effects
    re_levels <- levels(RE2[[i]])
    for(j in 1L:E[i]){
      temp1 <- which(RE2[[i]] == re_levels[j]) 
      temp2 <- integer(n) 
      temp2[temp1] <- 1L 
      Z <- cbind(Z, temp2, deparse.level = 0L)
    }
  } 
  Z
}

#' @importFrom stats get_all_vars
#' @noRd
getCovariates <- function(data, fixed){ 
  frame <- get_all_vars(fixed, data)
  continuous <- vapply(frame, is.numeric, logical(1L))
  list(
    continuous  = names(frame)[continuous],
    categorical = lapply(frame[!continuous], function(cvrt){
      levels(as.factor(cvrt))
    })
  )
}
