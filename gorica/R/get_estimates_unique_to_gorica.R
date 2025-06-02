# @title Get estimates from a model object
# @description Get estimates from a model object.
# This convenience function allows you to see that coefficients are properly
# extracted, note how their names will be parsed, and inspect their values.
# @param x A model object.
# @param ... Parameters passed to and from other functions.
# @param margin Integer, indicating the margins to split by.
# E.g., for a matrix 1 indicates rows, 2 indicates columns. See
# \code{\link[base:proportions]{prop.table}}.
# @param constraints Character, indicating the constraints on model parameters.
# Constraints are an extension of \code{\link[bain]{bain}} syntax that can be
# used to define parameters. The command used to define a parameter is
# \code{:=}, e.g.: \code{a := x[1,1]/(x[1,1]+x[1,2])}.
#
# Constraints are evaluated in an environment where
# the argument \code{x} exists as a literal object \code{x}. Thus, if \code{x}
# is an object of class \code{table}, the table can be indexed as illustrated
# in the example above.
# @param nboot Integer. Number of bootstrapping iterations to use when
# obtaining the covariance matrix of the estimates.
# @return An object of class 'model_estimates'
# @examples
# set.seed(83)
# tab <- table(rbinom(40, 1, .4), rbinom(40, 1, .4))
# get_estimates(tab)
# @rdname get_estimates
#' @export
#' @method get_estimates table
get_estimates.table <- function(x, ..., margin = NULL, constraints = NULL, nboot = 1000){
  tab <- as.data.frame.table(x, stringsAsFactors = TRUE,
                             base = list(as.character(1:max(dim(x))))) # Ensure that factor levels correspond to numeric indexes
  tab[-ncol(tab)] <- lapply(tab[-ncol(tab)], function(x){as.integer(factor(x))})
  dat_orig <- tab[rep(1:nrow(tab), times = tab$Freq), -which(names(tab) == "Freq")]
  dmz <- dim(x)
  for(i in 1:length(dmz)){
    dat_orig[[i]] <- ordered(dat_orig[[i]], levels = 1:dmz[i])
  }

  if(!is.null(margin) & !is.null(constraints)){
    stop("When calling get_estimates.table(), either apply constraints or specify a margin, but not both.")
  }
  # Case when constraints are an empty string (e.g., resulting from splitting hypothesis string)
  if(!is.null(constraints)){
    if(constraints == "") constraints <- NULL
  }
  # Main function, with conditional branches for constraints or prop.table margins
  if(!is.null(constraints)){
    constraints <- get_const(constraints)
    constraints <- strsplit(constraints, ":=", fixed = TRUE)
    the_names <- sapply(constraints, `[`, 1)
    constraints <- sapply(constraints, `[`, 2)
    env_const <- new.env()
    assign("x", prop.table(x), envir = env_const)
    estimate <- sapply(constraints, function(thisc){ eval(parse(text = thisc), envir = env_const)})
    boot_props <- t(replicate(nboot, {
      tab_boot <- prop.table(table(dat_orig[sample(nrow(dat_orig), replace = TRUE), ]))
      assign("x", tab_boot, envir = env_const)
      eval(parse(text = paste0("c(", paste0(constraints, collapse = ","), ")")), envir = env_const)
    }))
  } else {
    estimate <- as.vector(prop.table(x))
    boot_props <- t(replicate(nboot, {
      as.vector(prop.table(table(dat_orig[sample(nrow(dat_orig), replace = TRUE), ]), margin = margin))
    }))
    the_names <- paste0("x[", apply(tab[-which(names(tab) == "Freq")], 1, paste0, collapse = ","), "]")
  }
  Sigma <- if(nrow(boot_props) > 1){
    cov(boot_props, use = "complete.obs")
  } else {
    cov(t(boot_props), use = "complete.obs")
  }
  empty_prop <- is.na(boot_props)
  if(any(empty_prop)) {
    empty_prop <- colSums(empty_prop)
    names(empty_prop) <- the_names
    warning("Some model parameters could not be estimated in some bootstrap sample(s). The number of valid bootstrap samples for the estimates is (also provided in the attribute 'valid_samples' of the estimates):\n", paste0(the_names, ": ", empty_prop, collapse = "\n"))
    attr(estimate, "valid_samples") <- empty_prop
  }

  out <- list(estimate = estimate,
              Sigma = Sigma)

  names(out$estimate) <- rownames(out$Sigma) <- colnames(out$Sigma) <- the_names
  class(out) <- "model_estimates"
  attr(out, "analysisType") <- "contingency_table"
  return(out)
}


rename_table_est <- function(txt){
  txt <- gsub(" ", "", txt, fixed = TRUE)
  txt <- gsub("\\[(\\d+),(\\d+)\\]", "_BO_\\1_C_\\2_BC_", txt)
  txt
}
reverse_rename_table_est <- function(txt){
  txt <- gsub("_BO_", "[", txt, fixed = TRUE)
  txt <- gsub("_BC_", "]", txt, fixed = TRUE)
  txt <- gsub("_C_", ",", txt, fixed = TRUE)
  txt
}

get_const <- function(txt){
  const <- gsub("(\\s|\n|\r\n)", "", txt)
  const <- strsplit(const, ";")[[1]]
  const[grepl(":=", const, fixed = TRUE)]
}
get_hyp <- function(txt){
  const <- gsub("(\\s|\n|\r\n)", "", txt)
  const <- strsplit(const, ";")[[1]]
  const[!grepl(":=", const, fixed = TRUE)]
}



# @export
#get_estimates <- function (x, ...)
#{
#  UseMethod("get_estimates", x)
#}

# @method get_estimates lmerMod
# @export
#' @method get_estimates lmerMod
#' @export
#' @import bain
#' @importFrom lme4 fixef
get_estimates.lmerMod <- function (x, ...)
{
  out <- list(estimate = fixef(x), Sigma = vcov(x))
  class(out) <- "model_estimates"
  attr(out, "analysisType") <- "lme4"
  out
}

#' @method get_estimates lavaan
#' @export
get_estimates.lavaan <- function(x, standardize = FALSE, ...){
  cl <- as.list(match.call()[-1])
  cl <- c(cl, list(retain_which = c("=~", "~", "~1", "~~", "=="),
                    split_multigroup_sigma = FALSE,
                    allow_between_constraints = TRUE))
  out <- do.call(lav_get_estimates, cl)
  names(out)[which(names(out) == "x")] <- "estimate"
  class(out) <- "model_estimates"
  attr(out, "analysisType") <- "lavaan"
  out
}
