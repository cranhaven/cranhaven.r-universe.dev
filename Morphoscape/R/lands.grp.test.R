# Significance test between group weights
lands.grp.test <- function(grpa, grpb, method = "chi-squared", quantile = 0.05){
  #get top 1% based on likelihood
  
  if (!inherits(grpa, "grp_Wprime") || !inherits(grpb, "grp_Wprime")) {
    stop("'grpa' and 'grpb' must be grp_Wprime objects (the output of calcGrpWprime()).",
         call. = FALSE)
  }
  
  if (length(method) != 1 || !is.character(method)) {
    stop("'method' must be one of \"quantile\" or \"chi-squared\".", call. = FALSE)
  }
  method <- match.arg(method, c("quantile", "chi-squared"))
  
  besta <- gettop(grpa$W, method = method, quantile = quantile, sortby = "Z")
  bestb <- gettop(grpb$W, method = method, quantile = quantile, sortby = "Z")
  
  #Check for matching models
  matched <- merge(as.data.frame(besta[,-ncol(besta), drop = FALSE]),
                   as.data.frame(bestb[,-ncol(bestb), drop = FALSE]),
                   sort = FALSE)
  
  n.match <- nrow(matched)
  p.match <- n.match / nrow(besta)
  
  out <- list(n.match = n.match, p.val = p.match, 
              matching = as.matrix(matched), method = method,
              quantile = quantile)
  class(out) <- "lands.grp.test"
  return(out)
}

print.lands.grp.test <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  cat("Landscape group test\n")
  cat(sprintf("- method: %s | quantile: %s\n\n", x$method, x$quantile))
  cat(sprintf("Number of matches: %s\n", x$n.match))
  cat(sprintf("P-value: %s\n", round(x$p.val, digits)))
  
  invisible(x)
}

#groups list of weighting files
multi.lands.grp.test <- function(x, method = "chi-squared", quantile = 0.05){
  
  if (inherits(x, "by_Wprime")) {
    groups <- x$grp_Wprimes
  }
  else if (is.list(x) && !is.null(names(x)) &&
           all(vapply(x, inherits, logical(1L), "grp_Wprime"))) {
    groups <- x
  }
  else {
    stop("'x' must be a by_Wprime object or a *named* list of grp_Wprime objects.", call. = FALSE)
  }
  
  if (length(method) != 1 || !is.character(method)) {
    stop("'method' must be one of \"quantile\" or \"chi-squared\".", call. = FALSE)
  }
  method <- match.arg(method, c("quantile", "chi-squared"))
  
  tests <- combn(names(groups), 2, simplify = FALSE)
  res <- as.data.frame(matrix(nrow = length(tests), ncol = 4))
  names(res) <- c("Group A", "Group B", "Matches", "p value")
  
  #run tests pairwise
  for (i in seq_along(tests)){
    grpa <- groups[[tests[[i]][1]]]
    grpb <- groups[[tests[[i]][2]]]
    t <- lands.grp.test(grpa, grpb, method = method, quantile = quantile)
    
    res[i, "Group A"] <- tests[[i]][1]
    res[i, "Group B"] <- tests[[i]][2]
    res[i, "Matches"] <- t$n.match
    res[i, "p value"] <- t$p.val
  }
  
  out <- list(res = res, method = method,
              quantile = quantile)
  
  class(out) <- "multi.lands.grp.test"
  return(out)
}

print.multi.lands.grp.test <- function(x, digits = max(3L, getOption("digits") - 3L), style = "matrix", ...) {
  style <- match.arg(style, c("matrix", "table"))
  res <- x$res
  if (style == "matrix") {
    groups <- sort(unique(c(res[["Group A"]], res[["Group B"]])))
    tab <- matrix(NA_character_, nrow = length(groups), ncol = length(groups),
                  dimnames = list(groups, groups))
    
    for (i in seq_len(nrow(res))) {
      tab[res[i, "Group A"], res[i, "Group B"]] <- format(res[i, "Matches"])
      tab[res[i, "Group B"], res[i, "Group A"]] <- format(res[i, "p value"], digits = digits)
    }
    
    cat("Pairwise landscape group tests\n")
    cat(sprintf("- method: %s | quantile: %s\n\n", x$method, x$quantile))
    cat("Results:\n")
    print(as.table(tab), na.print = "-")
    cat("(lower triangle: p-values | upper triangle: number of matches)\n")
  }
  else {
    cat("Pairwise landscape group tests\n")
    cat(sprintf("- method: %s | quantile: %s\n\n", x$method, x$quantile))
    cat("Results:\n")
    print(res)
  }
  
  invisible(x)
}