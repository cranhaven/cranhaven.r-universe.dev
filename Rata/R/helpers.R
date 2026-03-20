#' Helper functions for ATA
#' @description helper functions for ATA
#' @name helpers
NULL


#' @rdname helpers
#' @description \code{ata_check_item_pool} checks the input of the item pool
#' @param pool the item pool, a list of 3pl, gpcm, and grm items
#' @keywords internal
ata_check_item_pool <- function(pool){
  if(!is.list(pool))
    stop('pool needs to be a list of 3pl, gpcm, and/or grm items')

  x <- list()

  # check a, b, c paramters for 3PL items
  if("3pl" %in% names(pool) && !is.null(pool$'3pl')) {
    x$'3pl' <- as.data.frame(pool$'3pl', stringsAsFactors=FALSE)
    if(!all(c('a', 'b', 'c') %in% colnames(pool$'3pl')))
      warning('a-, b-, and c-parameters are not all found in the 3PL items')
  }

  # check a, b, d paramters for GPCM items
  if("gpcm" %in% names(pool) && !is.null(pool$'gpcm')) {
    x$'gpcm' <- as.data.frame(pool$'gpcm', stringsAsFactors=FALSE)
    nc_gpcm <- grep('(d[0-9]{1,2})', colnames(pool$'gpcm'), value=TRUE)
    nc_gpcm <- length(nc_gpcm)
    if(nc_gpcm == 0)
      warning('No d-parameters are found in the GPCM items')
    if(!all(c('a', 'b') %in% colnames(pool$'gpcm')))
      warning('a-, and b-parameters are not all found in the GPCM items')
  }

  # check a, b paramters for GRM items
  if("grm" %in% names(pool) && !is.null(pool$'grm')) {
    x$'grm' <- as.data.frame(pool$'grm', stringsAsFactors=FALSE)
    nc_grm <- grep('(b[0-9]{1,2})', colnames(pool$'grm'), value=TRUE)
    nc_grm <- length(nc_grm)
    if(nc_grm == 0)
      warning('No b-parameters are found in the GRM items')
    if(!'a' %in% colnames(pool$'grm'))
      warning('a-parameters are not all found in the GRM items')
  }

  x
}


#' @rdname helpers
#' @description \code{ata_item_set_groups} creates grouping indices for item sets
#' @param opts the options, a list
#' @keywords internal
ata_item_set_groups <- function(pool, opts) {
  if(is.null(opts$group)){
    groups <- Map(function(x) if(is.null(x)) integer(0) else 1:nrow(x), pool)
  } else {
    groups <- Map(function(x) {
      if(!opts$group %in% colnames(x)) {
        return(if(is.null(x)) integer(0) else 1:nrow(x))
      }
      as.integer(factor(x[, opts$group]))
    }, pool)
  }

  group_max <- sapply(groups, max)
  group_max <- cumsum(c(0, group_max[-length(group_max)]))
  Map(function(x, y) x + y, groups, group_max)
}


#' @rdname helpers
#' @description \code{ata_form_map} creates a map of external and internal forms
#' @param n_forms the nubmer of forms to be assembled
#' @keywords internal
ata_form_map <- function(n_forms, opts) {
  if(!is.null(opts$common_items)){
    form_map <- cbind(1:n_forms, n_forms + 1)
  } else if(!is.null(opts$overlap_items)){
    form_map <- cbind(1:n_forms, 1:n_forms + n_forms, c(n_forms*2, 1:(n_forms - 1) + n_forms))
  } else if(!is.null(opts$form_map)) {
    form_map <- opts$form_map
  } else {
    form_map <- matrix(1:n_forms, ncol=1)
  }
  form_map
}


#' @rdname helpers
#' @description \code{ata_get_form_index} finds the internal form indices for the form input
#' @param forms form indices
#' @param collapse \code{TRUE} to collaspe forms into one form
#' @param internal_index \code{TRUE} to use internal form index
#' @keywords internal
ata_get_form_index <- function(x, forms, collapse, internal_index){
  if(internal_index){
    if(is.null(forms))
      forms <- 1:x$n_forms
    if(any(!forms %in% 1:x$n_forms))
      stop('Invalid form indices')
    forms <- as.matrix(forms)
  } else {
    if(is.null(forms))
      forms <- 1:nrow(x$form_map)
    if(any(!forms %in% 1:nrow(x$form_map)))
      stop('Invalid form indices')
    forms <- x$form_map[forms, , drop=FALSE]
  }

  if(collapse)
    forms <- matrix(unique(as.vector(forms)), nrow=1)

  forms
}


#' @rdname helpers
#' @description \code{ata_get_obj_coef} finds real coefficients for the coefficient input
#' @param coef coefficients
#' @param compensate \code{TRUE} to combine coefficients
#' @importFrom stats aggregate
#' @importFrom Rirt model_mixed_info
#' @keywords internal
ata_get_obj_coef <- function(x, coef, compensate){
  if(length(coef) == x$n_items){ # numeric coefficients at the group level
    coef <- matrix(coef, nrow=1)
  } else if(length(coef) == sum(sapply(x$pool, nrow))) { # numeric coefficients, at the item level
    coef <- aggregate(coef, by=list(group=unlist(x$groups)), sum, na.rm=TRUE)[,-1]
    coef <- matrix(coef, nrow=1)
  } else if(is.numeric(coef)) { # a vector of theta points where TIFs are controlled
    coef <- model_mixed_info(coef, x$pool, D=x$opts$D)
    coef <- aggregate(t(coef), by=list(group=unlist(x$groups)), sum, na.rm=TRUE)
    coef <- t(coef[,-1,drop=FALSE])
  } else if(is.character(coef)) { # a variable name
    coef <- Map(function(x, g) {
      if(is.null(x))
        return(numeric(0))
      coef <- coef[coef %in% colnames(x)]
      aggregate(x[, coef], by=list(group=g), sum, na.rm=TRUE)[, -1, drop=FALSE]
    }, x$pool, x$groups)
    coef <- t(Reduce(rbind, coef))
  } else {
    stop("Invalid coefficients")
  }

  if(compensate)
    coef <- matrix(colSums(coef), nrow=1)

  round(coef, 3)
}


#' @rdname helpers
#' @description \code{ata_append} appends constraints to the ATA model
#' @param mat coefficient matrix
#' @param dir direction
#' @param rhs right-hand-side value
#' @keywords internal
ata_append <- function(x, mat, dir, rhs) {
  x$mat <- rbind(x$mat, mat)
  x$dir <- c(x$dir, dir)
  x$rhs <- c(x$rhs, rhs)
  x
}


#' @rdname helpers
#' @description \code{x} retrieves items from the result matrix
#' @keywords internal
ata_extract_items <- function(x) {
  items <- list()
  for(i in 1:nrow(x$form_map)){
    f <- x$form_map[i, ]
    f <- f[!is.na(f)]
    ix <- apply(x$result[, f, drop=FALSE] == 1, 1, any)
    ix <- seq(x$n_items)[ix]
    ix <- Map(function(x, g) {
      itm <- x[g %in% ix,]
      if(nrow(itm) == 0)
        return(data.frame())
      cbind(itm, form=i)
    }, x$pool, x$groups)
    items[[i]] <- ix
  }
  items
}

#' @rdname helpers
#' @description \code{ata_results_to_model} converts results from the 'by-form' format to the 'by-model' format
#' @param items the assembled items
#' @keywords internal
ata_results_to_model <- function(items) {
  Reduce(function(x, y) {
    list('3pl'=rbind(x$'3pl', y$'3pl'),
         'gpcm'=rbind(x$'gpcm', y$'gpcm'),
         'grm'=rbind(x$'grm', y$'grm'))
  }, items)
}

#' @rdname helpers
#' @description \code{ata_results_to_dataframe} converts results from the 'by-model' format to the 'data.frame' format
#' @keywords internal
ata_results_to_dataframe <- function(items) {
  items <- Map(function(x, m) {
    if(is.null(x))
      return(numeric(0))
    x <- cbind(x, model=m)
    x[, colnames(x)[!grepl('^(a|b[0-9]{0,2}|c|d[0-9]{0,2})$', colnames(x))]]
  }, items, names(items))
  Reduce(rbind, items)
}

