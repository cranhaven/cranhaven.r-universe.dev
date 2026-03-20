#' Automated Test Assembly (ATA)
#' @name ata
#' @examples
#' ## generate a pool of 100 items
#' library(Rirt)
#' n_items <- 100
#' pool <- with(model_3pl_gendata(1, n_items), data.frame(id=1:n_items, a=a, b=b, c=c))
#' pool$content <- sample(1:3, n_items, replace=TRUE)
#' pool$time <- round(rlnorm(n_items, log(60), .2))
#' pool$group <- sort(sample(1:round(n_items/3), n_items, replace=TRUE))
#' pool <- list('3pl'=pool)
#'
#' ## ex. 1: four 10-item forms, maximize b parameter
#' x <- ata(pool, 4, test_len=10, max_use=1)
#' x <- ata_relative_objective(x, "b", "max")
#' x <- ata_solve(x, time_limit=2)
#' with(x$items$'3pl', aggregate(b, by=list(form=form), mean))
#' with(x$items$'3pl', table(form))
#'
#' \donttest{
#' ## ex. 2: four 10-item forms, minimize b parameter
#' x <- ata(pool, 4, test_len=10, max_use=1)
#' x <- ata_relative_objective(x, "b", "min", negative=TRUE)
#' x <- ata_solve(x, time_limit=5)
#' with(x$items$'3pl', aggregate(b, by=list(form=form), mean))
#' with(x$items$'3pl', table(form))
#'
#' ## ex. 3: two 10-item forms, mean(b)=0, sd(b)=1
#' ## content = (3, 3, 4), avg. time = 55--65 seconds
#' constr <- data.frame(name='content',level=1:3, min=c(3,3,4), max=c(3,3,4), stringsAsFactors=FALSE)
#' constr <- rbind(constr, c('time', NA, 55*10, 65*10))
#' x <- ata(pool, 2, test_len=10, max_use=1)
#' x <- ata_absolute_objective(x, pool$'3pl'$b, target=0*10)
#' x <- ata_absolute_objective(x, (pool$'3pl'$b-0)^2, target=1*10)
#' for(i in 1:nrow(constr))
#'   x <- with(constr, ata_constraint(x, name[i], min[i], max[i], level=level[i]))
#' x <- ata_solve(x)
#' with(x$items$'3pl', aggregate(b, by=list(form=form), mean))
#' with(x$items$'3pl', aggregate(b, by=list(form=form), sd))
#' with(x$items$'3pl', aggregate(time, by=list(form=form), mean))
#' with(x$items$'3pl', aggregate(content, by=list(form=form), function(x) freq(x, 1:3)$freq))
#'
#' ## ex. 4: two 10-item forms, max TIF over (-1, 1), consider item sets
#' x <- ata(pool, 2, test_len=10, max_use=1, group="group")
#' x <- ata_relative_objective(x, seq(-1, 1, .5), 'max')
#' x <- ata_solve(x, time_limit=5)
#' plot(x)
#' }
NULL


#' @rdname ata
#' @description \code{ata} creates a basic ATA model
#' @param pool the item pool(s), a list of '3pl', 'gpcm', and 'grm' items
#' @param n_forms the number of forms to be assembled
#' @param test_len test length of each form
#' @param max_use maximum use of each item
#' @param ... options, e.g. group, common_items, overlap_items
#' @return \code{ata} returns a \code{ata} object
#' @details
#' The ATA model stores the definitions of a MIP model. When \code{ata_solve}
#' is called, a real MIP object is created from the definitions.
#' @export
ata <- function(pool, n_forms=1, test_len=NULL, max_use=NULL, ...){
  pool <- ata_check_item_pool(pool)

  opts <- list(...)
  # scaling constants
  if(is.null(opts$D))
    opts$D <- 1.702

  # combinations of unqiue and common items in each form
  form_map <- ata_form_map(n_forms, opts)
  n_forms <- max(form_map)

  # group item sets
  groups <- ata_item_set_groups(pool, opts)
  n_items <- max(unlist(groups))

  # the number of items and
  n_lpvar <- n_items * n_forms + 2
  # LP: x's (binary) + y1 (continuous) + y2 (continuous)
  obj <- c(rep(0, n_lpvar - 2), 1, 1)
  names(obj) <- c(paste("f", rep(1:n_forms, each=n_items), "v", rep(1:n_items, n_forms), sep=""), "y1", "y2")
  # x's are binary and y is continuous
  types <- c(rep("B", n_lpvar - 2), "C", "C")
  # placehoders for fixing values
  bounds <- list(idx=c(), lb=c(), ub=c())
  # optimization direction: TRUE to maximize, FALSE to minimize
  max <- TRUE
  # TRUE if the obj.value is expected to be negative
  negative <- FALSE
  # constraints: coefficient matrix, directions, and right-hand-side values
  mat <- matrix(nrow=0, ncol=n_lpvar, dimnames=list(NULL, names(obj)))
  dir <- rhs <- NULL

  x <- list(n_items=n_items, n_forms=n_forms, n_lpvar=n_lpvar, pool=pool, groups=groups,
            form_map=form_map, obj=obj, mat=mat, dir=dir, rhs=rhs, types=types, bounds=bounds,
            max=max, negative=negative, opts=opts)
  class(x) <- "ata"

  # add constraint: test length
  if(!is.null(test_len)) {
    if(length(test_len) > 2)
      stop("Invalid test_len. Expect 1 or 2 elements")
    if(length(test_len) == 1)
      test_len <- rep(test_len, 2)
    x <- ata_constraint(x, 1, min=test_len[1], max=test_len[2])
  }

  # add constraint: common_items length
  if(!is.null(opts$common_items)){
    if(length(opts$common_items) == 1)
      opts$common_items <- rep(opts$common_items, 2)
    x <- ata_constraint(x, 1, min=opts$common_items[1], max=opts$common_items[2], forms=n_forms, internal_index=TRUE)
  }

  # add constraint: overlap_items length
  if(!is.null(opts$overlap_items)){
    if(length(opts$overlap_items) == 1)
      opts$overlap_items <- rep(opts$overlap_items, 2)
    x <- ata_constraint(x, rep(1, length(unlist(groups))), min=opts$overlap_items[1], max=opts$overlap_items[2], forms=unique(as.vector(form_map[,-1])), internal_index=TRUE)
  }

  # add constraint: max_use
  if(!is.null(max_use))
    x <- ata_item_use(x, max=max_use)

  x
}


#' @rdname ata
#' @description \code{ata_relative_objective} adds a relative objective to the model
#' @param coef the coefficients of the objective function
#' @param mode optimization direction: 'max' for maximization and 'min' for minimization
#' @param tol the tolerance paraemter
#' @param negative \code{TRUE} when the objective function is expected to be negative
#' @param forms forms where objectives are added. \code{NULL} for all forms
#' @param collapse \code{TRUE} to collapse into one objective function
#' @param internal_index \code{TRUE} to use internal form indices
#' @details
#' \code{ata_obj_relative}:
#' when mode='max', maximize (y-tol), subject to y <= sum(x) <= y+tol;
#' when mode='min', minimize (y+tol), subject to y-tol <= sum(x) <= y.
#' When \code{negative} is \code{TRUE}, y < 0, tol > 0.
#' \code{coef} can be a numeric vector that has the same length with the pool,
#' or a variable name in the pool, or a numeric vector of theta points.
#' When \code{tol} is \code{NULL}, it is optimized; when it's \code{FALSE}, ignored;
#' when it's a number, fixed; when it's a range, constrained with lower and upper bounds.
#' @export
ata_relative_objective <- function(x, coef, mode=c('max', 'min'), tol=NULL, negative=FALSE, forms=NULL, collapse=FALSE, internal_index=FALSE){
  if(class(x) != "ata")
    stop("Not an 'ata' object")

  forms <- ata_get_form_index(x, forms, collapse, internal_index)
  coef <- ata_get_obj_coef(x, coef, compensate=FALSE)
  x$negative <- negative

  # optimization direction
  x$max <- switch(match.arg(mode), "max"=TRUE, "min"=FALSE)
  if(x$max){
    x$obj[x$n_lpvar - 1:0] <- c(1, -1)
  } else {
    x$obj[x$n_lpvar - 1:0] <- c(1,  1)
  }

  # tolerance parameter
  if(!is.null(tol))
    if(length(tol) == 2) {
      x$bounds$idx <- c(x$bounds$idx, x$n_lpvar)
      x$bounds$lb <- c(x$bounds$lb, tol[1])
      x$bounds$ub <- c(x$bounds$ub, tol[2])
    } else if(is.numeric(tol)){
      x$bounds$idx <- c(x$bounds$idx, x$n_lpvar)
      x$bounds$lb <- c(x$bounds$lb, tol)
      x$bounds$ub <- c(x$bounds$ub, tol)
    } else if(!tol) {
      x$obj[x$n_lpvar] <- 0
    }

  # objective for each form
  mat <- matrix(0, nrow=nrow(forms) * nrow(coef) * 2, ncol=x$n_lpvar)
  dir <- rhs <- rep(NA, nrow(forms) * nrow(coef) * 2)
  for(i in 1:nrow(forms)) {
    f <- forms[i, ]
    ind <- outer(1:x$n_items, (f - 1) * x$n_items, "+")
    ind <- as.vector(ind)
    for(j in 1:nrow(coef)) {
      row <- (j - 1) * 2 + (i - 1) * nrow(coef) * 2
      mat[row + 1, ind] <- rep(coef[j, ], length(f))
      mat[row + 2, ind] <- rep(coef[j, ], length(f))
      if(x$max){
        mat[row + 1, (x$n_lpvar-1):x$n_lpvar] <- c(-1, 0)
        mat[row + 2, (x$n_lpvar-1):x$n_lpvar] <- c(-1, -1)
      } else {
        mat[row + 1, (x$n_lpvar-1):x$n_lpvar] <- c(-1, 1)
        mat[row + 2, (x$n_lpvar-1):x$n_lpvar] <- c(-1, 0)
      }
      dir[row + 1:2] <- c(">=", "<=")
      rhs[row + 1:2] <- 0
    }
  }

  ata_append(x, mat, dir, rhs)
}


#' @rdname ata
#' @description \code{ata_absolute_objective} adds an absolute objective to the model
#' @param target the target values of the objective function
#' @param equal_tol \code{TRUE} to force upward and downward tolerance to be equal
#' @param tol_up the range of upward tolerance
#' @param tol_down the range of downward tolerance
#' @details
#' \code{ata_obj_absolute} minimizes y0+y1 subject to t-y0 <= sum(x) <= t+y1.
#' @export
ata_absolute_objective <- function(x, coef, target, equal_tol=FALSE, tol_up=NULL, tol_down=NULL, forms=NULL, collapse=FALSE, internal_index=FALSE){
  if(class(x) != "ata")
    stop("Not an 'ata' object")

  forms <- ata_get_form_index(x, forms, collapse, internal_index)
  coef <- ata_get_obj_coef(x, coef, compensate=FALSE)
  if(length(target) == 1)
    target <- rep(target, nrow(coef))
  if(length(target) != nrow(coef))
    stop("Invalid target length.")

  # optimization direction
  x$max <- FALSE
  x$obj[x$n_lpvar - 1:0] <- 1

  # objective for each form
  mat <- matrix(0, nrow=nrow(forms) * nrow(coef) * 2, ncol=x$n_lpvar)
  dir <- rhs <- rep(NA, nrow(forms) * nrow(coef) * 2)
  for(i in 1:nrow(forms)){
    f <- forms[i,]
    ind <- as.vector(outer(1:x$n_items, (f - 1) * x$n_items, "+"))
    for(j in 1:nrow(coef)){
      row <- (j - 1) * 2 + (i - 1) * nrow(coef) * 2
      mat[row + 1, ind] <- rep(coef[j, ], length(f))
      mat[row + 1, x$n_lpvar - 1] <- 1
      mat[row + 2, ind] <- rep(coef[j, ], length(f))
      mat[row + 2, x$n_lpvar - 0] <- -1
      dir[row + 1:2] <- c(">=", "<=")
      rhs[row + 1:2] <- target[j]
    }
  }

  # tolerance parameters
  if(equal_tol){
    mat <- rbind(mat, c(rep(0, x$n_lpvar - 2), 1, -1))
    dir <- c(dir, '=')
    rhs <- c(rhs, 0)
  }
  if(!is.null(tol_down)){
    if(length(tol_down) != 2)
      stop('Invalid tol_down input. Expect 2 elements.')
    x$bounds$idx <- c(x$bounds$idx, x$n_lpvar - 1)
    x$bounds$lb <- c(x$bounds$lb, tol_down[1])
    x$bounds$ub <- c(x$bounds$ub, tol_down[2])
  }
  if(!is.null(tol_up)){
    if(length(tol_up) != 2)
      stop('Invalid tol_up input. Expect 2 elements.')
    x$bounds$idx <- c(x$bounds$idx, x$n_lpvar)
    x$bounds$lb <- c(x$bounds$lb, tol_up[1])
    x$bounds$ub <- c(x$bounds$ub, tol_up[2])
  }

  ata_append(x, mat, dir, rhs)
}


#' @rdname ata
#' @description \code{ata_constraint} adds a constraint to the model
#' @param level the level of a categorical variable to be constrained
#' @param min the lower bound of the constraint
#' @param max the upper bound of the constraint
#' @details
#' When \code{level} is \code{NA}, it is assumed that the constraint is on
#' a quantitative item property; otherwise, a categorical item property.
#' \code{coef} can be a variable name, a constant, or a numeric vector that has
#' the same size as the pool.
#' @importFrom stats aggregate
#' @export
ata_constraint <- function(x, coef, min=NA, max=NA, level=NULL, forms=NULL, collapse=FALSE, internal_index=FALSE){
  if(class(x) != "ata")
    stop("Not an 'ata' object")

  if(is.na(min) && is.na(max))
    return(x)

  if(!is.na(min) && !is.na(max) && min > max)
    stop("min is greater than max.")

  forms <- ata_get_form_index(x, forms, collapse, internal_index)

  if(length(coef) == x$n_items) { # a numeric vector at the group level
    coef <- coef
  } else if(length(coef) == length(unlist(x$groups))) { # a numeric vector at the item level
    coef <- aggregate(coef, by=list(group=unlist(x$groups)), sum, na.rm=TRUE)[, -1]
  } else if(is.numeric(coef) && length(coef) == 1) { # a constant, assuming it's at the item level
    coef <- rep(coef, length(unlist(x$groups)))
    coef <- aggregate(coef, by=list(group=unlist(x$groups)), sum, na.rm=TRUE)[, -1]
  } else if(is.character(coef) && length(coef) == 1) {
    coef <- Map(function(x, g) {
      if(is.null(x))
        return(numeric(0))
      if(!coef %in% colnames(x))
        return(rep(0, length(unique(g))))
      if(is.na(level) || is.null(level)) {
        x <- x[, coef]
      } else {
        x <- as.integer(x[, coef] == level)
      }
      aggregate(x, by=list(group=g), sum, na.rm=TRUE)[, -1]
    }, x$pool, x$groups)
    coef <- unlist(coef)
  } else {
    stop('Invalid coefficients')
  }

  n <- ifelse(!is.na(min) && !is.na(max) && min != max, nrow(forms) * 2, nrow(forms))
  mat <- matrix(0, nrow=n, ncol=x$n_lpvar)
  dir <- rhs <- rep(NA, n)
  for(i in 1:nrow(forms)) {
    f <- forms[i, ]
    ind <- as.vector(outer(1:x$n_items, (f - 1) * x$n_items, "+"))
    if(!is.na(min) && is.na(max)) {
      mat[i, ind] <- coef
      dir[i] <- ">="
      rhs[i] <- min
    } else if(is.na(min) && !is.na(max)) {
      mat[i, ind] <- coef
      dir[i] <- "<="
      rhs[i] <- max
    } else if(min == max) {
      mat[i, ind] <- coef
      dir[i] <- "="
      rhs[i] <- min
    } else {
      mat[(i - 1) * 2 + 1, ind] <- coef
      mat[(i - 1) * 2 + 2, ind] <- coef
      dir[(i - 1) * 2 + 1:2] <- c(">=", "<=")
      rhs[(i - 1) * 2 + 1:2] <- c(min, max)
    }
  }

  ata_append(x, mat, dir, rhs)
}


#' @rdname ata
#' @description \code{ata_item_use} limits the minimum and maximum usage for items
#' @param items a vector of item indices, \code{NULL} for all items
#' @export
ata_item_use <- function(x, min=NA, max=NA, items=NULL){
  if(class(x) != "ata")
    stop("not an 'ata' object")
  if(is.na(min) && is.na(max))
    stop('min and max are both NA')
  if(is.null(items))
    items <- 1:x$n_items
  if(any(!items %in% 1:x$n_items))
    stop("Invalid items input")

  nitems <- length(items)
  n <- sum(!is.na(min), !is.na(max))
  mat <- matrix(0, nrow=nitems * n, ncol=x$n_lpvar)
  for(i in 1:length(items)) {
    ind <- items[i] + (1:x$n_forms - 1) * x$n_items
    mat[(i - 1) * n + 1:n, ind] <- 1
  }
  if(!is.na(min) && is.na(max)){
    dir <- rep(">=", nitems)
    rhs <- rep(min, nitems)
  } else if(is.na(min) && !is.na(max)){
    dir <- rep("<=", nitems)
    rhs <- rep(max, nitems)
  } else {
    dir <- rep(c(">=", "<="), nitems)
    rhs <- rep(c(min, max), nitems)
  }

  ata_append(x, mat, dir, rhs)
}

#' @rdname ata
#' @description \code{ata_item_enemy} adds an enemy-item constraint to the model
#' @export
ata_item_enemy <- function(x, items){
  if(class(x) != "ata")
    stop("not an 'ata' object")
  if(!all(items %in% 1:x$n_items))
    stop("Invalid item index")

  mat <- matrix(0, nrow=nrow(x$form_map), ncol=x$n_lpvar)
  for(i in 1:nrow(x$form_map)){
    f <- x$form_map[i, ]
    ind <- items + (f - 1) * x$n_items
    mat[i, ind] <- 1
  }
  dir <- rep("<=", x$n_forms)
  rhs <- rep(1, x$n_forms)

  ata_append(x, mat, dir, rhs)
}


#' @rdname ata
#' @description \code{ata_item_fix} forces an item to be selected or not selected
#' @export
ata_item_fix <- function(x, items, min=NA, max=NA, forms){
  if(class(x) != "ata")
    stop("Not an 'ata' object")
  if(any(!items %in% 1:x$n_items))
    stop("Invalid items input.")
  if(length(forms) > 1)
    stop('Fix values in one form at each time.')

  n <- length(items)
  if(length(min) == 1)
    min <- rep(min, n)
  if(length(max) == 1)
    max <- rep(max, n)
  if(length(min) != n || length(max) != n)
    stop("Invalid min or max length.")
  x$bounds$idx <- c(x$bounds$idx, items+(forms-1)*x$n_items)
  x$bounds$lb <- c(x$bounds$lb, min)
  x$bounds$ub <- c(x$bounds$ub, max)

  x
}


#' @rdname ata
#' @description \code{ata_solve} solves the MIP model
#' @param solver use 'lpsolve' for lp_solve 5.5 or 'glpk' for GLPK
#' @param return_format the format of the results: use \code{'form'} to organize results in a list of forms,
#' \code{'model'} to organize results in a list of models, use \code{'simple'} to organize results in
#' data.frame after removing item paraemters.
#' @param silent \code{TRUE} to mute solution information
#' @param time_limit the time limit in seconds passed along to solvers
#' @param message \code{TRUE} to print messages from solvers
#' @return \code{ata_solve} returns a solved \code{ata} object
#' @details
#' \code{ata_solve} takes control options in \code{...}.
#' For lpsolve, see \code{lpSolveAPI::lp.control.options}.
#' For glpk, see \code{glpkAPI::glpkConstants}\cr
#' Once the model is solved, additional data are added to the model.
#' \code{status} shows the status of the solution, \code{optimum}
#' the optimal value of the objective fucntion found in the solution,
#' \code{obj_vars} the values of two critical variables in the objective
#' function, \code{result} the assembly results in a binary matrix, and
#' \code{items} the assembled items
#' @export
ata_solve <- function(x, solver=c('lpsolve', 'glpk'), return_format=c('model', 'form', 'simple'), silent=FALSE, time_limit=10, message=FALSE, ...) {
  if(class(x) != "ata")
    stop("Not an 'ata' object")

  rs <- switch(match.arg(solver, solver),
               'lpsolve'=ata_lpsolve(x, time_limit, message, ...),
               'glpk'=ata_glpk(x, time_limit, message, ...))

  x$code <- rs$code
  x$status <- rs$status
  x$optimum <- rs$optimum
  x$obj_vars <- rs$obj_vars
  x$start_time <- rs$start_time
  x$solve_time <- rs$solve_time

  if(all(rs$result == 0)) {
    x$result <- x$items <- NULL
    if(!silent)
      warning("No solution for the LP model.\n")
  } else {
    if(!silent)
      cat(rs$status, ', optimum: ', round(rs$optimum, 3), ' (', paste(round(rs$obj_vars, 3), collapse=', '), ')\n', sep='')
    x$result <- rs$result
    x$items <- ata_extract_items(x)
    return_format <- match.arg(return_format, return_format)
    if(return_format %in% c('model', 'simple'))
      x$items <- ata_results_to_model(x$items)
    if(return_format == 'simple')
      x$items <- ata_results_to_dataframe(x$items)
  }

  x
}
