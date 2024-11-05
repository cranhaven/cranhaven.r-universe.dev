#' Add random effects to a data frame
#'
#' @param .data the data frame
#' @param .by the grouping column (groups by row if NULL)
#' @param ... the name and standard deviation of each random effect
#' @param .cors the correlations among multiple random effects, to be passed to \code{\link{rnorm_multi}} as r
#' @param .empirical logical. To be passed to \code{\link{rnorm_multi}} as empirical
#'
#' @return data frame with new random effects columns
#' @export
#'
#' @examples
#' add_random(rater = 2, stimulus = 2, time = 2) %>%
#'   add_ranef("rater", u0r = 1.5) %>%
#'   add_ranef("stimulus", u0s = 2.2, u1s = 0.75, .cors = 0.5) %>%
#'   add_ranef(c("rater", "stimulus"), u0sr = 1.2)
add_ranef <- function(.data, .by = NULL, ..., .cors = 0, .empirical = FALSE) {
  if (is.null(.by)) {
    .by <- names(.data)
    grps <- .data
  } else {
    grps <- unique(.data[.by])
  }
  sd <- c(...)
  
  ranefs <- faux::rnorm_multi(
    n = nrow(grps),
    sd = sd,
    vars = length(sd),
    r = .cors,
    empirical = .empirical
  ) %>%
    dplyr::bind_cols(grps)
  
  dplyr::left_join(.data, ranefs, by = .by)
}

#' Recode a categorical column
#'
#' @param .data the data frame
#' @param .col the column to recode
#' @param .newcol the name of the recoded column (defaults to col.c)
#' @param ... coding for categorical column
#'
#' @return data frame with new fixed effects columns
#' @export
#'
#' @examples
#' add_random(subj = 4, item = 4) %>%
#'   add_between("subj", cond = c("cntl", "test")) %>%
#'   add_recode("cond", "cond.t", cntl = 0, test = 1)
add_recode <- function(.data, .col, .newcol = paste0(col, ".c"), ...) {
  .data[.newcol] <- list(.x = .data[[.col]]) %>%
      c(list(...)) %>%
      do.call(dplyr::recode, .)

  .data
}

#' Add random factors to a data structure
#'
#' @param .data the data frame
#' @param ... the new random factor column name and the number of values of the random factor (if crossed) or the n per group (if nested); can be a vector of n per group if nested
#' @param .nested_in the column(s) to nest in (if NULL, the factor is crossed with all columns)
#'
#' @return a data frame
#' @export
#'
#' @examples
#' # start a data frame
#' data1 <- add_random(school = 3)
#' # nest classes in schools (2 classes per school)
#' data2 <- add_random(data1, class = 2, .nested_in = "school")
#' # nest pupils in each class (different n per class)
#' data3 <- add_random(data2, pupil = c(20, 24, 23, 21, 25, 24), .nested_in = "class")
#' # cross each pupil with 10 questions
#' data4 <- add_random(data3, question = 10)
#' 
#' # compare nesting in 2 different factors
#' data <- add_random(A = 2, B = 2)
#' add_random(data, C = 2, .nested_in = "A")
#' add_random(data, C = 2, .nested_in = "B")
#' 
#' # specify item names
#' add_random(school = c("Hyndland Primary", "Hyndland Secondary")) %>%
#'   add_random(class = list(paste0("P", 1:7),
#'                           paste0("S", 1:6)),
#'              .nested_in = "school")
add_random <- function(.data = NULL, ..., .nested_in = NULL) {
  grps <- list(...)

  if (is.null(.nested_in)) {
    # create IDs
    ids <- mapply(function(grp, nm) {
      if (length(grp) == 1 && is.numeric(grp)) {
        make_id(n = grp, prefix = nm)
      } else {
        grp
      }
    }, grps, names(grps), SIMPLIFY = FALSE)
    ranfacs <- do.call(tidyr::crossing, ids)
    .mydata <- .data # stops rlang_data_pronoun warning
    new_data <- tidyr::crossing(.mydata, ranfacs)
  } else {
    if (length(grps) > 1) {
      stop("You can only add 1 nested random factor at a time")
    }
    name <- names(grps)[[1]]
    n <- grps[[1]]
    ingrps <- unique(.data[.nested_in])
    if (length(n) == 1) n <- rep(n, nrow(ingrps))
    if (length(n) != nrow(ingrps)) {
      stop("n must be a single integer or a vector ", 
           "with the same length as the number of unique values in ", 
           .nested_in)
    }
    
    if (is.list(n)) {
      all_ids <- unlist(n)
      n <- sapply(n, length)
    } else {
      all_ids <- make_id(sum(n), prefix = names(grps)[[1]])
    }
    
    ids <- data.frame(
      .row = rep(1:nrow(ingrps), times = n),
      y = all_ids
    )
    names(ids)[2] <- name
    ingrps[".row"] <- 1:nrow(ingrps)
    newdat <- dplyr::left_join(ingrps, ids, by = ".row")
    newdat[".row"] <- NULL
    
    new_data <- dplyr::right_join(.data, newdat, by = .nested_in)
  }

  new_data
}
  
#' Add between factors
#'
#' @param .data the data frame
#' @param .by the grouping column (groups by row if NULL)
#' @param ... the names and levels of the new factors
#' @param .shuffle whether to assign cells randomly or in "order"
#' @param .prob probability of each level, equal if NULL
#'
#' @return data frame
#' @export
#'
#' @examples
#' add_random(subj = 4, item = 2) %>%
#'   add_between("subj", condition = c("cntl", "test")) %>%
#'   add_between("item", version = c("A", "B"))
add_between <- function(.data, .by = NULL, ..., .shuffle = FALSE, .prob = NULL) {
  if (is.null(.by)) {
    .by <- names(.data)
    grps <- .data
  } else {
    grps <- unique(.data[.by])
  }
  
  if(isTRUE(.shuffle)) grps <- grps[sample(1:nrow(grps)), ]
  
  if (is.null(.prob)) {
    # equal probability for each level
    # return as equal combos as possible 
    vars <- list(...) %>%
      mapply(factor_char, ., SIMPLIFY = FALSE) %>%
      do.call(tidyr::crossing, .)
      
    for (v in names(vars)) {
      grps[v] <- rep_len(vars[[v]], nrow(grps))
    }
  } else {
    # set prob for each level
    vars <- list(...) %>% mapply(factor_char, ., SIMPLIFY = FALSE)
    exact_prob <- (sum(unlist(.prob)) == nrow(grps))
    crossed_vars <- do.call(tidyr::crossing, vars)
    
    if (exact_prob && nrow(crossed_vars) == length(.prob)) {
      grps <- crossed_vars %>%
        lapply(rep, times = .prob) %>%
        as.data.frame() %>%
        cbind(grps, .)
    } else {
      warn <- FALSE
      for (v in names(vars)) {
        p <- if (is.na(.prob[v]) || is.null(.prob[[v]])) unlist(.prob) else .prob[[v]]
        p <- rep_len(p, length(vars[[v]]))
        
        if (sum(p) == nrow(grps)) {
          if (!isTRUE(.shuffle) && length(vars) > 1) warn <- TRUE
          grps[v] <- rep(vars[[v]], p)
        } else {
          # randomly sample
          grps[v] <- sample(vars[[v]], nrow(grps), T, prob = p)
        }
      }
      
      if (warn) {
        warning("Allocation can be confounded with exact probabilities and no shuffling. Alternatively, you can specify an exact probability for each cell, e.g.:\n    .prob = c(A1_B1 = 10, A1_B2 = 20, A2_B1 = 30, A2_B2 = 40)")
      }
      
    }
  }
  
  dplyr::left_join(.data, grps, by = .by)
}

#' Add within factors
#'
#' @param .data the data frame
#' @param .by the grouping column (groups by row if NULL)
#' @param ... the names and levels of the new factors
#'
#' @return data frame
#' @export
#'
#' @examples
#' add_random(subj = 2, item =  2) %>%
#'   add_within("subj", time = c("pre", "post"))
add_within <- function(.data, .by = NULL, ...) {
  if (is.null(.by)) {
    .by <- names(.data)
    grps <- .data
  } else {
    grps <- unique(.data[.by])
  }
  
  # make vars factors, keep original order
  vars <- list(...) %>% mapply(factor_char, ., SIMPLIFY = FALSE)
  
  newdat <- c(list(grps), vars) %>%
    do.call(tidyr::crossing, .)
  
  dplyr::left_join(.data, newdat, by = .by)
}

# convert only character vectors to factors
factor_char <- function(x) {
  if (is.character(x)) {
    factor(x, x)
  } else {
    x
  }
}
