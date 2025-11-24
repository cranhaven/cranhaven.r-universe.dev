# ICAR --------------------------------------------------------------------

# Construct an ICAR penalization matrix for a given "dimension" and returns the
# double list args_and_vars that have the args to build a new call to s() and the
# vars necessary for the evaluation of that s() smooth
ICAR <- function(data_frame, boundaries, time, dimension,
                 k, bs, xt, is_spm, unused_names = c("var", "m"), ...) {

  checkmate::assert_class(data_frame, "sf")
  checkmate::assert_class(boundaries, "sspm_discrete_boundary")
  checkmate::assert_character(time)
  checkmate::assert_character(dimension)
  checkmate::assert_choice(dimension, choices = c("time", "space", "space_time"))

  # Recapture the ellipsis again
  args_list <- as.list(match.call(expand.dots = FALSE)$`...`)
  args_list <- args_list[!(names(args_list) %in% unused_names)]

  # ---- TIME ----
  time_levels <- levels(data_frame[[time]])

  # ---- SPACE ----
  # Here we assume the hardcoded convention that the patch column is patch_id
  # (from the discretization)
  space <- "patch_id"
  patches <- spm_patches(boundaries)

  # Setup done ----
  vars <- list()

  # NOTE: Some code duplication is a little bit of a shame but is necessary as
  #       previous attempts at functionalizing this part of the code made
  #       it seem more complicated and less easy to understand.
  if (dimension == "time") {

    out <- list(str2lang(time))

    ret <- ICAR_s_time(time_levels, bs, xt, vars, k, is_spm)
    k <- ret$k
    bs <- ret$bs
    xt <- ret$xt
    vars <- ret$vars
    xt_list <- ret$xt_list

  } else if (dimension == "space") {

    out <- list(str2lang(space))

    ret <- ICAR_s_space(patches, space, bs, xt, vars, k, is_spm)
    k <- ret$k
    bs <- ret$bs
    xt <- ret$xt
    vars <- ret$vars
    xt_list <- ret$xt_list

  } else if (dimension == "space_time") {

    out <- list(str2lang(time), str2lang(space))

    ret <- ICAR_s_space_time(patches, space, time_levels, bs, xt, vars, k, is_spm)
    k <- ret$k
    bs <- ret$bs
    xt <- ret$xt
    vars <- ret$vars
    xt_list <- ret$xt_list
    names(xt_list$xt) <- c(time, space)

  }

  return(list(args = do.call(c,
                             args = list(out,
                                         list(k = k, bs = bs),
                                         xt_list,
                                         args_list)),
              vars = vars))
}

ICAR_s_time <- function(time_levels, bs, xt, vars, k, is_spm){

  n_time_levels <- as.numeric(length(time_levels))

  if (is.null(k)) {
    if (!is_spm) {
      k <- n_time_levels
    }
  }

  if (is.null(bs)) {

    # If no bs specified, go with re
    bs <- "re"

  }

  if (bs == "re"){

    if (is.null(xt)) {

      xt_list <- NULL

    } else {

      if(is.na(xt)){
        xt <- list()
      }

      checkmate::assert_list(xt)

      if (is.null(xt$penalty)) {
        pen_mat_time <- ICAR_time(time_levels)
      } else {
        checkmate::assert_matrix(xt$penalty)
        pen_mat_time <- xt
      }

      pen_expression <- rlang::expr(pen_mat_time)
      vars$pen_mat_time <- pen_mat_time
      xt_list <- list(xt = list(penalty = pen_expression))

    }

  } else if (bs == "mrf"){

    if (is.null(xt)) {

      pen_mat_time <- ICAR_time(time_levels)

    } else {

      checkmate::assert_list(xt)

      if (is.null(xt$penalty)) {
        pen_mat_time <- ICAR_time(time_levels)
      } else {
        checkmate::assert_matrix(xt$penalty)
        pen_mat_time <- xt
      }

    }

    # Create symbol and assign to list
    pen_expression <- rlang::expr(pen_mat_time)
    vars$pen_mat_time <- pen_mat_time
    xt_list <- list(xt = list(penalty = pen_expression))

  }

  return(list(k = k, bs = bs, xt = xt, vars = vars, xt_list = xt_list))
}

ICAR_s_space <- function(patches, space, bs, xt, vars, k, is_spm){

  if (is.null(k)) {
    if (!is_spm) {
      k <- 30
    }
  }

  if (is.null(bs)) {
    bs <- "mrf"
  }

  if (is.null(xt)) {

    pen_mat_space <- ICAR_space(patches, space)

  } else {

    checkmate::assert_list(xt)

    if (is.null(xt$penalty)) {
      pen_mat_space <- ICAR_space(patches, space)
    } else {
      checkmate::assert_matrix(xt$penalty)
      pen_mat_space <- xt
    }

  }

  # Create symbol and assign to list
  pen_expression <- rlang::expr(pen_mat_space)
  vars$pen_mat_space <- pen_mat_space
  xt_list <- list(xt = list(penalty = pen_expression))

  return(list(k = k, bs = bs, xt = xt, vars = vars, xt_list = xt_list))
}

ICAR_s_space_time <- function(patches, space, time_levels, bs, xt, vars, k, is_spm){

  if (length(xt) == 0){
    xt <- list(NULL, NULL)
  }

  checkmate::assert_true(length(bs) == 2)
  checkmate::assert_true(length(xt) == 2)
  checkmate::assert_true(length(k) == 2)

  ret_time <- ICAR_s_time(time_levels, bs[1], xt[[1]], vars, k[1], is_spm)
  ret_space <- ICAR_s_space(patches, space, bs[2], xt[[2]], vars, k[2], is_spm)

  bs <- c(ret_time$bs, ret_space$bs)
  vars <- c(ret_time$vars, ret_space$vars)
  k <- c(ret_time$k, ret_space$k)
  xt <- list(ret_time$xt, ret_space$xt)

  xt_list <- list(xt = list(ret_time$xt_list, ret_space$xt_list))

  if(!is.null(xt_list$xt[[1]])){
    xt_list$xt[[1]] <- list(penalty = str2lang("pen_mat_time"))
  }

  if(!is.null(xt_list$xt[[2]])){
    xt_list$xt[[2]] <- list(penalty = str2lang("pen_mat_space"))
  }

  return(list(k = k, bs = bs, xt = xt, vars = vars, xt_list = xt_list))

}

ICAR_time <- function(time_levels) {

  # Creating an auto-regressive year penalty; this matrix means that the
  # estimate for each year is penalized to be close to the years before and
  # after it

  time_levels <- sort(time_levels)
  # time_levels <- 1979:2018

  n_time_levels <- length(unique(time_levels))

  pen_mat = matrix(0, nrow = n_time_levels, ncol = n_time_levels)
  dimnames(pen_mat) = list(time_levels, time_levels)
  diag(pen_mat[-1, -n_time_levels]) = diag(pen_mat[-n_time_levels, -1]) = -1
  diag(pen_mat) = -(colSums(pen_mat) - diag(pen_mat))

  return(pen_mat)

}

ICAR_space <- function(patches, space) {

  checkmate::assert_choice(space, names(patches))

  patches_adj_mat = suppressAll(sf::st_intersects(patches, sparse = FALSE))
  dimnames(patches_adj_mat) = list(unique(patches[[space]]),
                                   unique(patches[[space]]))
  patches_adj_mat = patches_adj_mat + 0
  diag(patches_adj_mat) = 0
  pen_mat = diag(rowSums(patches_adj_mat)) - patches_adj_mat

  return(pen_mat)

}

# LINPRED -----------------------------------------------------------------

# Construct the lag matrix and associated lag columns for the linear predictor
# method of fitting the smooth

LINPRED <- function(data_frame, boundaries, time, var, k, m,
                    unused_names = c("dimension", "bs", "xt", "is_spm"), ...) {

  checkmate::assert_class(data_frame, "sf")
  checkmate::assert_class(boundaries, "sspm_discrete_boundary")
  checkmate::assert_character(time)

  # Recapture the ellipsis again
  args_list <- as.list(match.call(expand.dots = FALSE)$`...`)
  args_list <- args_list[!(names(args_list) %in% unused_names)]

  # Make the lag and by matrices
  lag_matrix <- make_lag_matrix(data_frame, k, boundaries, time)
  by_matrix <- make_by_matrix(data_frame, k, boundaries, time, var)

  out <- list(str2lang("lag_matrix"))
  vars <- list()
  vars$lag_matrix <- lag_matrix
  vars$by_matrix <- by_matrix

  return(list(args = do.call(c,
                             args = list(out,
                                         list(k = k, m = m,
                                              by = str2lang("by_matrix")),
                                         args_list)),
              vars = vars))

}
