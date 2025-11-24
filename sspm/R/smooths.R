#' sspm Smoothing functions
#'
#' A full sspm formula contains calls to the smoothing terms `smooth_time()`,
#' `smooth_space()`, `smooth_space_time()`.
#'
#' @param data_frame **\[sf data.frame\]** The data.
#' @param time **\[character\]** The time column.
#' @param var **\[symbol\]** Variable (only for smooth_lag).
#' @param type **\[character\]** Type of smooth, currently only "ICAR" is
#'     supported.
#' @param k **\[numeric\]** Size of the smooths and/or size of the lag.
#' @inheritParams spm_smooth
#' @inheritParams mgcv::s
#' @param is_spm Whether or not an SPM is being fitted (used internally)
#'
#' @return
#' A list of 2 lists:
#' * `args`, contains the arguments to be passed on to the mgcv smooths
#' * `vars`, contains variables relevant to the evaluation of the smooth.
#'
#' @examples
#' \dontrun{
#' # Not meant to be used directly
#' smooth_time(borealis_data, bounds_voronoi, time = "year")
#' }
#'
#' @rdname smooths
#' @export
setGeneric(name = "smooth_time",
           def = function(data_frame,
                          boundaries,
                          time,
                          type = "ICAR",
                          k = NULL,
                          bs = "re",
                          xt = NA,
                          is_spm = FALSE,
                          ...) {
             standardGeneric("smooth_time")
           }
)

#' @export
#' @rdname smooths
setGeneric(name = "smooth_space",
           def = function(data_frame,
                          boundaries,
                          time,
                          type = "ICAR",
                          k = NULL,
                          bs = "mrf",
                          xt = NULL,
                          is_spm = FALSE,
                          ...) {
             standardGeneric("smooth_space")
           }
)

#' @export
#' @rdname smooths
setGeneric(name = "smooth_space_time",
           def = function(data_frame,
                          boundaries,
                          time,
                          type = "ICAR",
                          k = c(NA, 30),
                          bs = c("re", "mrf"),
                          xt = list(NA, NULL),
                          is_spm = FALSE,
                          ...) {
             standardGeneric("smooth_space_time")
           }
)

#' @export
#' @rdname smooths
setGeneric(name = "smooth_lag",
           def = function(var,
                          data_frame,
                          boundaries,
                          time,
                          type = "LINPRED",
                          k = 5,
                          m = 1,
                          ...) {
             standardGeneric("smooth_lag")
           }
)

# Methods -----------------------------------------------------------------

#' @export
#' @rdname smooths
setMethod(f = "smooth_time",
          signature(data_frame = "sf",
                    boundaries = "sspm_discrete_boundary"),
          function(data_frame, boundaries, time, type, k, bs, xt, is_spm, ...) {

            args_list <- as.list(match.call(expand.dots = FALSE)$`...`)

            do.call(smooth_routine,
                    append(list(dimension = "time", var = NULL, data_frame = data_frame,
                                boundaries = boundaries, time = time,
                                type = type, k = k, m = NULL, bs = bs, xt = xt,
                                is_spm = is_spm, smooth_type = "s"),
                           args_list))

          }
)

#' @export
#' @rdname smooths
setMethod(f = "smooth_space",
          signature(data_frame = "sf",
                    boundaries = "sspm_discrete_boundary"),
          function(data_frame, boundaries, time, type, k, bs, xt, is_spm, ...) {

            args_list <- as.list(match.call(expand.dots = FALSE)$`...`)

            do.call(smooth_routine,
                    append(list(dimension = "space", var = NULL, data_frame = data_frame,
                                boundaries = boundaries, time = time,
                                type = type, k = k, m = NULL, bs = bs, xt = xt,
                                is_spm = is_spm, smooth_type = "s"),
                           args_list))

          }
)

#' @export
#' @rdname smooths
setMethod(f = "smooth_space_time",
          signature(data_frame = "sf",
                    boundaries = "sspm_discrete_boundary"),
          function(data_frame, boundaries, time, type, k, bs, xt, is_spm, ...) {

            args_list <- as.list(match.call(expand.dots = FALSE)$`...`)

            do.call(smooth_routine,
                    append(list(dimension = "space_time", var = NULL,
                                data_frame = data_frame, boundaries = boundaries,
                                time = time, type = type, k = k, m = NULL, bs = bs,
                                xt = xt, is_spm = is_spm, smooth_type = "ti"),
                           args_list))

          }
)

#' @export
#' @rdname smooths
setMethod(f = "smooth_lag",
          signature(data_frame = "sf",
                    boundaries = "sspm_discrete_boundary"),
          function(var, data_frame, boundaries, time, type, k, m, ...) {

            args_list <- as.list(match.call(expand.dots = FALSE)$`...`)

            do.call(smooth_routine,
                    append(list(dimension = NULL, var = var, data_frame = data_frame,
                                boundaries = boundaries, time = time,
                                type = type, k = k, m = m, bs = NULL, xt = NULL,
                                is_spm = NULL, smooth_type = "s"),
                           args_list))

          }
)

# Routine -----------------------------------------------------------------

smooth_routine <- function(dimension, var, data_frame, boundaries, time,
                           type, k, m, bs, xt, is_spm, smooth_type, ...){

  # Get args from ellipsis for extra args: this form is necessary for
  # capturing symbols as well
  args_list <- as.list(match.call(expand.dots = FALSE)$`...`)

  # Get the default arguments for the smooth type used
  args_and_vars <- do.call(dispatch_smooth(type),
                           append(list(dimension = dimension,
                                       var = var,
                                       data_frame = data_frame,
                                       boundaries = boundaries,
                                       time = time,
                                       k = k, m = m,
                                       bs = bs, xt = xt,
                                       is_spm = is_spm),
                                  args_list))

  # Assemble the smooths
  string_smooth <- assemble_smooth(smooth_type, args_and_vars$args)
  ret_list <- list(smooth = string_smooth,
                   vars = args_and_vars$vars)

  if (!is.null(var)){
    ret_list$var_smooth_lag <- var
  } else {
    ret_list$var_smooth_lag <- NULL
  }

  return(ret_list)

}
