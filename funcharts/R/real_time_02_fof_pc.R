#' Get a list of function-on-function linear regression models estimated
#' on functional data
#' each evolving up to an intermediate domain point.
#'
#' This function produces a list of objects,
#' each of them contains the result of applying \code{\link{fof_pc}} to
#' a functional response variable and multivariate functional covariates
#' evolved up to an intermediate domain point.
#'
#' @param mfdobj_y_list
#' A list created using
#' \code{\link{get_mfd_df_real_time}} or
#' \code{get_mfd_list_real_time}, denoting a list of functional data objects,
#' each evolving up to an intermediate domain point,
#' with observations of the functional response variable.
#' @param mfdobj_x_list
#' A list created using
#' \code{\link{get_mfd_df_real_time}} or
#' \code{get_mfd_list_real_time}, denoting a list of functional data objects,
#' each evolving up to an intermediate domain point,
#' with observations of the multivariate functional covariates.
#' @param tot_variance_explained_x
#' See \code{\link{fof_pc}}.
#' @param tot_variance_explained_y
#' See \code{\link{fof_pc}}.
#' @param tot_variance_explained_res
#' See \code{\link{fof_pc}}.
#' @param components_x
#' See \code{\link{fof_pc}}.
#' @param components_y
#' See \code{\link{fof_pc}}.
#' @param type_residuals
#' See \code{\link{fof_pc}}.
#' @param ncores
#' If you want parallelization, give the number of cores/threads
#' to be used when creating objects separately for different instants.
#'
#' @return
#' A list of lists each produced by \code{\link{fof_pc}},
#' corresponding to a given instant.
#' @export
#'
#' @seealso
#' \code{\link{fof_pc}},
#' \code{\link{get_mfd_df_real_time}},
#' \code{\link{get_mfd_list_real_time}}
#'
#' @examples
#' library(funcharts)
#' data("air")
#' air <- lapply(air, function(x) x[1:10, , drop = FALSE])
#' mfdobj_y_list <- get_mfd_list_real_time(air["NO2"],
#'                                         n_basis = 15,
#'                                         lambda = 1e-2,
#'                                         k_seq = c(0.5, 0.75, 1))
#' mfdobj_x_list <- get_mfd_list_real_time(air[c("CO", "temperature")],
#'                                         n_basis = 15,
#'                                         lambda = 1e-2,
#'                                         k_seq = c(0.5, 0.75, 1))
#' mod_list <- fof_pc_real_time(mfdobj_y_list, mfdobj_x_list)
#'
fof_pc_real_time <- function(mfdobj_y_list,
                             mfdobj_x_list,
                             tot_variance_explained_x = 0.95,
                             tot_variance_explained_y = 0.95,
                             tot_variance_explained_res = 0.95,
                             components_x = NULL,
                             components_y = NULL,
                             type_residuals = "standard",
                             ncores = 1) {

  if (length(mfdobj_y_list) != length(mfdobj_x_list)) {
    stop("x and y lists must have the same length")
  }

  single_k <- function(ii) {

    fof_pc(
      mfdobj_y = mfdobj_y_list[[ii]],
      mfdobj_x = mfdobj_x_list[[ii]],
      tot_variance_explained_x = tot_variance_explained_x,
      tot_variance_explained_y = tot_variance_explained_y,
      tot_variance_explained_res = tot_variance_explained_res,
      components_x = components_x,
      components_y = components_y,
      type_residuals = type_residuals
    )

  }
  if (ncores == 1) {
    mod_list <- lapply(seq_along(mfdobj_y_list), single_k)
  } else {
    if (.Platform$OS.type == "unix") {
      mod_list <- parallel::mclapply(seq_along(mfdobj_y_list),
                                     single_k,
                                     mc.cores = ncores)
    } else {
      cl <- parallel::makeCluster(ncores)
      parallel::clusterExport(cl,
                              c("mfdobj_y_list",
                                "mfdobj_x_list",
                                "tot_variance_explained_x",
                                "tot_variance_explained_y",
                                "tot_variance_explained_res",
                                "components_x",
                                "components_y",
                                "type_residuals"),
                              envir = environment())
      mod_list <- parallel::parLapply(cl, seq_along(mfdobj_y_list), single_k)
      parallel::stopCluster(cl)
    }
  }

  names(mod_list) <- names(mfdobj_y_list)

  mod_list

}
