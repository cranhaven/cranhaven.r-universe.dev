#' Get a list of scalar-on-function linear regression models estimated
#' on functional data
#' each evolving up to an intermediate domain point.
#'
#' This function produces a list of objects,
#' each of them contains the result of applying \code{\link{sof_pc}} to
#' a scalar response variable and multivariate functional covariates
#' evolved up to an intermediate domain point.
#' See Capezza et al. (2020) for additional details on real-time monitoring.
#'
#' @param y
#' A numeric vector containing the observations of
#' the scalar response variable.
#' @param mfd_real_time_list
#' A list created using
#' \code{\link{get_mfd_df_real_time}} or
#' \code{get_mfd_list_real_time}, denoting a list of functional data objects,
#' each evolving up to an intermediate domain point,
#' with observations of the multivariate functional covariates.
#' @param single_min_variance_explained
#' See \code{\link{sof_pc}}.
#' @param tot_variance_explained
#' See \code{\link{sof_pc}}.
#' @param selection
#' See \code{\link{sof_pc}}.
#' @param components
#' See \code{\link{sof_pc}}.
#' @param ncores
#' If you want parallelization, give the number of cores/threads
#' to be used when creating objects separately for different instants.
#'
#' @return
#' A list of lists each produced by \code{\link{sof_pc}},
#' corresponding to a given instant.
#' @export
#'
#' @seealso
#' \code{\link{sof_pc}},
#' \code{\link{get_mfd_df_real_time}},
#' \code{\link{get_mfd_list_real_time}}
#'
#' @references
#' Capezza C, Lepore A, Menafoglio A, Palumbo B, Vantini S. (2020)
#' Control charts for
#' monitoring ship operating conditions and CO2 emissions
#' based on scalar-on-function regression.
#' \emph{Applied Stochastic Models in Business and Industry},
#' 36(3):477--500. <doi:10.1002/asmb.2507>
#' @examples
#' library(funcharts)
#' data("air")
#' air <- lapply(air, function(x) x[1:10, , drop = FALSE])
#' mfdobj_list <- get_mfd_list_real_time(air[c("CO", "temperature")],
#'                                       n_basis = 15,
#'                                       lambda = 1e-2,
#'                                       k_seq = c(0.5, 0.75, 1))
#' y <- rowMeans(air$NO2)
#' mod_list <- sof_pc_real_time(y, mfdobj_list)
#'
sof_pc_real_time <- function(y,
                             mfd_real_time_list,
                             single_min_variance_explained = 0,
                             tot_variance_explained = 0.9,
                             selection = "PRESS",
                             components = NULL,
                             ncores = 1) {

  single_k <- function(ii) {

    sof_pc(
      y = y,
      mfdobj_x = mfd_real_time_list[[ii]],
      selection = selection,
      single_min_variance_explained = single_min_variance_explained,
      tot_variance_explained = tot_variance_explained,
      components = components
    )

  }

  if (ncores == 1) {
    mod_list <- lapply(seq_along(mfd_real_time_list), single_k)
  } else {
    if (.Platform$OS.type == "unix") {
      mod_list <- parallel::mclapply(seq_along(mfd_real_time_list),
                                     single_k,
                                     mc.cores = ncores)
    } else {
      cl <- parallel::makeCluster(ncores)
      parallel::clusterExport(cl,
                              c("y",
                                "mfd_real_time_list",
                                "selection",
                                "single_min_variance_explained",
                                "tot_variance_explained",
                                "components"),
                              envir = environment())
      mod_list <- parallel::parLapply(cl, seq_along(mfd_real_time_list), single_k)
      parallel::stopCluster(cl)
    }
  }

  names(mod_list) <- names(mfd_real_time_list)

  mod_list

}
