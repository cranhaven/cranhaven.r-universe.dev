#' Get a list of multivariate functional principal component analysis
#' models estimated on functional data
#' each evolving up to an intermediate domain point.
#'
#' This function produces a list of objects,
#' each of them contains the result of applying \code{\link{pca_mfd}} to
#' a multivariate functional data object
#' evolved up to an intermediate domain point.
#'
#' @param mfdobj_list
#' A list created using
#' \code{\link{get_mfd_df_real_time}} or
#' \code{get_mfd_list_real_time}, denoting a list of functional data objects,
#' each evolving up to an intermediate domain point,
#' with observations of the multivariate functional data.
#' @param scale
#' See \code{\link{pca_mfd}}.
#' @param nharm
#' See \code{\link{pca_mfd}}.
#' @param ncores
#' If you want parallelization, give the number of cores/threads
#' to be used when creating objects separately for different instants.
#'
#' @return
#' A list of lists each produced by \code{\link{pca_mfd}},
#' corresponding to a given instant.
#' @export
#'
#' @seealso
#' \code{\link{pca_mfd}}
#'
#' @examples
#' library(funcharts)
#' data("air")
#' air <- lapply(air, function(x) x[1:10, , drop = FALSE])
#' mfdobj_list <- get_mfd_list_real_time(air[c("CO", "temperature")],
#'                                       n_basis = 15,
#'                                       lambda = 1e-2,
#'                                       k_seq = seq(0.25, 1, length = 5))
#' mod_list <- pca_mfd_real_time(mfdobj_list)
#'
pca_mfd_real_time <- function(mfdobj_list,
                              scale = TRUE,
                              nharm = 20,
                              ncores = 1) {

  single_k <- function(ii) {

    pca_mfd(mfdobj = mfdobj_list[[ii]],
            scale = scale,
            nharm = nharm)

  }
  if (ncores == 1) {
    mod_list <- lapply(seq_along(mfdobj_list), single_k)
  } else {
    if (.Platform$OS.type == "unix") {
      mod_list <- parallel::mclapply(seq_along(mfdobj_list),
                                     single_k,
                                     mc.cores = ncores)
    } else {
      cl <- parallel::makeCluster(ncores)
      parallel::clusterExport(cl,
                              c("mfdobj_list",
                                "scale",
                                "nharm"),
                              envir = environment())
      mod_list <- parallel::parLapply(cl, seq_along(mfdobj_list), single_k)
      parallel::stopCluster(cl)
    }
  }

  names(mod_list) <- names(mfdobj_list)

  mod_list

}
