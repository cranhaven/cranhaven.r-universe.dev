
#' Parametric bootstrap for rescaling a la the vander zanden appendix
#'
#' This is a vectorized and \strong{much} more efficient implementation
#' of the original \code{rescale} function from the Vander Zanden appendix.
#' It takes the output of \code{\link{group_birds_by_location}} directly
#' and does the parametric bootstrapping for vza_rescale_reps samples.
#' @param SBL the data frame that summarizes the isotope feather data and
#' isoscape predictions at each location. This must have columns of \code{cnt},
#' \code{meanH}, \code{sdH}, \code{meaniso}, \code{sdiso}
#' @param vza_rescale_reps Number of simulated regressions to do.  Default is 1000.
#' @return Returns a matrix with vza_rescale_reps rows.  Column 1 is "intercepts" and column
#' two is "slopes"
#' @keywords internal
vza_rescale <- function(SBL, vza_rescale_reps = 1000) {

  D <- as.data.frame(SBL) # make sure to un-tbl-df it if need be

  # make a list of matrices that hold simulated values for tissue  that
  # we will bind together into one big matrix.  The rows are the simulated
  # isotope values from the birds and the columns are the vza_rescale_reps
  tmp <- lapply(1:nrow(D), function(i) {
    n <- D[i,"cnt"]
    stats::rnorm(n * vza_rescale_reps, mean = D[i, "meanH"], sd = D[i, "sdH"]) %>%
      matrix(ncol = vza_rescale_reps)
  })
  tissue_mat <- do.call(rbind, tmp)

  # do the same for the precip values
  tmp <- lapply(1:nrow(D), function(i) {
    n <- D[i,"cnt"]
    stats::rnorm(n * vza_rescale_reps, mean = D[i, "meaniso"], sd = D[i, "sdiso"]) %>%
      matrix(ncol = vza_rescale_reps)
  })
  precip_mat <- do.call(rbind, tmp)

  # now we just do the regression of each column (1,...,vza_rescale_reps) of the tissue_mat and the precip_mat
  # and we grab the slopes and intercepts and return them as a data frame
  lapply(1:ncol(tissue_mat), function(i) {
    stats::lm(tissue_mat[,i] ~ precip_mat[,i]) %>%
      stats::coef() %>%
      unname()
  }) %>%
    unlist() %>%
    matrix(ncol = 2, byrow = TRUE) %>%
    as.data.frame() %>%
    stats::setNames(c("intercepts", "slopes")) %>%
    dplyr::tbl_df()
}


#' calculate a raster of mean and variance of expected tissue isotopes from precip data and resampled regressions
#'
#' This is a rewrite of the function \code{raster.conversion} from the Vander Zanden
#' appendix.  They expressed things in terms of the standard deviations, but they need to
#' be turned into variances, anyway, so that is what we've done here.  Following the notation
#' of the paper on Wilson's warbler, this function computes $tilde{T}^{(mu)}$ (returned as
#' list component \code{mean.raster}) and $R^{(sigma^2)}$ (returned as list component
#' \code{var.raster})
#' @param iso_raster the raster of isotope precipitation values, for example, like that
#' produced by \code{\link{isomap2raster}}.
#' @param si  slopes and intercepts from the resampled regressions.  This is a data frame
#' with columns named "slopes" and "intercepts" like that returned by \code{\link{vza_rescale}}
#' @keywords internal
vza_mean_and_var_rasters <- function(iso_raster, si) {

  # note, with really large rasters, we may end up wanting to write them to disk, but
  # I really doubt it for most of our work...
  stopifnot("slopes" %in% names(si),  "intercepts" %in% names(si))

  slopes <- si$slopes
  intercepts <- si$intercepts

  # make a list of rasters of tissue isotopes predicted by the resampled regressions:
  rlist <- lapply(seq_along(slopes), function(i) {
    iso_raster * slopes[i] + intercepts[i]
  })

  # make a RasterStack
  rstack <- raster::stack(rlist)

  # return the mean and var of those:
  list(
    mean.raster = iso_raster * mean(slopes) + mean(intercepts),
    var.raster = raster::stackApply(rstack, fun = stats::var, indices = 1)
  )
}



#' assign posterior probability of origin for a bird in each cell in the raster
#'
#' This is a rewrite of the function \code{assignment} from the Vander
#' Zanden appendix code.
#' @param rescale_mean the tissue specific mean raster, such as the mean.raster
#' component of the output of \code{vza_mean_and_var_rasters}.
#' @param rescale_var tissue specific raster of standard deviations, such as the var.raster
#' component of the output of \code{vza_mean_and_var_rasters}.
#' @param precip_sd SD raster associated with the IsoMAP output.
#' This is the precip component of the variance term.
#' @param sd_indiv the individual component of the variance term.
#' This is a value to be determined by the user.  The standard approach is to
#' use the mean of the SDs observed among individuals at all of the calibration sites.
#' @param bird_iso a single value giving the isotope ratio found in the individual's feather.
#' @details This is a fairly low-level function.  It returns a raster of posterior probs (they are
#' scaled to sum to one over all cells).
#' @keywords internal
vza_assign <- function(rescale_mean,
                       rescale_var,
                       precip_sd,
                       sd_indiv,
                       bird_iso
                       ) {

  # first we compute the tilde{T}'s.  call them Tmu and Tsig
  Tmu <- rescale_mean    # no extra conversion to be done here
  Tsig <- rescale_var + precip_sd^2 + sd_indiv^2

  # now copy Tmu to get a raster of the right dimensions to returns then set
  # its cell values as normal densities.
  ret <- Tmu
  raster::values(ret) <- stats::dnorm(x = bird_iso, mean = raster::values(Tmu), sd = sqrt(raster::values(Tsig)))
  ret / raster::cellStats(ret, sum)
}

