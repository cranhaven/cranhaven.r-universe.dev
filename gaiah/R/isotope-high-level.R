

#' compute posterior probabilities of origin given isotope values
#'
#' This function automates the whole process described in the appendix of
#' Vander Zanden et al. (2014) for computing the posterior probability of
#' origin of an individual (or group of individuals) given its stable isotope
#' values, and those of a set of reference individuals, and an ISOMAP prediction
#' of isotope values across a landscape.
#'
#' For details see:
#'
#' Vander Zanden HB, Wunder MB, Hobson KA, Van Wilgenburg SL, Wassenaar LI, Welker JM, Bowen GJ (2014)
#' Contrasting assignment of migratory organisms to geographic origins using long-term versus year-specific
#' precipitation isotope maps. Methods in Ecology and Evolution, 5, 891-900.
#'
#' And the re-explanation of that method in Ruegg et al. (2017).
#' @param isoscape the data frame read in from "prediction.txt" from ISOMAP. The
#' latitude column must be named "lat" and the longitude column must be named "long".  You have to choose
#' which columns to use with the parameters \code{isoscape_pred_column} and \code{isoscape_sd_column}.
#' @param isoscape_pred_column the name of the column in \code{isoscape} to be used as the prediction (default
#' is "predkrig").
#' @param isoscape_sd_column the name of the column in \code{isoscape} to be used as the standard deviation (default
#' is "stdkrig").
#' @param ref_birds a data frame of reference birds. This should have (at least) columns of "ID"
#' (for unique identifiers for each bird), "lat", "long",
#' "Isotope.Value" and "Location".  The "Location" column will be used to group samples for
#' the Vander Zanden Rescaling.
#' @param assign_birds  A data frame of birds whose breeding origins are to be inferred.  These must have
#' at a minimum the column "ID" (for uniqe identifiers for the birds) and the column "Isotope.Value". This can
#' be left NULL if there are no birds of unknown origin to assign (for example if you are performing cross-validation
#' on the ref_birds).
#' @param self_assign if TRUE, then the birds in \code{ref_birds} will each have posterior surfaces computed for
#' them using a leave one out procedure (i.e. each bird in turn is left out while rescaling the precip isomap to
#' a tissue isomap).  Should not be TRUE if assign_birds is non NULL.
#' @export
#' @examples
#' # obtain posterior probability rasters for the first 2 birds in the migrant_wiwa_isotopes
#' # data set.  This takes about 15 seconds on my laptop (most of that time is preparatory---once
#' # that is done, each bird goes much faster). So we don't run it here.
#' \dontrun{
#' birds2 <- isotope_posterior_probs(isoscape = isomap_job54152_prediction,
#'                         ref_birds = breeding_wiwa_isotopes,
#'                         assign_birds = migrant_wiwa_isotopes[1:2,]
#'                         )
#' }
#'
#' # However, you can load the results as a saved data object to see what they look like:
#' birds2 <- example_isotope_posteriors
#'
#'
#' # Since the ref_birds above were separate from the migrant birds, no leave-one-out was
#' # performed.  Hence birds2$loo_results is NULL, and all the results are in
#' # birds2$regular.
#'
#' # Look at the names of the resulting output for the first bird:
#' names(birds2$regular[[1]])
#'
#' names(birds2$regular[[1]]$assignment_parameters)
#'
#'
#'
#' # If you want to do self-assignment for a whole bunch of reference birds, it takes much longer.
#' # It looks like this:
#' \dontrun{
#' self_ass <- isotope_posterior_probs(isoscape = isomap_job54152_prediction,
#'                                     ref_birds = breeding_wiwa_isotopes,
#'                                     self_assign = TRUE)
#' }
isotope_posterior_probs <- function(isoscape,
                                    ref_birds,
                                    assign_birds = NULL,
                                    isoscape_pred_column = "predkrig",
                                    isoscape_sd_column = "stdkrig",
                                    self_assign = FALSE
) {

  if(is.null(assign_birds) && self_assign == FALSE) stop("self_assign cannot be FALSE without passing in a data frame of assign_birds")

  # set some defaults
  loo_ass <- NULL
  regular <- NULL

  # I think I can get away with not really doing this till later, but if I have to do it
  # repeatedly, it will be better to get that done here and use the variables later
  isoscape_prediction <- isomap2raster(isoscape, isoscape_pred_column)
  isoscape_std_err <- isomap2raster(isoscape, isoscape_sd_column)


  # put the isoscape predictions at each ref_birds sampling location
  ref_birds_with_isoscape_vals <- extract_isopredictions(isoscape = isoscape,
                                                         birds = ref_birds,
                                                         pred = isoscape_pred_column,
                                                         sd = isoscape_sd_column)


  #### Here is that block where we do the leave-one-out assignment for each bird in ref_birds
  if(self_assign == TRUE) {
    birds_vec <- ref_birds_with_isoscape_vals$ID
    names(birds_vec) <- birds_vec

    loo_ass <- lapply(birds_vec, function(bird) {
      globN <- which(birds_vec == bird)
      message("Doing leave-one-out isotope procedure for individual ", bird, ". Number ", globN, " of ", length(birds_vec))
      # drop the focal bird from the reference data set
      loo_birds <- ref_birds_with_isoscape_vals %>%
        dplyr::filter(ID != bird)

      # compute the rescaling parameters for bird
      ass_pars <- private_rescale(loo_birds, isoscape_prediction, isoscape_std_err)

      # now do the assignment
      bird_ass <- vza_assign(rescale_mean = ass_pars$Tilde_T_mu,
                                   rescale_var = ass_pars$R_sigma_squared,
                                   precip_sd = isoscape_std_err,
                                   sd_indiv = ass_pars$sd_indiv,
                                   bird_iso = ref_birds_with_isoscape_vals$Isotope.Value[ref_birds_with_isoscape_vals$ID == bird]
                                   )

      # now return the assignment parameters and also the posterior prob
      list(
        posterior_probs = bird_ass,
        assignment_parameters = ass_pars
      )
    })
  }  else {  # if we aren't doing leave one out then we just compute the rescaling for the ref birds all together and there is no LOO
    birds_vec <- assign_birds$ID
    names(birds_vec) <- birds_vec

    # compute the rescaling parameters from all the ref birds
    ass_pars <- private_rescale(ref_birds_with_isoscape_vals, isoscape_prediction, isoscape_std_err)

    # then lapply over the birds and assign them
    regular <- lapply(birds_vec, function(bird) {
      globN <- which(birds_vec == bird)
      message("Doing (NOT-leave-one-out) isotope procedure for individual ", bird, ". Number ", globN, " of ", length(birds_vec))

      # now do the assignment
      bird_ass <- vza_assign(rescale_mean = ass_pars$Tilde_T_mu,
                                    rescale_var = ass_pars$R_sigma_squared,
                                    precip_sd = isoscape_std_err,
                                    sd_indiv = ass_pars$sd_indiv,
                                    bird_iso = assign_birds$Isotope.Value[assign_birds$ID == bird]
      )

      # now return the assignment parameters and also the posterior prob
      list(
        posterior_probs = bird_ass,
        assignment_parameters = ass_pars
      )
    })
  }

  # later on we will return other thigns in this list too.
  list(
    loo_results = loo_ass,
    regular = regular
  )
}
if(getRversion() >= "2.15.1")  utils::globalVariables(c("n", "Location", "ID", "<<-")) # keep R CMD Check from making Notes

#' internal function for isotope machinations
#'
#' does these things:
#' 1. removes birds in Locations that have only 1 bird in them (printing a warning message while it does so)
#' 2. does the vza rescaling process and returns the output that is needed for the vza_assign
#'    function.  (basically the ouput of the mean and var rasters). Should return a-bar and b-bar
#'    too --- all of that in a big list.
#' Then with that output I can pump it all into vza_assign for a particular (left-out) ref_bird or for
#' an assign bird.  I should return the fitted model for each one.
#' @param birds  a data frame like "ref_birds" in \code{isotope_posterior_probs} after the isopredictions
#' have been attached to it.
#' @param pred  The raster of isoscape predictions
#' @param std The raster of isoscape standard deviations
#' @keywords internal
private_rescale <- function(birds, pred, std) {

    # remove locations that have only one bird in them
    birds_tossed <- birds %>%
      dplyr::group_by(Location) %>%
      dplyr::filter(n() > 1)

    # group birds by location
    bird_groups <- group_birds_by_location(D = birds_tossed, feather_isotope_col = "Isotope.Value", location_col = "Location")

    # do the vza rescaling.  This gives us 1000 interepts and slopes
    ints_and_slopes <- vza_rescale(bird_groups)

    # get the mean and SD of the of the bootstrapped tissue predictions
    tmp_list <- vza_mean_and_var_rasters(pred, ints_and_slopes)

    # now we are ready to return a list of everything we need for vza_assignment
    list(
      Tilde_T_mu = tmp_list$mean.raster,
      R_sigma_squared = tmp_list$var.raster,
      a_bar = mean(ints_and_slopes$slopes),
      b_bar = mean(ints_and_slopes$intercepts),
      sd_indiv = mean(bird_groups$sdH)
    )
}
