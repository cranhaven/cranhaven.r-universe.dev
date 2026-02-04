
# Functions for combining results from isotopes, genetics, and habitat



#' combine genetics, isotopes, and habitat raster with exponents as given
#'
#' This just multiplies the rasters together, each raised to the appropriate
#' exponent, normalizes and returns the result
#' @param Mgen  the genetic posteriors rasterStack.  Must be a rasterStack
#' @param Miso the isotope posteriors rasterStack.
#' @param Mhab a single layer raster with the habitat suitabiilty measure as a normalized
#' probability surface.
#' @param beta_hab the exponent to raise the genetic raster to
#' @param beta_iso the exponent to raise the isotope raster to
#' @param beta_gen the exponent to raise the habitat raster to
#' @export
#' @examples
#' # first, run through the example for isotope_posterior_probs() to get
#' # the rasters for two migrant birds. This gives us the list "birds2"
#' example(isotope_posterior_probs)
#'
#' # extract the posterior probs rasters from  that output into a raster stack
#' miso <- lapply(birds2$regular, function(x) x$posterior_probs) %>%
#'   raster::stack()
#'
#' # see the names of the birds we are dealing with:
#' names(miso)
#'
#' # get the genetic posteriors for those two birds
#' mig_gen2 <- migrant_wiwa_genetic_posteriors %>%
#'   dplyr::filter(ID %in% c(names(miso)))
#'
#' # make genetic posterior rasters for those two birds, make sure they are
#' # sorted in the same order as miso, and make a raster stack of it
#' mgen <- genetic_posteriors2rasters(G = mig_gen2, R = genetic_regions)[names(miso)] %>%
#'   raster::stack()
#'
#' # make a normalized prior from habitat quality that is zeros everywhere
#' # outside of the "known" range.
#' tmp <- wiwa_habitat_unclipped * wiwa_breed
#' mhab <- tmp / raster::cellStats(tmp, sum)
#'
#' # combine genetics, isotopes and habitat with exponents of 1 on each
#' mcombo <- comboize(mgen, miso, mhab, 1, 1, 1)
comboize <- function(Mgen, Miso, Mhab, beta_gen, beta_iso, beta_hab) {
  stopifnot(class(Mgen) == "RasterStack")
  stopifnot(class(Miso) == "RasterStack")
  stopifnot(class(Mhab) == "RasterLayer")

  # make a rasterStack of Mhab that is the right length
  Mhab_stack <- raster::stack(lapply(1:raster::nlayers(Miso), function(x) Mhab))

  tmp  <- (Mgen ^ beta_gen) * (Miso ^ beta_iso) * (Mhab_stack ^ beta_hab)

  # then normalize and return each one
  tmp / raster::cellStats(tmp, sum)

}




#' prepare fortified output for multipanel plot
#'
#' This takes Mgen, Miso, and Mhab for a single bird
#' and, if available, the true breeding location.  Then it
#' computes the combo-ized raster at all the requested levels
#' of the exponents, and creates a fortified data frame of the
#' results suitable for plotting in ggplot
#' @param mgen  genetics posterior raster
#' @param miso isotope posterior raster
#' @param mhab habitat suitability raster
#' @param gen_beta_levels vector of the desired values of gen_beta
#' @param iso_beta_levels vector of the desired values of iso_beta
#' @param hab_beta_levels vector of the desired values of hab_beta
#' @export
#' @examples
#' # run through the example for comboize to get the variables
#' # mgen, miso, and mhab that we will use.
#' example(comboize)
#'
#' # then run that on the first bird to get a data frame
#' # that you can use with ggplot
#' ff <- comboize_and_fortify(mgen[[1]], miso[[1]], mhab)
#'
#' # this can be plotted with ggplot2
#' \dontrun{
#' library(ggplot2)
#' wmap <- get_wrld_simpl()
#' ggplot(mapping = aes(x=long, y = lat)) +
#'   coord_fixed(1.3, xlim = c(-170, -50), ylim = c(33, 70)) +
#'   geom_polygon(data = wmap, aes(group = group), fill = NA, color = "black", size = .05) +
#'   geom_raster(data = ff, mapping = aes(fill = prob), interpolate = TRUE) +
#'   scale_fill_gradientn(colours = c("#EBEBEB", rainbow(7)), na.value = NA) +
#'   theme_bw() +
#'   facet_wrap( ~ beta_vals, ncol = 2) +
#'   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
#' }
#'
comboize_and_fortify <- function(mgen, miso, mhab,
                                 gen_beta_levels = 1,
                                 iso_beta_levels = c(1.0),
                                 hab_beta_levels = c(1.0)
) {
  names(gen_beta_levels) <- gen_beta_levels
  names(iso_beta_levels) <- iso_beta_levels
  names(hab_beta_levels) <- hab_beta_levels

  # get all the different levels there
  levs_ret <- lapply(gen_beta_levels, function(gbl) {
    lapply(iso_beta_levels, function(ibl) {
      lapply(hab_beta_levels, function(hbl) {
        comboize(raster::stack(mgen), raster::stack(miso), mhab, gbl, ibl, hbl) %>%
          raster::as.data.frame(xy = TRUE, stringsAsFactors = FALSE) %>%
          stats::setNames(c("long", "lat", "prob"))  %>%
          dplyr::tbl_df()
      }) %>% dplyr::bind_rows(.id = "habitat_beta")
    }) %>% dplyr::bind_rows(.id = "isotope_beta")
  }) %>% dplyr::bind_rows(.id = "genetics_beta")


  # add a column that names these betas:
  levs_ret2 <- levs_ret %>%
     dplyr::mutate(beta_vals = paste("G=", genetics_beta, ", I=", isotope_beta, ", H=", habitat_beta, sep = ""))

  beta_levs <- unique(levs_ret2$beta_vals)

  # now, create three more that are just genetics, habitat, and density alone
  solos <- lapply(list(`Genetics Alone` = comboize(raster::stack(mgen), raster::stack(miso), mhab, 1, 0, 0),
       `Isotopes Alone` = comboize(raster::stack(mgen), raster::stack(miso), mhab, 0, 1, 0),
       `Habitat Alone` = comboize(raster::stack(mgen), raster::stack(miso), mhab, 0, 0, 1)), function(x)
         {
         raster::as.data.frame(x, xy = TRUE, stringsAsFactors = FALSE) %>%
           stats::setNames(c("long", "lat", "prob"))  %>%
           dplyr::tbl_df()
       })  %>%
    dplyr::bind_rows(.id = "beta_vals")

  ret <- dplyr::bind_rows(solos, levs_ret2)
  ret$beta_vals <- factor(ret$beta_vals, levels = c("Habitat Alone", "Genetics Alone", "Isotopes Alone", beta_levs))
  ret %>% dplyr::select(beta_vals, long, lat, prob)
}
if(getRversion() >= "2.15.1")  utils::globalVariables(c("genetics_beta",
                                                        "isotope_beta",
                                                        "habitat_beta",
                                                        "beta_vals",
                                                        "long",
                                                        "lat",
                                                        "prob"))



