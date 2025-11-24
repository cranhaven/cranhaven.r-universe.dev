#' Perform voronoi tesselation
#'
#' Generates voronoi polygons by first performing stratified sampling across
#' boundary polygons, then by running  the voronoisation with
#' [st_voronoi()][sf::st_voronoi()].
#'
#' @param boundaries **\[sf\]** The boundaries to be used.
#' @param with **\[sf\]** A set of data points to use for voronoisation.
#' @param boundary **\[character\]** The column in `boundaries` that is to
#'     be used for the stratified sampling.
#'
#' @param sample_surface **\[logical]** Whether to sample the surfaces in
#'     `boundaries`, Default to `FALSE`.
#' @param sample_points **\[logical]** Whether to sample points from `with` or
#'     to take all points in `with`. Default to `TRUE`.
#'
#' @param nb_samples **\[named character vector\]** The number of samples to draw
#'     by boundary polygons (must bear the levels of `boundary` as names
#'     or be a single value to be applied to each level).
#' @param min_size **\[numeric\]** The minimum size for a polygon above which it
#'     will be merged (in km2).
#' @param stratify **\[logical]** Whether the discretization happens within the
#'     boundaries or whether the whole area is to be used (default to TRUE).
#'
#' @param seed **\[numeric\]** Passed onto [`set.seed()`][base::set.seed()],
#'     important for reproducibility of sampling.
#'
#' @return
#' A named list with three elements (each an `sf` object):
#'     * `patches`, the voronoi polygons generated
#'     * `points`, the points used for the tessellation.
#'
#' @examples
#' data(borealis_simulated, package = "sspm")
#' data(sfa_boundaries, package = "sspm")
#' tesselate_voronoi(sfa_boundaries, with = borealis, sample_surface = TRUE,
#'                   boundary = "sfa", nb_samples = 10)
#'
#' @export
tesselate_voronoi <- function(boundaries,
                              with,
                              boundary = "sfa",
                              sample_surface = FALSE,
                              sample_points = TRUE,
                              nb_samples = NULL,
                              min_size = 1500,
                              stratify = TRUE,
                              seed = 1) {

  # 1. Prep -----------------------------------------------------------------

  # Check main params

  check_spatial_inputs(boundaries, sample_surface, sample_points,
                       boundary, nb_samples, min_size, seed)

  nb_samples <- check_nb_samples_formatting(nb_samples, boundaries, boundary)

  # 2. Sample points, if need be --------------------------------------------

  # 2. Create (sample) the points

  if (sample_surface){

    voronoi_points <- sample_points(mode = "surface", with,
                                    boundaries, boundary, nb_samples, seed)

  } else {

    if (sample_points) {

      voronoi_points <- sample_points(mode = "points", with,
                                      boundaries, boundary, nb_samples, seed)

    } else {

      stopifnot(sf::st_is(with, "POINT"))

      voronoi_points <- suppressMessages(sf::st_join(with, boundaries,
                                                     suffix = c("", "_duplicate"))) %>%
        dplyr::filter(!is.na(eval(dplyr::sym(boundary))))
    }

  }

  # 3. Create patches -------------------------------------------------------

  voronoi <- make_patches_voronoi(stratify, voronoi_points, boundaries, boundary)

  # 4. Merge small polygons -------------------------------------------------

  voronoi <- merge_small_polygons(voronoi, min_size, boundary)

  # 5. Summarise and re - calculate area ------------------------------------

  voronoi <- cleanup_polygons(voronoi, boundary)

  # Core function must return a list of "patches" and "points"
  return(list(patches = voronoi,
              points = voronoi_points))
}
