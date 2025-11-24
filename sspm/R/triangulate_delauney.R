#' Perform delaunay triangulation
#'
#' Generates delaunay triangles with [ct_triangulate()][sfdct::ct_triangulate()].
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
#' @param min_size **\[numeric\]** The minimum size for a triangle above which it
#'     will be merged (in km2).
#'
#' @param seed **\[numeric\]** Passed onto [`set.seed()`][base::set.seed()],
#'     important for reproducibility of sampling.
#'
#' @inheritDotParams RTriangle::triangulate
#'
#' @return
#' A named list with three elements (each an `sf` object):
#'     * `patches`, the voronoi polygons generated
#'     * `points`, the points used for the tessellation.
#'
#' @examples
#' data(borealis_simulated, package = "sspm")
#' data(sfa_boundaries, package = "sspm")
#' triangulate_delaunay(sfa_boundaries, with = borealis, sample_surface = TRUE,
#'                      boundary = "sfa", nb_samples = 10)
#'
#' @export
triangulate_delaunay <- function(boundaries,
                                 with = NULL,
                                 boundary = "sfa",
                                 sample_surface = FALSE,
                                 sample_points = FALSE,
                                 nb_samples = NULL,
                                 min_size = 1000,
                                 seed = 1,
                                 ...) {

  # 1. Prep -----------------------------------------------------------------

  # Check main params

  check_spatial_inputs(boundaries, sample_surface, sample_points,
                       boundary, nb_samples, min_size, seed)

  nb_samples <- check_nb_samples_formatting(nb_samples, boundaries, boundary)

  # 2. Sample points, if need be --------------------------------------------

  # 2. Create (sample) the points
  boundaries_split <- split(boundaries, boundaries[[boundary]])

  if (sample_surface){

    delaunay_base <- sample_points(mode = "surface", with,
                                   boundaries, boundary, nb_samples, seed)
    points <- delaunay_base

  } else if (sample_points) {

    delaunay_base <- sample_points(mode = "points", with,
                                   boundaries, boundary, nb_samples, seed)
    points <- delaunay_base

  } else if (!is.null(with)) {

    stopifnot(sf::st_is(with, "POINT"))

    delaunay_base <- with
    points <- delaunay_base

  } else {

    delaunay_base <- make_base_from_bounds(boundaries) %>%
      # Need a temp col for ct_triangulate to work
      dplyr::mutate(temp_col = "temp")
    points <- NULL

  }

  # 3. Create patches -------------------------------------------------------

  delaunay_mesh <-
    suppressAll(delaunay_base %>%
                  sfdct::ct_triangulate(...))

  if("temp_col" %in% names(delaunay_mesh)){
    delaunay_mesh <- delaunay_mesh %>% dplyr::select(-"temp_col")
  }

  if("npoints" %in% names(delaunay_mesh)){
    delaunay_mesh <- delaunay_mesh %>% dplyr::select(-"npoints")
  }

  delaunay_mesh <-
    suppressAll(delaunay_mesh %>%
                  sf::st_collection_extract() %>%
                  sf::st_cast() %>%
                  sf::st_cast("POLYGON") %>%
                  sf::st_join(boundaries, largest = TRUE) %>%
                  sf::st_intersection(sf::st_union(boundaries)) %>%
                  sf::st_make_valid() %>%
                  sf::st_cast() %>%
                  sf::st_cast("POLYGON") %>%
                  sf::st_make_valid() %>%
                  dplyr::mutate(patch_id = paste0("P", 1:dplyr::n())))

  delaunay_mesh <-
    suppressAll(dplyr::mutate(delaunay_mesh,
                              area = sf::st_area(delaunay_mesh)))
  delaunay_mesh <-
    suppressAll(dplyr::mutate(delaunay_mesh,
                              area = units::set_units(.data$area,
                                                      value = "km^2")))

  # 4. Merge small polygons -------------------------------------------------

  delaunay_mesh <- merge_small_polygons(delaunay_mesh, min_size, boundary)

  # 5. Summarise and re - calculate area ------------------------------------

  delaunay_mesh <- cleanup_polygons(delaunay_mesh, boundary) %>%
    sf::st_make_valid()

  return(list(patches = delaunay_mesh,
              points = points))

}

# Helpers -----------------------------------------------------------------
