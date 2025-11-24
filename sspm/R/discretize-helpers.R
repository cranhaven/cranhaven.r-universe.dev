
# Helpers for discretization methods --------------------------------------

# Function to check the spatial inputs to discretization methids
check_spatial_inputs <- function(boundaries, sample_surface, sample_points,
                                 boundary, nb_samples, min_size, seed){

  if (is.null(boundaries)) {
    stop("boundaries argument is missing")
  } else {
    checkmate::assert_class(boundaries, "sf")
  }

  checkmate::assert_logical(sample_surface)
  checkmate::assert_logical(sample_points)
  checkmate::assert_character(boundary)

  assert_column(boundaries, boundary)

  checkmate::assert_numeric(nb_samples, null.ok = TRUE)
  if(all(!c(sample_surface, sample_points)) && !is.null(nb_samples)){
    warning("nb_sample ignored")
  }
  checkmate::assert_numeric(min_size)
  checkmate::assert_numeric(seed)

}

# Check that nb_samples passed to the discretization methods are named
# properly by name of each boundary
check_nb_samples_formatting <- function(nb_samples, boundaries, boundary){

  unique_boundaries <- unique(boundaries[[boundary]])

  if (length(nb_samples) == 1){
    nb_samples <- rep(nb_samples, length(unique_boundaries))
    names(nb_samples) <- unique_boundaries
  } else {
    if(any(!(names(nb_samples) %in% unique_boundaries))){
      stop("nb_samples incorrectly named")
    }
  }

  return(nb_samples)

}

# Function to sample points on a surface for tessellation on boundaries
sample_points <- function(mode, with, boundaries, boundary, nb_samples, seed){

  boundaries_split <- split(boundaries, boundaries[[boundary]])

  if (mode == "surface"){

    if (is.null(nb_samples)){
      cli::cli_alert_danger("You must specify nb_samples when sampling surfaces or points")
      stop("nb_samples is NULL")
    }

    sample_fun <- function(polygon, boundary, nb_samples){
      sf::st_sample(polygon,
                    size = nb_samples[polygon[[boundary]]])
    }

    set.seed(seed) ; the_points <-
      lapply(boundaries_split, FUN = sample_fun,
             boundary = boundary,
             nb_samples = nb_samples) %>%
      lapply(sf::st_as_sf) %>%
      dplyr::bind_rows() %>%
      dplyr::rename(geometry = "x") %>%
      sf::st_join(boundaries)

  } else if (mode == "points") {

    if (is.null(nb_samples)){
      cli::cli_alert_danger("You must specify nb_samples when sampling surfaces or points")
      stop("nb_samples is NULL")
    }

    if (is.null(with)){
      cli::cli_alert_danger("with cannot be NULL when sampling points")
      stop("with is NULL")
    }

    set.seed(seed) ; the_points <-
      suppressMessages(sf::st_join(with, boundaries,
                                   suffix = c("_duplicate", ""))) %>%
      dplyr::select(-dplyr::contains("duplicate")) %>%
      dplyr::filter(!is.na(eval(dplyr::sym(boundary)))) %>%
      dplyr::group_by(.data[[boundary]]) %>%
      dplyr::filter(1:dplyr::n() %in%
                      sample(1:dplyr::n(),
                             size = nb_samples[[.data[[boundary]][1]]]))
  }

  return(the_points)

}

# Function to make voronoi patches
make_patches_voronoi <- function(stratify, voronoi_points, boundaries, boundary){

  boundaries_split <- split(boundaries, boundaries[[boundary]])

  if(stratify){

    envelopes <- boundaries_split %>%
      lapply(function(x) { x[["geometry"]] } )

    voronoi <- voronoi_points %>%
      split(voronoi_points[[boundary]]) %>%
      lapply(function(x) { suppressAll(sf::st_union(x)) } ) %>%
      mapply(FUN = function(x, y) {
        suppressAll(sf::st_voronoi(x, envelope = y)) },
        envelopes, SIMPLIFY = FALSE) %>%
      lapply(function(x) { suppressAll(sf::st_cast(x)) } ) %>%
      lapply(function(x) { suppressAll(sf::st_sf(x)) } ) %>%
      mapply(FUN = function(x, y) { suppressAll(sf::st_intersection(x, y)) },
             boundaries_split, SIMPLIFY = FALSE) %>%
      dplyr::bind_rows()

    voronoi <- sf::st_sf(voronoi) %>%
      dplyr::rename(geometry = "x")

  } else {

    voronoi <-
      suppressAll(voronoi_points %>%
                    sf::st_union() %>%
                    sf::st_voronoi() %>%
                    sf::st_cast() %>%
                    sf::st_sf())

    voronoi <-
      suppressAll(sf::st_intersection(x = boundaries, y = voronoi))

  }

  voronoi <-
    suppressAll(voronoi %>%
                  st_cast() %>%
                  st_cast("POLYGON") %>%
                  sf::st_make_valid() %>%
                  dplyr::mutate(patch_id = paste("P", 1:dplyr::n(), sep = "")) %>%
                  dplyr::group_by(.data$patch_id, .data[[boundary]]) %>%
                  dplyr::summarize() %>%
                  sf::st_make_valid() %>%
                  dplyr::ungroup())
  voronoi <-
    suppressAll(dplyr::mutate(voronoi,
                              area = sf::st_area(voronoi)))
  voronoi <-
    suppressAll(dplyr::mutate(voronoi,
                              area = units::set_units(.data$area,
                                                      value = "km^2")))

  return(voronoi)
}

# This function makes sure smaller polygons are fused to their nearest neighbor
merge_small_polygons <- function(pols, min_size, boundary){

  # The removal of small polygons is not stratified (it works on the same
  # parameters and mechanism for each boundary).
  small_pols <- pols$patch_id[which(pols$area <
                                      units::set_units(min_size, value = "km^2"))]
  pols_edges <- suppressMessages(sf::st_intersects(pols))
  names(pols_edges) <- pols$patch_id

  for (i in small_pols) {
    current_polygons <- pols[pols_edges[[i]], ] %>%
      dplyr::filter(.data[[boundary]] ==
                      unique(.data[[boundary]][.data$patch_id == i])) %>%
      dplyr::filter(.data$area == max(.data$area))
    max_id <- current_polygons$patch_id
    pols$patch_id[pols$patch_id == i] <- max_id
  }

  return(pols)
}

# This function fuses polygons based on the output of the merging of smaller
# polygons
cleanup_polygons <- function(pols, boundary){

  # Group by boundary and union
  pols <-
    suppressWarnings(
      suppressMessages(
        pols %>%
          dplyr::group_by(.data[[boundary]], .data$patch_id) %>%
          dplyr::summarize() %>%
          dplyr::ungroup()))

  # Add an area column
  pols <-
    dplyr::mutate(pols, patch_area = units::set_units(sf::st_area(pols), value = "km^2")) %>%
    dplyr::relocate("patch_area", .before = "geometry")

  # Add a patch_id column
  pols <-
    dplyr::mutate(pols,
                  patch_id = factor(paste("P", 1:dplyr::n(), sep = ""),
                                    levels = paste0("P", 1:length(unique(.data$patch_id)))))

  return(pols)
}

# This function makes a base for meshing from a set of boundaries
make_base_from_bounds <- function(boundaries){

  boundaries %>%
    sf::st_union() %>%
    sf::st_convex_hull() %>%
    sf::st_cast() %>%
    sf::st_cast("POLYGON") %>%
    sf::st_as_sf() %>%
    dplyr::rename(geometry = "x")

}
