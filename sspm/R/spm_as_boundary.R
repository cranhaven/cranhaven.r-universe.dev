#' Create a sspm_boundary object
#'
#' Create a sspm_boundary object. A boundary object serves as a basis to encode
#' the spatial extent of the model.
#'
#' @param boundaries **\[sf\]** The sf object to cast.
#' @param boundary **\[character\]** The column that contains the possible
#'   subdivisions of the boundaries.
#' @param patches **\[sf\]** Patches resulting from discretization.
#' @param points **\[sf\]** Sample points used for discretization.
#' @param boundary_area **\[character\]** The column that contains the area
#'   of the subdivisions (optional).
#' @param patch_area **\[character\]** The column that contains the area
#'   of the patches (optional).
#'
#' @return
#' An object of class [sspm_boundary][sspm_boundary-class] or
#' [sspm_discrete_boundary][sspm_discrete_boundary-class].
#'
#' @examples
#' sfa_boundaries
#' bounds <- spm_as_boundary(boundaries = sfa_boundaries,
#'                           boundary = "sfa")
#' plot(bounds)
#'
#' @export
setGeneric(name = "spm_as_boundary",
           def = function(boundaries,
                          boundary,
                          patches = NULL,
                          points = NULL,
                          boundary_area = NULL,
                          patch_area = NULL) {
             standardGeneric("spm_as_boundary")
           }
)

# Methods -----------------------------------------------------------------

#' @export
#' @rdname spm_as_boundary
setMethod(f = "spm_as_boundary",
          signature(boundaries = "missing"),
          function(boundaries, boundary, patches, points,
                   boundary_area, patch_area) {
            stop("`boundaries` cannot be missing",
                 call. = FALSE)
          }
)

#' @export
#' @rdname spm_as_boundary
setMethod(f = "spm_as_boundary",
          signature(boundary = "missing"),
          function(boundaries, boundary, patches, points,
                   boundary_area, patch_area) {
            stop("`boundary` cannot be missing",
                 call. = FALSE)
          }
)

#' @export
#' @rdname spm_as_boundary
setMethod(f = "spm_as_boundary",
          signature(boundaries = "sf",
                    boundary = "character",
                    patches = "missing",
                    points = "missing"),
          function(boundaries, boundary, patches, points,
                   boundary_area, patch_area) {

            boundaries_list <- check_boundaries(boundaries, boundary,
                                                boundary_area)

            boundaries <- boundaries_list$features
            boundary_area <- boundaries_list$column

            boundary_object <-
              new("sspm_boundary",
                  boundaries = boundaries,
                  boundary = boundary,
                  boundary_area = boundary_area)

            return(boundary_object)

          }
)

#' @export
#' @rdname spm_as_boundary
setMethod(f = "spm_as_boundary",
          signature(boundaries = "sf",
                    boundary = "character",
                    patches = "ANY",
                    points = "ANY"),
          function(boundaries, boundary, patches, points,
                   boundary_area, patch_area) {

            checkmate::assert_class(patches, "sf", null.ok = TRUE)
            checkmate::assert_class(points, "sf", null.ok = TRUE)

            # Boundaries
            boundaries_list <- check_boundaries(boundaries, boundary,
                                                boundary_area)

            boundaries <- boundaries_list$features
            boundary_area <- boundaries_list$column

            # Patches
            patches_list <- check_patches(patches,
                                          patch_area)

            patches <- patches_list$features
            patch_area <- patches_list$column

            patches <- patches %>%
              dplyr::mutate(patch_id =
                              factor(paste("P", 1:dplyr::n(), sep = ""))) %>%
              dplyr::mutate(patch_id =
                              factor(.data$patch_id,
                                     levels = paste0("P", 1:length(unique(.data$patch_id))))) %>%
              dplyr::relocate("patch_id", .after = dplyr::all_of(boundary)) %>%
              # TODO add option for joining here as well
              dplyr::mutate(!!boundary := as.factor(.data[[boundary]]))

            # Points
            ## TODO

            boundary_object <-
              new("sspm_discrete_boundary",
                  boundaries = boundaries,
                  boundary = boundary,
                  boundary_area = boundary_area,
                  method = as_discretization_method(method = I),
                  patches_area = patch_area,
                  patches = patches,
                  points = points)

          }
)

# -------------------------------------------------------------------------

check_boundaries <- function(boundaries, boundary,
                             boundary_area){

  checkmate::assert_class(boundaries, "sf")
  checkmate::assert_class(boundary, "character")
  checkmate::assert_class(boundary_area, "character", null.ok = TRUE)
  assert_column(boundaries, boundary)

  # Ensure boundaries are factors
  boundaries[[boundary]] <- as.factor(boundaries[[boundary]])

  new_boundary_area <- paste0("area_", boundary)

  if(!is.null(boundary_area)){

    assert_column(boundaries, boundary_area)

    cli::cli_alert_warning("SSPM assumes areas are supplied in km^2")

    boundaries <-
      dplyr::mutate(boundaries, !!boundary_area :=
                      units::set_units(.data[[boundary_area]], value = "km^2")) %>%
      dplyr::rename(!!new_boundary_area := dplyr::all_of(boundary_area))

  } else {

    boundaries <- calculate_spatial_feature_areas(boundaries) %>%
      dplyr::rename(!!new_boundary_area := "area")

  }

  boundary_list <- list(features = boundaries,
                        column = new_boundary_area)

  return(boundary_list)

}

check_patches <- function(patches,
                          patches_area){

  checkmate::assert_class(patches, "sf")
  checkmate::assert_class(patches_area, "character", null.ok = TRUE)

  if(!is.null(patches_area)){

    assert_column(patches, patches_area)

    cli::cli_alert_warning("SSPM assumes areas are supplied in km^2")

    patches <-
      dplyr::mutate(patches, !!patches_area :=
                      units::set_units(.data[[patches_area]], value = "km^2"))

  } else {

    if ("patch_area" %in% names(patches)){
      stop("`patch_area` column already present, please cast with argument `patch_area`")
    }

    patches <- calculate_spatial_feature_areas(patches) %>%
      dplyr::rename(patch_area = "area")
    patches_area <- "patch_area"

  }

  patches_list <- list(features = patches,
                       column = patches_area)

  return(patches_list)

}

calculate_spatial_feature_areas <- function(features){

  checkmate::assert_class(features, "sf")

  features <- features %>%
    dplyr::mutate(area = sf::st_area(features))

  features <-
    dplyr::mutate(features,
                  area = units::set_units(.data$area, value = "km^2"))

  return(features)
}
