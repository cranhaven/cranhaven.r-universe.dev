#' Discretize a `sspm` model object
#'
#' Discretize a [sspm][sspm-class] model object with a function from a
#' [discretization_method][discretization_method-class] object class. This
#' function divides the boundary polygons into smaller patches.
#'
#' @param boundary_object **\[sspm\]** An object of class
#'    [sspm_boundary][sspm-class].
#' @param method **\[character OR method\]**
#'    Either a `character` from the list of available methods
#'    (see [spm_methods][spm_methods] for the list) **OR** an object of class
#'    [discretization_method][discretization_method-class].
#' @param with **\[sspm_dataset OR sf\]** Either an object of class sspm_dataset or
#'    a set of custom points.
#' @param ... **\[named list\]** Further arguments to be passed onto the function
#'    used in `method`.
#'
#' @return
#' An object of class [sspm_discrete_boundary][sspm-class] (the updated
#' and discretized `sspm` object given as input).
#'
#' @details
#' Custom discretization functions can be written. The function must:
#'
#' 1. Accept at least 1 argument: **boundaries** (the `sf` boundary object),
#'    and optionnaly **with** (can be NULL) a separate object to be used for
#'    discretization and **boundary**, the boundary column of
#'    **boundaries** (these last 2 arguments are passed and connot be
#'    overwritten but could be ignored).
#'
#' 2. Returns a named list with 2 elements: `patches`. an `sf` object that
#'    stores the discretized polygons, and `points`, an `sf` object that
#'    stores the points that were used for discretization.
#'
#' @examples
#' # Voronoi tesselation
#' sfa_boundaries
#' bounds <- spm_as_boundary(boundaries = sfa_boundaries,
#'                           boundary = "sfa")
#' biomass_dataset <- spm_as_dataset(data.frame(borealis_simulated), name = "borealis",
#'                                   density = "weight_per_km2",
#'                                   time = "year_f",
#'                                   coords = c('lon_dec','lat_dec'),
#'                                   uniqueID = "uniqueID")
#' bounds_voronoi <- bounds %>%
#'   spm_discretize(method = "tesselate_voronoi",
#'                  with = biomass_dataset,
#'                  nb_samples = 10)
#'
#' # Custom method
#' custom_func <- function(boundaries, ...){
#'   args <- list(...)
#'   # Can access passed arguments with args$arg_name
#'   # Do your custom discretization
#'   # Careful: must return sf objects!
#'   return(list(patches = c(),
#'               points = c())
#'          )
#' }
#'
#' @export
setGeneric(name = "spm_discretize",
           def = function(boundary_object,
                          method = "tesselate_voronoi",
                          with = NULL,
                          ...) {
             standardGeneric("spm_discretize")
           }
)

# Methods -----------------------------------------------------------------
# TODO finish the rdname description

# If invalid method, throw error
#' @rdname spm_discretize
#' @export
setMethod(f = "spm_discretize",
          signature(boundary_object = "sspm_boundary",
                    method = "missing"),
          function(boundary_object, method, with, ...) {
            stop("method argument missing.")
          }
)

# with missing
#' @rdname spm_discretize
#' @export
setMethod(f = "spm_discretize",
          signature(boundary_object = "sspm_boundary",
                    method = "ANY",
                    with = "missing"),
          function(boundary_object, method, with, ...) {

            discrete <- spm_discretize(boundary_object, method, with = NULL, ...)

          }
)

# If method is character, check against list, create `discretization_method`
# and call next signature.
#' @rdname spm_discretize
#' @export
setMethod(f = "spm_discretize",
          signature(boundary_object = "sspm_boundary",
                    method = "character"),
          function(boundary_object, method, with, ...) {

            method_f <- as_discretization_method(name = method)

            discrete <- spm_discretize(boundary_object, method_f, with, ...)
          }
)

# If method is function, make a `discretization_method`
# and call next signature.
#' @rdname spm_discretize
#' @export
setMethod(f = "spm_discretize",
          signature(boundary_object = "sspm_boundary",
                    method = "function"),
          function(boundary_object, method, with, ...) {

            method_f <- as_discretization_method(method = method)

            discrete <- spm_discretize(boundary_object, method_f, with, ...)
          }
)

# sf case
#' @rdname spm_discretize
#' @export
setMethod(f = "spm_discretize",
          signature(boundary_object = "sspm_boundary",
                    method = "discretization_method"),
          function(boundary_object, method, with, ...) {

            if (checkmate::test_class(boundary_object,
                                      "sspm_discrete_boundary")) {
              stop(" Boundary is already discretized")
            }

            if (checkmate::test_class(with, "sspm_dataset")){
              with <- spm_data(with)
            } else if (!is.null(with)) {
              stop("`with` must be a `sspm_dataset` or NULL")
            }

            # Info message
            cli::cli_alert_info(paste0(" Discretizing using method ",
                                       cli::col_yellow(spm_name(method))))

            # Send to discretization routine
            boundaries <- spm_boundaries(boundary_object)
            boundary <- spm_boundary(boundary_object)
            other_args <- list(...)

            discrete <-
              do.call(method_func(method),
                      args = append(list(boundaries = boundaries,
                                         boundary = boundary,
                                         with = with),
                                    other_args))

            # Check names of results
            checkmate::assert_names(x = names(discrete),
                                    subset.of = c("patches",
                                                  "points"))

            new_sspm_discrete_boundary <-
              new("sspm_discrete_boundary",
                  boundaries = spm_boundaries(boundary_object),
                  boundary = boundary,
                  boundary_area = spm_boundary_area(boundary_object),
                  patches_area = "patch_area",
                  method = method,
                  patches = discrete[["patches"]],
                  points = discrete[["points"]])

            return(new_sspm_discrete_boundary)
          }
)
