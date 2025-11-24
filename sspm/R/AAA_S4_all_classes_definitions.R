# Imports -----------------------------------------------------------------

#' @import sf cli mgcv
#' @importFrom rlang .data :=
#' @importFrom methods new show validObject slot
#' @importFrom stats terms as.formula sd
#' @importFrom graphics par

# OldClasses --------------------------------------------------------------

setOldClass("data.frame")
setOldClass("sf")
setOldClass("bam")

# ClassUnions -------------------------------------------------------------

setClassUnion("characterOrNULL", c("character", "NULL"))
setClassUnion("missingOrNULL", c("missing", "NULL"))

# -------------------------------------------------------------------------

#' sspm discretization method class
#'
#' This class encapsulates a name and a method (function) used for
#' discretization.
#'
#' @slot name **\[character\]** Name of the discretization method.
#' @slot method **\[function\]** Function used for discretization.
#'
#' @name discretization_method-class
#' @rdname discretization_method-class
#'
setClass("discretization_method",
         slots = list(name = "character",
                      method = 'function')
)

# -------------------------------------------------------------------------

#' sspm boundary structure
#'
#' One of the first steps in the `sspm` workflow is to create one or more
#' object(s) of class `sspm_boundary` from an `sf` object.
#'
#' @slot boundaries **\[sf\]** Spatial boundaries (polygons).
#' @slot boundary **\[character\]** The column of `data` that represents the
#'     spatial boundaries.
#' @slot boundary_area **\[character\]** The column of `data` that represents the
#'     area of spatial boundaries.
#'
#' @name sspm_boundary-class
#' @rdname sspm_boundary-class
#'
setClass("sspm_boundary",
         slots = list(boundaries = "sf",
                      boundary = "character",
                      boundary_area = "character")
)

# -------------------------------------------------------------------------

#' sspm discrete boundary structure
#'
#' One of the first steps in the `sspm` workflow is to create one or more
#' object(s) of class `sspm_boundary` from an `sf` object.
#'
#' @slot boundaries **\[sf\]** Spatial boundaries (polygons).
#' @slot boundary **\[character\]** The column of `data` that represents the
#'     spatial boundaries.
#' @slot boundary_area **\[character\]** The column of `data` that represents the
#'     area of spatial boundaries.
#' @slot method **\[[discretization_method][discretization_method-class]\]**
#'     *(if discrete)* discretization method used.
#' @slot patches **\[sf\]** *(if discrete)* Patches resulting from
#'     discretization.
#' @slot points **\[sf or NULL\]** *(if discrete)* Sample points used for
#'     discretization.
#' @slot patches_area **\[character\]** The column of `data` that represents the
#'     area of patches.
#'
#' @name sspm_discrete_boundary-class
#' @rdname sspm_discrete_boundary-class
#'
setClass("sspm_discrete_boundary",
         slots = list(method = "discretization_method",
                      patches_area = "character",
                      patches = "sf",
                      points = "ANY"),
         contains = "sspm_boundary"
)

# -------------------------------------------------------------------------

#' sspm dataset structure
#'
#' One of the first step in the `sspm` workflow is to create one or more
#' object(s) of class `sspm_dataset` from a `data.frame`, `tibble` or `sf` object.
#'
#' @slot name **\[character\]** The name of the dataset, default to "Biomass".
#' @slot data **\[data.frame OR sf OR tibble\]** The dataset.
#' @slot biomass **\[character\]** The biomass columns of `data`.
#' @slot density **\[character\]** The biomass density columns of `data`.
#' @slot time **\[character\]** The column of `data` that represents the
#'     temporal dimension of the dataset.
#' @slot coords **\[character\]** The columns of `data` that represent the
#'     spatial dimension of the dataset: the two columns for longitude and
#'     latitude of the observations.
#' @slot uniqueID **\[character\]** The column of `data` that is unique for all
#'     rows of the data matrix.
#' @slot boundaries **\[sspm_discrete_boundary\]** Spatial boundaries (polygons).
#' @slot formulas **\[list\]** List of
#'     [sspm_formula][sspm_formula-class] objects that specifies the smoothed
#'     variables.
#' @slot smoothed_data **\[ANY (sf)\]** The smoothed data.
#' @slot smoothed_vars **\[character\]** A vector storing the smoothed vars.
#' @slot smoothed_fit **\[list\]** The fit from smoothing the data
#' @slot is_mapped **\[logical\]** Whether the dataset has been mapped to
#'     boundaries (used internally).
#'
#' @name sspm_dataset-class
#' @rdname sspm_dataset-class
#'
setClass("sspm_dataset",
         slots = list(name = "character",
                      data = "ANY",
                      biomass = "characterOrNULL",
                      density = "characterOrNULL",
                      time = "character",
                      coords = "characterOrNULL",
                      uniqueID = "character",
                      boundaries = "sspm_discrete_boundary",
                      formulas = "list",
                      smoothed_data = "ANY",
                      smoothed_vars = "character",
                      smoothed_fit = "list",
                      is_mapped = "logical"),
         prototype = prototype(smoothed_data = NULL,
                               is_mapped = FALSE)
)

# ClassUnions

setClassUnion("DatasetORANYOrNULL", c("sspm_dataset", "ANY", "NULL"))

# -------------------------------------------------------------------------

#' sspm formula object
#'
#' This class is a wrapper around the `formula` class. It is not intended for
#' users to directly manipulate and create new objects.
#'
#' @slot raw_formula **\[formula\]** The raw formula call
#' @slot translated_formula **\[formula\]** The translated formula call ready
#'     to be evaluated.
#' @slot vars **\[list\]** List of relevant variables for the evaluation of the
#'     different smooths.
#' @slot lag_vars Smooth lag variables used for predictions
#' @slot response **\[charatcer\]** The response variable in the formula.
#' @slot is_fitted **\[logical\]** Whether this formula has already been fitted.
#'
#' @seealso See the `mgcv` function for defining smooths: [s()][mgcv::s].
#'
setClass("sspm_formula",
         slots = list(raw_formula = "formula",
                      translated_formula = "formula",
                      vars = "list",
                      lag_vars = "characterOrNULL",
                      type = "character",
                      response = "character",
                      is_fitted = "logical"),
         prototype = prototype(is_fitted = FALSE)
)

# -------------------------------------------------------------------------

#' sspm model class
#'
#' The **`sspm`** model object, made from biomass, predictor and catch data.
#'
#' @slot datasets **\[list\]** List of
#'     [sspm_dataset][sspm_dataset-class] that define variables in the SPM model.
#' @slot time **\[character\]** The column of `data` that represents the
#'     temporal dimension of the dataset.
# @slot biomass_var **\[character\]** The column of `datasets` that
#     represents the biomass.
#' @slot uniqueID **\[character\]** The column of `datasets` that is unique for
#'     all rows of the data matrix.
#' @slot boundaries **\[sf\]** Spatial boundaries (polygons).
#' @slot smoothed_data **\[ANY (sf)\]** The smoothed data.
#' @slot smoothed_vars **\[character\]** A vector storing the smoothed vars.
#' @slot is_split **\[logical\]** Whether this object has been split into
#'     train/test sets.
#'
#' @name sspm-class
#' @rdname sspm-class
setClass("sspm",
         slots = list(datasets = "list",
                      time = "character",
                      uniqueID = "character",
                      boundaries = "sspm_discrete_boundary",
                      smoothed_data = "ANY",
                      smoothed_vars = "character",
                      is_split = "logical"),
         prototype = prototype(datasets = list(),
                               is_split = FALSE)
)

# -------------------------------------------------------------------------

#' sspm fit
#'
#' The fit object for a sspm model
#'
#' @slot smoothed_data **\[ANY (sf)\]** The smoothed data.
#' @slot time **\[character\]** The column of `smoothed_data` that
#'     represents the temporal dimension of the dataset.
# @slot biomass_var **\[character\]** The column of `smoothed_data` that
#     represents the biomass.
#' @slot uniqueID **\[character\]** The column of `smoothed_data` that is unique
#'     for all rows of the data matrix.
#' @slot formula **\[list\]** The [sspm_formula][sspm_formula-class] object that
#'     specifies the spm model.
#' @slot boundaries **\[sf\]** Spatial boundaries (polygons).
#' @slot fit **\[bam\]** The fit of the spm model.
#'
#' @name sspm_fit-class
#' @rdname sspm_fit-class
setClass("sspm_fit",
         slots = list(smoothed_data = "ANY",
                      time = "character",
                      uniqueID = "character",
                      formula = "sspm_formula",
                      boundaries = "sspm_discrete_boundary",
                      fit = "bam")
)
