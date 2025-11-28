#' The VESPA dataset
#'
#' A set of QTS representing individual gait patterns (IGPs) of individuals
#' collected under a number of varying factors.
#'
#' The IGP measures the hip rotation during a typical gait cycle. Each rotation
#' is expressed with respect to the mean position of the sensor during the gait
#' cycle. Each IGP is then straightened so that it is periodic with a last point
#' matching the first one.
#'
#' @format A \code{\link[tibble]{tibble}} with 320 rows and 7 columns:
#'
#' - `V`: a categorical variable with two levels specifying the ID of the
#' `Volunteer`;
#' - `E`: a categorical variable with two levels specifying the ID of the
#' `Experimenter`;
#' - `S`: a categorical variable with four levels specifying the type of
#' `Sensor`;
#' - `P`: a categorical variable with four levels specifying the `Position` of
#' the sensor;
#' - `A`: a categorical variable with two levels specifying the ID of the
#' `Acquisition` pathway;
#' - `R`: a categorical variable with 5 levels specifying the ID of the
#' `Repetition`;
#' - `igp`: A 101x5 \code{\link[tibble]{tibble}} storing a QTS which
#' represents the IGP of the individual under a specific set of VESPA
#' conditions.
"vespa"

#' The VESPA64 dataset
#'
#' A set of QTS representing individual gait patterns (IGPs) of individuals
#' collected under a number of varying factors.
#'
#' The IGP measures the hip rotation during a typical gait cycle. Each rotation
#' is expressed with respect to the mean position of the sensor during the gait
#' cycle. Each IGP is then straightened so that it is periodic with a last point
#' matching the first one.
#'
#' It is essentially a reduced version of the VESPA data set where IGPs have
#' been averaged over the repetition for each set of conditions.
#'
#' @format A \code{\link[tibble]{tibble}} with 320 rows and 7 columns:
#'
#' - `V`: a categorical variable with two levels specifying the ID of the
#' `Volunteer`;
#' - `E`: a categorical variable with two levels specifying the ID of the
#' `Experimenter`;
#' - `S`: a categorical variable with four levels specifying the type of
#' `Sensor`;
#' - `P`: a categorical variable with four levels specifying the `Position` of
#' the sensor;
#' - `A`: a categorical variable with two levels specifying the ID of the
#' `Acquisition` pathway;
#' - `igp`: A 101x5 \code{\link[tibble]{tibble}} storing a QTS which
#' represents the IGP of the individual under a specific set of VESPA
#' conditions.
"vespa64"
