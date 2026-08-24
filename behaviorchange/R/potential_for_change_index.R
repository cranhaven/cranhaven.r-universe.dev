#' Potential for Change Index and the Determinant Selection Table
#'
#' These functions compute the Potential for Change Index for one or multiple
#' (sub-)determinants, the room for improvement (an intermediate estimate),
#' and produce a convenient table with an overview of all (sub-)determinants.
#' Note that for determinant selection purposes, quantitative estimates such
#' as the Potential for Change Index should never be used without also
#' thoroughly inspecting the visualisations of the univariate distributions
#' and the confidence intervals for the associations to the ultimate
#' intervention targets (usually the target behavior or a proxy measure). For
#' this purpose, the Confidence Interval-Based Estimation of Relevance plots
#' can be used (see [CIBER()]).
#'
#' The Potential for Change index was developed by Keegan et al. and is a
#' numerical representation of a number of important features in [CIBER()]
#' plots (for more details, please see the references below). It turned out
#' a similar measure, the Intervention Potential, was developed by Huber &
#' Mosler (2013). The latter uses regression coefficients as weights, which
#' is problematic for a number of reasons (see Crutzen, Peters & Noijen, 2017),
#' and has therefore not been implemented as a default, but it is possible to
#' use regression coefficients by specifying a custom weight function.
#'
#' The original Potential for Change Index was conceptualized to optimize
#' intervention tailoring and improve the prediction of individual-level
#' intervention effectiveness. A second conceptualization of the Potential
#' for Change Index can facilitate sub-determinant selection.
#'
#' In addition to using the `minimum`, `maximum`, `center`, and `weight`
#' functions to specify custom functions, specific types have also been
#' implemented to quickly use a prespecified combination of functions.
#'
#' The first (`type = '1'`) is computed as follows:
#'
#' - For sub-determinants with a positive zero-order correlation with
#' behavior, the sample mean is subtracted from the observed maximum score,
#' and the result is multiplied by the zero-order correlation;
#' - For sub-determinants with a negative zero-order correlation with
#' behavior, the sample mean is subtracted from the observed minimum score,
#' and the result is multiplied by the zero-order correlation.
#'
#' The second (`type = '2'`) is computed as follows:
#'
#' - For sub-determinants with a positive zero-order correlation with
#' behavior, the sample mean is subtracted from the .95 quantile of the
#' scores, and the result is multiplied by the squared zero-order
#' correlation (i.e. the proportion of explained variance);
#' - For sub-determinants with a negative zero-order correlation with
#' behavior, the sample mean is subtracted from the .05 quantile of the
#' scores, and the result is multiplied by the squared zero-order
#' correlation (i.e. the proportion of explained variance);
#'
#' The second variant effectively takes the 5% trimmed maximum and minimum,
#' rendering it less sensitive to outliers, penalizes weak associations with
#' behavior more severely, and decreases sensitivity to differences between
#' correlations. These differences should render the second variant a bit more
#' robust over different samples.
#'
#' The room for improvement is one of the ingredients of the Potential
#' for Change Index or P_delta, a generalized version of the Intervention
#' Potential. The Determinant Selection Table efficiently presents the
#' Potential for Change Indices for a set of (sub-)determinants.
#'
#' @param data The dataframe containing the variables.
#' @param determinants The name(s) of the determinant(s).
#' @param target The target (e.g. behavior or intention).
#' @param type The type of potential for change index. Currently implemented
#' are type `'1'` and type `'2'` - see details for more information.
#' @param increasesAreImprovements Whether increases are improvements (`TRUE`)
#' or decreases are improvements (`FALSE`).
#' @param sampleLevel Whether to return sample-level estimates (`TRUE`) or
#' individual-level estimates (`FALSE`).
#' @param minimum,maximum The minimum and maximum, as functions that take
#' a vector and return the minimum and maximum scores, as numbers, or as
#' vectors of numbers specifying the minimum and maximum to use for each
#' column in `x` or `determinants`, or a lists of functions specifying the functions to use
#' for each column in `x` or `determinants`.
#' @param center For the sample-level version, a function that takes a
#' vector and returns the center (e.g. mean, median, etc), or a list of
#' functions specifying the function to use for each column in `x` or
#' `determinants`.
#' @param weight The function to return the weight/multiplier to use in the
#' computation.
#' @param minimumArgs,maximumArgs,centerArgs,weightArgs lists with arguments
#' to pass to the corresponding functions. Note that these are not vectorized.
#' @param x For room for improvement, either a numeric vector with scores on
#' a (sub-)determinant, or a data frame with multiple such vectors. For the
#' Determinant Selection Table functions, the object to print/knit.
#'
#' @return For the individual-level version, a vector or data
#' frame with the same dimensions as provided; for the sample-level version, if
#' a vector is provided, a single number, and if a data frame is provided, a
#' vector with as many values as the data frame has columns. For
#' Determinant Selection Table, a data frame.
#'
#' @rdname potential_for_change
#' @aliases potential_for_change_index P_delta intervention_potential
#' determinant_selection_table
#'
#' @references Knittle, K. P., Peters, G.-J. Y., Heino, M. T. J., Tobias, R., &
#' Hankonen, N. (2019). Potential for change: New metrics for tailoring and
#' predicting response to behavior change interventions. \doi{10/ghqmg3}
#'
#' Huber, A. C. & Mosler, H.-J. (2013) Determining behavioral factors for
#' interventions to increase safe water consumption: a cross-sectional field
#' study in rural Ethiopia, International Journal of Environmental Health
#' Research, 23:2, 96-107 \doi{10.1080/09603123.2012.699032}
#'
#' @examples ### Get example data
#' dat <- get(data("BBC_pp15.1", package="behaviorchange"));
#'
#' ### Individual-level version, for one sub-determinant
#' P_delta_example <-
#'   behaviorchange::potential_for_change_index(
#'     data=dat,
#'     determinants='highDose_attitude',
#'     target='highDose_intention'
#'   );
#'
#' head(P_delta_example);
#' hist(P_delta_example);
#'
#' ### Sample-level version
#' behaviorchange::potential_for_change_index(
#'   data=dat,
#'   determinants='highDose_attitude',
#'   target='highDose_intention',
#'   sampleLevel = TRUE
#' );
#'
#' ### Individual-level for multiple determinants
#' P_delta_example <-
#'   behaviorchange::potential_for_change_index(
#'     data=dat,
#'     determinants=c('highDose_attitude', 'highDose_perceivedNorm'),
#'     target='highDose_intention'
#'   );
#' head(P_delta_example);
#'
#' ### Sample-level version for multiple determinants
#' behaviorchange::potential_for_change_index(
#'   data=dat,
#'   determinants=c('highDose_attitude', 'highDose_perceivedNorm'),
#'   target='highDose_intention',
#'   sampleLevel = TRUE
#' );
#'
#' ### Get the Potential for Change Index Type 2
#' behaviorchange::potential_for_change_index(
#'   data=dat,
#'   determinants=c('highDose_attitude', 'highDose_perceivedNorm'),
#'   target='highDose_intention',
#'   type = '2',
#'   sampleLevel = TRUE
#' );
#'
#' ### Get a Determinant Selection Table
#' behaviorchange::determinant_selection_table(
#'   data=dat,
#'   determinants = c('highDose_AttBeliefs_long',
#'                    'highDose_AttBeliefs_intensity',
#'                    'highDose_AttBeliefs_euphoria'),
#'   target = 'highDose_intention',
#'   sortBy = 6
#' );
#'
#' ### R Markdown partials can smoothly be included in RMarkdown documents
#' behaviorchange::determinantSelectionTable_partial(
#'   behaviorchange::determinant_selection_table(
#'     data=dat,
#'     determinants = c('highDose_AttBeliefs_long',
#'                      'highDose_AttBeliefs_intensity',
#'                      'highDose_AttBeliefs_euphoria'),
#'     target = 'highDose_intention',
#'     sortBy = 6
#'   )
#' );
#'
#' ### Room for improvement for one variable
#' head(
#'   room_for_improvement(
#'     dat$highDose_AttBeliefs_long
#'   )
#' );
#'
#' room_for_improvement(
#'   dat$highDose_AttBeliefs_long,
#'   sampleLevel = TRUE
#' );
#'
#' ### For multiple (sub-)determinants
#' head(
#'   room_for_improvement(
#'     dat[, c('highDose_AttBeliefs_long',
#'             'highDose_AttBeliefs_intensity',
#'             'highDose_AttBeliefs_euphoria')]
#'   )
#' );
#'
#' room_for_improvement(
#'   dat[, c('highDose_AttBeliefs_long',
#'           'highDose_AttBeliefs_intensity',
#'           'highDose_AttBeliefs_euphoria')],
#'   sampleLevel = TRUE
#' );
#'
#' @export
potential_for_change_index <-
  intervention_potential <-
  P_delta <-
  function(data,
           determinants,
           target,
           increasesAreImprovements = TRUE,
           sampleLevel = FALSE,
           minimum = base::min,
           maximum = base::max,
           center = base::mean,
           weight = stats::cor,
           type = NULL,
           minimumArgs = list(na.rm=TRUE),
           maximumArgs = list(na.rm=TRUE),
           centerArgs = list(na.rm=TRUE),
           weightArgs = list(use="complete.obs")) {

    ### Templates can override all functions and arguments
    if (!is.null(type)) {
      if (type == 1) {
        minimum <- base::min;
        maximum <- base::max;
        center <- base::mean;
        weight <- stats::cor;
        minimumArgs <- list(na.rm=TRUE);
        maximumArgs <- list(na.rm=TRUE);
        centerArgs <- list(na.rm=TRUE);
        weightArgs <- list(use="complete.obs");
      } else if (type == 2) {
        minimum <- base::min;
        minimum <- function(x) { stats::quantile(x, .05, na.rm=TRUE) }
        maximum <- function(x) { stats::quantile(x, .95, na.rm=TRUE) }
        center <- base::mean;
        weight <- function(x, y) { stats::cor(x, y, use = "complete.obs")^2 }
        minimumArgs <- list();
        maximumArgs <- list();
        centerArgs <- list(na.rm=TRUE);
        weightArgs <- list();
      } else {
        stop("If specifying a type, the only valid values (for now) ",
             "are 1 and 2. You passed ", vecTxtQ(type), ".");
      }
    }

    roomForImprovement <-
      room_for_improvement(
        data[, determinants],
        increasesAreImprovements = increasesAreImprovements,
        sampleLevel = sampleLevel,
        minimum = minimum,
        maximum = maximum,
        center = center,
        minimumArgs = minimumArgs,
        maximumArgs = maximumArgs,
        centerArgs = centerArgs
      );

    ### At this point, roomForImprovement is either a data frame for the
    ### individual-level or a vector for the sample-level P_delta.
    ### Now we get the weights to use.

    if ((is.function(weight)) &&
        (length(methods::formalArgs(weight)) > 1 )) {
    weights <-
      do.call(
        weight,
        c(list(data[, determinants],
               data[, target]),
          weightArgs)
      );
    } else {
      stop("As argument `weight`, you must pass a function that takes as its ",
           "first argument a data frame with determinants and as its second ",
           "argument a data frame with the target.");
    }

    ### Now we have the weights in `weights`. Weights can be positive or
    ### negative, which, in combination with increasesAreImprovements creates
    ### four possible scenarios:
    ###
    ### If increases are improvements and the weight is positive,
    ###   the room for improvement can simply be multiplied (higher weight
    ###   corresponds to higher P_delta).
    ### If increases are improvements and the weight is negative,
    ###   the room for improvement can also simply be multiplied (higher weight
    ###   corresponds to more negative P_delta: this determinant should not be
    ###   increased, but decreased to change the target.
    ### If decreases are improvements and the weight is positive,
    ###   the room for improvement can be multiplied but the sign should change
    ###   (higher weight corresponds to more negative P_delta): this
    ###   determinant should not be increased, but decreased to change the
    ###   target.
    ### If decreases are improvements and the weight is negative,
    ###   the room for improvement can be multiplied but the sign should change
    ###   (higher weight corresponds to higher P_delta).

    if (!increasesAreImprovements) {
      weights <- -1 * weights;
    }

    ### Now we can safely multiply. We have either a vector of individual
    ### 'rooms for improvement' (if one (sub-)determinant was passed and
    ### sampleLevel = FALSE); or a data frame of individual 'rooms for
    ### improvement' (if multiple (sub-)determinant were passed and
    ### sampleLevel = FALSE); or a single value (if one (sub-)determinant
    ### was passed and sampleLevel = TRUE), or a vector of values (if multiple
    ### (sub-)determinants were passed and sampleLevel = TRUE).

    if (sampleLevel) {

      ### Always return a vector
      res <- unlist(weights * roomForImprovement);
      names(res) <- names(roomForImprovement);
      return(res);

    } else {

      if (is.vector(roomForImprovement) && length(weights) == 1) {
        res <- weights * roomForImprovement;
        return(res);
      } else {
        res <-
          lapply(
            seq_along(weights),
            function(i) {
              return(roomForImprovement[, i] * weights[i]);
            }
          );
        res <- as.data.frame(res);
        names(res) <- names(roomForImprovement);
        return(res);
      }

    }

  }
