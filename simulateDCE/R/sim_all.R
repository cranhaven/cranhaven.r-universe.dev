#' Is a wrapper for sim_choice executing the simulation over all designs stored in a specific folder
#' update
#' @param nosim Number of runs or simulations. For testing use 2 but once you go serious, use at least 200, for better results use 2000.
#' @param resps Number of respondents you want to simulate
#' @inheritParams readdesign
#' @param designpath The path to the folder where the designs are stored. For example "c:/myfancydec/Designs"
#' @param reshape_type Must be "auto", "stats" to use the reshape from the stats package or tidyr to use pivot longer. Default is auto and should not bother you. Only change it once you face an error at this position and you may be lucky that it works then.
#' @inheritParams sim_choice
#' @inheritParams simulate_choices
#' @return A list, with all information on the simulation. This list an be easily processed by the user and in the rmarkdown template.
#' @export
#'
#' @examples
#' library(rlang)
#' designpath <- system.file("extdata", "SE_DRIVE", package = "simulateDCE")
#' resps <- 120 # number of respondents
#' nosim <- 2 # number of simulations to run (about 500 is minimum)
#'
#' decisiongroups <- c(0, 0.7, 1)
#'
#' # pass beta coefficients as a list
#' bcoeff <- list(
#'   b.preis = -0.01,
#'   b.lade = -0.07,
#'   b.warte = 0.02
#' )
#'
#' manipulations <- list(
#'   alt1.x2 = expr(alt1.x2 / 10),
#'   alt1.x3 = expr(alt1.x3 / 10),
#'   alt2.x2 = expr(alt2.x2 / 10),
#'   alt2.x3 = expr(alt2.x3 / 10)
#' )
#'
#'
#' # place your utility functions here
#' ul <- list(
#'   u1 =
#'
#'     list(
#'       v1 = V.1 ~ b.preis * alt1.x1 + b.lade * alt1.x2 + b.warte * alt1.x3,
#'       v2 = V.2 ~ b.preis * alt2.x1 + b.lade * alt2.x2 + b.warte * alt2.x3
#'     ),
#'   u2 = list(
#'     v1 = V.1 ~ b.preis * alt1.x1,
#'     v2 = V.2 ~ b.preis * alt2.x1
#'   )
#' )
#'
#'
#' sedrive <- sim_all(
#'   nosim = nosim,
#'   resps = resps,
#'   designpath = designpath,
#'   u = ul,
#'   bcoeff = bcoeff,
#'   decisiongroups = decisiongroups,
#'   manipulations = manipulations,
#'   utility_transform_type = "exact",
#'   mode = "sequential",
#'   estimate=FALSE
#' )
#'
sim_all <- function(nosim = 2,
                    resps,
                    designtype = NULL,
                    destype = NULL,
                    designpath,
                    u,
                    bcoeff,
                    decisiongroups = c(0, 1),
                    manipulations = list(),
                    estimate = TRUE,
                    chunks = 1,
                    utility_transform_type = "simple",
                    reshape_type = "auto",
                    mode = c("parallel", "sequential"),
                    preprocess_function = NULL,
                    savefile = NULL) {
  #################################################
  ########## Input Validation Test ###############
  #################################################
  mode <- match.arg(mode)
  ########### validate the utility function ########
  if (missing(u) || !(is.list(u) && any(sapply(u, is.list)))) {
    stop(
      " 'u' must be provided and must be a list containing at least one list element (list of lists)."
    )
  }

  ########## validate the bcoeff list ################
  # Check if bcoeff is provided
  if (missing(bcoeff)) {
    stop("Argument 'bcoeff' is required.")
  }


  if (nosim < chunks) {
    stop(
      "You cannot have more chunks than runs. The number of chunks tells us how often we save the simulation results on disk. Maximum one per run."
    )
  }

  # Check if bcoeff is a list
  if (!is.list(bcoeff)) {
    stop("Argument 'bcoeff' must be a list.")
  }

  if (length(u) != length(decisiongroups) - 1) {
    stop("Number of decision groups must equal number of utility functions!")
  }
  if (!is.vector(decisiongroups)) {
    stop("Decision groups must be a vector.")
  }

  # Check if decisiongroups starts with 0
  if (decisiongroups[1] != 0) {
    stop("Decision groups must start with 0.")
  }

  # Check if decisiongroups ends with 1
  if (utils::tail(decisiongroups, 1) != 1) {
    stop("Decision groups must end with 1.")
  }


  # Check if values in bcoeff are numeric
  if (!all(sapply(bcoeff, is.numeric))) {
    stop("Values in 'bcoeff' must be numeric.")
  }

  #### check that all the coefficients in utility function have a corresponding value in bcoeff ####
  # Extract coefficients from utility function starting with "b"
  coeff_names_ul <- unique(unlist(lapply(u, function(u) {
    formula_strings <- unlist(u)
    coef_names <- unique(unlist(lapply(formula_strings, function(f) {
      # Parse the formula to extract coefficient names
      all_vars <- all.vars(stats::as.formula(f))
      coef_vars <- all_vars[grep("^b", all_vars)]
      return(coef_vars)
    })))
    return(coef_names)
  })))

  # Check if all utility function coefficients starting with "b" are covered in bcoeff list
  missing_coeffs <- coeff_names_ul[!(coeff_names_ul %in% names(bcoeff))]
  if (length(missing_coeffs) > 0) {
    stop(paste(
      "Missing coefficients in 'bcoeff':",
      paste(missing_coeffs, collapse = ", "),
      ". Perhaps there is a typo?"
    ))
  }
  ########## validate resps #####################
  if (missing(resps) ||
    !(is.integer(resps) ||
      (is.numeric(resps) && identical(trunc(resps), resps)))) {
    stop(
      " 'resps' must be provided and must be an integer indicating  the number of respondents per run."
    )
  }
  ########## validate designpath ################
  if (!dir.exists(designpath)) {
    stop(
      " The folder where your designs are stored does not exist. \n Check if designpath is correctly specified"
    )
  }

  #################################################
  ########## End Validation Tests #################
  #################################################


  bcoeff_result <- modify_bcoeff_names(bcoeff)
  bcoeff <- bcoeff_result$bcoeff



  designfile <- list.files(designpath, full.names = T)
  designname <- stringr::str_remove_all(list.files(designpath, full.names = F), "(.ngd|_|.RDS)") ## Make sure designnames to not contain file ending and "_", as the may cause issues when replace


  tictoc::tic("total time for simulation and estimation")

  if (is.null(savefile)) {
    all_designs <- purrr::map(
      designfile,
      sim_choice,
      no_sim = nosim,
      respondents = resps,
      designtype = designtype,
      destype = destype,
      u = u,
      bcoeff = bcoeff,
      decisiongroups = decisiongroups,
      manipulations = manipulations,
      estimate = estimate,
      chunks = chunks,
      utility_transform_type = utility_transform_type,
      mode = mode,
      preprocess_function = preprocess_function,
      savefile = NULL
    ) %>% ## iterate simulation over all designs
      stats::setNames(designname)
  } else {
    purrr::walk(
      designfile,
      sim_choice,
      no_sim = nosim,
      respondents = resps,
      designtype = designtype,
      destype = destype,
      u = u,
      bcoeff = bcoeff,
      decisiongroups = decisiongroups,
      manipulations = manipulations,
      estimate = estimate,
      chunks = chunks,
      utility_transform_type = utility_transform_type,
      mode = mode,
      preprocess_function = preprocess_function,
      savefile = savefile
    )
    gc()

    all_designs <- purrr::map(list.files(dirname(savefile), full.names = TRUE), qs::qread) %>%
      stats::setNames(designname)
  }

  time <- tictoc::toc()

  message(paste(utils::capture.output(print(time)), collapse = "\n"))


  all_designs[["time"]] <- time
  all_designs[["arguements"]] <- list(
    "Beta values" = bcoeff,
    "Utility functions" = u,
    "Decision groups" = decisiongroups,
    "Manipulation of vars" = manipulations,
    "Number Simulations" = nosim,
    "Respondents" = resps,
    "Designpath" = designpath,
    "Reshape Type" = reshape_type,
    "mode" = mode,
    "designname" = designname
  )

  if (estimate == TRUE) {
    all_designs <- simulateDCE::aggregateResults(all_designs = all_designs)
  }





  return(all_designs)
}
