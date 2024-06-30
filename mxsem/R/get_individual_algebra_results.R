#' get_individual_algebra_results
#'
#' evaluates algebras for each subject in the data set. This function is
#' useful if you have algebras with definition variables (e.g., in mnlfa).
#' @param mxModel mxModel with algebras
#' @param algebra_names optional: Only compute individual algebras for a subset
#' of the parameters
#' @param progress_bar should a progress bar be shown?
#' @returns a list of data frames. The list contains data frames for each of the algebras.
#' The data frames contain the individual specific algebra results as well as all
#' definition variables used to predict said algebra
#' @export
#' @importFrom utils txtProgressBar
#' @importFrom utils setTxtProgressBar
#' @examples
#' library(mxsem)
#'
#' set.seed(123)
#' dataset <- simulate_moderated_nonlinear_factor_analysis(N = 50)
#'
#' model <- "
#'   xi  =~ x1 + x2 + x3
#'   eta =~ y1 + y2 + y3
#'   eta ~  {a := a0 + data.k*a1}*xi
#'   "
#' fit <- mxsem(model = model,
#'              data = dataset) |>
#'   mxTryHard()
#'
#' algebra_results <- get_individual_algebra_results(mxModel = fit,
#'                                                   progress_bar = FALSE)
#'
#' # the following plot will only show two data points because there is only
#' # two values for the definition variable k (0 or 1).
#'
#' plot(x = algebra_results[["a"]]$k,
#'      y = algebra_results[["a"]]$algebra_result)
get_individual_algebra_results <- function(mxModel,
                                           algebra_names = NULL,
                                           progress_bar = TRUE){
  n_subjects <- mxModel$data$numObs
  if(is.null(algebra_names)){
    algebra_names <- names(mxModel$algebras)
  }else{
    if(any(! algebra_names %in% names(mxModel$algebras)))
      stop("Could not find the following algebra(s) in your model: ",
           paste0(algebra_names[! algebra_names %in% names(mxModel$algebras)], collapse = ", "),
           ". The algebras are: ",
           paste0(names(mxModel$algebras), collapse = ", "), ".")
  }

  if(is.null(algebra_names) | (length(algebra_names) == 0))
    stop("Could not find any algebras in your OpenMx model.")


  algebra_results <- vector("list", length(algebra_names))
  names(algebra_results) <- algebra_names

  if(progress_bar)
    pb <- utils::txtProgressBar(min = 0,
                                max = length(algebra_names) * n_subjects,
                                initial = 0,
                                style = 3)

  it <- 0

  for(algebra_name in algebra_names){

    # find definition variables used in this algebra
    algebra_elements <- extract_algebra_elements(mxAlgebra_formula = mxModel$algebras[[algebra_name]]$formula)
    definition_variables <- algebra_elements[grepl("^data\\.", x = algebra_elements)] |>
      gsub(pattern = "data\\.", replacement = "", x = _)

    algebra_result <- data.frame(person = 1:n_subjects,
                                 mxModel$data$observed[,definition_variables, drop = FALSE],
                                 algebra_result = NA)

    for(i in 1:n_subjects){
      it <- it + 1
      if(progress_bar)
        utils::setTxtProgressBar(pb = pb,
                                 value = it)

      algebra_result_i <- OpenMx::mxEvalByName(name = algebra_name,
                                               model = mxModel,
                                               compute = TRUE,
                                               defvar.row = i)
      if((nrow(algebra_result_i) != 1) |
         (ncol(algebra_result_i) != 1))
        stop("This function cannot handle algebras with non-scalar outcomes.")

      algebra_result$algebra_result[i] <- algebra_result_i[1,1]
    }

    algebra_results[[algebra_name]] <- algebra_result
  }

  return(algebra_results)
}
