############ Validation of Vectors containing frequency values #################

#' Validate methylation state equilibrium frequencies: sum 1 within tolerance
#'
#' @param freqVector Numeric vector containing 3 frequency values.
#' @param tolerance Numeric. Tolerance value to floating-point arithmetic deviations.
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{\code{valid}}{A logical value indicating the validity of the freqVector TRUE if the freqVector is valid, FALSE otherwise.}
#'   \item{\code{error}}{A character string containing the error message if the freqVector is invalid. NULL if the freqVector is valid.}
#'   \item{\code{freqVector}}{The input freqVector that was validated.}
#' }
#'
#'@noRd
validate_freqVectorSums1 <- function(freqVector, tolerance = 1e-5) {
  if (!is.vector(freqVector) || !is.numeric(freqVector)) {stop("Argument 'freqVector' must be a numeric vector") }
  tryCatch({
    # Check if freqs sum to 1
    if (!isTRUE(all.equal(sum(freqVector), 1, tolerance))) {
      stop("Methylation equilibrium freqs do not sum to 1 within tolerance")
    } else {
      return(list(valid = TRUE, error = NULL, freqVector = freqVector))
    }
  }, error = function(e) {
    return(list(valid = FALSE, error = conditionMessage(e), freqVector = freqVector))
  })
}

#' Validate methylation state equilibrium frequencies: elements between 0 and 1
#'
#' @param freqVector Numeric vector containing 3 frequency values.
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{\code{valid}}{A logical value indicating the validity of the freqVector. TRUE if the freqVector is valid, FALSE otherwise.}
#'   \item{\code{error}}{A character string containing the error message if the freqVector is invalid. NULL if the freqVector is valid.}
#'   \item{\code{freqVector}}{The input freqVector that was validated.}
#' }
#'
#'@noRd
validate_freqVector_elements <- function(freqVector) {
  if (!is.vector(freqVector) || !is.numeric(freqVector)) {stop("Argument 'freqVector' must be a numeric vector") }
  tryCatch({
    # Check if freqVector elements are between 0 and 1
    if (all(freqVector >= 0 & freqVector <= 1 )) {
      return(list(valid = TRUE, error = NULL, freqVector = freqVector))
    } else {
      stop("All freqVector elements should be between 0 and 1 (inclusive)")
    }
  }, error = function(e) {
    return(list(valid = FALSE, error = conditionMessage(e), freqVector = freqVector))
  })
}

#' Validate a list of frequency vectors
#'
#' This function validates a list of methylation state tripple of equilibrium frequencies.
#' It checks whether each vector of frequencies in the input list is a valid
#' freqVector by performing specific validation checks such as
#' validating frequencies sum, and elements between 0 and 1.
#'
#' @param listFreqVector A list of freqVector frequency vectors to be validated (as list).
#' @param listName The name of the input list of frequency vectors (as character string).
#' @return The output list contains two elements: $validationStates and $listName.
#'         $listName is the name of the input list provided.
#'         $validationStates is a list containing validation states for each input frequency vector.
#'         Each element of the list corresponds to a frequency vector and contains 2
#'         lists with the validation states for (1) frequencies sum and (2) elements between 0 and 1.
#'         The validation states contain $valid, $error and $freqs.
#'         E.g. vector one, frequencies sum validation state information can be indexed as:
#'         output$validationStates[[1]][[1]]$valid
#' @details This function calls specific validation functions to validate
#'          frequencies sum, and elements between 0 and 1.
#'          The validation results are stored in a list for each freqVector in the
#'          input list. If a freqVector passes all validation checks, the corresponding
#'          validation state will indicate that the matrix is valid.
#' @seealso \code{\link{validate_freqVectorSums1}},
#'           \code{\link{validate_freqVector_elements}}
#'
#'@noRd
listFreqVector_validation <- function(listFreqVector, listName){
  if (!is.character(listName)) {stop("Argument 'listName' must be a character string")}
  validationStates <- list()
  for (i in 1: length(listFreqVector)){
    if (!is.vector(listFreqVector[[i]])) {stop("Argument 'listFreqVector' must be a vector")}
    validationState_freqVector_sum <- validate_freqVectorSums1(listFreqVector[[i]])
    validationState_freqVector_elements <- validate_freqVector_elements(listFreqVector[[i]])
    validationStates[[i]] <- list(validationState_freqVector_sum,
                                  validationState_freqVector_elements)
  }
  return(list(validationStates = validationStates, listName = listName))
}

#' Check errors in Frequency Vector Validation Results and Print Report
#'
#' This function checks Validation Results for a list of frequency vectors and prints a report
#' for vectors with invalid states. It checks the 'valid' field in the
#' list of validation states for each frequency vector. If any vector has 'valid = FALSE',
#' it prints the vector's details to the console and logs the information in a
#' validation results file.
#'
#' @param listValidationStates A list of validation states for frequency vectors.
#'         It contains two elements: $validationStates and $listName.
#'         $listName is the name of the input list provided.
#'         $validationStates is a list containing validation states for each input frequency vector.
#'         Each element of the list corresponds to a frequency vector and contains 3
#'         lists with the validation states for (1) row sums, (2) diagonal elements,
#'         and (3) non-diagonal elements.
#'         The validation states contain $valid, $error and $freqs
#'         E.g. vector one, row sums validation state information can be indexed as:
#'         output$validationStates[[1]][[1]]$valid
#'        Input structure as output in \code{\link{listFreqVector_validation}}
#'
#'
#' @return None. The function prints the validation results and logs them in a file.
#'
#'
#' @noRd
listFreqVector_validationResults <- function(listValidationStates) {
  # Check if the input has the correct structure
  if (!is.list(listValidationStates) ||
      length(listValidationStates) != 2 ||
      !is.list(listValidationStates$validationStates) ||
      !is.character(listValidationStates$listName)) {
    stop("Invalid input structure.")
  }
  # Set the input elements
  validationStates <- listValidationStates$validationStates
  listName <- listValidationStates$listName
  # Check if any validation state has valid = FALSE
  if (any(sapply(validationStates, function(states) any(!sapply(states, function(state) state$valid))))) {
    # Open a file connection for writing
    warnstring <- paste("Run date and time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), 
                        "\nThis is a redirected validation result for list of frequency vectors:", listName, "\n")
    for (i in 1:length(validationStates)) {
      # Check if any validation state in the current list has valid = FALSE
      if (any(!sapply(validationStates[[i]], function(state) state$valid))) {
        warnstring <- paste(warnstring, "Validation result listFreqVector vector number:", i, "\n")
        # If validation state is FALSE, print the results in the file
        for (j in 1:length(validationStates[[i]])) {
          if (!validationStates[[i]][[j]]$valid) {
            warnstring <- paste(warnstring, toString(validationStates[[i]][[j]]), "\n")
          }
        }
      }
    }
    warning(warnstring)
  } 
}

############ Validation of Matrices ############################################


#### Rate Matrices ############################## # #

#' Validate rate matrix: rows sum 0 within tolerance
#'
#' @param matrix Rate matrix.
#' @param tolerance Numeric. Tolerance value to floating-point arithmetic deviations.
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{\code{valid}}{A logical value indicating the validity of the matrix. TRUE if the matrix is valid, FALSE otherwise.}
#'   \item{\code{error}}{A character string containing the error message if the matrix is invalid. NULL if the matrix is valid.}
#'   \item{\code{matrix}}{The input matrix that was validated.}
#' }
#'
#'@noRd
validate_rate_matrix_rowSums <- function(matrix, tolerance = 1e-5) {
  if (!is.matrix(matrix)) {stop("Argument 'matrix' must be a matrix") }
  tryCatch({
    # Check if rows sum to 0 with tolerance
    row_sums <- apply(matrix, 1, sum)
    if (any(abs(row_sums) > tolerance)) {
      stop("Rows do not sum to 0 within tolerance")
    } else {
      return(list(valid = TRUE, error = NULL, matrix = matrix))
    }
  }, error = function(e) {
    return(list(valid = FALSE, error = conditionMessage(e), matrix = matrix))
  })
}

#' Validate rate matrix: non-positive diagonal elements
#'
#' @param matrix Rate matrix.
#' @param tolerance Numeric. Tolerance value to floating-point arithmetic deviations.
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{\code{valid}}{A logical value indicating the validity of the matrix. TRUE if the matrix is valid, FALSE otherwise.}
#'   \item{\code{error}}{A character string containing the error message if the matrix is invalid. NULL if the matrix is valid.}
#'   \item{\code{matrix}}{The input matrix that was validated.}
#' }
#'
#'@noRd
validate_rate_matrix_diagonalElements <- function(matrix) {
  if (!is.matrix(matrix)) {stop("Argument 'matrix' must be a matrix")}
  tryCatch({
    diagonal_elements <- diag(matrix)
    if (any(diagonal_elements > 0)) {
      stop("Diagonal elements should be equal to or lower than 0")
    } else {
      return(list(valid = TRUE, error = NULL, matrix = matrix))
    }
  }, error = function(e) {
    return(list(valid = FALSE, error = conditionMessage(e), matrix = matrix))
  })
}

#' Validate rate matrix: non-negative non-diagonal elements
#'
#' @param matrix Rate matrix.
#' @param tolerance Numeric. Tolerance value to floating-point arithmetic deviations.
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{\code{valid}}{A logical value indicating the validity of the matrix. TRUE if the matrix is valid, FALSE otherwise.}
#'   \item{\code{error}}{A character string containing the error message if the matrix is invalid. NULL if the matrix is valid.}
#'   \item{\code{matrix}}{The input matrix that was validated.}
#' }
#'
#'@noRd
validate_rate_matrix_nonDiagonalElements <- function(matrix) {
  if (!is.matrix(matrix)) {stop("Argument 'matrix' must be a matrix")}
  tryCatch({
    # Get the non-diagonal elements using logical indexing
    non_diagonal_elements <- matrix[lower.tri(matrix) | upper.tri(matrix)]

    # Check if any non-diagonal element is less than 0
    if (any(non_diagonal_elements < 0)) {
      stop("Non-diagonal elements should be equal to or greater than 0")
    } else {
      return(list(valid = TRUE, error = NULL, matrix = matrix))
    }
  }, error = function(e) {
    return(list(valid = FALSE, error = conditionMessage(e), matrix = matrix))
  })
}


#' Validate a list of rate matrices
#'
#' This function validates a list of rate matrices. It checks whether each element
#' in the input list is a valid matrix by performing specific validation checks
#' such as validating row sums, diagonal elements, and non-diagonal elements.
#'
#' @param listRateMatrix A list of rate matrices to be validated.
#' @param listName The name of the input list of rate matrices.
#' @return The output list contains two elements: $validationStates and $listName.
#'         $listName is the name of the input list provided.
#'         $validationStates is a list containing validation states for each input rate matrix.
#'         Each element of the list corresponds to a rate matrix and contains 3
#'         lists with the validation states for (1) row sums, (2) diagonal elements,
#'         and (3) non-diagonal elements.
#'         The validation states contain $valid, $error and $matrix.
#'         E.g. matrix one, row sums validation state information can be indexed as:
#'         output$validationStates[[1]][[1]]$valid
#' @details This function calls specific validation functions to validate row sums,
#'          diagonal elements, and non-diagonal elements of each input matrix.
#'          The validation results are stored in a list for each matrix in the
#'          input list. If a matrix passes all validation checks, the corresponding
#'          validation state will indicate that the matrix is valid.
#' @seealso \code{\link{validate_rate_matrix_rowSums}},
#'           \code{\link{validate_rate_matrix_diagonalElements}},
#'           \code{\link{validate_rate_matrix_nonDiagonalElements}}
#'
#'@noRd
listRateMatrix_validation <- function(listRateMatrix, listName){
  if (!is.character(listName)) {stop("Argument 'listName' must be a character string")}
  validationStates <- list()
  for (i in 1: length(listRateMatrix)){
    if (!is.matrix(listRateMatrix[[i]])) {stop("Argument 'matrix' must be a matrix")}
    validationState_rate_matrix_rowSums <- validate_rate_matrix_rowSums(listRateMatrix[[i]])
    validationState_rate_matrix_diagonalElements <- validate_rate_matrix_diagonalElements(listRateMatrix[[i]])
    validationState_rate_matrix_nonDiagonalElements <- validate_rate_matrix_nonDiagonalElements(listRateMatrix[[i]])
    validationStates[[i]] <- list(validationState_rate_matrix_rowSums,
                                  validationState_rate_matrix_diagonalElements,
                                  validationState_rate_matrix_nonDiagonalElements)
  }
  return(list(validationStates = validationStates, listName = listName))
}

#### Transition Matrices ############################## # #

#' Validate transition matrix: rows sum 1 within tolerance
#'
#' @param matrix Transition matrix.
#' @param tolerance Numeric. Tolerance value to floating-point arithmetic deviations.
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{\code{valid}}{A logical value indicating the validity of the matrix. TRUE if the matrix is valid, FALSE otherwise.}
#'   \item{\code{error}}{A character string containing the error message if the matrix is invalid. NULL if the matrix is valid.}
#'   \item{\code{matrix}}{The input matrix that was validated.}
#' }
#'
#'@noRd
validate_transition_matrix_rowSums <- function(matrix, tolerance = 1e-5) {
  if (!is.matrix(matrix)) {stop("Argument 'matrix' must be a matrix") }
  tryCatch({
    # Check if rows sum to 1 within tolerance
    row_sums <- apply(matrix, 1, sum)
    if (any(abs(row_sums -1) > tolerance)) {
      stop("Rows do not sum to 1 within tolerance")
    } else {
      return(list(valid = TRUE, error = NULL, matrix = matrix))
    }
  }, error = function(e) {
    return(list(valid = FALSE, error = conditionMessage(e), matrix = matrix))
  })
}

#' Validate transition matrix: matrix elements between 0 and 1
#'
#' @param matrix Transition matrix.
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{\code{valid}}{A logical value indicating the validity of the matrix. TRUE if the matrix is valid, FALSE otherwise.}
#'   \item{\code{error}}{A character string containing the error message if the matrix is invalid. NULL if the matrix is valid.}
#'   \item{\code{matrix}}{The input matrix that was validated.}
#' }
#'
#'@noRd
validate_transition_matrix_elements <- function(matrix) {
  if (!is.matrix(matrix)) {stop("Argument 'matrix' must be a matrix")}
  tryCatch({
    # Check if matrix elements are between 0 and 1
    diagonal_elements <- diag(matrix)
    if (all(matrix >= 0 & matrix <= 1 )) {
      return(list(valid = TRUE, error = NULL, matrix = matrix))
    } else {
      stop("All elements should be between 0 and 1 (inclusive)")
    }
  }, error = function(e) {
    return(list(valid = FALSE, error = conditionMessage(e), matrix = matrix))
  })
}

#' Validate a list of transition matrices
#'
#' This function validates a list of transition matrices. It checks whether each element
#' in the input list is a valid matrix by performing specific validation checks
#' such as validating row sums, and elements between 0 and 1.
#'
#' @param listTransitionMatrix A list of transition matrices to be validated (as list).
#' @param listName The name of the input list of transition matrices (as character string).
#' @return The output list contains two elements: $validationStates and $listName.
#'         $listName is the name of the input list provided.
#'         $validationStates is a list containing validation states for each input transition matrix.
#'         Each element of the list corresponds to a transition matrix and contains 2
#'         lists with the validation states for (1) row sums and (2) matrix elements.
#'         The validation states contain $valid, $error and $matrix.
#'         E.g. matrix one, row sums validation state information can be indexed as:
#'         output$validationStates[[1]][[1]]$valid
#' @details This function calls specific validation functions to validate row sums,
#'          and elements between 0 and 1 of each input matrix.
#'          The validation results are stored in a list for each matrix in the
#'          input list. If a matrix passes all validation checks, the corresponding
#'          validation state will indicate that the matrix is valid.
#' @seealso \code{\link{validate_transition_matrix_rowSums}},
#'           \code{\link{validate_transition_matrix_elements}}
#'
#'@noRd
listTransitionMatrix_validation <- function(listTransitionMatrix, listName){
  if (!is.character(listName)) {stop("Argument 'listName' must be a character string")}
  validationStates <- list()
  for (i in 1: length(listTransitionMatrix)){
    if (!is.matrix(listTransitionMatrix[[i]])) {stop("Argument 'matrix' must be a matrix")}
    validationState_transition_matrix_rowSums <- validate_transition_matrix_rowSums(listTransitionMatrix[[i]])
    validationState_transition_matrix_elements <- validate_transition_matrix_elements(listTransitionMatrix[[i]])
    validationStates[[i]] <- list(validationState_transition_matrix_rowSums,
                                  validationState_transition_matrix_elements)
  }
  return(list(validationStates = validationStates, listName = listName))
}

#' Check errors in Matrix Validation Results and Print Report
#'
#' This function checks Validation Results for a list of rate matrices and prints a report
#' for matrices with invalid states. It checks the 'valid' field in the
#' list of validation states for each rate matrix. If any matrix has 'valid = FALSE',
#' it prints the matrix's details to the console and logs the information in a
#' validation results file.
#'
#' @param listValidationStates A list of validation states for rate matrices.
#'         It contains two elements: $validationStates and $listName.
#'         $listName is the name of the input list provided.
#'         $validationStates is a list containing validation states for each input rate matrix.
#'         Each element of the list corresponds to a rate matrix and contains 3
#'         lists with the validation states for (1) row sums, (2) diagonal elements,
#'         and (3) non-diagonal elements.
#'         The validation states contain $valid, $error and $matrix.
#'         E.g. matrix one, row sums validation state information can be indexed as:
#'         output$validationStates[[1]][[1]]$valid
#'        Input structure as output in \code{\link{listRateMatrix_validation}}
#'
#'
#' @return None. The function prints the validation results and logs them in a file.
#'
#'
#' @noRd
listMatrices_validationResults <- function(listValidationStates) {
  # Check if the input has the correct structure
  if (!is.list(listValidationStates) ||
      length(listValidationStates) != 2 ||
      !is.list(listValidationStates$validationStates) ||
      !is.character(listValidationStates$listName)) {
    stop("Invalid input structure.")
  }
  # Set the input elements
  validationStates <- listValidationStates$validationStates
  listName <- listValidationStates$listName
  # Check if any validation state has valid = FALSE
  if (any(sapply(validationStates, function(states) any(!sapply(states, function(state) state$valid))))) {
    warnstring <- paste("Run date and time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      "\nThis is a redirected validation result for list of matrices:", listName, "\n")
    for (i in 1:length(validationStates)) {
      # Check if any validation state in the current list has valid = FALSE
      if (any(!sapply(validationStates[[i]], function(state) state$valid))) {
        warnstring <- paste(warnstring, "Validation result listMatrices matrix number:", i, "\n") 
        # If validation state is FALSE, print the results
        for (j in 1:length(validationStates[[i]])) {
          if (!validationStates[[i]][[j]]$valid) {
            warnstring <- paste(warnstring, toString(validationStates[[i]][[j]]), "\n")
          }
        }
      }
    }
    warning(warnstring)
  }else{ return(NULL) }
}


############ Validation Transition Property of Markov Chain ####################

#' Validate state transition property of Markov Chain in IWE
#'
#' This function validates the state transition property of a Markov Chain in the context of Incomplete Waves of Evolution (IWE). It checks if the transition matrix (Mk) correctly transforms the initial state frequencies (old_eqFreqs) to the new state frequencies (new_eqFreqs). The validation is performed within a specified tolerance level.
#'
#' @param old_eqFreqs Numeric vector containing the initial equilibrium state frequencies.
#' @param Mk Transition matrix representing the state transitions.
#' @param new_eqFreqs Numeric vector containing the resulting equilibrium state frequencies after the transition.
#' @param tolerance Numeric. Tolerance value to account for floating-point arithmetic deviations in the validation.
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{\code{valid}}{A logical value indicating the validity of the state transition property. TRUE if the property is valid, FALSE otherwise.}
#'   \item{\code{error}}{A character string containing the error message if the property is invalid. NULL if the property is valid.}
#'   \item{\code{old_eqFreqs}}{The input vector of initial equilibrium state frequencies that were validated.}
#'   \item{\code{Mk}}{The transition matrix used for validation.}
#'   \item{\code{new_eqFreqs}}{The resulting vector of equilibrium state frequencies after the transition.}
#' }
#'
#'@noRd
validate_transPropMC <- function(old_eqFreqs, Mk, new_eqFreqs, tolerance = 1e-5) {
  if (!is.vector(old_eqFreqs) || !is.numeric(old_eqFreqs)) {stop("Argument 'old_eqFreqs' must be a numeric vector") }
  if (!is.vector(new_eqFreqs) || !is.numeric(new_eqFreqs)) {stop("Argument 'new_eqFreqs' must be a numeric vector") }
  if (!is.matrix(Mk)) {stop("Argument 'Mk' must be a matrix") }
  tryCatch({
    # Check if freqs sum to 1
    if (!isTRUE(all.equal(as.vector(old_eqFreqs %*% Mk), new_eqFreqs, tolerance))) {
      stop("IWE does not fullfil state transition property of Markov Chain")
    } else {
      return(list(valid = TRUE, error = NULL, old_eqFreqs = old_eqFreqs,
                  Mk = Mk, new_eqFreqs = new_eqFreqs))
    }
  }, error = function(e) {
    return(list(valid = FALSE, error = conditionMessage(e), old_eqFreqs = old_eqFreqs,
                Mk = Mk, new_eqFreqs = new_eqFreqs))
  })
}

#' Validate Markov Chain State Transition Property with listName
#'
#' This function validates the state transition property of a Markov Chain.
#' It checks whether the given transition matrix (Mk) correctly transforms the initial state frequencies (\code{old_eqFreqs}) to the resulting state frequencies (\code{new_eqFreqs}).
#' The validation results are stored in a list indicating the validity of the state transition property for the provided inputs and its name.
#'
#' @param old_eqFreqs Numeric vector containing the initial equilibrium state frequencies.
#' @param Mk Transition matrix representing the state transitions.
#' @param new_eqFreqs Numeric vector containing the resulting equilibrium state frequencies after the transition.
#' @param listName Character string specifying the name of the input list of transition matrices.
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{\code{validationStates}}{A list containing validation states for the given transition matrix. Each element of the list corresponds to a validation check and contains \code{valid} (logical indicating validation status), \code{error} (character string with error message if invalid), and \code{matrix} (the input matrix being validated).}
#'   \item{\code{listName}}{The name of the input list provided (as specified by \code{listName} parameter).}
#' }
#'
#' @seealso \code{\link{validate_transPropMC}}
#'
#' @noRd
transPropMC_validation <- function(old_eqFreqs, Mk, new_eqFreqs, listName){
  if (!is.character(listName)) {stop("Argument 'listName' must be a character string")}
  if (!is.vector(old_eqFreqs) || !is.numeric(old_eqFreqs)) {stop("Argument 'old_eqFreqs' must be a numeric vector")}
  if (!is.matrix(Mk)) {stop("Argument 'Mk' must be a matrix")}
  if (!is.vector(new_eqFreqs) || !is.numeric(new_eqFreqs)) {stop("Argument 'new_eqFreqs' must be a numeric vector")}
  validationStates <- validate_transPropMC(old_eqFreqs, Mk, new_eqFreqs)
  return(list(validationStates = validationStates, listName = listName))
}

#' Check Errors in Markov Chain State Transition Property Validation Results and Print Report
#'
#' This function checks the validation results for a list of Markov Chain state transition property matrices and prints a report for matrices with invalid states. It checks the 'valid' field in the list of validation states for each matrix. If any matrix has 'valid = FALSE', it prints the matrix's details to the console and logs the information in a validation results file.
#'
#' @param listValidationStates A list of validation states for Markov Chain state transition property matrices.
#'         It contains two elements: $validationStates and $listName.
#'         $listName is the name of the input list provided.
#'         $validationStates is a list containing validation states.
#'         The validation states contain $valid, $error, $old_eqFreqs, $Mk and $new_eqFreqs
#'         Input structure as output in \code{\link{validate_transPropMC}}
#'
#' @return None. The function prints the validation results and logs them in a file.
#'
#' @details The function appends the validation results to an existing validation results file. If any invalid states are found, it prints the detailed information about the invalid matrices and redirects the output to the validation results file.
#'
#' @seealso \code{\link{transPropMC_validation}}
#'
#' @noRd
transPropMC_validationResults <- function(listValidationStates) {
  # Check if the input has the correct structure
  if (!is.list(listValidationStates) ||
      length(listValidationStates) != 2 ||
      !is.list(listValidationStates$validationStates) ||
      !is.character(listValidationStates$listName)) {
    stop("Invalid input structure.")
  }
  # Set the input elements
  validationStates <- listValidationStates$validationStates
  listName <- listValidationStates$listName
  # Check if validation state has valid = FALSE
  if (!listValidationStates$validationStates$valid) {
    warning(paste("Run date and time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n",
                  "This is a redirected validation result for Markov Chain State Transition Property:", listName, "\n",
                  paste(names(listValidationStates$validationStates), sapply(listValidationStates$validationStates, toString), sep=": ", collapse="; "),"\n"))
    }
}
