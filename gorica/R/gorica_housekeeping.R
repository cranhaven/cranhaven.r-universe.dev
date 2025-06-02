gorica_housekeeping <- function(){
  if(!inherits(comparison, "character")|length(comparison) > 1){
    stop("Argument 'comparison' must be an atomic character string.")
  } else {
    comp_arg <- pmatch(comparison, c("unconstrained", "complement", "none"))
    if(is.na(comp_arg)) stop("Argument 'comparison' did not match one of the available options: 'unconstrained', 'complement', or 'none'.")
    comparison <- c("unconstrained", "complement", "none")[pmatch(comparison, c("unconstrained", "complement", "none"))]
  }
  if(inherits(hypothesis, "character")){
    hypothesis <- rename_function(hypothesis)
    hyp_params <- params_in_hyp(hypothesis)
    coef_in_hyp <- sort(unique(charmatch(rename_function(hyp_params),
                                         rename_function(names(x)))))
    if(anyNA(coef_in_hyp)){
      stop("Some of the parameters referred to in the 'hypothesis' do not correspond to parameter names of object 'x'.\n  The following parameter names in the 'hypothesis' did not match any parameters in 'x': ",
           paste(hyp_params[is.na(coef_in_hyp)], collapse = ", "),
           "\n  The parameters in object 'x' are named: ",
           paste(names(x), collapse = ", "))
    }
    if(any(coef_in_hyp == 0)){
      stop("Some of the parameters referred to in the 'hypothesis' matched multiple parameter names of object 'x'.\n  The following parameter names in the 'hypothesis' matched multiple parameters in 'x': ",
           paste(hyp_params[coef_in_hyp == 0], collapse = ", "),
           "\n  The parameters in object 'x' are named: ",
           paste(names(x), collapse = ", "))
    }
    # Drop parameters not in hypothesis
    x <- x[coef_in_hyp]
    Sigma <- Sigma[coef_in_hyp, coef_in_hyp, drop = FALSE]

    hypothesis <- parse_hypothesis(names(x), hypothesis)
  } else {
    if(inherits(hypothesis, "list") & !is.null(hypothesis[["hyp_mat"]]) & !is.null(hypothesis[["n_ec"]])){
      hypothesis$original_hypothesis <- matrix_to_hyp(hypothesis, names(x))
    } else {
      stop("Argument 'hypothesis' must either be a character string with inequality constraints, or a list with an element 'hyp_mat', consisting of a list of hypothesis matrices, and and element 'n_ec', consisting of an integer vector with the number of equality constraints for each hypothesis matrix in 'hyp_mat'.")
    }
  }
  x <<- x
  hypothesis <<- hypothesis
  comparison <<- comparison
  Sigma <<- Sigma

}


