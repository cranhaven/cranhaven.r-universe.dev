#' check_modifier_for_algebra
#'
#' takes in the parameter table and checks if any of the modifiers therein
#' is an mxAlgebra. If so, it replaces the modifier with a temporary name and
#' adds an algebra to the algebra data.frame
#' @param parameter_table parameter table
#' @param directed symbol used to indicate directed effects (regressions and loadings)
#' @param undirected symbol used to indicate undirected effects (variances and covariances)
#' @return parameter_table
#' @keywords internal
check_modifier_for_algebra <- function(parameter_table,
                                       directed,
                                       undirected){

  is_algebra <- grepl(pattern = "^\\{", x = parameter_table$parameter_table$modifier) &
    grepl(pattern = "\\}$", x = parameter_table$parameter_table$modifier)

  if(!any(is_algebra))
    return(parameter_table)

  for(i in which(is_algebra)){

    # remove braces
    cleaned_algebra <-  gsub(
      pattern = "^\\{", replacement = "",
      x = gsub(pattern = "\\}$", replacement = "", x = parameter_table$parameter_table$modifier[i])
    )

    if(grepl(pattern = ":=", x = cleaned_algebra)){
      # already has a variable name
      has_name <- TRUE

      splitted_algebra <- strsplit(x = cleaned_algebra, split = ":=")
      if(length(splitted_algebra) != 1)
        stop("Error while splitting algebra ", cleaned_algebra, ". Are there multiple := ?")
      if(length(splitted_algebra[[1]]) != 2)
        stop("Error while splitting algebra ", cleaned_algebra, ". Are there multiple := ?")

      new_name <- clean_syntax(splitted_algebra[[1]][1])

      cleaned_algebra <- splitted_algebra[[1]][2]
    }else{

      # create new variable name
      has_name <- FALSE
      # directed effect
      if(parameter_table$parameter_table$op[i]  == "~"){
        from <- parameter_table$parameter_table$rhs[i]
        to <- parameter_table$parameter_table$lhs[i]

        if(from == "1"){
          from <- "one"
        }

      }else{
        from <- parameter_table$parameter_table$lhs[i]
        to <- parameter_table$parameter_table$rhs[i]
      }

      if(parameter_table$parameter_table$op[i] == "~~"){
        arrows <- 2
      }else{
        arrows <- 1
      }

      if(arrows == 1){
        new_name <- paste0(from, directed, to)
      }else{
        new_name <- paste0(from, undirected, to)
      }
    }

    parameter_table$algebras <- rbind(parameter_table$algebras,
                                      data.frame(lhs = new_name,
                                                 op  = ":=",
                                                 rhs = cleaned_algebra)
    )

    # create temporary algebra to extract parameter labels
    tmp_algebra <- OpenMx::mxAlgebraFromString(cleaned_algebra)

    tmp_algebra_elements <- extract_algebra_elements(mxAlgebra_formula = tmp_algebra@formula)

    for(tmp_elem in tmp_algebra_elements){

      # check if this is a numeric value
      if(grepl(pattern = "^[0-9\\.]+$", x = tmp_elem))
        next

      # check if this is a definition variable
      if(grepl(pattern = "^data\\.", x = tmp_elem))
        next

      # check if this is already another parameter label; in this case we can
      # just skip and go to the next element
      if(tmp_elem %in% parameter_table$parameter_table$modifier)
        next

      # check if this parameter was already added
      if(tmp_elem %in% parameter_table$new_parameters)
        next

      # check if this parameter is referring to a matrix in the model (e.g., A[1,1])
      if(tmp_elem %in% c("A", "S", "M")){
        warning("Using A, S, or M in your algebras provides direct access to the ",
                "A (directed effects), S (undirected effects), and M (intercepts) ",
                "matrices of your model. For instance, A[1,1] allows direct access to the ",
                "element in row 1, column 1 of the A matrix. If you did want to create a new ",
                "parameter called A, S, or M please rename the parameter.")
        next
      }

      parameter_table$new_parameters <- c(parameter_table$new_parameters,
                                          tmp_elem)
      parameter_table$new_parameters_free <- c(
        parameter_table$new_parameters_free,
        TRUE
      )
    }

    parameter_table$parameter_table$modifier[i] <- ifelse(
      has_name,
      new_name,
      # the modifier will automatically be replaced with the label used above
      "")
    parameter_table$parameter_table$free[i]     <- FALSE
  }
  return(parameter_table)
}
