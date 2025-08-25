utils::globalVariables("K_i")
utils::globalVariables("K_pop")

#' Title Internal function used to validate the .tbl input
#'
#' @param .tbl tibble with threshold and role information
#' @param pid personal identified
#' @param fid family identifier
#' @param role role of the individual(s)
#' @param useMixture logical variable. If TRUE, the function will return for K_i and K_pop columns for mixture estimates of liability.
#'
#' @returns validated tibble
#'
#' @importFrom dplyr pull select
#' @noRd
validating_tbl_input = function(.tbl, pid, fid, role, useMixture) {
  # Turning .tbl into a tibble
  # if it is not of class tbl
  if (!is.null(.tbl) && !tibble::is_tibble(.tbl))  .tbl <- tibble::as_tibble(.tbl)

  # role (as string) must be supplied
  if (is.null(role)) stop("role must be specified.")
  # if role is supplied, convert to string
  if (!is.null(role)) role <- as.character(role)

  # Checking that .tbl has three columns named pid, fid and role
  if (!(pid %in% colnames(.tbl))) stop(paste0("The column ", pid," does not exist in the tibble .tbl..."))
  if (!(fid %in% colnames(.tbl))) stop(paste0("The column ", fid," does not exist in the tibble .tbl..."))
  if (!(role %in% colnames(.tbl))) stop(paste0("The column ", role," does not exist in the tibble .tbl..."))

  # In addition, we check that two columns named lower and upper are present
  if (any(!c("lower","upper") %in% colnames(.tbl))) stop("The tibble .tbl must include two columns named 'lower' and 'upper'!")
  # if use mixture is true, we will also check for columns K_i and K_pop
  if (useMixture) {
    if (!("K_i" %in% colnames(.tbl))) stop("The column K_i does not exist in the tibble .tbl...")
    if (!("K_pop" %in% colnames(.tbl))) stop("The column K_pop does not exist in the tibble .tbl...")
  }

  # If the tibble consists of more than the required columns,
  # we select only the relevant ones.
  if (useMixture) {
    .tbl <- select(.tbl,
                   !!as.symbol(fid),
                   !!as.symbol(pid),
                   !!as.symbol(role),
                   tidyselect::starts_with("lower"),
                   tidyselect::starts_with("upper"),
                   tidyselect::starts_with("K_i"),
                   tidyselect::starts_with("K_pop"))
  } else {

    .tbl <- select(.tbl,
                   !!as.symbol(fid),
                   !!as.symbol(pid),
                   !!as.symbol(role),
                   tidyselect::starts_with("lower"),
                   tidyselect::starts_with("upper"))
  }


  # Finally, we also check whether all lower thresholds are
  # smaller than or equal to the upper thresholds
  if (any(pull(.tbl, lower) > pull(.tbl, upper))) {
    warning("Some lower thresholds are larger than the corresponding upper thresholds! \n
  The lower and upper thresholds will be swapped...")

    swapping_indx <- which(pull(.tbl, lower) > pull(.tbl, upper))

    .tbl$lower[swapping_indx] <- .tbl$lower[swapping_indx] + .tbl$upper[swapping_indx]
    .tbl$upper[swapping_indx] <- .tbl$lower[swapping_indx] - .tbl$upper[swapping_indx]
    .tbl$lower[swapping_indx] <- .tbl$lower[swapping_indx] - .tbl$upper[swapping_indx]
  }

  if (useMixture) {
    if (any(pull(.tbl, K_i) > pull(.tbl, K_pop))) {
      warning("Some K_i values are larger than the corresponding K_pop values! \n")
    }
  }

  # Returning the validated tibble
  return(.tbl)
}


#' Title Internal function used to validate the family graph input
#'
#' @param family_graphs tibble with proband ID and list column with family graphs
#' @param pid column name of proband ID
#' @param family_graphs_col column name of the family graphs
#'
#' @returns Nothing. The function is used for validation purposes.
#'
#' @importFrom igraph vertex_attr
#' @importFrom dplyr pull
#' @noRd

validating_graph_input = function(family_graphs, pid, family_graphs_col, useMixture) {

  #check if family_graphs is present, and if the pid column is present.
  if ( !(pid %in% colnames(family_graphs)) ) {
    stop(paste0("The column ", pid," does not exist in the tibble family_graphs."))
  }
  # checking if the family graph column present.
  if ( !(family_graphs_col %in% colnames(family_graphs)) ) {
    stop(paste0("The column ", family_graphs_col," does not exist in the tibble family_graphs."))
  }

  # extract attributes from graph
  graph_attrs = vertex_attr((family_graphs %>% pull(!!as.symbol(family_graphs_col)))[[1]])

  if (useMixture) {
    if ( !(any(c("lower", "upper", "K_i", "K_pop") %in% names(graph_attrs))) ) {
      stop("lower, upper, K_i, or K_pop are not present as attributes in family_graph.")
    }

  } else {
    if ( !(any(c("lower", "upper") %in% names(graph_attrs))) ) {
      stop("lower and upper are not present as attributes in family_graph.")
    }
  }

}



#' Checking that proportions are valid
#'
#' \code{validate_proportion} checks whether proportions are valid, i.e.
#' whether they are non-negative and at most one.
#'
#' This function can be used to check whether proportions are non-negative
#' and at most one.
#'
#' @param prop A number, integer or numeric vector representing the proportions that
#' need to be validated.
#' @param from_covmat logical variable. Only used internally. allows for skip of negative check.
#'
#' @return If \code{prop} is a vector holding valid proportions of class \code{numeric}
#' or \code{integer} that are non-negative and at most one,
#' then the function will return TRUE. Otherwise, the function aborts.
#'
#' @examples
#' LTFGRS:::validate_proportion(0.2)
#' LTFGRS:::validate_proportion(0.04)
#' LTFGRS:::validate_proportion(0)
#' LTFGRS:::validate_proportion(1)
#'
#' # This will result in errors:
#' try(LTFGRS:::validate_proportion(2))
#' try(LTFGRS:::validate_proportion(-0.5))
#' @noRd
validate_proportion <- function(prop, from_covmat = FALSE){
  if(is.null(prop)){

    stop(paste0(deparse(substitute(prop)), " must be specified!"))

  }else if(!is.numeric(prop) && !is.integer(prop)){

    stop(paste0(deparse(substitute(prop)), " must be numeric!"))

  }else if(any(prop<0) & !from_covmat){

    stop(paste0(deparse(substitute(prop)), " must be non-negative!"))

  }else if(any(prop>1)){

    stop(paste0(deparse(substitute(prop)), " must be smaller than or equal to 1!"))

  }else{
    return(TRUE)
  }
}


#' Checking that a correlation matrix is valid
#'
#' \code{validate_correlation_matrix} checks whether a matrix is a valid
#' correlation matrix, i.e. whether its diagonal entries are equal to one,
#' while all off-diagonal entries are between -1 and 1, and whether it is
#' symmetric.
#'
#' This function can be used to check whether a correlation matrix has diagonal
#' entries equal to 1 and off-diagonal entries between -1 and 1 as well as whether
#' it is symmetric.
#'
#' @param corrmat A numeric matrix holding the correlations.
#' All diagonal entries must be equal to one, while all off-diagonal entries
#' must be between -1 and 1. In addition, the matrix must be symmetric.
#'
#' @return If \code{corrmat} is a valid correlation matrix that is symmetric,
#' has one on all diagonal entries and numbers between -1 and 1 on all off-
#' diagonal entries,
#' then the function will return TRUE. Otherwise, the function will be aborted.
#'
#' @examples
#' LTFGRS:::validate_correlation_matrix(matrix(c(1,0.4,0.4,1), nrow = 2))
#' LTFGRS:::validate_correlation_matrix(diag(3))
#'
#' # This will result in errors:
#' try(LTFGRS:::validate_correlation_matrix(matrix(c(0.2,0.4,0.4,0.2), nrow = 2)))
#' try(LTFGRS:::validate_correlation_matrix(matrix(nrow=2, ncol = 2)))
#' @noRd
validate_correlation_matrix <- function(corrmat){

  if(is.null(corrmat)){

    stop(paste0(deparse(substitute(corrmat)), " must be specified!"))

  }else if(any(diag(corrmat)!= 1)){

    stop(paste0("All diagonal entries in ", deparse(substitute(corrmat))," must be 1!"))

  }else if(any(abs(corrmat)>1)){
    stop(paste0("All off-diagonal entries in ", deparse(substitute(corrmat))," must be between -1 and 1!"))
  }else if(!isSymmetric.matrix(corrmat)){
    stop(paste0(deparse(substitute(corrmat)), " must be symmetric!"))
  }else{
    return(TRUE)
  }
}



#' Checking that relatives are represented by valid strings
#'
#' \code{validate_relatives} checks whether relatives are represented
#' by valid abbreviations.
#'
#' This function can be used to check whether relatives are represented
#' by valid abbreviations. A valid abbreviation is one of the following:
#' - \code{g} (Genetic component of full liability)
#' - \code{o} (Full liability)
#' - \code{m} (Mother)
#' - \code{f} (Father)
#' - \code{c[0-9]*.[0-9]*} (Children)
#' - \code{mgm} (Maternal grandmother)
#' - \code{mgf} (Maternal grandfather)
#' - \code{pgm} (Paternal grandmother)
#' - \code{pgf} (Paternal grandfather)
#' - \code{s[0-9]*} (Full siblings)
#' - \code{mhs[0-9]*} (Half-siblings - maternal side)
#' - \code{phs[0-9]*} (Half-siblings - paternal side)
#' - \code{mau[0-9]*} (Aunts/Uncles - maternal side)
#' - \code{pau[0-9]*} (Aunts/Uncles - paternal side).
#'
#' @param relatives A string or character vector representing
#' the relatives.
#' All strings must be chosen among the following abbreviations
#' - \code{g} (Genetic component of full liability)
#' - \code{o} (Full liability)
#' - \code{m} (Mother)
#' - \code{f} (Father)
#' - \code{c[0-9]*.[0-9]*} (Children)
#' - \code{mgm} (Maternal grandmother)
#' - \code{mgf} (Maternal grandfather)
#' - \code{pgm} (Paternal grandmother)
#' - \code{pgf} (Paternal grandfather)
#' - \code{s[0-9]*} (Full siblings)
#' - \code{mhs[0-9]*} (Half-siblings - maternal side)
#' - \code{phs[0-9]*} (Half-siblings - paternal side)
#' - \code{mau[0-9]*} (Aunts/Uncles - maternal side)
#' - \code{pau[0-9]*} (Aunts/Uncles - paternal side)
#' for the function to return TRUE.
#'
#' @return If \code{relatives} is a string or character vector such that
#' all strings are chosen from the mentioned list of strings,
#' then the function will return TRUE. Otherwise, the function is aborted.
#'
#' @examples
#' LTFGRS:::validate_relatives("g")
#' LTFGRS:::validate_relatives("o")
#' LTFGRS:::validate_relatives("mgm")
#'
#' # This will result in errors:
#' try(LTFGRS:::validate_relatives("a"))
#' try(LTFGRS:::validate_relatives(m))
#'
#' @importFrom stringr str_detect
#' @noRd
validate_relatives <- function(relatives){

  if(!is.character(relatives)){

    stop(paste0(deparse(substitute(relatives)), " must be a string or character vector!"))

  }else if(any(!(str_detect(relatives, "^[gomf]$") | str_detect(relatives, "^c[0-9]*.[0-9]*")|
                 str_detect(relatives, "^[mp]g[mf]$") | str_detect(relatives, "^s[0-9]*") |
                 str_detect(relatives, "^[mp]hs[0-9]*")| str_detect(relatives, "^[mp]au[0-9]*")))){

    stop(paste0(deparse(substitute(relatives)), " contains invalid abbreviations! Use strings from the following list: \n
  - g (Genetic component of full liability)\n
  - o (Full liability)\n
  - m (Mother)\n
  - f (Father)\n
  - c[0-9]*.[0-9]* (Children)\n
  - mgm (Maternal grandmother)\n
  - mgf (Maternal grandfather)\n
  - pgm (Paternal grandmother)\n
  - pgf (Paternal grandfather)\n
  - s[0-9]* (Full siblings)\n
  - mhs[0-9]* (Half-siblings - maternal side)\n
  - phs[0-9]* (Half-siblings - paternal side)\n
  - mau[0-9]* (Aunts/Uncles - maternal side)\n
  - pau[0-9]* (Aunts/Uncles - paternal side)."))
  }else{
    return(TRUE)
  }
}
