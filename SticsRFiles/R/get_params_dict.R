#' Getting an intern dictionary or merging it with a given dictionary
#' given as argument
#'
#' @param in_dict a named list with parameters names as values
#'
#' @return A named list with XML parameters names
#'
#' @keywords internal
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' # Getting internal dictionary default content
#' # get_param_dict()
#'
#' # Giving a new dictionary
#' in_dict <- list(name1 = "param_name1", name2 = "param_name2")
#'
#' get_params_dict(in_dict)
#'
#' # Giving a new dictionary with common values with the internal one
#' in_dict <- list(name1 = "amount", name2 = "julapI_or_sum_upvt")
#'
#' get_params_dict(in_dict)
#' }
get_params_dict <- function(in_dict = NULL) { # , javastics_dir = NULL) {

  # TODO: replace get_params_from_table with a call
  # to the future function that will load the correspondence table
  # (inputs.csv ?)
  # file when it will contain the XML parameters names correspondence
  # or if specifying a dict type as files types i.e. tec, ini,...
  # that will allow to load parameters names from anb XML file
  # (regarding to the STICS version)

  # For V9.1
  base_dict <- list(
    doseN = "absolute_value/%",
    doseI = "amount",
    codcueille = "codceuille",
    code_auto_profres_1 = "code_auto_profres(1)",
    code_auto_profres_2 = "code_auto_profres(2)",
    codedyntalle_1 = "codedyntalle(1)",
    codedyntalle_2 = "codedyntalle(2)",
    codemontaison_1 = "codemontaison(1)",
    codemontaison_2 = "codemontaison(2)",
    coefracoupe_1 = "coefracoupe(1)",
    coefracoupe_2 = "coefracoupe(2)",
    CsurNsol0 = "csurNsol",
    ficinit = "finit",
    Hinitf = "hinit",
    julapI = "julapI_or_sum_upvt",
    julapN = "julapN_or_sum_upvt",
    kdesat = "k_desat",
    maxtalle_1 = "MaxTalle(1)",
    maxtalle_2 = "MaxTalle(2)",
    nbcueille = "nbceuille",
    NH4initf = "NH4init",
    NO3initf = "NO3init",
    codedate_irrigauto = "P_codedate_irrigauto",
    pH0 = "pH",
    Pns = "ps",
    resk_1 = "resk(1)",
    resk_2 = "resk(2)",
    resplmax_1 = "resplmax(1)",
    resplmax_2 = "resplmax(2)",
    resz_1 = "resz(1)",
    resz_2 = "resz(2)",
    seuilLAIapex_1 = "SeuilLAIapex(1)",
    seuilLAIapex_2 = "SeuilLAIapex(2)",
    seuilmortalle_1 = "SeuilMorTalle(1)",
    seuilmortalle_2 = "SeuilMorTalle(2)",
    seuilreconspeupl_1 = "SeuilReconsPeupl(1)",
    seuilreconspeupl_2 = "SeuilReconsPeupl(2)",
    sigmadistalle_1 = "SigmaDisTalle(1)",
    sigmadistalle_2 = "SigmaDisTalle(2)",
    surfapex_1 = "SurfApex(1)",
    surfapex_2 = "SurfApex(2)",
    tigefeuilcoupe_1 = "tigefeuilcoupe(1)",
    tigefeuilcoupe_2 = "tigefeuilcoupe(2)",
    vitreconspeupl_1 = "VitReconsPeupl(1)",
    vitreconspeupl_2 = "VitReconsPeupl(2)"
  )





  # Returning intern dict
  if (base::is.null(in_dict)) {
    return(base_dict)
  }

  checked <- check_dict(in_dict = in_dict)

  if (!checked)
    stop("A least one XML parameter name does not match the reference list ! ")

  # Merging the in_dict and the intern dict
  new_dict <- merge_dict(in_dict, base_dict)

  return(new_dict)
}


merge_dict <- function(in_dict, base_dict) {

  # We suppose here that all XML parameters names of the in_dict list
  # have been previously checked in set_param_dict

  # Calculate in_dict XML parameters names indices in base_dict
  in_names <- unlist(in_dict, use.names = FALSE)
  base_names <- unlist(base_dict, use.names = FALSE)

  in_names_idx <- base_names %in% in_names

  # Removing correponding fields
  base_dict[in_names_idx] <- NULL

  # Adding dict lists
  new_dict <- c(base_dict, in_dict)

  return(new_dict)
}

check_dict <- function(in_dict) { # , javastics_dir, file_name = "inputs.csv") {

  # TODO: will be useful when in inputs.csv when correspondence will
  # be integrated between code names and param names in XML files
  # to be able to check param names in XML files !!!


  # checks if fields values are unique
  if (base::is.null(in_dict)) {
    return(TRUE)
  }

  in_values <- unlist(in_dict, use.names = FALSE)
  uniq_values <- length(in_values) == length(unique(in_values))
  checked <- uniq_values
  if (!checked) {
    return(FALSE)
  }


  return(TRUE)
}
