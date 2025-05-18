#' Extracting a data.frame of parameters values for an xml file or a set of
#' (whatever the xml file kind)
#'
#' @param file_path A file path or a vector of
#' @param select node name or attribute name to use for selection
#' (optional, default to no selection)
#' @param name value used for select (optional)
#' @param param_names vector of parameters names (optional)
#' @param wide_shape Optional logical for keeping the long data.frame
#' format FALSE, default) or converting it to a wider format one (TRUE)
#'
#' @return A data.frame with name, type, param, id and value columns
#'
#'
#' @details An extract of an example of data.frame for the 2 formats
#'
#'  * Default one
#'
#' |name          |type       |param     | id|value            |
#' |:-------------|:----------|:---------|--:|:----------------|
#' |SugarCane     |usms       |datedebut | NA|286              |
#' |SugarCane     |usms       |datefin   | NA|650              |
#' |SugarCane     |usms       |finit     | NA|canne_ini.xml    |
#' |SugarCane     |usms       |nomsol    | NA|solcanne         |
#' |SugarCane     |usms       |fstation  | NA|climcanj_sta.xml |
#' |...           |...        |...       |...|...              |
#' |...           |...        |...       |...|...              |
#' |param_gen.xml |fichierpar |Qmulchdec | 17|0                |
#' |param_gen.xml |fichierpar |Qmulchdec | 18|0                |
#' |param_gen.xml |fichierpar |Qmulchdec | 19|0                |
#' |param_gen.xml |fichierpar |Qmulchdec | 20|0                |
#' |param_gen.xml |fichierpar |Qmulchdec | 21|0                |
#'
#' name: a file name or an usm or soil name
#' type: type of file
#' param: param name
#' id: NA for scalar, integer id for vectors parameters
#' value: parameter value
#'
#' * wide format
#'
#'  |name      |type | datedebut| datefin|finit          |nomsol    |
#'  |:---------|:----|---------:|-------:|:--------------|:---------|
#'  |SugarCane |usms |       286|     650|canne_ini.xml  |solcanne  |
#'  |potato    |usms |        91|     250|patate_ini.xml |solpatate |
#'  |banana    |usms |        30|     300|banane_ini.xml |solbanane |
#'  |sorghum   |usms |       112|     360|sorgho_ini.xml |solsorgho |
#'  |barley    |usms |        88|     196|orge_ini.xml   |solorge   |
#'
#' name: a file name or an usm or soil name
#' type: type of file
#' all other columns names correspond to parameter names, with indices as suffix
#' for multiple values
#'
#' @keywords internal
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' dir_path <- get_examples_path("xml")
#' get_xml_files_param_df(file_path = file.path(dir_path, "sols.xml"))
#'
#' get_xml_files_param_df(
#'   file_path = file.path(dir_path, "sols.xml"),
#'   select = "sol", c("solcanne", "solble")
#' )
#'
#' files_list <- file.path(dir_path, c("sols.xml", "usms.xml", "param_gen.xml"))
#' get_xml_files_param_df(file_path = files_list)
#' }
#'
get_xml_files_param_df <- function(file_path,
                                   select = NULL,
                                   name = NULL,
                                   param_names = NULL,
                                   wide_shape = FALSE) {


  # For managing a files list
  if (length(file_path) > 1) {
    files_exist <- file.exists(file_path)

    if (!all(files_exist))
      warning("Missing files: ", paste(file_path[!files_exist], collapse = ","))

    if (!any(files_exist))
      stop("Not any files exist !")

    files_df <- lapply(
      file_path[files_exist],
      function(x) {
        get_xml_files_param_df(x,
                               select = select,
                               name = name,
                               param_names = param_names,
                               wide_shape = FALSE
        )
      }
    )

    df <- data.table::rbindlist(files_df)

    # Conversion to a wider table (with type conversion)
    if (wide_shape) {
      df <- df_wider(df)
    }

    return(df)
  }

  # Getting parameters file type
  file_type <- get_xml_type(file_path)

  # Checking if select can be set (for sols and usms)
  if (base::is.null(select)) {
    if (is_sols_xml(file_path)) select <- "sol"
    if (is_usms_xml(file_path)) select <- "usm"
  }

  # Getting usm or sol names vector
  names_list <- NULL
  if (!base::is.null(select))
    names_list <- get_param_xml(file_path, param = select)[[1]][[select]]

  # Getting all usm or sol names from the file
  select_name <- FALSE
  if (!is.null(name)) {
    # Checking names
    exist_names <- names_list %in% name
    if (sum(exist_names) < length(name)) {
      miss_names <- setdiff(name, names_list[exist_names])
      warning("Missing names in file: ", paste(miss_names, collapse = ","))
    }
    select_name <- TRUE
    target_name <- names_list[exist_names]
  }

  # for one name
  param_values <- get_param_xml(file_path, param = param_names)[[1]]

  if (file_type == "initialisations")
    param_values <- reformat_param_values_init(param_values)

  # Checking if only one parameter, param_values == numerical vector
  if (length(param_names) == 1) {
    param_values <- list(param_values)
    names(param_values) <- param_names
  }


  # Getting parameters values number
  values_nb <- unlist(lapply(X = param_values, function(x) length(x)))

  if (base::is.null(select)) {
    # calling the function calculating ids
    param_id_names <- get_params_id(file_type, file_path, param_values)
    param_id <- param_id_names$id
    param_names <- param_id_names$names

    names(param_values) <- param_names

    name_col <- rep(basename(file_path), sum(values_nb))
  } else {
    # Getting values nb for each usm or sol
    values_per_par <- length(names_list)


    param_id <- unlist(lapply(values_nb, function(x) {
      l <- rep(NA, x)
      if (x > values_per_par) l <- rep(1:(x / values_per_par), values_per_par)
      return(l)
    }),
    use.names = FALSE
    )

    name_col <- unlist(lapply(values_nb, function(x) {
      l <- names_list
      if (x > values_per_par)
        l <- unlist(lapply(names_list, function(y) rep(y, x / values_per_par)))
      return(l)
    }),
    use.names = FALSE
    )
  }


  # Getting expanded parameters names vector
  param <- rep(names(param_values), values_nb)

  # Getting param file type
  type_col <- rep(file_type, sum(values_nb))


  # Defining the returned data.frame
  data_df <- data.frame(
    name = name_col,
    type = type_col,
    param = param,
    id = param_id,
    value = unlist(param_values, use.names = FALSE),
    stringsAsFactors = FALSE
  )

  if (select_name) {
    data_df <- dplyr::filter(data_df, name %in% target_name)
  }

  # Conversion to a wider table (with type conversion)
  if (wide_shape) {
    data_df <- df_wider(data_df)
  }


  return(data_df)
}



df_wider <- function(df, convert_type = TRUE, string_as_factors = FALSE) {
  valid_id <- !is.na(df$id)
  df$param[valid_id] <-
    paste0(df$param[valid_id], "_", as.character(df$id[valid_id]))

  # parameters wider data.frame
  df <- df %>%
    dplyr::select(-"id") %>%
    tidyr::pivot_wider(names_from = "param", values_from = "value")

  if (convert_type) df <- utils::type.convert(df, as.is = !string_as_factors)

  return(df)
}


get_params_id <- function(file_type, file_path, param_values) {

  # files types
  # "initialisations" "usms" "sols" "fichiertec" "fichiersta"
  # "fichierplt"  "fichierpar" "fichierparamgen"


  values_nb <- unlist(lapply(X = param_values, function(x) length(x)))

  param <- list()
  param$names <- names(param_values)

  if (file_type == "fichiertec") {
    xml_doc <- xmldocument(file_path)
    param_types_data <- get_param_type(
      xml_doc,
      param_name = names(param_values))

    param$id <- unlist(lapply(param_types_data, function(x) {
      l <- NA
      if (x$type %in% c("table", "table2")) l <- 1:x$length
      return(l)
    }), use.names = FALSE)


    delete(xml_doc)

    return(param)
  }


  # "fichierplt" ???


  # general case: usms, sols, "fichiersta", "fichierpar", "fichierparamgen",
  # "initialisations"
  param$id <- unlist(lapply(values_nb, function(x) {
    l <- NA
    if (x > 1) l <- 1:x
    return(l)
  }), use.names = FALSE)



  return(param)

}

reformat_param_values_init <- function(param_values) {

  # change id => 1, plant param
  plt_init_names <- c("stade0", "lai0", "magrain0", "zrac0", "maperenne0",
                      "QNperenne0", "masecnp0", "QNplantenp0", "masec0",
                      "QNplante0", "restemp0", "code_acti_reserve")
  par_names <- names(param_values)

  new_param_values <- param_values

  # removing plant init parameters
  for (p in c(plt_init_names, "densinitial")) new_param_values[[p]] <- NULL

  for (n in seq_along(par_names)) {
    par_name <-  par_names[n]
    idx <- grep(x = plt_init_names, pattern = paste0("^", par_name))
    if (length(idx) > 0) {

      new_param_values[[paste0(par_name, "_Crop1")]] <-
        param_values[[par_name]][1]
      new_param_values[[paste0(par_name, "_Crop2")]] <-
        param_values[[par_name]][2]

    }
  }
  # treating densinitial
  id_start <- 0
  for (i in 1:2) {
    for (j in 1:5) {

      new_param_values[[paste0("densinitial_", j, "_Crop", i)]] <-
        param_values$densinitial[id_start + j]

    }
    id_start <- 5
  }
  new_param_values
}
