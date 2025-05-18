#' Read STICS input parameters from text files
#'
#' @description Read STICS model input parameters from a usm in text format
#' (STICS input)
#' Generally used after calling building a usm with `JavaSTICS`.
#'
#' @param workspace      Path of the workspace containing the STICS (txt)
#' input files.
#' @param param        Vector of parameter names. Optional, if not provided,
#' the function returns an object with all parameters.
#' @param plant_id plant index (1, 2), default(NULL) calculated from from
#' plant number in STICS initialization file
#' @param variety      Either the variety name or index for plant parameters
#' (optional, see details).
#' @param value_id index of technical interventions to be used to
#' retrieve parameter values, or layer index for soil parameters
#' @param exact        Boolean indicating if the function must return results
#' only for exact match.
#' @param stics_version An optional version name as listed in
#' get_stics_versions_compat() return
#' @param dirpath `r lifecycle::badge("deprecated")` `dirpath` is no
#'   longer supported, use `workspace` instead.
#' @param ...          Further arguments to pass (for future-proofing only).
#'
#' @details If the `variety` is not given and a `param` is asked,
#' the function will return the values for the variety that is simulated in
#' the USM by checking the `variete` parameter in the technical file.
#' If `param` is not provided by the user, the values from all varieties
#' will be returned unless the user ask for a given `variety`.
#'
#' @note Users would generally use `get_param_txt` to identify parameters
#' names and values and pass them to other functions.
#'
#' @return A list of parameters value(s),
#' or if `param = NULL` a list of all parameters:
#'         \item{ini}{Initialization parameters}
#'         \item{general}{General parameters}
#'         \item{tec}{Technical parameters}
#'         \item{plant}{Plant parameters}
#'         \item{soil}{Soil parameters}
#'         \item{station}{Station parameters}
#'
#' @seealso `gen_varmod()`,
#'
#'
#' @examples
#' path <- get_examples_path(file_type = "txt")
#'
#' # Getting the interrow distance parameter value
#' get_param_txt(path, param = "interrang")
#'
#' # Getting varietal parameters values
#' # Get the leaf lifespan of the variety used in the usm:
#' get_param_txt(workspace = path, param = "durvieF")
#' # Get the leaf lifespan of another variety available in the plant file:
#' get_param_txt(workspace = path, param = "durvieF", variety = "Furio")
#' # To get the values for several (or all) varieties, either put all varieties:
#' varieties <- c("Pactol", "Cherif", "Furio", "Dunia", "Volga", "Cecilia")
#' get_param_txt(workspace = path, param = "durvieF", variety = varieties)
#' # Or get it from the output of the function returning all parameters:
#' get_param_txt(workspace = path)$plant$plant1$durvieF
#'
#' # Get parameters for a specific plant
#' get_param_txt(workspace = path, plant_id = 1)
#' get_param_txt(workspace = path, param = "durvieF", plant_id = 1)
#' get_param_txt(workspace = path, param = "durvieF", plant_id = 1,
#' variety = varieties)
#'
#' # Get parameters for specific interventions or soil layers
#' get_param_txt(workspace = path, param = "amount", value_id = c(1,3))
#' get_param_txt(workspace = path, param = "Hinitf", value_id = c(1,3))
#' get_param_txt(workspace = path, param = "epc", value_id = c(1,3))
#'
#' @export
#'
get_param_txt <- function(workspace,
                          param = NULL,
                          plant_id = NULL,
                          variety = NULL,
                          value_id = NULL,
                          exact = FALSE,
                          stics_version = "latest",
                          dirpath = lifecycle::deprecated(),
                          ...) {

  # dirpath
  if (lifecycle::is_present(dirpath)) {
    lifecycle::deprecate_warn(
      "1.0.0", "get_param_txt(dirpath)",
      "get_param_txt(workspace)"
    )
  } else {
    dirpath <- workspace # to remove when we update inside the function
  }

  stics_version <- check_version_compat(stics_version = stics_version)

  ini <- get_ini_txt(file.path(dirpath, "ficini.txt"),
                     stics_version = stics_version)

  # specifying plant(s) to use, and checking if a given plant_id is
  # available ones
  avail_plant_id <- seq_len(ini$nbplantes)

  if(is.null(plant_id)) {
    plant_id <- avail_plant_id
  } else {
    if(!all(plant_id %in% avail_plant_id))
      stop("Given plant id(s) (",
           paste(plant_id, collapse = ","), ") ",
           " do(es) not match available one(s) (",
           paste(avail_plant_id, collapse = ","), ")")
  }


  general <- get_general_txt(file.path(dirpath, "tempopar.sti"))

  soil <- get_soil_txt(file.path(dirpath, "param.sol"),
                       stics_version = stics_version)

  station <- get_station_txt(file.path(dirpath, "station.txt"))

  usm <- get_usm_txt(file.path(dirpath, "new_travail.usm"),
                     plant_id = plant_id)

  tmp <- get_tmp_txt(file.path(dirpath, "tempoparv6.sti"))


  # add tests on option_* name existence in tmp
  # NOT IN V10
  several_fert <- several_thin <- is_pasture <- NULL
  tmp_names <- names(tmp)
  several_fert <- ifelse("option_engrais_multiple" %in% tmp_names &&
                           tmp$option_engrais_multiple == 1, TRUE, FALSE)
  several_thin <- ifelse("option_thinning" %in% tmp_names &&
                           tmp$option_thinning == 1, TRUE, FALSE)
  is_pasture <- ifelse("option_pature" %in% tmp_names &&
                         tmp$option_pature == 1, TRUE, FALSE)

  tec <- plant <- stats::setNames(
    vector(mode = "list", length = ini$nbplantes),
    paste0("plant", 1:ini$nbplantes)
  )

  varieties <- vector("list", ini$nbplantes)
  if (is.null(variety)) {
    variety <- vector("list", ini$nbplantes)
  } else if (length(variety) == 1) {
    variety <- lapply(1:ini$nbplantes, function(x) variety)
  } else {
    variety <- list(variety)
  }

  for (i in plant_id) {
    tec[paste0("plant", i)] <-
      list(get_tec_txt(
        file = file.path(dirpath, paste0("fictec", i, ".txt")),
        several_fert = several_fert, several_thin = several_thin,
        is_pasture = is_pasture
      ))

    varieties[[i]] <-
      get_plant_txt(file = file.path(dirpath,
                                     paste0("ficplt", i, ".txt")))$codevar

    tec_variety <- tec[[paste0("plant", i)]]$variete

    alert_msg <- paste0("Variety not found in plant file. Possible ",
                        "varieties are: {.val {varieties}}")

    plant[paste0("plant", i)] <-
      list(get_plant_txt(file.path(dirpath, paste0("ficplt", i, ".txt")),
                         variety =
                           if (is.null(variety[[i]])) {
                             if (!is.null(param)) {
                               varieties[[i]][tec_variety]
                             } else {
                               NULL
                             }
                           } else {
                             # variety
                             if (is.character(variety[[i]])) {
                               variety[[i]] <- match(variety[[i]],
                                                     varieties[[i]])
                               if (any(is.na(variety))) {
                                 cli::cli_alert_danger(alert_msg)
                                 return()
                               }
                               varieties[[i]][variety[[i]]]
                             } else {
                               varieties[[i]][variety[[i]]]
                             }
                           }
      ))

    # Fixes the current variety
    if (is.null(variety[[i]])) variety[[i]] <- tec_variety
  }

  parameters <- list(
    usm = usm, ini = ini, general = general, tec = tec,
    plant = plant, soil = soil, station = station,
    tmp = tmp
  )

  # Returning the parameters full list
  if (is.null(param)) {
    return(parameters)
  }


  # Extracting a sublist of desired parameters, with respect to the original
  # full list structure, with or without exact search
  # using or not ids for soil layers, technical interventions (ini, soil or
  # tec parameters files)
  parameters <- filter_param(parameters,
                             param = param,
                             exact = exact,
                             value_id = value_id)


  return(parameters)
}



filter_param <- function(in_list,
                         param = NULL,
                         exact = FALSE,
                         value_id = NULL) {
  out_list <- list()
  names_vec <- names(in_list)

  for (i in seq_along(names_vec)) {
    name <- names_vec[[i]]

    if (is.list(in_list[[name]])) {
      tmp <- filter_param(in_list[[name]], param = param,
                          exact = exact, value_id = value_id)

      if (length(tmp) > 0) out_list[[name]] <- tmp
      next
    }

    # For identity return
    if (is.null(param)) out_list[[name]] <- in_list[[name]]

    # Filtering using param vector
    # Exact names or partial names search
    if (exact) {
      idx <- param %in% name
    } else {
      # either for paameter names containing () or not
      pattern <- gsub("\\)", "\\\\)", gsub("\\(", "\\\\(", param))
      idx <- unlist(lapply(pattern, function(x) grepl(pattern = x, x = name)))
    }

    if (any(idx)) {
      out_list[[name]] <- in_list[[name]]

      if(is.null(value_id)) next

      # checking if given ids exist in parameter values
      avail_ids <- seq_len(length(out_list[[name]]))
      if (!all(value_id %in% avail_ids))
        stop("Given ids (",
             paste(value_id, collapse = ", "),
             ") are not all available in existing ids (",
             paste(avail_ids, collapse = ", "),")")

      # sub scripting values
      out_list[[name]] <- out_list[[name]][value_id]
    }
  }

  return(out_list)
}


#' Read STICS input parameters files
#'
#' @description Read a specific STICS model input parameter file.
#' Users would generally use the wrapper `get_param_txt()` instead.
#'
#' @param file File path
#' @param several_fert Is there several fertilization in the USM ? See details.
#' @param several_thin Is there several thinning in the USM ? See details.
#' @param is_pasture   Is the plant a pasture ? See details.
#' @param variety      Integer. The plant variety to get the parameter from.
#' @param filepath `r lifecycle::badge("deprecated")` `filepath` is no
#'   longer supported, use `file` instead.
#'
#' @param ...          Further arguments to pass (for future-proofing only)
#'
#' @details `several_fert`, `several_thin` and `is_pasture` are read from
#' the tmp file (`tempoparv6.sti`). `get_param_txt()` does it automatically.
#' If you absolutely need to use directly `get_tec_txt`, please see example.
#'
#'
#' @note The functions are compatible with intercrops. Users generally only use
#'  `get_param_txt()`, which is a wrapper for all these functions.
#'
#' @return A list of parameters, depending on the file/function:
#'         \item{ini}{Initialization parameters}
#'         \item{general}{General parameters}
#'         \item{tec}{Technical parameters}
#'         \item{plant}{Plant parameters}
#'         \item{soil}{Soil parameters}
#'         \item{station}{Station parameters}
#'         \item{tmp}{Temporary parameters}
#'
#' @seealso `get_param_txt()`.
#'
#' @examples
#' \dontrun{
#' # Read the initialisation file (ficini.txt):
#' library(SticsRFiles)
#' path <- file.path(get_examples_path(file_type = "txt"), "ficini.txt")
#' get_ini_txt(path)
#'
#' # Read the tec file directly:
#'
#' # First, get the parameters from the tmp file:
#' tmp <- get_tmp_txt(file = file.path(get_examples_path(file_type = "txt"),
#'                                     "tempoparv6.sti"))
#' several_fert <- ifelse(tmp$option_engrais_multiple == 1, TRUE, FALSE)
#' several_thin <- ifelse(tmp$option_thinning == 1, TRUE, FALSE)
#' is_pasture <- ifelse(tmp$option_pature == 1, TRUE, FALSE)
#'
#' # Then, get the technical parameters:
#' get_tec_txt(
#'   file = file.path(get_examples_path(file_type = "txt"), "fictec1.txt"),
#'   several_fert = several_fert, several_thin = several_thin,
#'   is_pasture = is_pasture
#' )
#' }
#'
#' @rdname get_param_txt
#' @export
get_ini_txt <- function(file = "ficini.txt",
                        stics_version,
                        filepath = lifecycle::deprecated()) {

  # filepath
  if (lifecycle::is_present(filepath)) {
    lifecycle::deprecate_warn(
      "1.0.0", "get_ini_txt(filepath)",
      "get_ini_txt(file)"
    )
  } else {
    filepath <- file # to remove when we update inside the function
  }

  stics_version <- check_version_compat(stics_version = stics_version)

  if (!file.exists(filepath))
    stop(filepath, ": does not exist !")

  params <- readLines(filepath)
  ini <- list()

  ini$nbplantes <- params[[2]]
  ini$plant <- list()


  stics_version_num <- get_version_num(stics_version = stics_version)

  if (stics_version_num < 10) {

    if (length(params) > 48)
      stop("The used STICS version ",
           stics_version_num,
           " does not correspond to the file content (STICS version >= 10)")

    ini$plant$plant1 <- list(
      stade0 = params[[4]],
      lai0 = params[[5]],
      masec0 = params[[6]],
      QNplante0 = params[[7]],
      magrain0 = params[[8]],
      zrac0 = params[[9]],
      resperenne0 = params[[10]],
      densinitial = params[[12]]
    )

    ini$plant$plant2 <- list(
      stade0 = params[[14]],
      lai0 = params[[15]],
      masec0 = params[[16]],
      QNplante0 = params[[17]],
      magrain0 = params[[18]],
      zrac0 = params[[19]],
      resperenne0 = params[[20]],
      densinitial = params[[22]]
    )

    ini$hinit <- params[[24]]
    ini$NO3init <- params[[26]]
    ini$NH4init <- params[[28]]
  } else {

    if (length(params) < 48)
      stop("The used STICS version ",
           stics_version_num,
           " does not correspond to the file content (STICS version < 10)")

    ini$plant$plant1 <- list(
      stade0 = params[[4]],
      lai0 = params[[5]],
      magrain0 = params[[6]],
      zrac0 = params[[7]],
      code_acti_reserve = params[[9]],
      maperenne0 = params[[10]],
      QNperenne0 = params[[11]],
      masecnp0 = params[[12]],
      QNplantenp0 = params[[13]],
      masec0 = params[[14]],
      QNplante0 = params[[15]],
      restemp0 = params[[16]],
      densinitial = params[[18]]
    )

    ini$plant$plant2 <- list(
      stade0 = params[[20]],
      lai0 = params[[21]],
      magrain0 = params[[22]],
      zrac0 = params[[23]],
      code_acti_reserve = params[[25]],
      maperenne0 = params[[26]],
      QNperenne0 = params[[27]],
      masecnp0 = params[[28]],
      QNplantenp0 = params[[29]],
      masec0 = params[[30]],
      QNplante0 = params[[31]],
      restemp0 = params[[32]],
      densinitial = params[[34]]
    )

    ini$Hinitf <- params[[36]]
    ini$NO3initf <- params[[38]]
    ini$NH4initf <- params[[40]]
    ini$Sdepth0 <- params[[43]]
    ini$Sdry0 <- params[[45]]
    ini$Swet0 <- params[[47]]
    ini$ps0 <- params[[49]]
  }

  ini <- character_to_numeric_list(ini)

  return(ini)
}

#' @rdname get_param_txt
#' @export
get_general_txt <- function(file = "tempopar.sti",
                            filepath = lifecycle::deprecated()) {

  # filepath
  if (lifecycle::is_present(filepath)) {
    lifecycle::deprecate_warn(
      "1.0.0", "get_general_txt(filepath)",
      "get_general_txt(file)"
    )
  } else {
    filepath <- file # to remove when we update inside the function
  }

  c(nbresidus = 21, get_txt_generic(filepath))
}


#' @rdname get_param_txt
#' @export
get_tmp_txt <- function(file = "tempoparv6.sti",
                        filepath = lifecycle::deprecated()) {

  # filepath
  if (lifecycle::is_present(filepath)) {
    lifecycle::deprecate_warn(
      "1.0.0", "get_tmp_txt(filepath)",
      "get_tmp_txt(file)"
    )
  } else {
    filepath <- file # to remove when we update inside the function
  }

  get_txt_generic(filepath)
}

#' @rdname get_param_txt
#' @export
get_plant_txt <- function(file = "ficplt1.txt", variety = NULL,
                          filepath = lifecycle::deprecated()) {

  # filepath
  if (lifecycle::is_present(filepath)) {
    lifecycle::deprecate_warn(
      "1.0.0", "get_plant_txt(filepath)",
      "get_plant_txt(file)"
    )
  } else {
    filepath <- file # to remove when we update inside the function
  }

  x <- get_txt_generic(filepath)

  index_codevar <- which(names(x) == "codevar")
  varieties <- x[[index_codevar]]

  # add nbVariete
  x_1 <- c(x[1:(index_codevar - 1)], nbVariete = length(varieties))
  # variety-related parameters
  x_2 <- x[index_codevar:length(x)]

  # Keep only the variety asked by the user:
  if (!is.null(variety)) {
    # If variety is given with name(s), find the index
    if (is.character(variety)) {
      variety <- match(variety, varieties)
    }
    x_2 <- lapply(x_2, function(x) {
      x[variety]
    })
  } else {
    variety <- seq_along(varieties)
  }

  # Setting variety names to vectors
  # skipping "codevar" containing varieties names
  for (i in 2:length(x_2)) {
    names(x_2[[i]]) <- varieties[variety]
  }

  c(x_1, x_2)
}




#' @rdname get_param_txt
#' @export
get_tec_txt <- function(file = "fictec1.txt",
                        stics_version = "latest",
                        several_fert = NULL,
                        several_thin = NULL,
                        is_pasture = NULL,
                        filepath = lifecycle::deprecated(),
                        ...) {

  # filepath
  if (lifecycle::is_present(filepath)) {
    lifecycle::deprecate_warn(
      "1.0.0", "get_tec_txt(filepath)",
      "get_tec_txt(file)"
    )
  } else {
    filepath <- file # to remove when we update inside the function
  }

  if (!file.exists(filepath))
    stop(filepath, ": does not exist !")


  # TODO: add dot args management
  # Future-proofing the function. We can add arguments now without
  # breaking it. I think for example to a "version argument" because
  # the tec file is not generic.

  stics_version <- check_version_compat(stics_version = stics_version)

  par_lines <- readLines(filepath)
  itk <- vector(mode = "list", length = 0)
  ids_val <- !seq_along(par_lines) %% 2
  params <- par_lines[!ids_val]
  values <- par_lines[ids_val]


  # Early return here for version >= 10.0
  # get_tec_txt_ is not fully generic for the moment!
  if (get_version_num(stics_version = stics_version) >= 10) {
    return(get_tec_txt_(params, values))
  }

  # Treatment for STICS version < V10.0
  pval <- val(values = values)
  itk$nbjres <- pval$val

  if (itk$nbjres > 0) {
    for (i in 1:itk$nbjres) {
      pval <- val(pval, values)
      vec <- pval$val

      itk$julres <- c(itk$julres, vec[1])
      itk$coderes <- c(itk$coderes, vec[2])
      itk$qres <- c(itk$qres, vec[3])
      itk$Crespc <- c(itk$Crespc, vec[4])
      itk$CsurNres <- c(itk$CsurNres, vec[5])
      itk$Nminres <- c(itk$Nminres, vec[6])
      itk$eaures <- c(itk$eaures, vec[7])
    }
  }

  pval <- val(values = values)
  itk$nbjtrav <- pval$val

  if (itk$nbjtrav > 0) {
    for (i in 1:itk$nbjtrav) {
      pval <- val(pval, values)
      vec <- pval$val

      itk$jultrav <- c(itk$jultrav, vec[1])
      itk$profres <- c(itk$profres, vec[2])
      itk$proftrav <- c(itk$proftrav, vec[3])
    }
  }

  for (i in 1:27) {
    pval <- val(pval, values)
    pname <- parname(pval$index, params, -1)
    itk[[pname]] <- pval$val
  }

  pval <- val(values = values)
  itk$nap <- pval$val


  if (itk$nap > 0) {
    for (i in 1:itk$nap) {
      pval <- val(pval, values)
      vec <- pval$val

      if (itk$codedateappH2O != 1) {
        itk$julapI <- c(itk$julapI, vec[1])
      } else {
        itk$upvttapI <- c(itk$upvttapI, vec[1])
      }
      itk$doseI <- c(itk$doseI, vec[2])
    }
  }

  for (i in 1:3) {
    pval <- val(pval, values)
    pname <- parname(pval$index, params, -1)
    itk[[pname]] <- pval$val
  }

  pval <- val(pval, values)
  if (!is.null(several_fert) && !several_fert) {
    itk$engrais <- pval$val
  }


  for (i in 1:4) {
    pval <- val(pval, values)
    pname <- parname(pval$index, params, -1)
    itk[[pname]] <- pval$val
  }

  pval <- val(pval, values)
  itk$napN <- pval$val

  if (itk$napN > 0) {
    for (i in 1:itk$napN) {
      pval <- val(pval, values)
      vec <- pval$val

      if (itk$codedateappN != 1) {
        if (itk$codefracappN == 1) {
          if (!is.null(several_fert) && several_fert) {
            itk$engrais <- c(itk$engrais, vec[3])
          }
          itk$julapN <- c(itk$julapN, vec[1])
          itk$doseN <- c(itk$doseN, vec[2])
        } else {
          if (!is.null(several_fert) && several_fert) {
            itk$engrais <- c(itk$engrais, vec[3])
          }
          itk$julapN <- c(itk$julapN, vec[1])
          itk$fracN <- c(itk$fracN, vec[2])
        }
      } else {
        if (itk$codefracappN == 1) {
          if (!is.null(several_fert) && several_fert) {
            itk$engrais <- c(itk$engrais, vec[3])
          }
          itk$upvttapN <- c(itk$upvttapN, vec[1])
          itk$doseN <- c(itk$doseN, vec[2])
        } else {
          if (!is.null(several_fert) && several_fert) {
            itk$engrais <- c(itk$engrais, vec[3])
          }
          itk$upvttapN <- c(itk$upvttapN, vec[1])
          itk$fracN <- c(itk$fracN, vec[2])
        }
      }
    }
  }


  for (i in 1:19) {
    pval <- val(pval, values)
    pname <- parname(pval$index, params, -1)
    itk[[pname]] <- pval$val
  }

  if (itk$codemodfauche == 1) {
    itk$lecfauche <- FALSE
  } else {
    itk$lecfauche <- TRUE
  }


  for (i in 1:2) {
    pval <- val(pval, values)
    pname <- parname(pval$index, params, -1)
    itk[[pname]] <- pval$val
  }

  pval <- val(pval, values)
  nbcoupe2 <- pval$val

  if (itk$codemodfauche == 2) {
    for (i in 1:nbcoupe2) {
      pval <- val(pval, values)
      vec <- pval$val

      if (is_pasture) {
        itk$restit <- c(itk$restit, vec[6])
        itk$mscoupemini <- c(itk$mscoupemini, vec[7])
      }
      itk$julfauche <- c(itk$julfauche, vec[1])
      itk$hautcoupe <- c(itk$hautcoupe, vec[2])
      itk$lairesiduel <- c(itk$lairesiduel, vec[3])
      itk$msresiduel <- c(itk$msresiduel, vec[4])
      itk$anitcoupe <- c(itk$anitcoupe, vec[5])
    }
    itk$nbcoupe <- nbcoupe2
  }

  pval <- val(pval, values)
  nbcoupe3 <- pval$val

  if (itk$codemodfauche == 3) {
    for (i in 1:nbcoupe3) {
      pval <- val(pval, values)
      vec <- pval$val

      if (is_pasture) {
        itk$restit <- c(itk$restit, vec[6])
        itk$mscoupemini <- c(itk$mscoupemini, vec[7])
      }

      itk$tempfauche <- c(itk$tempfauche, vec[1])
      itk$hautcoupe <- c(itk$hautcoupe, vec[2])
      itk$lairesiduel <- c(itk$lairesiduel, vec[3])
      itk$msresiduel <- c(itk$msresiduel, vec[4])
      itk$anitcoupe <- c(itk$anitcoupe, vec[5])
    }
    itk$nbcoupe <- nbcoupe3
  }


  for (i in 1:11) {
    pval <- val(pval, values)
    pname <- parname(pval$index, params, -1)
    itk[[pname]] <- pval$val
  }

  if (!is.null(several_thin) && several_thin) {
    pval <- val(pval, values)
    itk$nb_eclair <- pval$val

    for (i in 1:itk$nb_eclair) {
      pval <- val(pval, values)
      vec <- pval$val

      itk$juleclair <- c(itk$juleclair, vec[1])
      itk$nbinfloecl <- c(itk$nbinfloecl, vec[2])
    }
  } else {
    itk$nb_eclair <- 1
    pval <- val(pval, values)
    itk$juleclair <- pval$val

    pval <- val(pval, values)
    itk$nbinfloecl <- pval$val
  }


  for (i in 1:30) {
    pval <- val(pval, values)
    pname <- parname(pval$index, params, -1)
    itk[[pname]] <- pval$val
  }

  return(itk)
}

parname <- function(index, params, idx = NULL) {
  if (!is.null(idx)) {
    loc_idx <- index + idx
  } else {
    loc_idx <- index
  }
  if (loc_idx <= 0 || loc_idx > length(params)) {
    return()
  }
  unlist(lapply(X = params[loc_idx], FUN = function(x) {
    strsplit(trimws(x), split = " ")
  }))
}

val <- function(pval = list(index = 1, val = NA), values) {

  if (pval$index == length(values)) {
    return()
  }

  pval$index <- pval$index + 1

  val_txt <- unlist(strsplit(trimws(values[pval$index - 1]), split = " "))

  out_val <- suppressWarnings(as.numeric(val_txt))
  if (any(is.na(out_val))) {
    out_val <- val_txt
  }

  pval$val <- out_val

  return(pval)
}

#'
# @examples
get_tec_txt_ <- function(params, values) {
  itk <- list()
  num_op <- 0
  nb_interventions <- 0
  intervention_type <- c(
    "nbjres", "nbjtrav", "nap", "napN",
    "nbcoupe", "nbcoupe", "nb_eclair"
  )

  v <- list()

  multi <- FALSE
  pval <- list(index = 1, val = NA)

  while (TRUE) {
    param <- parname(pval$index, params)
    pval <- val(pval, values)
    value <- pval$val

    if (is.null(value)) break

    # Single parameter
    if (length(param) == 1) {
      if (param == "nbinterventions") {
        num_op <- num_op + 1
        param <- intervention_type[num_op]
        itk[[param]] <- value
        nb_interventions <- value
        if (value > 1) multi <- TRUE
      } else {
        itk[[param]] <- value
      }
      next
    }

    # multiple parameters
    if (all(param == parname(pval$index, params, -2))) {
      value <- as.data.frame(as.list(value),
                             stringsAsFactors = FALSE
      )
      names(value) <- param
      v <- rbind(v, value)

      if (dim(v)[1] == nb_interventions) {
        itk <- c(itk, as.list(v))
        multi <- FALSE
      }
      next
    } else {
      v <- as.data.frame(as.list(value),
                         stringsAsFactors = FALSE
      )
      names(v) <- param
    }

    if (!multi) {
      itk <- c(itk, as.list(v))
    }
  }
  return(itk)
}


#' @rdname get_param_txt
#' @export
get_soil_txt <- function(file = "param.sol",
                         stics_version,
                         filepath = lifecycle::deprecated()) {

  # filepath
  if (lifecycle::is_present(filepath)) {
    lifecycle::deprecate_warn(
      "1.0.0", "get_soil_txt(filepath)",
      "get_soil_txt(file)"
    )
  } else {
    filepath <- file # to remove when we update inside the function
  }

  stics_version <- check_version_compat(stics_version = stics_version)

  if (!file.exists(filepath))
    stop(filepath, ": does not exist !")

  params <- readLines(filepath, warn = FALSE)
  soil <- vector(mode = "list", length = 0)

  val <- function(index = 1) {
    index <- index + 1
    vec <- strsplit(x = params[index - 1], split = " ")[[1]]
    vec <- vec[vec != ""]
    return(list(vec = vec, index = index))
  }

  soil$nbcouchessol_max <- "1000"

  if (get_version_num(stics_version = stics_version) < 10) {
    par_vec <- c(
      "numsol", "typsol", "argi", "Norg", "profhum", "calc", "pH",
      "concseuil", "albedo", "q0", "ruisolnu", "obstarac", "pluiebat",
      "mulchbat", "zesx", "cfes", "z0solnu", "CsurNsol", "penterui"
    )
  } else {
    par_vec <- c(
      "numsol", "typsol", "argi", "Norg", "profhum", "calc", "pH",
      "concseuil", "albedo", "q0", "ruisolnu", "obstarac", "pluiebat",
      "mulchbat", "zesx", "cfes", "z0solnu", "CsurNsol", "finert", "penterui"
    )
  }

  ret_val <- val()
  soil[par_vec] <- ret_val$vec


  ret_val <- val(ret_val$index)

  soil[c(
    "numsol", "codecailloux", "codemacropor", "codefente",
    "codrainage", "coderemontcap", "codenitrif", "codedenit"
  )] <- ret_val$vec

  ret_val <- val(ret_val$index)


  soil[c(
    "numsol", "profimper", "ecartdrain", "ksol", "profdrain",
    "capiljour", "humcapil", "profdenit", "vpotdenit"
  )] <- ret_val$vec

  vec <- matrix(data = NA, nrow = 9, ncol = 5)
  for (i in 1:5) {
    ret_val <- val(ret_val$index)
    vec[, i] <- ret_val$vec
  }
  vec <- apply(vec, MARGIN = 1, FUN = list)

  soil[c(
    "numsol", "epc", "hccf", "hminf", "DAF",
    "cailloux", "typecailloux", "infil", "epd"
  )] <-
    lapply(vec, unlist)



  # Transform into numeric:
  soil <- character_to_numeric_list(soil)

  # removing duplicates
  soil$numsol <- unique(soil$numsol)

  return(soil)
}

#' @rdname get_param_txt
#' @export
get_station_txt <- function(file = "station.txt",
                            filepath = lifecycle::deprecated()) {
  # filepath
  if (lifecycle::is_present(filepath)) {
    lifecycle::deprecate_warn(
      "1.0.0", "get_station_txt(filepath)",
      "get_station_txt(file)"
    )
  } else {
    filepath <- file # to remove when we update inside the function
  }

  get_txt_generic(file = filepath)
}


#' @rdname get_param_txt
#' @export
get_usm_txt <- function(file = "new_travail.usm",
                        plant_id = NULL,
                        filepath = lifecycle::deprecated()) {

  # filepath
  if (lifecycle::is_present(filepath)) {
    lifecycle::deprecate_warn(
      "1.0.0", "get_usm_txt(filepath)",
      "get_usm_txt(file)"
    )
  } else {
    filepath <- file # to remove when we update inside the function
  }

  usm_params <- get_txt_generic(filepath)

  idx <- plant_id == 1:2

  if (is.null(plant_id) ||
      (length(plant_id) == 2 && all(idx))) return(usm_params)

  # getting specific info attached to plant id
  # fplt1 ou fplt2, ftec1 ou ftec2, flai1 ou flai2,
  # fobs1 ou fobs2
  tags <- c("fplt", "ftec", "flai", "fobs")
  usm_fields <- names(usm_params)
  id_to_rm <- setdiff(1:2, plant_id)
  for (i in seq_along(tags)) {
    par_name <- paste0(tags[i], id_to_rm)

    if (!(par_name %in% usm_fields)) next

    usm_params[[par_name]] <- NULL
  }
  return(usm_params)
}


#' Read parameter values from file
#'
#' @description Generic function to read STICS parameter files
#'
#' @param file Path (including name) of the file to read
#' @param names    Boolean, read the parameter names ?
#'
#' @return A named (if names=TRUE) list of parameter values
#' @keywords internal
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' path <- file.path(get_examples_path(file_type = "txt",
#'                                    stics_version = "V8.5"), "station.txt")
#' get_txt_generic(path)
#' }
#'
get_txt_generic <- function(file,
                            names = TRUE) {

  if (!file.exists(file))
    stop(file, ": does not exist !")

  params <- readLines(file)

  x <- as.list(params[!seq_along(params) %% 2])
  if (names) {
    names(x) <- gsub(":", "", params[!!seq_along(params) %% 2])
  }

  is_dupli <- duplicated(names(x))
  dupli_names <- unique(names(x)[is_dupli])

  # Remove duplicated names if any, and put the values as a vector instead
  for (i in dupli_names) {
    index_dupli <- which(names(x) == i)
    x[[index_dupli[1]]] <- unlist(x[index_dupli], use.names = FALSE)
    x <- x[-index_dupli[-1]]
  }

  character_to_numeric_list(x)
}



#' Character list to numeric list
#'
#' @description Tries to convert the values in a list into numeric values,
#' and if it fails, return as character.
#'
#' @param x A list with potential numeric values written a characters
#'
#' @return A list with numeric values when possible
#'
#' @keywords internal
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' test <- list(a = "2", b = "toto")
#' character_to_numeric_list(test)
#' }
#'
character_to_numeric_list <- function(x) {
  rapply(x, char2num, how = "replace")
}


char2num <- function(x) {
  if (!all(is.character(x))) {
    return()
  }

  x_trim <- trimws(x)

  if (any(x_trim == "")) {
    return(x)
  }

  if (!all(grepl(pattern = "[0-9]", x = x)) ||
      any(grepl(pattern = "[a-zA-Z]", x = x))) {
    return(x)
  }


  as.numeric(unlist(strsplit(x_trim, split = " ")))
}

list_to_character_vector <- function(x) {
  rapply(x, f = function(y) paste(as.character(y), collapse = " "))
}
