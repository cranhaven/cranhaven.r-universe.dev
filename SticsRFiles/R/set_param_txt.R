#' Set (replace) STICS input file parameters
#'
#' @description Replace or set an input parameter from a pre-existing
#'              STICS input file.
#'
#' @param workspace  Path of the workspace containing the STICS (txt)
#' input files.
#' @param param    Vector of parameter names.
#' @param value    New parameter value
#' @param append      Boolean. Append input to existing file
#' @param plant_id    The plant identifier (main crop: 1 ; associated crop: 2).
#' Only used for plant, technical or initialisation parameters (default = 1).
#' @param variety The plant variety to set the parameter value,
#' either the variety name (`codevar` in the plant file) or
#' the index (`variete` in the technical file).
#' @param value_id    The soil layers id or technical interventions id
#' @param stics_version An optional version name as listed in
#' get_stics_versions_compat() return
#' @param dirpath `r lifecycle::badge("deprecated")` `dirpath` is no
#'   longer supported, use `workspace` instead.
#' @param add `r lifecycle::badge("deprecated")` `add` is no
#'   longer supported, use `append` instead.
#' @param plant `r lifecycle::badge("deprecated")` `plant` is no
#'   longer supported, use `plant_id` instead.
#' @param layer `r lifecycle::badge("deprecated")` `layer` is no
#'   longer supported, use `value_id` instead.
#'
#' @param file Path (including name) of the file to modify
#' @param filepath `r lifecycle::badge("deprecated")` `filepath` is no
#'   longer supported, use `file` instead.
#'
#'
#' @details The \code{plant} parameter can be either equal to \code{1},
#'          \code{2} for the associated plant in the case of intercrop, or
#'          \code{c(1,2)} for both Principal and associated plants.
#'          \code{\link{get_var_info}} is a helper function that returns
#'          all possible output variables.
#'          If the \code{variety} is not given and if \code{param}
#'          is a varietal parameter, the function will modify the value of
#'          \code{param} for the simulated variety, as given
#'          in the technical file.
#'
#' @note \code{gen_varmod} is not used by \code{set_param_txt}.
#'       To replace the output variables required from STICS,
#'       please directly call \code{gen_varmod}.
#'
#' @return None
#'
#' @export
#'
#' @examples
#' # Getting example data path
#' path <- get_examples_path(file_type = "txt")
#'
#' # Change the value of durvieF for the current variety:
#' set_param_txt(workspace = path, param = "durvieF", value = 245)
#'
#' # Change the value of durvieF for another variety:
#' set_param_txt(workspace = path, param = "durvieF",
#'               variety = "Nefer", value = 178)
#' # Change the value of soil parameter "cailloux" for all layers
#' # or a specific one
#' set_param_txt(workspace = path, param = "cailloux", value = 1)
#' set_param_txt(workspace = path, param = "cailloux", value_id = 2, value = 2)
#'
#' # Change the value of parameter "amount" for all water supply interventions
#' # or a specific one
#' set_param_txt(workspace = path, param = "amount", value = 50)
#' set_param_txt(workspace = path, param = "amount", value_id = 2, value = 40)
#'
#'
#'
set_param_txt <- function(workspace,
                          param,
                          value,
                          append = FALSE,
                          plant_id = 1,
                          variety = NULL,
                          value_id = NULL,
                          stics_version = "latest",
                          dirpath = lifecycle::deprecated(),
                          add = lifecycle::deprecated(),
                          plant = lifecycle::deprecated(),
                          layer = lifecycle::deprecated()) {

  # dirpath
  if (lifecycle::is_present(dirpath)) {
    lifecycle::deprecate_warn(
      "1.0.0", "set_param_txt(dirpath)",
      "set_param_txt(workspace)"
    )
  } else {
    dirpath <- workspace # to remove when we update inside the function
  }

  # add
  if (lifecycle::is_present(add)) {
    lifecycle::deprecate_warn(
      "1.0.0", "set_param_txt(add)",
      "set_param_txt(append)"
    )
  } else {
    add <- append # to remove when we update inside the function
  }

  # plant
  if (lifecycle::is_present(plant)) {
    lifecycle::deprecate_warn(
      "1.0.0", "set_param_txt(plant)",
      "set_param_txt(plant_id)"
    )
  } else {
    plant <- plant_id # to remove when we update inside the function
  }

  # layer
  if (lifecycle::is_present(layer)) {
    lifecycle::deprecate_warn(
      "1.4.0", "set_param_txt(plant)",
      "set_param_txt(plant_id)"
    )
    value_id <- layer
  }

  stics_version <- check_version_compat(stics_version = stics_version)

  param <- gsub("P_", "", param)

  param_val <- get_param_txt(
    workspace = dirpath,
    param = param,
    exact = TRUE,
    stics_version = stics_version
  )

  if (length(param_val) == 0)
    stop("Unknown parameter: ", param, "\n",
         "Check case sensitivity or ",
         "use get_param_info for searching the exact name")

  file_param_list <- lapply(
    strsplit(names(param_val), "\\$"), function(x) {
      x[1] }
  )

  file_type <- file_param_list %>%
    unlist() %>%
    unique()

  if (length(file_type) > 1) {
    stop(
      "Parameter found in several files:", paste(file_type, collapse = ", "),
      "\nPlease use the set_* functions directly to set the parameter value."
    )
  }
  switch(file_type,
         ini = {
           set_ini_txt(
             file = file.path(dirpath, "ficini.txt"),
             param = param, value = value, append = add,
             plant_id = plant, layer = value_id,
             stics_version = stics_version
           )
         },
         general = {
           set_general_txt(
             file = file.path(dirpath, "tempopar.sti"),
             param = param, value = value, append = add
           )
         },
         tmp = {
           set_tmp_txt(
             file = file.path(dirpath, "tempoparV6.sti"),
             param = param, value = value, append = add
           )
         },
         soil = {
           set_soil_txt(
             file = file.path(dirpath, "param.sol"),
             param = param,
             value = value,
             layer = value_id,
             stics_version = stics_version
           )
         },
         usm = {
           set_usm_txt(
             file = file.path(dirpath, "new_travail.usm"),
             param = param, value = value
           )
         },
         station = {
           set_station_txt(
             file = file.path(dirpath, "station.txt"),
             param = param, value = value, append = add
           )
         },
         tec = {
           lapply(plant, function(x) {
             set_tec_txt(
               file = file.path(dirpath, paste0("fictec", x, ".txt")),
               param = param, value = value, append = add,
               value_id = value_id
             )
           })
         },
         plant = {
           lapply(plant, function(x) {
             if (is.null(variety)) {
               variety <-
                 unlist(get_param_txt(workspace = dirpath,
                                      param = "variete",
                                      exact = TRUE,
                                      stics_version = stics_version))[plant]
             } else {
               if (is.character(variety)) {
                 varieties <-
                   get_plant_txt(file = file.path(dirpath,
                                                  paste0("ficplt",
                                                         x,
                                                         ".txt")))$codevar
                 variety <- match(variety, varieties)
                 if (is.na(variety)) {
                   cli::cli_alert_danger(
                     paste0("Variety not found in plant",
                            "file. Possible varieties are: ",
                            "{.val {varieties}}")
                   )
                   return()
                 }
               }
             }
             set_plant_txt(
               file = file.path(dirpath, paste0("ficplt", x, ".txt")),
               param = param, value = value, append = add, variety = variety
             )
           })
         },
         stop("Parameter not found")
  )
  invisible()
}


#' @rdname set_param_txt
#' @export
set_usm_txt <- function(file = "new_travail.usm",
                        param,
                        value,
                        append = FALSE,
                        filepath = lifecycle::deprecated(),
                        add = lifecycle::deprecated()) {

  # filepath
  if (lifecycle::is_present(filepath)) {
    lifecycle::deprecate_warn(
      "1.0.0", "set_usm_txt(filepath)",
      "set_usm_txt(file)"
    )
  } else {
    filepath <- file # to remove when we update inside the function
  }
  # add
  if (lifecycle::is_present(add)) {
    lifecycle::deprecate_warn(
      "1.0.0", "set_usm_txt(add)",
      "set_usm_txt(append)"
    )
  } else {
    add <- append # to remove when we update inside the function
  }

  set_file_txt(filepath, param, value, add)
}

#' @rdname set_param_txt
#' @export
set_station_txt <- function(file = "station.txt",
                            param,
                            value,
                            append = FALSE,
                            filepath = lifecycle::deprecated(),
                            add = lifecycle::deprecated()) {

  # filepath
  if (lifecycle::is_present(filepath)) {
    lifecycle::deprecate_warn(
      "1.0.0", "set_station_txt(filepath)",
      "set_station_txt(file)"
    )
  } else {
    filepath <- file # to remove when we update inside the function
  }
  # add
  if (lifecycle::is_present(add)) {
    lifecycle::deprecate_warn(
      "1.0.0", "set_station_txt(add)",
      "set_station_txt(append)"
    )
  } else {
    add <- append # to remove when we update inside the function
  }

  set_file_txt(filepath, param, value, add)
}


#' @rdname set_param_txt
#' @export
set_ini_txt <- function(file = "ficini.txt",
                        param,
                        value,
                        append = FALSE,
                        plant_id = 1,
                        layer = NULL,
                        stics_version = "latest",
                        filepath = lifecycle::deprecated(),
                        add = lifecycle::deprecated()) {

  # filepath
  if (lifecycle::is_present(filepath)) {
    lifecycle::deprecate_warn(
      "1.0.0", "set_ini_txt(filepath)",
      "set_ini_txt(file)"
    )
  } else {
    filepath <- file # to remove when we update inside the function
  }
  # add
  if (lifecycle::is_present(add)) {
    lifecycle::deprecate_warn(
      "1.0.0", "set_ini_txt(add)",
      "set_ini_txt(append)"
    )
  } else {
    add <- append # to remove when we update inside the function
  }

  set_file_txt(filepath, param, value, add,
               plant_id = plant_id,
               value_id = layer,
               stics_version = stics_version
  )
}


#' @rdname set_param_txt
#' @export
set_general_txt <- function(file = "tempopar.sti",
                            param,
                            value,
                            append = FALSE,
                            filepath = lifecycle::deprecated(),
                            add = lifecycle::deprecated()) {

  # filepath
  if (lifecycle::is_present(filepath)) {
    lifecycle::deprecate_warn(
      "1.0.0", "set_general_txt(filepath)",
      "set_general_txt(file)"
    )
  } else {
    filepath <- file # to remove when we update inside the function
  }
  # add
  if (lifecycle::is_present(add)) {
    lifecycle::deprecate_warn(
      "1.0.0", "set_general_txt(add)",
      "set_general_txt(append)"
    )
  } else {
    add <- append # to remove when we update inside the function
  }


  set_file_txt(filepath, param, value, add)
}

#' @rdname set_param_txt
#' @export
set_tmp_txt <- function(file = "tempoparv6.sti",
                        param,
                        value,
                        append = FALSE,
                        filepath = lifecycle::deprecated(),
                        add = lifecycle::deprecated()) {

  # filepath
  if (lifecycle::is_present(filepath)) {
    lifecycle::deprecate_warn(
      "1.0.0", "set_tmp_txt(filepath)",
      "set_tmp_txt(file)"
    )
  } else {
    filepath <- file # to remove when we update inside the function
  }
  # add
  if (lifecycle::is_present(add)) {
    lifecycle::deprecate_warn(
      "1.0.0", "set_tmp_txt(add)",
      "set_tmp_txt(append)"
    )
  } else {
    add <- append # to remove when we update inside the function
  }


  set_file_txt(filepath, param, value, add)
}

#' @rdname set_param_txt
#' @export
set_plant_txt <- function(file = "ficplt1.txt",
                          param,
                          value,
                          append = FALSE,
                          variety = NULL,
                          filepath = lifecycle::deprecated(),
                          add = lifecycle::deprecated()) {

  # filepath
  if (lifecycle::is_present(filepath)) {
    lifecycle::deprecate_warn(
      "1.0.0", "set_plant_txt(filepath)",
      "set_plant_txt(file)"
    )
  } else {
    filepath <- file # to remove when we update inside the function
  }
  # add
  if (lifecycle::is_present(add)) {
    lifecycle::deprecate_warn(
      "1.0.0", "set_plant_txt(add)",
      "set_plant_txt(append)"
    )
  } else {
    add <- append # to remove when we update inside the function
  }

  set_file_txt(filepath, param, value, add, variety = variety)
}

#' @rdname set_param_txt
#' @export
set_tec_txt <- function(file = "fictec1.txt",
                        param,
                        value,
                        append = FALSE,
                        value_id = NULL,
                        filepath = lifecycle::deprecated(),
                        add = lifecycle::deprecated()) {

  # filepath
  if (lifecycle::is_present(filepath)) {
    lifecycle::deprecate_warn(
      "1.0.0", "set_tec_txt(filepath)",
      "set_tec_txt(file)"
    )
  } else {
    filepath <- file # to remove when we update inside the function
  }
  # add
  if (lifecycle::is_present(add)) {
    lifecycle::deprecate_warn(
      "1.0.0", "set_tec_txt(add)",
      "set_tec_txt(append)"
    )
  } else {
    add <- append # to remove when we update inside the function
  }

  set_file_txt(file = filepath,
               param =  param,
               value = value,
               append = add,
               value_id = value_id)
}

#' @rdname set_param_txt
#' @export
set_soil_txt <- function(file = "param.sol",
                         param,
                         value,
                         layer = NULL,
                         stics_version = "latest",
                         filepath = lifecycle::deprecated()) {

  # filepath
  if (lifecycle::is_present(filepath)) {
    lifecycle::deprecate_warn(
      "1.0.0", "set_soil_txt(filepath)",
      "set_soil_txt(file)"
    )
  } else {
    filepath <- file # to remove when we update inside the function
  }

  param <- gsub("P_", "", param)
  ref <- get_soil_txt(filepath, stics_version = stics_version)
  param <- paste0("^", param, "$")

  if (!is.null(layer)) {
    check_param_dim(param = param,
                    file_value = ref[[grep(param, names(ref))]],
                    value_id = layer,
                    value = value)
    ref[[grep(param, names(ref))]][layer] <- format(value, scientific = FALSE)
  } else {
    if(length(value) > 1) {
      check_param_dim(param = param,
                      file_value = ref[[grep(param, names(ref))]],
                      value = value)
    }
    ref[[grep(param, names(ref))]][] <- format(value, scientific = FALSE)
  }



  if (get_version_num(stics_version = stics_version) < 10) {
    line <- paste(
      " ", " ", " ", ref$numsol[1], " ", " ", " ", ref$typsol,
      ref$argi, ref$Norg, ref$profhum, ref$calc,
      ref$pH, ref$concseuil, ref$albedo, ref$q0,
      ref$ruisolnu, ref$obstarac, ref$pluiebat,
      ref$mulchbat, ref$zesx, ref$cfes,
      ref$z0solnu, ref$CsurNsol, ref$penterui
    )
  } else {
    line <- paste(
      " ", " ", " ", ref$numsol[1], " ", " ", " ", ref$typsol,
      ref$argi, ref$Norg, ref$profhum, ref$calc,
      ref$pH, ref$concseuil, ref$albedo, ref$q0,
      ref$ruisolnu, ref$obstarac, ref$pluiebat,
      ref$mulchbat, ref$zesx, ref$cfes,
      ref$z0solnu, ref$CsurNsol, ref$finert, ref$penterui
    )
  }

  writeLines(line, filepath)

  write(paste(
    " ", " ", " ", ref$numsol[1], " ", " ", " ",
    ref$codecailloux, ref$codemacropor,
    ref$codefente, ref$codrainage, ref$coderemontcap,
    ref$codenitrif, ref$codedenit
  ),
  filepath,
  append = TRUE
  )

  write(paste(
    " ", " ", " ", ref$numsol[1], " ", " ", " ", ref$profimper,
    ref$ecartdrain, ref$ksol,
    ref$profdrain, ref$capiljour, ref$humcapil,
    ref$profdenit, ref$vpotdenit
  ),
  filepath,
  append = TRUE
  )

  for (icou in 1:5) {
    write(paste(
      " ", " ", " ", ref$numsol[1], " ", " ", " ",
      ref$epc[icou], ref$hccf[icou],
      ref$hminf[icou], ref$DAF[icou], ref$cailloux[icou],
      ref$typecailloux[icou], ref$infil[icou],
      ref$epd[icou]
    ),
    filepath,
    append = TRUE
    )
  }
}


#' Internal function to set some STICS input file parameters
#'
#' @description Replace or set an input parameter from a pre-existing
#'              STICS input file. This function is called by some of the
#'               generic \code{set_*} functions under the hood.
#'
#' @param file Path to the parameter file
#' @param param    Parameter name
#' @param value    New parameter value
#' @param append      Boolean. Append input to existing file
#' @param plant_id    The plant identifier (main crop: 1 ; associated crop: 2).
#' @param variety The plant variety to set the parameter value,
#' either the variety
#' name (`codevar` in the plant file) or the index
#' (`variete` in the technical file).
#' @param value_id index of technical interventions to be used to
#' set parameter values, or layer index for soil parameters
#' @param stics_version An optional version name as listed in
#' get_stics_versions_compat() return
#'
#' @details The function uses `base::sys.call()` to know from which function
#'          of the \code{set_*} family it is called, so it won't work properly
#'          if called by the user directly. This is why this function
#'          is internal.
#'
#' @note This function is not used for \code{\link{set_soil_txt}}.
#'
#' @seealso \code{\link{set_param_txt}}.
#'
#' @keywords internal
#'
#' @noRd
#'
set_file_txt <- function(file,
                         param,
                         value,
                         append,
                         plant_id = NULL,
                         variety = NULL,
                         value_id = NULL,
                         stics_version = "latest") {
  param <- gsub("P_", "", param)

  stics_version <- check_version_compat(stics_version = stics_version)

  # access the function name from which set_file_txt was called
  type <- strsplit(deparse(sys.call(-1)), split = "\\(")[[1]][1]
  params <- readLines(file)
  param_ <- paste0("^:{0,1}", param, "$")
  switch(type,
         set_usm_txt = {
           ref <- get_usm_txt(file)
           if (grep(param_, names(ref)) < grep("fplt", names(ref))) {
             ref_index <- grep(param_, names(ref)) * 2
           } else {
             ref_index <- grep(param_, params) + 1
           }
         },
         set_station_txt = {
           ref <- get_station_txt(file)
           ref_index <- grep(param_, names(ref)) * 2
         },
         set_ini_txt = {
           ref <- get_ini_txt(file, stics_version = stics_version)

           # fix plant id if param is attached to a plant
           if (is.null(plant_id) &&
               (param %in% names(ref$plant$plant1))) {
             plant_id <- 1
           }

           # changing param value in ref
           if (is.null(plant_id)) {
             if (is.null(value_id)) {
               if (length(value) > 1){
                 check_param_dim(param = param,
                                 file_value = ref[[param]],
                                 value = value)
               }
               # all values take the same now
               ref[[param]][] <- value
             } else {
               # check layers idx
               # and values number
               check_param_dim(param = param,
                               file_value = ref[[param]],
                               value_id = value_id,
                               value = value)
               ref[[param]][[value_id]] <- value
             }
           } else {
             plt_tag <- paste0("plant", plant_id)
             if (is.null(value_id)) {
               if (length(value) > 1) {
                 check_param_dim(param = param,
                                 file_value = ref$plant[[plt_tag]][[param]],
                                 value = value)
               }
               # all values take the same now
               ref$plant[[plt_tag]][[param]][] <- value
             } else {
               check_param_dim(param = param,
                               file_value = ref$plant[[plt_tag]][[param]],
                               value_id = value_id,
                               value = value)
               ref$plant[[plt_tag]][[param]][value_id] <- value
             }
           }

           value <- list_to_character_vector(ref)

           # rows index according to version
           ref_index <- get_ini_val_idx(stics_version)
         },
         set_plant_txt = {
           ref_index <- grep(param_, params) + 1
           if (!is.null(variety) & length(ref_index) > 1) {
             if (length(ref_index) >= variety) {
               ref_index <- ref_index[variety]
             } else {
               stop("Variety number set in the tec file is superior",
                    "to the number of varieties defined in the plant file.")
             }
           }
         },
         set_tec_txt = {
           ref <- get_tec_txt(file, stics_version = stics_version)
           # add treatment for getting lines
           # add index on the line for the parameters when several
           # (interventions)
           # question: replacing existing individual values and
           # modifying interventions plan (i.e. reduce irrigations nb )

           # getting sublist from ref, change values and
           # transform to text and replace using lines index !
           idx_lines <- grep(param, params)

           # Getting par names on one line
           line_param <- unlist(strsplit(params[idx_lines[1]], split = " "))

           lines_values <- ref[line_param]

           # replacing values
           # all values with a single
           if(is.null(value_id)) {
             if (length(value) > 1) {
               check_param_dim(param = param,
                               file_value = lines_values[[param]],
                               value = value)
             }
             lines_values[[param]][] <- value
           } else {
             # several values for specific ids
             check_param_dim(param = param,
                             file_value = lines_values[[param]],
                             value_id = value_id,
                             value = value)
             lines_values[[param]][value_id] <- value
           }

           df_lines_values <- as.data.frame(
             lapply(lines_values, as.character), stringsAsFactors = FALSE)

           # Values of parameters to replace in params at idx_lines + 1
           value <- apply(df_lines_values,
                          1,
                          function(x) paste(x, collapse = " "))

           ref_index <- idx_lines + 1
         },
         # Default here
         {
           ref_index <- grep(param_, params) + 1
         }
  )

  if (!length(ref_index) > 0) {
    if (append) {
      value <- paste(value, collapse = " ")
      params <- c(params, param, value)
      ref_index <- grep(param_, params) + 1
    } else {
      stop(paste(param, "parameter not found in:\n", file))
    }
  } else {
    if (append) {
      stop(paste(
        "Parameter", param, "already present in the file,",
        "try to replace its value",
        "instead of adding the parameter"
      ))
    }
  }

  if (length(ref_index) != length(value)) {
    stop(paste(
      "Length of input value different from parameter value length.\n",
      "Original values:\n", paste(params[ref_index], collapse = ", "),
      "\ninput:\n", paste(value, collapse = ", ")
    ))
  }
  params[ref_index] <- format(value, scientific = FALSE)
  writeLines(params, file)
}



get_ini_val_idx <- function(stics_version) {
  if (get_version_num(stics_version = stics_version) < 10) {
    idx <- c(
      2,
      4:10,
      12,
      14:20,
      22,
      24,
      26,
      28
    )
  } else {
    idx <- c(
      2,
      4:7,
      9:16,
      18,
      20:23,
      25:32,
      34,
      36,
      38,
      40,
      43,
      45,
      47,
      49
    )
  }
  idx
}


#' Check consistency of 2 vectors lengths
#'
#' @param param parameter name
#' @param file_value vector of param values
#' @param value_id vector of values id used for replacement
#' @param value vector of new values for `param` (its length is
#' either equal to file_value or value_id length)
#'
#' @return None
#' @keywords internal
#'
#' @noRd
#'
check_param_dim <- function(param,
                            file_value,
                            value_id = NULL,
                            value = NULL) {

  file_val_nb <- length(file_value)
  if (is.null(value_id)) {
    max_id <- file_val_nb
  } else {
    value_id <- unique(value_id)
    max_id <- max(value_id)
  }

  if (max_id > file_val_nb)
    stop("for ", param, " parameter values replacement\n",
         "the maximum number of values to be replaced in the file (",
         file_val_nb,
         ") ",
         "exceeds with the maximum of given id (",
         max_id,
         ")")

  # no more checks
  if (is.null(value)) return(invisible())

  # checking replacing value
  replace_val_nb <- length(value)

  if (file_val_nb == replace_val_nb) return(invisible())

  if (!is.null(value_id)) {
    replace_val_id_nb <- length(value_id)
    if (replace_val_id_nb == replace_val_nb)
      return(invisible())

  }

  stop("for ", param, " parameter values replacement\n",
       "the number of values to be replaced in the file (", replace_val_nb, ") ",
       "is not consistent with the given values' ids (", replace_val_id_nb,
       ")")

}

