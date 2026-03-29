# restructure_data script:
#==============================
#' Ingest and stack variables needed in calibration.
#'
#' Opens and stacks isotope ratio and water/carbon dioxide mole fraction
#' variables from monthly HDF5 files. If a new enough version of `neonUtilities`
#' is available, this function will try to use `fasttime` in order to accelerate
#' data stacking.
#'
#' @author Rich Fiorella \email{rfiorella@@lanl.gov}
#'
#' @param inname A file (or list of files) to extract data from for calibration.
#' @param analyte Carbon (Co2) or water (H2o)?
#' @param name_fix Fix to data frame required for next-generation calibration
#'                 functions, but breaks old 'by_month()' functions. This
#'                 parameter provides a necessary work around until these
#'                 functions are removed.
#' @param amb_avg The averaging interval of the ambient data to extract.
#' @param ref_avg The averaging interval of the reference data to extract.
#'
#' @return List of data frames, taken from files specified in `inname`
#' @export
#'
#' @importFrom stats setNames
#' @importFrom magrittr %>%
ingest_data <- function(inname,
                        analyte,
                        name_fix = TRUE,
                        amb_avg,
                        ref_avg) {

  # this function needs to:
  # 1. read in and stack variables.
  # 2. restructure them to have the same setup as output files.
  # 3. return list structure where elements are: a) ambient data,
  # b) ambient qfqm, c) ambient ucrt, d-f) same, but for ref vars.

  analyte <- validate_analyte(analyte)
  backupMethod <- FALSE

  # read site name and attributes from (first file in) inname
  # combined into sequential calls to avoid redundant file opens
  site <- h5_ls(inname[1])$name[1]
  attrs <- h5_read_attrs(inname[1], site)
  # attrs are also returned in the output list to avoid re-reading later

  nheights <- attrs$LvlMeasTow

  if (analyte == "Co2") {

    data <- try(neonUtilities::stackEddy(inname,
                                     avg = amb_avg,
                                     level = "dp01",
                                     var = "isoCo2",
                                     useFasttime = TRUE,
                                     runLocal = TRUE)[[1]], silent = TRUE)
    if ("try-error" %in% class(data)) {
      data <- neonUtilities::stackEddy(inname,
                                           avg = 9,
                                           level = "dp01",
                                           var = "isoCo2",
                                           useFasttime = TRUE,
                                           runLocal = TRUE)[[1]]
      backupMethod <- TRUE
    }

    # filter data and remove rows that are all NaNs:
    data <- data %>%
      dplyr::select("verticalPosition", "timeBgn", "timeEnd",
                    tidyselect::contains("isoCo2"))

    # stack required variables.
    amb_stack <- c("dlta13CCo2", "pres", "presEnvHut", "rhEnvHut",
                   "rtioMoleDry12CCo2", "rtioMoleDry13CCo2", "rtioMoleDryCo2",
                   "rtioMoleDryH2o", "rtioMoleWet12CCo2", "rtioMoleWet13CCo2",
                   "rtioMoleWetCo2", "rtioMoleWetH2o", "rtioMoleWetH2oEnvHut",
                   "temp", "tempEnvHut")

    ref_stack <- base::sort(base::append(amb_stack,
                                         c("dlta13CCo2Refe",
                                           "rtioMoleDryCo2Refe")))

    # split data into ambient and reference data frames.
    ambient <- data %>%
      dplyr::filter(.data$verticalPosition %in% c("010", "020", "030", "040",
                                                  "050", "060", "070", "080"))

    # check how many heights are present in ambient.
    if (length(unique(ambient$verticalPosition)) < nheights) {
      print("Height missing, attempting to resolve:")

      # determine which height is missing:
      hgts_present <- seq(from = 1, to = nheights, by = 1) %in%
        (as.numeric(unique(ambient$verticalPosition)) / 10)

      hgts_absentl <- !hgts_present

      hgts_absent <- seq(from = 1, to = nheights, by = 1)[hgts_absentl]

      # add a row to data, and then change verticalPosition to missing heights
      for (i in hgts_absent) {
        target_row <- nrow(ambient) + 1
        ambient[target_row, ] <- NA
        ambient[target_row, "verticalPosition"] <- paste0("0", i, "0")
      }
    }

    reference <- data %>%
      dplyr::filter(.data$verticalPosition %in%
                      c("co2Low", "co2Med", "co2High"))

  } else if (analyte == "H2o") {

    # stack data available for a given site into a single timeseries.
    data9 <- neonUtilities::stackEddy(inname,
                                      level = "dp01",
                                      var = "isoH2o",
                                      avg = amb_avg,
                                      useFasttime = TRUE,
                                      runLocal = TRUE)[[1]]
    data3 <- neonUtilities::stackEddy(inname,
                                      level = "dp01",
                                      var = "isoH2o",
                                      avg = ref_avg,
                                      useFasttime = TRUE,
                                      runLocal = TRUE)[[1]]

    # filter data and remove rows that are all NaNs:
    data9 <- data9 %>%
      dplyr::select("verticalPosition", "timeBgn", "timeEnd",
                    tidyselect::contains("isoH2o"))

    # stack required variables.
    amb_stack <- c("dlta18OH2o", "dlta2HH2o", "pres", "presEnvHut", "rhEnvHut",
                   "rtioMoleDryH2o", "rtioMoleWetH2o", "rtioMoleWetH2oEnvHut",
                   "temp", "tempEnvHut")

    ref_stack <- base::sort(base::append(amb_stack,
                                         c("dlta18OH2oRefe",
                                           "dlta2HH2oRefe")))

    # split data into ambient and reference data frames.
    ambient <- data9 %>%
      dplyr::filter(.data$verticalPosition %in% c("010", "020", "030", "040",
                                                  "050", "060", "070", "080"))

    # check how many heights are present in ambient.
    if (length(unique(ambient$verticalPosition)) < nheights) {
      print("Height missing, attempting to resolve:")

      # determine which height is missing:
      hgts_present <- seq(from = 1, to = nheights, by = 1) %in%
        (as.numeric(unique(ambient$verticalPosition)) / 10)

      hgts_absentl <- !hgts_present

      hgts_absent <- seq(from = 1, to = nheights, by = 1)[hgts_absentl]

      # add a row to data, and then change verticalPosition to missing heights
      for (i in hgts_absent) {
        target_row <- nrow(ambient) + 1
        ambient[target_row, ] <- NA
        ambient[target_row, "verticalPosition"] <- paste0("0", i, "0")
      }
    }

    reference <- data3 %>%
      dplyr::filter(.data$verticalPosition %in%
                      c("h2oLow", "h2oMed", "h2oHigh"))

  }

  ambi_by_height <- base::split(ambient, factor(ambient$verticalPosition))
  refe_by_height <- base::split(reference, factor(reference$verticalPosition))

  #-------------------------
  # RESTRUCTURE AMBIENT
  # feed into restructure carbon variables:
  if (analyte == "Co2") {
    ambi_out <- lapply(ambi_by_height,
                       function(y) {
                         lapply(amb_stack,
                                function(x) {
                                  restructure_variables(y,
                                                        varname = x,
                                                        mode = "ambient",
                                                        group = "data",
                                                        species = "Co2")
                                })
                       })
  } else {
    ambi_out <- lapply(ambi_by_height,
                       function(y) {
                         lapply(amb_stack,
                                function(x) {
                                  restructure_variables(y,
                                                        varname = x,
                                                        mode = "ambient",
                                                        group = "data",
                                                        species = "H2o")
                                })
                       })
  }


  # loop through again to rename data frames.
  ambi_out <- lapply(ambi_out, setNames, amb_stack)

  # check length, and error out if a height has been dropped.
  #================== THIS SHOULD BE MOVED TO UNIT TEST
  test_var <- identical(as.integer(nheights), length(ambi_out))

  if (!test_var) {
    stop("Tower height dropped somewhere within ingest_data...")
  }

  #-------------------------
  # RESTRUCTURE REFERENCE
  # feed into restructure carbon variables:
  if (analyte == "Co2") {
    refe_out <- lapply(refe_by_height,
                       function(y) {
                         lapply(ref_stack,
                                function(x) {
                                  restructure_variables(y,
                                                        varname = x,
                                                        mode = "reference",
                                                        group = "data",
                                                        species = "Co2")
                                })
                       }) # replace the of the variables.
  } else {
    refe_out <- lapply(refe_by_height,
                       function(y) {
                         lapply(ref_stack,
                                function(x) {
                                  restructure_variables(y,
                                                        varname = x,
                                                        mode = "reference",
                                                        group = "data",
                                                        species = "H2o")
                                })
                       }) # replace the of the variables.
  }


  # loop through again to rename data frames.
  refe_out <- lapply(refe_out, setNames, ref_stack)

  # remove variable name from ambi_out data frames -
  # could be used here though to validate in future version.
  # variable name has been removed in restructure_carbon_variables
  # - could move it back here to validate!

  #changing average period in numeric to characters, e.g. 9 to 09m
  if (backupMethod) {
    avg_char <- "09m"
  } else {
    avg_char <- paste0("0", amb_avg, "m")
  }

  # get number of heights
  if (nrow(ambient) > 0) {
    heights <- unique(ambient$verticalPosition) # not that efficient, but needed
    names_vector <- vector()
    for (i in seq_along(heights)) {
      names_vector[i] <- paste0("000_0", i, "0_", avg_char)
    }
    names(ambi_out) <- names_vector
  }

  if (name_fix) {
    if (analyte == "H2o") {
      avg_char <- paste0("0", ref_avg, "m")
    }
    # append _09m to refe_out....MAY CAUSE PROBLEMS FOR OTHER METHODS!!!!!!
    names(refe_out) <- paste0(names(refe_out), "_", avg_char)
  }

  output <- list(ambi_out, refe_out, reference, attrs)
  names(output) <- c("ambient", "reference", "refe_stacked", "attrs")

  return(output)
}

#-----------------------------------------
# Restructure stacked data.frames to essential variables.
#'
#' Restructures data.frames imported by ingest_data to shorten variable names
#' and  Wrapper function around restructure_carbon_variables
#' and restructure_water_variables.
#'
#' @author Rich Fiorella \email{rfiorella@@lanl.gov}
#'
#' @param varname Which variable are we applying this function to? There's
#'                a list of ~10 common ones to write to the hdf5 file.
#' @param dataframe Input data.frame, from `neonUtilities::stackEddy`
#' @param mode Are we fixing a reference data frame or an ambient data frame?
#' @param group Data, ucrt, or qfqm?
#' @param species Set to 'Co2' for carbon; 'H2o' for water
#'
#' @return data.frame formatted for output to hdf5 file.
#' @export
#'
#' @importFrom magrittr %>%
#'
restructure_variables <- function(dataframe,
                                  varname,
                                  mode,
                                  group,
                                  species) {

  species <- validate_analyte(species)
  if (species == "Co2") {
    output <- restructure_carbon_variables(dataframe,
                                           varname,
                                           mode,
                                           group)
  } else {
    output <- restructure_water_variables(dataframe,
                                          varname,
                                          mode,
                                          group)
  }
  return(output)

}

#-----------------------------------------
#' Restructure ingested variables for the carbon isotope system.
#'
#' Restructures carbon isotope measurement system variables and shortens names 
#' to simplify referencing variables elsewhere in calibration code.
#'
#' @param varname Which variable are we applying this function to? There's
#'                a list of ~10 common ones to write to the hdf5 file.
#' @param dataframe Input data.frame, from `neonUtilities::stackEddy`
#' @param mode Are we fixing a reference data frame or an ambient data frame?
#' @param group Data, ucrt, or qfqm?
#'
#' @return data.frame formatted for output to hdf5 file.
#'
#' @importFrom magrittr %>%

restructure_carbon_variables <- function(dataframe,
                                         varname,
                                         mode,
                                         group) {

  if (mode != "reference" & mode != "ambient") {

    stop("Invalid selection to mode argument.")

  } else if (mode == "reference") {

    if (group == "data") {

      output <- dataframe %>%
        dplyr::select("verticalPosition", "timeBgn", "timeEnd",
                      tidyselect::starts_with(paste0("data.isoCo2.",
                                                     varname,
                                                     "."))) %>%
        dplyr::filter(!(.data$verticalPosition %in% c("010", "020", "030",
                                                      "040", "050", "060",
                                                      "070", "080"))) %>%
        dplyr::rename(mean = paste0("data.isoCo2.", varname, ".mean"),
                      min  = paste0("data.isoCo2.", varname, ".min"),
                      max  = paste0("data.isoCo2.", varname, ".max"),
                      vari = paste0("data.isoCo2.", varname, ".vari"),
                      numSamp = paste0("data.isoCo2.", varname, ".numSamp")) %>%
        dplyr::mutate(dom = lubridate::day(.data$timeBgn),
                      yr  = lubridate::year(.data$timeBgn),
                      mn  = lubridate::month(.data$timeBgn)) %>%
        dplyr::group_by(.data$yr, .data$mn, .data$dom) %>%
        dplyr::filter(.data$numSamp > 30 | is.na(.data$numSamp)) %>%
        dplyr::ungroup() %>%
        dplyr::select(-"dom", -"yr", -"mn", -"verticalPosition")

    } else if (group == "qfqm") {

      if (!grepl("Refe", varname)) {

        output <- dataframe %>%
          dplyr::select("verticalPosition", "timeBgn", "timeEnd",
                        tidyselect::starts_with(paste0("qfqm.isoCo2.",
                                                       varname,
                                                       "."))) %>%
          dplyr::filter(!(.data$verticalPosition %in% c("010", "020", "030",
                                                        "040", "050", "060",
                                                        "070", "080"))) %>%
          dplyr::rename(qfFinl = paste0("qfqm.isoCo2.", varname, ".qfFinl")) %>%
          dplyr::mutate(varname = varname)

      }

    } else if (group == "ucrt") {

      if (!grepl("Refe", varname)) {

        output <- dataframe %>%
          dplyr::select("verticalPosition", "timeBgn", "timeEnd",
                        tidyselect::starts_with(paste0("ucrt.isoCo2.",
                                                       varname,
                                                       "."))) %>%
          dplyr::filter(!(.data$verticalPosition %in% c("010", "020", "030",
                                                        "040", "050", "060",
                                                        "070", "080"))) %>%
          dplyr::rename(mean = paste0("ucrt.isoCo2.", varname, ".mean"),
                        vari = paste0("ucrt.isoCo2.", varname, ".vari"),
                        se   = paste0("ucrt.isoCo2.", varname, ".se")) %>%
          dplyr::mutate(varname = varname)

      }
    }

  } else if (mode == "ambient") {
    output <- dataframe %>%
      dplyr::select("verticalPosition", "timeBgn", "timeEnd",
                    tidyselect::starts_with(paste0("data.isoCo2.",
                                                   varname,
                                                   "."))) %>%
      dplyr::filter(!(.data$verticalPosition %in% c("co2Low", "co2Med",
                                                    "co2High", "co2Arch"))) %>%
      dplyr::rename(mean = paste0("data.isoCo2.", varname, ".mean"),
                    min  = paste0("data.isoCo2.", varname, ".min"),
                    max  = paste0("data.isoCo2.", varname, ".max"),
                    vari = paste0("data.isoCo2.", varname, ".vari"),
                    numSamp = paste0("data.isoCo2.", varname, ".numSamp")) %>%
      dplyr::select(-"verticalPosition")

  }

  # stackEddy will have converted time to posixct - covert back here.
  output$timeBgn <- convert_POSIXct_to_NEONhdf5_time(output$timeBgn)
  output$timeEnd <- convert_POSIXct_to_NEONhdf5_time(output$timeEnd)

  return(output)
}

#-----------------------------------------
#' Restructure ingested variables for the water isotope system.
#'
#' Restructures water isotope measurement system variables and shortens names 
#' to simplify referencing variables elsewhere in calibration code.
#' 
#' @param varname Which variable are we applying this function to? There's
#'                a list of ~10 common ones to write to the hdf5 file.
#' @param dataframe Input data.frame, from `neonUtilities::stackEddy`
#' @param mode Are we fixing a reference data frame or an ambient data frame?
#' @param group Data, ucrt, or qfqm?
#'
#' @return data.frame formatted for output to hdf5 file.
#'
restructure_water_variables <- function(dataframe,
                                        varname,
                                        mode,
                                        group) {

  # ensure that varname is a string but standard is a data.frame
  if (!is.character(varname)) {
    stop("varname must be a string")
  } else if ((!is.data.frame(dataframe) & mode == "reference") |
               (!is.list(dataframe) & mode == "ambient")) {
    stop("dataframe must be data.frame (reference mode) or list (ambient mode)")
  }

  if (mode != "reference" & mode != "ambient") {
    stop("Invalid selection to mode argument.")
  } else if (mode == "reference") {

    if (group == "data") {

      output <- dataframe %>%
        dplyr::select("timeBgn", "timeEnd",
                      tidyselect::starts_with(paste0("data.isoH2o.",
                                                     varname,
                                                     "."))) %>%
        dplyr::rename(mean = paste0("data.isoH2o.", varname, ".mean"),
                      min  = paste0("data.isoH2o.", varname, ".min"),
                      max  = paste0("data.isoH2o.", varname, ".max"),
                      vari = paste0("data.isoH2o.", varname, ".vari"),
                      numSamp = paste0("data.isoH2o.", varname, ".numSamp")) %>%
        dplyr::mutate(varname = varname) %>%
        dplyr::mutate(dom = lubridate::day(.data$timeBgn),
                      yr  = lubridate::year(.data$timeBgn),
                      mn  = lubridate::month(.data$timeBgn)) %>%
        dplyr::group_by(.data$yr, .data$mn, .data$dom) %>%
        dplyr::filter(.data$numSamp > 30 | is.na(.data$numSamp)) %>%
        dplyr::slice(tail(dplyr::row_number(), 3)) %>%
        dplyr::ungroup() %>%
        dplyr::select(-"dom", -"yr", -"mn")

    } else if (group == "qfqm") {

      if (!grepl("Refe", varname)) {

        output <- dataframe %>%
          dplyr::select("timeBgn", "timeEnd",
                        tidyselect::starts_with(paste0("qfqm.isoH2o.",
                                                       varname, "."))) %>%
          dplyr::rename(qfFinl = paste0("qfqm.isoH2o.", varname, ".qfFinl")) %>%
          dplyr::mutate(varname = varname) %>%
          dplyr::filter(.data$timeBgn %in% output$timeBgn)
      }

    } else if (group == "ucrt") {

      if (!grepl("Refe", varname)) {

        output <- dataframe %>%
          dplyr::select("timeBgn", "timeEnd",
                        tidyselect::starts_with(paste0("ucrt.isoH2o.",
                                                       varname,
                                                       "."))) %>%
          dplyr::rename(mean = paste0("ucrt.isoH2o.", varname, ".mean"),
                        vari = paste0("ucrt.isoH2o.", varname, ".vari"),
                        se   = paste0("ucrt.isoH2o.", varname, ".se")) %>%
          dplyr::mutate(varname = varname) %>%
          dplyr::filter(.data$timeBgn %in% output$timeBgn)

      }
    }

  } else if (mode == "ambient") {

    output <- dataframe %>%
      dplyr::select("verticalPosition", "timeBgn", "timeEnd",
                    tidyselect::starts_with(paste0("data.isoH2o.",
                                                   varname,
                                                   "."))) %>%
      dplyr::filter(!(.data$verticalPosition %in% c("h2oLow",
                                                    "h2oMed",
                                                    "h2oHigh"))) %>%
      dplyr::rename(mean = paste0("data.isoH2o.", varname, ".mean"),
                    min  = paste0("data.isoH2o.", varname, ".min"),
                    max  = paste0("data.isoH2o.", varname, ".max"),
                    vari = paste0("data.isoH2o.", varname, ".vari"),
                    numSamp = paste0("data.isoH2o.", varname, ".numSamp")) %>%
      dplyr::mutate(varname = varname)

  }

  # stackEddy will have converted time to posixct - covert back here.
  output$timeBgn <- convert_POSIXct_to_NEONhdf5_time(output$timeBgn)
  output$timeEnd <- convert_POSIXct_to_NEONhdf5_time(output$timeEnd)

  return(output)
}
