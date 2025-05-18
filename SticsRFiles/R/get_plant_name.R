
#' Get plant names
#'
#' @description Get the plant name (and file name) for each usm in a workspace
#'
#' @param workspace      Path of a JavaSTICS workspace, or a vector of
#' (recursive call).
#' @param usms_filepath  Path of the usms file (`usms.xml`)
#' @param usm_name      Vector of usms to read (optional, used to filter usms)
#' @param javastics_path JavaSTICS installation path (Optional, needed if the
#' plant files are not in the `workspace` but rather in the JavaSTICS
#' default workspace). Only used to get the plants names.
#' @param verbose Logical value (optional), TRUE to display information
#' on error, FALSE otherwise (default)
#'
#'
#' @return A list of plant names for each usm with one value for sole crop
#' and two values for intercrop (principal, associated).
#' @keywords internal
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' path <- get_examples_path(file_type = "xml")
#'
#' # Get plant names for all usms (no javastics path, so only the file
#' # name is returned):
#' get_plant_name(path)
#' # Get plant names only for banana:
#' get_plant_name(path, "banana")
#'
#' # Get plant names for banana with a javastics path, so the plant
#' # name is returned:
#' get_plant_name(path)
#' # NB: if the plant folder is in the workspace, no need to provide
#' # javastics_path
#' }
#'
get_plant_name <- function(workspace,
                           usms_filepath,
                           usm_name = NULL,
                           javastics_path = NULL,
                           verbose = TRUE) {
  usms_path <- normalizePath(usms_filepath, mustWork = FALSE)

  if (!file.exists(usms_path)) {
    stop(usms_filepath, ": not found !")
  }

  # recalculating usms file name
  usms_filename <- basename(usms_path)

  # getting usms names from the usms.xml file:
  usms <- get_usms_list(file = usms_path)

  # Filtering USMs if required:
  if (!is.null(usm_name)) {
    usm_exist <- usm_name %in% usms

    # Some provided usms are not available:
    if (!all(usm_exist)) {
      if (verbose) {
        cli::cli_alert_danger(paste0("The usm{?s} ",
                                     "{.val {usm_name[!usm_exist]}}",
                                     " d{?oes/o} not exist in the workspace!"))
        cli::cli_alert_info("Usm{?s} found in the workspace: {.val {usms}}")
      }
      stop(usm_name, ": do(es) not match usms")
    }
    usms <- usm_name
  }

  nb_plant <- get_plants_nb(usms_path)[usms]

  # Getting plant files (fplt) for a set of usm
  plant_files <- get_param_xml(file = usms_path,
                               param = "fplt",
                               select = "usm",
                               select_value = usms)
  plant_files <- plant_files[[usms_filename]]$fplt

  # Getting plant list:
  if (length(plant_files) == 2 * length(usms)) {
    # If plante dominance="1" and plante dominance="2" are declared,
    #put each one in a column:
    plant_list <- unlist(apply(matrix(plant_files,
                                      ncol = 2,
                                      byrow = TRUE),
                               MARGIN = 1, list),
                         recursive = FALSE)

  } else if (length(plant_files) == length(usms)) {
    # If plante dominance="2" is not declared, repeat plante dominance="1"
    # twice to get the same data structure:
    plant_list <- unlist(apply(matrix(c(plant_files, plant_files),
                                      ncol = 2,
                                      byrow = TRUE),
                               MARGIN = 1, list),
                         recursive = FALSE)

  } else {
    stop("plante dominance=\"2\" should always be declared in usms.xml",
         " even for sole crops (use null as values).")
  }
  names(plant_list) <- usms

  # Keeping only useful files names according to nb_plant data
  plant_xml <- try(
    mapply(function(x, y) {
      list(x[1:y])
    }, x = plant_list, y = nb_plant)
  )

  if (inherits(plant_xml, "try-error")) {
    plant_xml <- vector(mode = "list", length = length(usms))
    plant_xml[nb_plant == 1] <- "plant_1"
    plant_xml[nb_plant > 1] <- c("plant_1", "plant_2")
    names(plant_xml) <- usms
    if (verbose)
      cli::cli_alert_warning("Error reading usms file, using dummy plant names")
    return(plant_xml)
  }

  alert_msg <- paste0("plant folder not found in the workspace, please add ",
                      "{.code javastics_path} to use real plant names",
                      " from javaStics.")
  if (is.null(javastics_path)) {
    plt_path <- file.path(workspace, "plant")
    if (!all(dir.exists(plt_path))) {
      cli::cli_alert_warning(alert_msg)
      return(plant_xml)
    }
  } else {
    plt_path <- try(normalizePath(file.path(javastics_path, "plant")))
  }

  plant_names <-
    try(
      lapply(plant_xml, function(x) {
        unlist(get_param_xml(
          file = normalizePath(file.path(plt_path, x)),
          param = "codeplante"
        ))
      })
    )

  if (inherits(plant_names, "try-error")) {
    plant_names <- plant_xml
    if (verbose) cli::cli_alert_warning(paste0("Error reading plant names, ",
                                               "using plant file names for the",
                                               " output instead"))
  }

  return(plant_names)
}
