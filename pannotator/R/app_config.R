#' Access files in the current app
#'
#' NOTE: If you manually change your package name in the DESCRIPTION,
#' don't forget to change it here too, and in the config file.
#' For a safer name change mechanism, use the `golem::set_golem_name()` function.
#'
#' @param ... character vectors, specifying subdirectory and file(s)
#' within your package. The default, none, returns the root of the app.
#'
#' @noRd
app_sys <- function(...) {
  system.file(..., package = "pannotator")
}


#' Read App Config
#'
#' @param value Value to retrieve from the config file.
#' @param config GOLEM_CONFIG_ACTIVE value. If unset, R_CONFIG_ACTIVE.
#' If unset, "default".
#' @param use_parent Logical, scan the parent directory for config file.
#' @param file Location of the config file
#'
#' @noRd
get_golem_config <- function(
  value,
  config = Sys.getenv(
    "GOLEM_CONFIG_ACTIVE",
    Sys.getenv(
      "R_CONFIG_ACTIVE",
      "default"
    )
  ),
  use_parent = TRUE,
  # Modify this if your config file is somewhere else
  file = app_sys("golem-config.yml")
) {
  config::get(
    value = value,
    config = config,
    file = file,
    use_parent = use_parent
  )
}

# r for all the reactive values
r <- shiny::reactiveValues()
# track active annotations so we can remove them
r$active_annotations <- reactiveVal(value = NULL)
r$remove_leafletMap_item <- reactiveVal(value = NULL)
r$remove_leaflet360_item <- reactiveVal(value = NULL)
r$active_annotations_collapse <- NULL
r$refresh_user_config <- NULL
r$current_map_zoom <-  12
#r$current_image <- reactiveVal(value = NULL)


# Function to initialize the user config if it doesn't exist
#' @noRd
initialize_config <- function() {
  #config_path <- get_config_path()
  config_path <- normalizePath(file.path(tools::R_user_dir("pannotator", which = "config"), "default-project-config.yml"), , mustWork = FALSE)
  #print(config_path)
  data_path <- normalizePath(file.path(tools::R_user_dir("pannotator", which = "data")), mustWork = FALSE)

  if (!file.exists(config_path)) {
    # Create the directory if it doesn't exist
    dir.create(dirname(config_path), recursive = TRUE, showWarnings = TRUE)

    # Define the configuration settings as a list
    config <- list(
      showPopupAlerts = TRUE,
      appTheme = "cerulean",
      mapPanelWidth = 5,
      panoPanelWidth = 5,
      formPanelWidth = 2,
      mapPanelSource = "Esri.WorldImagery",
      mapAPIKey = "",
      mapIconColour = "green",
      mapMarkerColour = "white",
      mapPolygonStroke = TRUE,
      mapPolygonStrokeColour = "blue",
      mapPolygonStrokeWeight = 2,
      mapPolygonStrokeOpacity = 0.7,
      mapPolygonFill = TRUE,
      mapPolygonFillColour = "navy",
      mapPolygonFillOpacity = 0.3,
      pano360IconColour = "maroon",
      pano360MarkerColour = "white",
      pano360PolygonStroke = TRUE,
      pano360PolygonStrokeColour = "blue",
      pano360PolygonStrokeWeight = 1,
      pano360PolygonStrokeOpacity = 0.9,
      showPano360PolygonStrokeInCropExport = FALSE,
      pano360PolygonFill = TRUE,
      pano360PolygonFillColour = "purple",
      pano360PolygonFillOpacity = 0.1,
      showPano360PolygonFillInCropExport = TRUE,
      projectFolder = data_path,
      annotationsFile = "userAnnotations.rds",
      usernameLookupFile = "username_lookup.csv",
      exportFileFormat = "csv",
      lookup1Label = "Lookup_1",
      lookup1CsvFile = "lookup1.csv",
      lookup1HelpFile = "help1.pdf",
      lookup2Label = "Lookup_2",
      lookup2CsvFile = "lookup2.csv",
      lookup2HelpFile = "help2.pdf",
      lookup2Enabled = FALSE,
      lookup3Label = "Lookup_3",
      lookup3CsvFile = "lookup3.csv",
      lookup3HelpFile = "help3.pdf",
      lookup3Enabled = FALSE,
      lookup4Label = "Lookup_4",
      lookup4CsvFile = "lookup4.csv",
      lookup4HelpFile = "help4.pdf",
      lookup4Enabled = FALSE
    )
    # Write the list to a YAML file
    configr::write.config(config, config_path, write.type = "yaml")

  }

  if (!dir.exists(data_path)) {
    print("No data directory found, creating data directory.")
    dir.create(data_path, recursive = TRUE, showWarnings = TRUE)  # Create the data directory itself
  }

  create_lookup_files <- function(n = 4) {
    # Loop through 1 to n to create lookup files
    for (i in 1:n) {
      # Define the lookup file path
      lookup_file <- normalizePath(file.path(tools::R_user_dir("pannotator", which = "data"), paste0("lookup", i, ".csv")), mustWork = FALSE)

      # Check if the file exists, if not create it
      if (!file.exists(lookup_file)) {
        # Create the dataframe
        df <- data.frame(
          display = paste0("lookup ", i),
          value = paste0("lookup_", i),
          stringsAsFactors = FALSE
        )

        # Write the dataframe to a CSV file
        utils::write.csv(df, file = lookup_file, row.names = FALSE)
        cat("Created:", lookup_file, "\n")
      }
    }
  }

  # Call the function to create 4 lookup files
  create_lookup_files(4)

  lookup_file <- normalizePath(file.path(tools::R_user_dir("pannotator", which = "data"), paste0("username_lookup.csv")), mustWork = FALSE)

  # Check if the file exists, if not create it
  if (!file.exists(lookup_file)) {
    # Create the dataframe
    df <- data.frame(
      user_name = c("Guest Person", "Jane Doh", "Jack Smith"),
      value = c("Guest_Person", "Jane_Doh", "Jack_Smith"),
      stringsAsFactors = FALSE
    )

    # Write the dataframe to a CSV file
    utils::write.csv(df, file = lookup_file, row.names = FALSE)
    cat("Created:", lookup_file, "\n")
  }

  create_help_pdfs <- function(n = 4) {
    # Loop through 1 to n to create PDF files
    for (i in 1:n) {
      # Define the help file path
      help_file <- normalizePath(file.path(tools::R_user_dir("pannotator", which = "data"), paste0("help", i, ".pdf")), mustWork = FALSE)

      # Check if the file exists, if not create it
      if (!file.exists(help_file)) {
        # Create a new PDF file
        grDevices::pdf(file = help_file, width = 8, height = 11)  # Standard letter size

        # Plot the "HELP" text in the center of the page
        graphics::plot.new()
        graphics::text(0.5, 0.5, paste0("HELP ", i), cex = 3, font = 2)  # Centered and large

        # Close the PDF device to save the file
        grDevices::dev.off()

        cat("Created:", help_file, "\n")
      }
    }
  }

  # Call the function to create 4 help PDF files
  create_help_pdfs(4)

}


myEnv <- new.env(parent = emptyenv())

#' @noRd
globalVariables(c("imagefile", "feature_type", "."))

