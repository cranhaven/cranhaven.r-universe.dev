#' Export dataframe to 'Vorteks Visualization Environment (VVE)'
#'
#' A .vvedf file will be written to the current working directory.
#' In windows, the file will be executed in an attempt to launch 'Vorteks Visualization Environment (VVE)'
#' @export
#' @param df dataframe variable
#' @param filename name of exported file variable
#' @return (No return value, called to perform export process)
#' @importFrom utils write.csv
ExportDataframeToVVE <- function(df, filename = "dataframe_export") {
  ExportDataframeToVorteks(df, ".vvedf", filename)
}

#' Export dataframe to 'Vorteks Data Manager (VDM)'
#'
#' A .vdmdf file will be written to the current working directory.
#' In windows, the file will be executed in an attempt to launch 'Vorteks Data Manager (VDM)'
#' @export
#' @param df dataframe variable
#' @param filename name of exported file variable
#' @return (No return value, called to perform export process)
#' @importFrom utils write.csv
ExportDataframeToVDM <- function(df, filename = "dataframe_export") {
  ExportDataframeToVorteks(df, ".vdmdf", filename)
}

#' Export dataframe to 'Vorteks Processing Environment (VPE)'
#'
#' A .vpedf file will be written to the current working directory.
#' In windows, the file will be executed in an attempt to launch 'Vorteks Processing Environment (VPE)'
#' @export
#' @param df dataframe variable
#' @param filename name of exported file variable
#' @return (No return value, called to perform export process)
#' @importFrom utils write.csv
ExportDataframeToVPE <- function(df, filename = "dataframe_export") {
  ExportDataframeToVorteks(df, ".vpedf", filename)
}

#' Export dataframe to 'Vorteks'
#'
#' 'ExportDataframeToVorteks()' will write a file with specified extension
#' to the current working directory.
#' In windows, the file will be executed in an attempt to launch 'Vorteks' software.
#' @param df dataframe variable
#' @param extension must be one of supported extensions (.vvedf, .vpedf, .vdmdf)
#' @param filename name of exported file variable
#' @return (No return value, called to perform export process)
#' @importFrom utils write.csv
ExportDataframeToVorteks <- function(df, extension, filename = "dataframe_export") {

  # Validate dataframe arg
  if(missing(df) || is.null(df) || !is.data.frame(df))
    stop("Invalid dataframe")

  # Validate filename arg
  if(is.null(filename) || !(is.character(filename) && length(filename) == 1) || nchar(filename) < 1)
    stop("Invalid filename")

  check <- tryCatch({
    #create filename with vorteks dataframe extension
    path <- paste(filename, extension, sep = "")

    # Write dataframe to current working directory
    write.csv(df, path, row.names = FALSE)

    # open saved file - Should open Vorteks if appropriate version is installed via file association
    vorteks_check <- try(silent = TRUE, {
      shell.exec(path)
    })

    print(paste("Vorteks Export:", path))
  },
  warning = function(war)
  {
    print(paste("Vorteks Export WARNING:", war))
  },
  error = function(err)
  {
    print(paste("Vorteks Export ERROR:", err))
  },
  finally = function(f)
  {
  })
}

