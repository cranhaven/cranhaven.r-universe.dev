#' @name load_ramlegacy
#' @family ramlegacy functions
#' @title Read-in dataframes from downloaded RAM Legacy Database
#' @description Returns a list of specific dataframes or a list of all the
#'  dataframes present in the requested version of the database.
#' @param version A character vector of length 1 specifying the version number
#' of the database. As of writing this package, the available versions are
#' "1.0", "2.0", "2.5", "3.0", "4.3", "4.40", "4.41" and "4.44". If version
#' argument is not specified then it defaults to newest version (v4.44).
#' @param tables A character vector specifying the names of particular
#' dataframes to load from a specified version. If not specified then
#' a list containing all the dataframes within the requested version
#' is returned. For a description of the different tables present
#' in the database please see below.
#' @param ram_path path to the local directory where the specified version of
#' the RAM Legacy Stock Excel Assessment Database was downloaded. By default
#' this path is set to within the rappdirs directory and can be viewed using
#' calling the function \code{\link{ram_dir}} and specifying the version number
#' inside the function call. Although this is not the \strong{recommended}
#' approach \code{load_ramlegacy} supports reading in the database's
#' dataframes from a user-specified path provided that the database is present
#' at the specified path as an rds object.
#' @export
#'
#' @examples
#' \donttest{
#' \dontshow{Sys.setenv(RAM_HOME = tempfile())}
#' # first download version 4.44 of the database
#' download_ramlegacy(version = "4.44")
#'
#' # get a list containing area and bioparams tables
#' # from version 4.44 database
#' load_ramlegacy(version = "4.44", tables = c("area", "bioparams"))
#' }
#' @section Description of the dataframes present in the database:
#'
#' \itemize{
#' \item metadata: Table with summarized metadata (only available in newer
#'  versions starting from v4.40)
#' \item stock: This stores the stock database table
#' \item assessment: This stores the assessment database table
#' \item taxonomy: This stores the taxonomy database table
#' \item management: This stores the management database table
#' \item assessor: This stores the assessor database table
#' \item assessmetod: This stores the assessmetod database table
#' \item area: This stores the area database table
#' \item biometrics: This stores the biometrics database table
#' \item tsmetrics: This stores the tsmetrics database table
#' \item timeseries: The time series data is a matrix object with the following
#' headers/columns: (1) assessid (2) stockid (3) stocklong (4) tsid (5) tsyear
#' (6) tsvalue
#' \item bioparams: The parameter data is a matrix object with the following
#' headers/columns: (1) assessid (2) stockid (3) stocklong (4) bioid (5) biovalue
#' (6) bioyear (7) bionotes
#' \item timeseries_values_views: This stores the timeseries values with timeseries
#' type along the columns and stocks along the rows
#' \item timeseries_units_views: This stores the timeseries values with timeseries
#' type along the columns and stocks along the rows
#' \item timeseries_ids_views: This stores the timeseries IDs with timeseries type
#'  along the columns and stocks along the rows
#' \item timeseries_assessments_views: This stores the timeseries assessments with
#' timeseries type along the columns and stocks along the rows
#' \item timeseries_notes_views: This stores the timeseries notes with timeseries
#' type along the columns and stocks along the rows
#' \item timeseries_sources_views: This stores the timeseries sources with timeseries
#' type along the columns and stocks along the rows
#' \item timeseries_years_views: This stores the timeseries years with timeseries
#' type along the columns and stocks along the rows
#' \item bioparams_values_views: This stores the reference point values, with
#' reference point type along the columns and stocks along the rows
#' \item bioparams_units_views: This stores the reference point units, with
#' reference point type along the columns and stocks along the rows
#' \item bioparams_ids_views: This stores the reference point IDs, with reference
#' point type along the columns and stocks along the rows
#' \item bioparams_assessments_views: This stores the reference point assessments,
#' with reference point type along the columns and stocks along the rows
#' \item bioparams_sources_views: This stores the reference point sources, with
#' reference point type along the columns and stocks along the rows
#' \item bioparams_notes_views: This stores the reference point notes, with
#' reference point type along the columns and stocks along the rows
#' }
#'
#' @section Newer versions (v4.40 onwards) also contains tables of individual most-used time series:
#' \itemize{
#' \item tb.data: Total Biomass
#' \item ssb.data: Spawning Stock Biomass
#' \item tn.data: Total Abundance
#' \item r.data: Recruits
#' \item tc.data: Total Catch
#' \item tl.data: Total Landings
#' \item recc.data: Recreational Catch
#' \item f.data: Fishing Mortality
#' \item er.data: Exploitation Rate
#' \item divtb.data: TB/TBmsy
#' \item divssb.data: SSB/SSBmsy
#' \item ivf.data: F/Fmsy
#' \item diver.data: ER/ERmsy
#' \item divbpref.data: B/Bmsypref
#' \item divupref.data: U/Umsypref
#' \item tbbest.data: TBbest
#' \item tcbest.data: TCbest
#' \item erbest.data: ERbest
#' \item divtb.mgt.data: TB/TBmgt
#' \item divssb.mgt.data: SSB/SSBmgt
#' \item divf.mgt.data: F/Fmgt
#' \item diver.mgt.data: ER/ERmgt
#' \item divbpref.mgt.data: B/Bmgtpref
#' \item divupref.mgt.data: U/Umgtpref
#' \item cpair.data: Cpair
#' \item tac.data: TAC
#' \item cadv.data: Cadvised
#' \item survb.data: survB
#' \item cpue.data: CPUE
#' \item effort.data: EFFORT
#' }

load_ramlegacy <- function(version = NULL, tables = NULL, ram_path = NULL) {
  ram_url <- "https://doi.org/10.5281/zenodo.2542918"

  # convert names to lowercase

  if (!is.null(version)) {
    # ensure that the version is properly formatted
    version <- fmt_version(version)
    check_version_arg(version)
  } else {
    version <- find_latest(ram_url)
  }

  if (is.null(ram_path)) {
    vers_path <- file.path(ram_dir(), version)
    if (version < "4.40") {
      rds_path <- file.path(vers_path, paste0("v", version, ".rds"))
    } else {
      rds_path <- file.path(vers_path, paste0("RLSADB v", version))
      rds_path <- file.path(rds_path, "DB Files With Assessment Data")
      rds_path <- file.path(rds_path, paste0("v", version, ".rds"))
    }
  } else {
    check_path(ram_path)
    rds_path <- ram_path
  }
  # make sure version is present
  if (!file.exists(rds_path)) {
    stop(paste0("Version ", version, " not found locally."))
  }

  list_dataframes <- readRDS(rds_path)

  if (!is.null(tables)) {
    tables <- tolower(tables)
    listToReturn <- vector("list", length(tables))
    for (i in seq_len(length(tables))) {
      #  construct df name
      df_name <- tables[i]
      if (grepl("\\.data", tables[i])) {
        most.used.time.series <- list_dataframes[[26]]
        listToReturn[[i]] <- most.used.time.series[[df_name]]
      } else {
        listToReturn[[i]] <- list_dataframes[[df_name]]
      }
    }

    names(listToReturn) <- tables
    class(listToReturn) <- c("ramlist", class(listToReturn))
    return(listToReturn)
  } else {
    class(list_dataframes) <- c("ramlist", class(list_dataframes))
    return(list_dataframes)
  }
}
