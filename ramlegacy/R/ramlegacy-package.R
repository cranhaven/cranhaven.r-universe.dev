#' ramlegacy: download, cache and read RAM Legacy Stock Assessment Database
#'
#' ramlegacy contains functions to download, cache and read in specified tables
#' from the excel version of the RAM Legacy Stock Assessment Database,
#' an online compilation of stock assessment results for commercially exploited
#' marine populations from around the world. More information about the database
#' can be found at <https://ramlegacy.org/>.
#' @keywords internal
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
#' @references Ricard, D., Minto, C., Jensen, O.P. and Baum, J.K. (2012)
#' Evaluating the knowledge base and status of commercially exploited marine
#' species with the RAM Legacy Stock Assessment Database.
#' Fish and Fisheries 13 (4) 380-398. <doi:10.1111/j.1467-2979.2011.00435.x>
#' @seealso \url{www.ramlegacy.org}
#' @seealso \url{www.github.com/ropensci/ramlegacy}
#' @seealso \url{www.github.com/ropensci/ramlegacy/issues}

"_PACKAGE"
