#' Basin average observed and simulated daily precipitation and temperature
#'
#' A list of three data.tables with observed (\code{obs_ctrl}) and RCM simulated data for the control (\code{sim_ctrl}) and scenario (\code{sim_scen}) periods for Oslava basin (downto Cucice) in the Czech Republic. The basin average precipitation and temperature were obtained from griddedb observations and RCM simulation (EUR-11_CNRM-CERFACS-CNRM-CM5_rcp45_r1i1p1_CLMcom-CCLM4-8-17 simulation conducted within the CORDEX project).
#'
#' @format List of 3 data.tables:
#' \describe{
#'  \item{obs_ctrl}{observed data for the basin for a period 1981-01-01 -- 2005-21-31}
#'  \item{sim_ctrl}{simulated data for the basin for a period 1981-01-01 -- 2005-21-31}
#'  \item{sim_scen}{simulated data for the basin for a period 2070-01-01 -- 2099-21-31}
#' }
#' Each data.table contains 3 variables:
#' \describe{
#'   \item{DTM}{date}
#'   \item{PR}{precipitation, mm}
#'   \item{TAS}{temperature, degrees C}
#' }
"basin_PT"
