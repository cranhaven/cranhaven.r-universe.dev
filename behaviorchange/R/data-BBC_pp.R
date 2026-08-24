#' Subsets of Party Panel datasets
#'
#' These are subsets of Party Panel datasets. Party Panel is an annual
#' semi-panel determinant study among Dutch nightlife patrons, where every
#' year, the determinants of another nightlife-related risk behavior are
#' mapped.
#'
#' The behaviors of the Party Panel waves were:
#'
#' - 2015: Behaviors related to using highly dosed ecstasy pills
#' - 2016: Behaviors related to visiting nightlife first-aid facilities
#' - 2017: Behaviors related to hearing protection
#' - 2018: Behaviors related to flirting and boundary crossing
#' - 2019: Behaviors related to sleeping hygiene surrounding nightlife participation
#'
#' The full datasets are publicly available through the Open Science
#' Framework (https://osf.io/s4fmu/). Also see the GitLab
#' repositories (https://gitlab.com/partypanel) and the website
#' at https://partypanel.eu.
#'
#' @docType data
#' @aliases BBC_data BBC_pp15.1 BBC_pp16.1 BBC_pp17.1 BBC_pp18.1
#' @keywords data
#' @name partypanelData
#' @usage data(BBC_pp15.1)
#' @usage data(BBC_pp16.1)
#' @usage data(BBC_pp17.1)
#' @usage data(BBC_pp18.1)
#' @format For BBC_pp15.1, a `data.frame` with 123 columns and 829 rows.
#' For BBC_pp16.1, a `data.frame` with 63 columns and 1077 rows.
#' For BBC_pp17.1, a `data.frame` with 94 columns and 943 rows.
#' For BBC_pp18.1, a `data.frame` with 84 columns and 880 rows.
#' Note that many rows contain missing values; the columns and rows
#' were taken directly from the original Party Panel datasets, and
#' represent all participants that made it past a given behavior.
#' @examples data('BBC_pp17.1', package='behaviorchange');
#' behaviorchange::CIBERlite(data=BBC_pp17.1,
#'                           determinants=c("epw_attitude",
#'                                          "epw_perceivedNorm",
#'                                          "epw_pbc",
#'                                          "epw_habit"),
#'                           targets=c("epw_intention"));
c("BBC_pp15.1", "BBC_pp16.1", "BBC_pp17.1", "BBC_pp18.1");
