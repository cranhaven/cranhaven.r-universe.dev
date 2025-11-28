#' Subsets of the Party Panel 2015 dataset
#'
#' This is a subsets of the Party Panel 2015 dataset. Party Panel is an annual
#' semi-panel determinant study among Dutch nightlife patrons, where every
#' year, the determinants of another nightlife-related risk behavior are
#' mapped. In 2015, determinants were measured of behaviors related to using
#' highly dosed ecstasy pills.
#'
#' The full dataset is publicly available through the Open Science
#' Framework (https://osf.io/s4fmu/). Also see the GitLab
#' repository (https://gitlab.com/partypanel) and the website
#' at https://partypanel.eu.
#'
#' @docType data
#' @aliases pp15
#' @keywords data
#' @name partypanelData
#' @usage data(pp15)
#' @format A `data.frame` with 128 columns and 829 rows.
#' Note that many rows contain missing values; the columns and rows
#' were taken directly from the original Party Panel dataset, and
#' represent all participants that made it past a given behavior.
#' @examples data('pp15', package='rosetta');
#' rosetta::freq(pp15$gender);
c("pp15");

# pp15$gender_bi <-
#   factor(
#     ifelse(
#       as.numeric(pp15$gender) < 3,
#       pp15$gender,
#       NA),
#     levels=1:2,
#     labels=c("Female", "Male")
#   );
# table(pp15$gender, pp15$gender_bi)
#
# pp15$hasJob_bi <-
#   factor(
#     ifelse(
#       as.numeric(pp15$hasJob) > 1,
#       pp15$hasJob,
#       NA),
#     levels=2:3,
#     labels=c("No", "Yes")
#   );
# table(pp15$hasJob, pp15$hasJob_bi);
#
# pp15$weight_other <-
#   as.numeric(as.character(rosetta::pp15$weight_other));
# pp15$xtcUsePillPref <-
#   as.numeric(gsub(",", ".", as.character(rosetta::pp15$xtcUsePillPref)))
# pp15$xtcUsePillHigh <-
#   as.numeric(gsub(",", ".", as.character(rosetta::pp15$xtcUsePillHigh)))
#
# pp15$highDose_intention <-
#   as.numeric(gsub(",", ".", as.character(rosetta::pp15$xtcUsePillHigh)))
# pp15$highDose_attitude <-
#   as.numeric(gsub(",", ".", as.character(rosetta::pp15$highDose_attitude)))
# pp15$highDose_perceivedNorm <-
#   as.numeric(gsub(",", ".", as.character(rosetta::pp15$highDose_perceivedNorm)))
# pp15$highDose_pbc <-
#   as.numeric(gsub(",", ".", as.character(rosetta::pp15$highDose_pbc)))
#
# write.table(
#   pp15,
#   here::here("data", "pp15.csv"),
#   dec = ".",
#   sep = ";",
#   row.names = FALSE
# );
#
# pp15$highDose_OpenWhy <- as.character(pp15$highDose_OpenWhy);
# pp15$highDose_OpenWhyNot <- as.character(pp15$highDose_OpenWhyNot);
#
# haven::write_sav(
#   pp15,
#   here::here("data", "pp15.sav")
# );
#
