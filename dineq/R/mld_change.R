#' @title Decomposition of the change of the mean log deviation
#'
#' @description Decomposes the change of the mean log deviation between two years/data sets
#' into population subgroups.
#'
#' @details The change of the mean log deviation can be decomposed into three components: inequality
#' changes between and within groups and changes in the relative sizes of the groups. The change
#' of between group inequality is measures by a change in the relative income of the subgroups.
#' The change of within group inequality by adding up all changes in mean log deviation within the
#' subgroups. And the contribution of changes in relative population size effects the change
#' on both the within and between group components. For the relative contributions those two are
#' added together.
#'
#' This method is introduced by Mookherjee and Shorrocks. It is an accurate approximation of
#' the exact decomposition.
#
#' It uses a logarithmic transformation of the values of the distribution. Therefore it
#' cannot handle negative or zero values. Those are excluded from the computation in this function.
#'
#' @param x1 a numeric vector for the first year/dataset containing at least non-negative elements.
#' @param z1 a factor for the first year/dataset containing the population subgroups.
#' @param weights1 an optional vector of weights of x for the first year/dataset
#' to be used in the computation of the decomposition. Should be NULL or a numeric vector.
#' @param x2 a numeric vector for the second year/dataset containing at least non-negative elements.
#' @param z2 a factor for the second year/dataset containing the population subgroups.
#' @param weights2 an optional vector of weights of x for the second year/dataset
#' to be used in the computation of the decomposition. Should be NULL or a numeric vector.
#'
#' @return a list with the results of the decomposition and the parts used for the
#' decomposition, containing the following components:
#'    \item{mld_data1}{the value of the mean log deviation index of x for the first year/dataset,
#'    and the decomposition into within-group and between-group inequality}
#'    \item{mld_data2}{the value of the mean log deviation index of x for the second year/dataset,
#'    and the decomposition into within-group and between-group inequality}
#'    \item{mld_difference}{the difference between the mean log deviation and the decomposition
#'    between the second and first year/dataset}
#'    \item{absolute_contributions_difference}{decomposition of the absolute change in inequality
#'    into: within group changes, group size changes (split into the effect of within and between
#'    group components) and between group changes.}
#'    \item{relative_contributions_difference}{decomposition of the change in inequality into
#'    relatieve contributions of: within group changes, group size changes and between group changes.
#'    Adds up to 100 percent (or -100 percent for negative change)}
#'    \item{note}{number of zero or negative observations in both datasets. The mean log deviation
#'    uses a logarithmic transformation of x. Therefore these observations are deleted from the
#'    analysis}
#'
#'
#' @seealso \code{\link{mld_decomp}}
#'
#' @examples
#' #Decomposition of the change in mean log deviation by level of eduction using
#' #Mexican Income data set
#' data(mex_inc_2008)
#'
#' change_education <- mld_change(x1=mex_inc_2008$income, z1=mex_inc_2008$education,
#' weights1=mex_inc_2008$factor, x2=mex_inc_2016$income, z2=mex_inc_2016$education,
#' weights2=mex_inc_2016$factor)
#'
#' #selection of the output: decomposition of the change into within- and between-group
#' #contribution and change in de size of groups (adds up to 100 percent)
#' change_education["relative_contributions_difference"]
#'
#' @references
#' Mookherjee, D. and A. Shorrocks (1982) A decomposition analysis of the trend in UK income
#' inequality, \emph{Economic Journal}, 92 (368), p. 886-902.
#'
#' Brewer M., and L. Wren-Lewis (2016) Accounting for Changes in Income Inequality:
#' Decomposition Analyses for the UK, 1978–2008. \emph{Oxford Bulletin of economics and
#' statistics}, 78 (3), p. 289-322,
#'
#' @export

mld_change <- function (x1, z1, weights1 = NULL, x2, z2, weights2 = NULL) {

  #mld decompositions both data points
  mld_decomp_data1 <- mld_decomp(x=x1, z=z1, weights=weights1)
  mld_decomp_data2 <- mld_decomp(x=x2, z=z2, weights=weights2)

  #differences and averages
  share_groups_average <- (unlist(mld_decomp_data1["share_groups"])+unlist(mld_decomp_data2["share_groups"]))/2
  share_groups_difference <- unlist(mld_decomp_data2["share_groups"]) - unlist(mld_decomp_data1["share_groups"])
  mld_group_average <- (unlist(mld_decomp_data1[["mld_group"]]["mld_group"])+unlist(mld_decomp_data2[["mld_group"]]["mld_group"]))/2
  mld_group_difference <-   unlist(mld_decomp_data2[["mld_group"]]["mld_group"])-unlist(mld_decomp_data1[["mld_group"]]["mld_group"])

  xMean_ratio1 <- unlist(mld_decomp_data1[["mean"]]["mean_group"])/unlist(mld_decomp_data1[["mean"]]["mean_total"])
  xMean_ratio2 <- unlist(mld_decomp_data2[["mean"]]["mean_group"])/unlist(mld_decomp_data2[["mean"]]["mean_total"])
  xMean_ratio_average <- (xMean_ratio1+xMean_ratio2)/2
  xMean_ratio_average_log <- (log(xMean_ratio1)+log(xMean_ratio2))/2
  share_income_groups_average <- (unlist(mld_decomp_data1["share_income_groups"])+unlist(mld_decomp_data2["share_income_groups"]))/2
  XMean_group_difference <-  log(unlist(mld_decomp_data2[["mean"]]["mean_group"]))-log(unlist(mld_decomp_data1[["mean"]]["mean_group"]))

  #contributions
  within_inequality <- sum(share_groups_average*mld_group_difference)
  within_share_groups <- sum(share_groups_difference*mld_group_average)
  between_share_groups <-  sum((xMean_ratio_average-xMean_ratio_average_log)*share_groups_difference)
  between_inequality <- sum((share_income_groups_average-share_groups_average)*XMean_group_difference)
  total_change <- within_inequality+within_share_groups+between_share_groups+between_inequality

  within_group_relative <- within_inequality/total_change
  group_size_relative <- (within_share_groups+between_share_groups)/total_change
  between_groups_relative <- between_inequality/total_change

  if (unlist(mld_decomp_data2[["mld_decomp"]]["mld_total"])-
      unlist(mld_decomp_data1[["mld_decomp"]]["mld_total"])<0){
    within_group_relative <- within_group_relative*-1
    group_size_relative <- group_size_relative*-1
    between_groups_relative <- between_groups_relative*-1
  }

  mld_data1=c(unlist(mld_decomp_data1[["mld_decomp"]]["mld_total"]),
              unlist(mld_decomp_data1[["mld_decomp"]]["mld_within"]),
              unlist(mld_decomp_data1[["mld_decomp"]]["mld_between"]))
  mld_data2=c(unlist(mld_decomp_data2[["mld_decomp"]]["mld_total"]),
              unlist(mld_decomp_data2[["mld_decomp"]]["mld_within"]),
              unlist(mld_decomp_data2[["mld_decomp"]]["mld_between"]))

  return(list(mld_data1=mld_data1,
              mld_data2=mld_data2,
              mld_difference=mld_data2-mld_data1,
              absolute_contributions_difference=c(within_inequality=within_inequality,within_share_groups=within_share_groups,
                                                  between_share_groups=between_share_groups,between_inequality=between_inequality),
              relative_contributions_difference=c(within_group_relative=within_group_relative,group_size_relative=group_size_relative,
                                                  between_groups_relative=between_groups_relative),
              notes=c(note_data1=unname(mld_decomp_data1["note"][1]),note_data2=unname(mld_decomp_data2["note"][1]))))
}
