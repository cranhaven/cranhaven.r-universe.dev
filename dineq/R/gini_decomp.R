#' @title Decomposition of the Gini coefficient
#'
#' @description Decomposes the Gini coefficient into population subgroups. Distinction is made
#' by between and within group inequality and an overlap (interaction) term.
#'
#' @details The decomposition of the Gini coefficient by between and within group
#' inequality. In most cases there is an overlap of the distribution of both groups. Consequence
#' is that between and within group inequality doesn't add up to the total Gini coefficient. In
#' those cases there is an overlap term. Also referred to as interaction effect.
#'
#' Within group inequality is calculated by using the Gini coefficient for each sub group. Between
#' group inequality by using the gini coefficient of the average of both sub groups.
#'
#' @param x a numeric vector containing at least non-negative elements.
#' @param z a factor containing the population sub groups.
#' @param weights an optional vector of weights of x to be used in the computation of
#' the decomposition. Should be NULL or a numeric vector.
#'
#' @return a list with the results of the decomposition and the parts used for the
#' decomposition, containing the following components:
#'    \item{gini_decomp}{a list containing the decomposition: gini_total (value of the gini
#'    coefficient of x), gini_within (value of within-group inequality), gini_between (value
#'    of between-group inequality) and gini_overlap (value of overlap in inequality)}
#'    \item{gini_group}{a list containing gini_group (the gini coefficients of the different
#'    subgroups) and gini_group_contribution(the contribution of the subgroups to the total
#'    within-group inequality: adds up to gini_within)}
#'    \item{gini_decomp}{a list containing the means of x: mean_total (value of the mean of x of
#'    all subgroups combined) and mean_group (value of the mean of x of the individual subgroups)
#'    inequality) and gini_between (value of between-group inequality)}
#'    \item{share_groups}{the distribution of the subgroups z }
#'    \item{share_income_groups}{the distribution of vector x by subgroups z }
#'    \item{number_cases}{a list containing the number of cases in total, by subgroup (weighted
#'    and unweighted): n_unweighted (total number of unweighted x), n_weighted (total number of
#'    weighted x), n_group_unweighted (number of unweighted x by subgroup z), n_group_unweighted
#'    (number of weighted x by subgroup z)}
#'
#' @references
#' Mookherjee, D. and A. Shorrocks (1982) A decomposition analysis of the trend in UK income
#' inequality, \emph{Economic Journal}, 92 (368), p. 886-902.
#'
#' Cowell F. (2000) Measurement of Inequality. In Atkinson A. and Bourguignon F. (eds.) \emph{
#' Handbook of Income Distribution}. Amsterdam: Elsevier, p. 87-166.
#'
#' @seealso \code{\link{mld_decomp}}
#'
#' @examples
#' #Decomposition of the gini coefficient by level of education using Mexican Income data set
#' data(mex_inc_2008)
#' education_decomp <- gini_decomp(x=mex_inc_2008$income,z=mex_inc_2008$education,
#' weights=mex_inc_2008$factor)
#'
#' #complete output
#' education_decomp
#'
#' #Selected output: decomposition into between- and within-group inequality and overlap (interaction)
#' education_decomp["gini_decomp"]
#'
#' @export


gini_decomp <- function(x,z,weights=NULL) {

  if (is.null(weights)){
    weights <- rep(1, length(x))}
  if (!all(weights>=0, na.rm=TRUE)) stop("At least one weight is negative", call.=FALSE)
  if (all(weights == 0, na.rm=TRUE)) stop("All weights are zero", call.=FALSE)
  z <- factor(z)
  df <- data.frame(x = as.numeric(x), z = as.factor(z), w = as.numeric(weights))
  df <- df[stats::complete.cases(df), ,drop = FALSE]

  #totals
  n <- as.numeric(nrow(df))
  n_weighted <- sum(df[, "w"])
  dfSplit <- split(df[, c("x", "w")], df[, "z"])
  n_group <- table(df[, "z"])
  n_group_weighted <- sapply(dfSplit, function(df) sum(df[, "w"]), simplify = TRUE)


  #different parts of decomposition
  df[, "w"] <- df[, "w"]/sum(df[, "w"])
  xMean <- stats::weighted.mean(df[, "x"], df[, "w"])
  xMean_group <- sapply(dfSplit, function(df) stats::weighted.mean(df[,"x"], df[, "w"]), simplify = TRUE)
  share_group <- n_group_weighted/n_weighted
  share_group_income <- share_group * xMean_group/xMean
  weight_group <- share_group*share_group_income

  #mld and decompositon
  gini_total <- gini.wtd(df[,"x"], df[,"w"])
  gini_group <- sapply(dfSplit, function(df) gini.wtd(df[,"x"], df[,"w"]), simplify = TRUE)
  gini_group_contribution <- gini_group * weight_group
  gini_within <- sum(gini_group_contribution)
  gini_between <- gini.wtd(xMean_group, share_group)
  gini_overlap <- gini_total - gini_within - gini_between

  return(list(gini_decomp= list(gini_total= gini_total,gini_within=gini_within, gini_between=gini_between,
                                gini_overlap=gini_overlap),
              gini_group=list(gini_group=gini_group, gini_group_contribution=gini_group_contribution),
              mean= list(mean_total=xMean, mean_group=xMean_group),
              share_groups= share_group,
              share_income_groups=  share_group_income,
              number_cases= list(n_unweighted=n, n_weighted=n_weighted, n_group_unweighted=n_group,
                                 n_group_weighted=n_group_weighted)
  ))
}
