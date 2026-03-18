require(dplyr, quietly = TRUE)
require(tidyr, quietly = TRUE)


#' Compute standard error of AUC score, using its equivalence to the Wilcoxon statistic.
#' @family fbh method
#' @references Hanley and McNeil, The meaning and use of the area under a receiver
#' operating characteristic (ROC) curve. Radiology (1982) 43 (1) pp. 29-36.
#' @references Fogarty, Baker and Hudson, Case Studies in the use of ROC Curve Analysis
#' for Sensor-Based Estimates in Human Computer Interaction,
#' Proceedings of Graphics Interface (2005) pp. 129-136.
#' @param auc value of A' statistic (or AUC, or Area Under the Receiver operating
#' characteristic curve) (numeric).
#' @param n_p number of positive cases (integer).
#' @param n_n number of negative cases (integer).
#' @export
#' @examples
#' se_auc(0.75, 20, 200)
#' ## standard error decreases when data become more balanced over
#' ## positive/negative outcome class, holding sample size fixed
#' se_auc(0.75, 110, 110)
#' ## standard error increases when sample size shrinks
#' se_auc(0.75, 20, 20)
se_auc <- function(auc, n_p, n_n) {
    D_p = (n_p - 1) * ((auc/(2 - auc)) - auc^2)
    D_n = (n_n - 1) * ((2 * auc^2)/(1 + auc) - auc^2)
    SE_auc = sqrt((auc * (1 - auc) + D_p + D_n)/(n_p * n_n))
    return(SE_auc)
}

#' Apply z-test for difference between auc_1 and auc_2 using FBH method.
#' @family fbh method
#' @references Fogarty, Baker and Hudson, Case Studies in the use of ROC Curve Analysis
#' for Sensor-Based Estimates in Human Computer Interaction,
#' Proceedings of Graphics Interface (2005) pp. 129-136.
#'
#' @param auc_1 value of A' statistic (or AUC, or Area Under the Receiver operating
#' characteristic curve) for the first group (numeric).
#' @param auc_2 value of A' statistic (or AUC, or Area Under the Receiver operating
#' characteristic curve) for the second group (numeric).
#' @param n_p number of positive observations (needed for calculation of standard
#' error of Wilcoxon statistic) (numeric).
#' @param n_n number of negative observations (needed for calculation of standard
#' error of Wilcoxon statistic) (numeric).
#' @return numeric, single aggregated z-score of comparison A'_1 - A'_2.
#' @export
#'
#' @examples
#' ## Two models with identical AUC return z-score of zero
#' fbh_test(0.56, 0.56, 1000, 2500)
#' ## Compare two models; note that changing order changes sign of z-statistic
#' fbh_test(0.56, 0.59, 1000, 2500)
#' fbh_test(0.59, 0.56, 1000, 2500)
fbh_test <- function(auc_1, auc_2, n_p, n_n) {
    SE_auc_1 = se_auc(auc_1, n_p, n_n)
    SE_auc_2 = se_auc(auc_2, n_p, n_n)
    z = (auc_1 - auc_2)/sqrt(SE_auc_1^2 + SE_auc_2^2)
    return(z)
}

#' Compute aggregate z-score using Stouffer's method.
#'
#' @references Stouffer, S.A.; Suchman, E.A.; DeVinney, L.C.; Star, S.A.;
#' Williams, R.M. Jr. The American Soldier, Vol.1: Adjustment during Army Life (1949).
#' @param z_vec vector of z-scores (numeric).
#' @param ignore.na should NA values be ignored? defaults to TRUE.
#' @return numeric, Z-score using Stouffer's method aggregated over \code{z_vec}.
#' @export
#'
stouffer_z <- function(z_vec, ignore.na = TRUE) {
    S_z = sum(z_vec, na.rm = ignore.na)
    k = sum(!is.na(z_vec))
    stouffers_z = S_z/sqrt(k)
    return(stouffers_z)
}

#' Compare AUC values using the FBH method.
#'
#' Apply the FBH method to compare \code{outcome_col} by \code{compare_col}, averaging
#' over \code{time_col} (due to non-independence) and then over \code{over_col} by
#' using Stouffer's Method.
#' @family fbh method
#'
#' @references Fogarty, Baker and Hudson, Case Studies in the use of ROC Curve Analysis
#' for Sensor-Based Estimates in Human Computer Interaction,
#' Proceedings of Graphics Interface (2005) pp. 129-136.
#' @references Stouffer, S.A.; Suchman, E.A.; DeVinney, L.C.; Star, S.A.;
#' Williams, R.M. Jr. The American Soldier, Vol.1: Adjustment during Army Life (1949).
#'
#' @param df DataFrame containing \code{time_col}, \code{outcome_col},
#' \code{compare_col}, and \code{over_col}.
#' @param compare_values names of models to compare (character vector of length 2).
#' These should match exactly the names as they appear in compare_col.
#' @param filter_value (optional) keep only observations which contain
#' \code{filter_value} for \code{filter_col}.
#' @param time_col name of column in df representing time of observations
#' (z-scores are averaged over time_col within each model/dataset due to
#' non-independence). These can also be other dependent groupings, such as
#'  cross-validation folds.
#' @param outcome_col name of column in df representing outcome to compare; this should
#' be Area Under the Receiver Operating Characteristic or A' statistic (this method
#' applies specifically to AUC and not other metrics (i.e., sensitivity, precision, F1)..
#' @param compare_col name of column in df representing two conditions to compare
#' (should contain at least 2 unique values; these two values are specified as
#' \code{compare_values}).
#' @param over_col identifier for independent experiments, iterations, etc. over which
#' z-scores for models are to be compared (using Stouffer's Z).
#' @param n_col name of column in df with total number of observations in the sample
#' tested by each row.
#' @param n_p_col name of column in df with n_p, number of positive observations.
#' @param n_n_col name of column in df with n_n, number of negative observations.
#' @param filter_col (optional) name of column in df to filter observations on; keep only
#' observations which contain \code{filter_value} for \code{filter_col}.
#' @return numeric, overall z-score of comparison using the FBH method.
#' @export
#' @importFrom dplyr %>%
#'
#' @examples
#' ## load sample experiment data
#' data(sample_experiment_data)
#' ## compare VariantA of ModelA and ModelB
#' auc_compare(sample_experiment_data,
#'     compare_values = c('ModelA', 'ModelB'),
#'     filter_value = c('VariantA'),
#'     time_col = 'time',
#'     outcome_col = 'auc',
#'     compare_col = 'model_id',
#'     over_col = 'dataset',
#'     filter_col = 'model_variant')
#' ## compare VariantC of ModelA and ModelB
#' auc_compare(sample_experiment_data,
#'     compare_values = c('ModelA', 'ModelB'),
#'     filter_value = c('VariantC'),
#'     time_col = 'time',
#'     outcome_col = 'auc',
#'     compare_col = 'model_id',
#'     over_col = 'dataset',
#'     filter_col = 'model_variant')
#' ## compare ModelC, VariantA and VariantB
#' auc_compare(sample_experiment_data,
#'     compare_values = c('VariantA', 'VariantB'),
#'     filter_value = c('ModelC'),
#'     time_col = 'time',
#'     outcome_col = 'auc',
#'     compare_col = 'model_variant',
#'     over_col = 'dataset',
#'     filter_col = 'model_id')
auc_compare <- function(df, compare_values, filter_value, time_col = "time", outcome_col = "auc", compare_col = "model_id", over_col = "dataset",
    n_col = "n", n_p_col = "n_p", n_n_col = "n_n", filter_col = "model_variant") {
    filter_str = paste0(compare_col, " %in% c('", compare_values[1], "', '", compare_values[2], "') & ", filter_col, " == '", filter_value,
        "'")
    comp_df = dplyr::select(df, dplyr::one_of(c(time_col, outcome_col, compare_col, over_col, n_col, n_p_col, n_n_col, filter_col))) %>%
        dplyr::filter_(filter_str)
    n_datasets = length(unique(comp_df[, over_col]))
    dataset_z_scores = rep(NA, n_datasets)
    for (dataset_ix in seq_along(unique(comp_df[, over_col]))) {
        dataset = unique(comp_df[, over_col])[dataset_ix]
        message(paste0("fetching comparison results for models ", compare_values[1], ", ", compare_values[2], " in dataset ", dataset,
            " with filter value ", filter_value))
        dataset_df = comp_df[comp_df[, over_col] == dataset, ]
        n_weeks = length(unique(dataset_df[, time_col]))
        dataset_week_z_scores = rep(NA, n_weeks)
        for (week in unique(dataset_df[, time_col])) {
            dataset_week_df = dataset_df[dataset_df[, time_col] == week, ]
            # check whether both models have valid output for this week; otherwise skip
            if (length(unique(dataset_week_df[, compare_col])) != length(compare_values)) {
                msg = paste0("Missing performance data for at least one model in dataset ", dataset, " week ", week, "; skipping")
                message(msg)
                next
            }
            auc.1 = dataset_week_df[dataset_week_df[, compare_col] == compare_values[1], outcome_col]
            auc.2 = dataset_week_df[dataset_week_df[, compare_col] == compare_values[2], outcome_col]
            n.p = dataset_week_df[1, n_p_col]
            n.n = dataset_week_df[1, n_n_col]
            # conduct test and store z score in dataset_week_z_scores
            z = fbh_test(auc_1 = auc.1, auc_2 = auc.2, n_p = n.p, n_n = n.n)
            dataset_week_z_scores[week] <- z
        }
        # calculate average z-score across weeks and store
        dataset_z = mean(dataset_week_z_scores, na.rm = TRUE)
        dataset_z_scores[dataset_ix] <- dataset_z
    }
    # applying stouffer's Z method to dataset_z_scores
    overall_z = stouffer_z(dataset_z_scores)
    return(overall_z)
}

