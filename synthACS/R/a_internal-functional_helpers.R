
# @description standard checks to make sure calls to library(acs) will work
# @param endyear An integer, indicating the latest year of the data in the survey.
# @param span An integer in \code{c(1,3,5)} indicating the span of the desired data.
# @param geography a valid \code{geo.set} object specifying the census geography or 
# geographies to be fetched.
check_geo_inputs <- function(endyear, span, geography) {
  if (! span %in% c(1,3,5)) stop("The ACS API only supports data spans of 1, 3, and 5 years.")
  if (endyear %% 1 != 0 | endyear < 2009) stop("endyear must be an integer >= 2009 (when ACS data begins).")
  # other span/endyear issues handled by library(acs)
  if (!acs::is.geo.set(geography)) stop("Supply valid geography -- class 'geo.set'.")
  
  # else okay; return
}

# helper functions to coerce all non-probability vectors to factors
# Need to hard code factor levels for all vars which have dependencies
fact_coerce <- function(df, idx, nms) {
    for (i in idx) {
        if (nms[i] == 'age') {
            df$age <- factor(df$age, levels= c("under15", "15_17", "18_24", "25_29", "30_34", "35_39", 
                             "40_44", "45_49", "50_54", "55_59", "60_64", "65_69", "70_74", "75_79", 
                             "80_84", "85up"))
        }
        else if (nms[i] == 'gender') {df$gender <- factor(df$gender, levels= c("Male", "Female"))}
        else if (nms[i] == 'marital_status') {
            df$marital_status <- factor(
                df$marital_status, levels= c("divorced", "mar_apart", "married", "never_mar", "widowed")
            )
        }
        else if (nms[i] == 'edu_attain') {
            df$edu_attain <- factor(
                df$edu_attain, levels= c("lt_hs", "some_hs", "hs_grad", "some_col", "assoc_dec", "ba_deg", "grad_deg")
            )
        }
        else if (nms[i] == 'emp_status') {
            df$emp_status <- factor(df$emp_status, levels= c("not_in_labor_force", "employed", "unemployed"))
        }
        else if (nms[i] == 'nativity') {
            df$nativity <- factor(
                df$nativity, levels= c("born_other_state", "born_out_us", "born_state_residence", "foreigner")
            )
        }
        else if (nms[i] == 'pov_status') {
            df$pov_status <- factor(df$pov_status, levels= c("below_pov_level", "at_above_pov_level"))
        }
        else if (!is.factor(df[,i])) {df[,i] <- factor(df[,i])}
    }
    return(df)
}

factor_return <- function(df, prob_name) {
  fact_ind <- which(names(df) != prob_name)
  nms <- names(df)
  df <- fact_coerce(df, idx= fact_ind, nms= nms)
  rownames(df) <- NULL
  return(df[stats::complete.cases(df) & df[prob_name] > 0,])
}

# helper function during return phase of all pull_* functions to alphabetize all datasets
# Needed for consistency as library(acs) does not return consistent dataset ordering across
# versions
geo_alphabetize <- function(geo, est, se) {
  if (is.data.frame(est)) {
    est <- geo_alphabetize_df(est)
    se <- geo_alphabetize_df(se)
  } else {
    est <- lapply(est, geo_alphabetize_df)
    se <- lapply(se, geo_alphabetize_df)
  }
  geo <- geo[order(geo$NAME), , drop= FALSE] 
  return(list(geo= geo, est= est, se= se))
}

geo_alphabetize_df <- function(df) {
    return(df[order(rownames(df)), , drop= FALSE])
}

