#' Get random intercepts for subjects and items
#'
#' Get error terms from an existing data table.
#'
#' @param data the existing tbl
#' @param dv the column name or index containing the DV
#' @param sub_id the column name or index for the subject IDs
#' @param item_id the column name or index for the item IDs
#' @param formula the formula to run in lmer (defaults to null model dv ~ 1 + (1|sub_id) + (1|item_id))
#' 
#' @return a list of parameters
#' @examples
#' des <- check_mixed_design(fr4, "rating", "rater_id", "face_id")
#' str(des[1:4])
#' @export
check_mixed_design <- function(data, dv = 1, sub_id = 2, item_id = 3, formula = NULL) {
  # error checking -------------------------------------------------------------
  if (is.matrix(data)) {
    data = as.data.frame(data)
  } else if (!is.data.frame(data)) {
    stop("data must be a data frame or matrix")
  }
  
  # get column names if specified by index
  if (is.numeric(dv)) dv <- names(data)[dv]
  if (is.numeric(sub_id)) sub_id <- names(data)[sub_id]
  if (is.numeric(item_id)) item_id <- names(data)[item_id]
  
  if (is.null(formula)) {
    formula <- paste0(dv, " ~ 1 + (1 | ", sub_id, ") + (1 | ", item_id, ")")
  }
  
  lmer_formula <- stats::as.formula(formula)
  mod <- lme4::lmer(lmer_formula, data = data)
  grand_i <- lme4::fixef(mod)[["(Intercept)"]]
  
  sds <- lme4::VarCorr(mod) %>% as.data.frame()
  
  sub_sd <- sds[which((sds$grp==sub_id & 
                       sds$var1== "(Intercept)") & 
                       is.na(sds$var2)), "sdcor"]
  item_sd <- sds[which((sds$grp==item_id & 
                        sds$var1== "(Intercept)") & 
                        is.na(sds$var2)), "sdcor"]
  error_sd <- sds[which(sds$grp=="Residual"), "sdcor"]
  
  random_effects <- lme4::ranef(mod)
  
  list(
    grand_i = grand_i,
    sub_sd = sub_sd,
    item_sd = item_sd,
    error_sd = error_sd,
    random_effects = random_effects
  )
}
