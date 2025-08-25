#' SEMinR function to compare CV-PAT loss of two models
#'
#' `assess_cvpat_compare` conducts a CV-PAT significance test of loss between
#' two models.
#'
#' @param established_model The base seminr model for CV-PAT comparison.
#' @param alternative_model The alternate seminr model for CV-PAT comparison.
#' @param testtype Either "two.sided" (default) or "greater".
#' @param nboot The number of bootstrap subsamples to execute (defaults to 2000).
#' @param seed The seed for reproducibility (defaults to 123).
#' @param technique predict_EA or predict_DA (default).
#' @param noFolds Mumber of folds for k-fold cross validation.
#' @param reps Number of repetitions for cross validation.
#' @param cores Number of cores for parallelization.
#'
#' @return A matrix of the estimated loss and results of significance testing.
#'
#' @references Sharma, P. N., Liengaard, B. D., Hair, J. F., Sarstedt, M., &
#' Ringle, C. M. (2022). Predictive model assessment and selection in
#' composite-based modeling using PLS-SEM: extensions and guidelines for
#' using CVPAT. European journal of marketing, 57(6), 1662-1677.
#'
#' Liengaard, B. D., Sharma, P. N., Hult, G. T. M., Jensen, M. B.,
#' Sarstedt, M., Hair, J. F., & Ringle, C. M. (2021). Prediction: coveted,
#' yet forsaken? Introducing a cross‐validated predictive ability test in
#' partial least squares path modeling. Decision Sciences, 52(2), 362-392.
#'
#' @examples
#' # Load libraries
#'library(seminr)
#'
# # Create measurement model ----
# corp_rep_mm_ext <- constructs(
#  composite("QUAL", multi_items("qual_", 1:8), weights = mode_B),
#  composite("PERF", multi_items("perf_", 1:5), weights = mode_B),
#  composite("CSOR", multi_items("csor_", 1:5), weights = mode_B),
#  composite("ATTR", multi_items("attr_", 1:3), weights = mode_B),
#  composite("COMP", multi_items("comp_", 1:3)),
#  composite("LIKE", multi_items("like_", 1:3))
# )
#
# # Create structural model ----
#
# corp_rep_sm_ext <- relationships(
#  paths(from = c("QUAL", "PERF", "CSOR", "ATTR"), to = c("COMP", "LIKE"))
# )
# alt_sm <- relationships(
#  paths(from = c("QUAL", "PERF", "CSOR"), to = c("COMP", "LIKE"))
# )
#
# # Estimate the model ----
# established_model <- estimate_pls(
#  data = corp_rep_data,
#  measurement_model = corp_rep_mm_ext,
#  structural_model  = corp_rep_sm_ext,
#  missing = mean_replacement,
#  missing_value = "-99")
#
# alternative_model <- estimate_pls(
#  data = corp_rep_data,
#  measurement_model = corp_rep_mm_ext,
#  structural_model  = alt_sm,
#  missing = mean_replacement,
#  missing_value = "-99")
#
# # Function to compare the Loss of two models
# assess_cvpat_compare(established_model,
#                     alternative_model ,
#                     testtype = "two.sided",
#                     nboot = 20,
#                     seed = 123,
#                     technique = predict_DA,
#                     noFolds = 5,
#                     reps = 1,
#                     cores = 1)
#'
#' @export
assess_cvpat_compare <- function(established_model,
                                 alternative_model ,
                                 testtype = "two.sided",
                                 nboot = 2000,
                                 seed = 123,
                                 technique = predict_DA,
                                 noFolds = NULL,
                                 reps = NULL,
                                 cores = NULL) {
  # Abort if received a higher-order-model or moderated model
  if (!(is.null(established_model$hoc) & is.null(alternative_model$hoc))) {
    message("There is no published solution for applying PLSpredict to higher-order-models")
    return()
  }
  if (!(is.null(established_model$interaction) & is.null(alternative_model$interaction))) {
    message("There is no published solution for applying PLSpredict to moderated models")
    return()
  }
  set.seed(seed)

  # Rerieve the endogenous constructs
  endo_lvs1 <- seminr:::all_endogenous(established_model$smMatrix)
  endo_lvs2 <- seminr:::all_endogenous(alternative_model$smMatrix)


  # Retrieve the items of endogenous constructs
  endo_mvs1 <- unlist(lapply(endo_lvs1,
                             function(x) seminr:::items_of_construct(construct = x,
                                                                     model = established_model)))
  endo_mvs2 <- unlist(lapply(endo_lvs2,
                             function(x) seminr:::items_of_construct(construct = x,
                                                                     model = alternative_model)))
  # The models must have identical endogenous constructs and items to compare.
  if (!(all(endo_mvs1 %in% endo_mvs2) &
        all(endo_mvs2 %in% endo_mvs1) &
        all(endo_lvs1 %in% endo_lvs2) &
        all(endo_lvs2 %in% endo_lvs1))) {
    stop("CV-PAT can only be applied to models with identical endogenous constructs and measures")
  }
  # Calculate PLS predictions for each model
  pls_predict_model_one <- predict_pls(established_model,
                                       technique = technique,
                                       noFolds = noFolds,
                                       reps = reps,
                                       cores = cores)
  pls_predict_model_two <- predict_pls(alternative_model,
                                       technique = technique,
                                       noFolds = noFolds,
                                       reps = reps,
                                       cores = cores)

  pls_predict_error_one_item <- as.matrix(pls_predict_model_one$PLS_out_of_sample_residuals)
  colnames(pls_predict_error_one_item) <- endo_mvs1
  pls_predict_error_two_item <- as.matrix(pls_predict_model_two$PLS_out_of_sample_residuals)
  colnames(pls_predict_error_two_item) <- endo_mvs2

  PLS_predict_error_one <- pls_predict_error_one_item[,endo_mvs1,drop = F]
  PLS_predict_error_two <- pls_predict_error_two_item[,endo_mvs2,drop = F]

  ## Calculate LV losses for each PLS model
  ## model one
  LV_losses_PLS_one <- do.call("cbind", lapply(endo_lvs1,
                                               function(x) lv_loss(construct = x,
                                                                   model = established_model,
                                                                   error = PLS_predict_error_one)))
  ## model two
  LV_losses_PLS_two <- do.call("cbind", lapply(endo_lvs2,
                                               function(x) lv_loss(construct = x,
                                                                   model = alternative_model,
                                                                   error = PLS_predict_error_two)))

  # Name LVs
  colnames(LV_losses_PLS_one) <- endo_lvs1
  colnames(LV_losses_PLS_two) <- endo_lvs2

  # Calculate overall loss
  # for PLS model one (base)
  PLS_overall_one <- overall_loss(LV_losses_PLS_one)

  # for PLS model two (alt)
  PLS_overall_two <- overall_loss(LV_losses_PLS_two)

  # If there is 100% overlap in endogenous, then we direct compare
  if (identical(endo_lvs1, endo_lvs2)) {
    # CVPAT: PLS1 vs PLS2 overall
    PLS_v_PLS_overall <- bootstrap_cvpat(PLS_overall_one,
                                        PLS_overall_two,
                                        testtype = testtype,
                                        nboot = nboot)
    LV_cvpat <- cvpat_per_construct(loss_one = LV_losses_PLS_one,
                                    loss_two = LV_losses_PLS_two,
                                    testtype = testtype,
                                    nboot = nboot)
    mat_one <- cbind(colMeans(LV_losses_PLS_one),colMeans(LV_losses_PLS_two),
                     colMeans(LV_losses_PLS_one) - colMeans(LV_losses_PLS_two),
                     LV_cvpat[,-1])
  }
  # if there is less than 100% overlap in endogneous, we compare only the
  # relevant endogenous
  if (!identical(endo_lvs1, endo_lvs2)) {
    # CVPAT: PLS1 vs PLS2 overall
    overlap <- intersect(endo_lvs1, endo_lvs2)
    PLS_v_PLS_overall <- bootstrap_cvpat(PLS_overall_one,
                                         PLS_overall_two,
                                         testtype = testtype,
                                         nboot = nboot)

    LV_cvpat <- cvpat_per_construct(loss_one = LV_losses_PLS_one[,overlap,drop = F],
                                    loss_two = LV_losses_PLS_two[,overlap, drop = F],
                                    testtype = testtype,
                                    nboot = nboot)
    message("Not all endogenous vars co-occur in models 1 and 2. Only comparing overlap. ")
    mat_one <- cbind(colMeans(LV_losses_PLS_one)[overlap],colMeans(LV_losses_PLS_two)[overlap],
                     colMeans(LV_losses_PLS_one)[overlap] - colMeans(LV_losses_PLS_two)[overlap],
                     LV_cvpat[,-1,drop = F])

  }
  if (length(intersect(endo_lvs1, endo_lvs2) ) == 0) {
    return(list(results = "Cannot compare directly"))
  }



  mat_one <- rbind(mat_one, unlist(c(mean(PLS_overall_one),
                              mean(PLS_overall_two),
                              mean(PLS_overall_one) -  mean(PLS_overall_two),
                              PLS_v_PLS_overall)))

  mat_out <- matrix(as.numeric(unlist(mat_one)),nrow=nrow(mat_one))
  rownames(mat_out) <- rownames(mat_one)
  rownames(mat_out)[nrow(mat_one)] <- "Overall"
  mat_out <- mat_out[,c(1,2,3,6,7)]
  comment(mat_out) <- "CV-PAT as per Sharma et al. (2023).
  Both models under comparison have identical endoogenous constructs with identical measurement models.
  Purely exogenous constructs can be differ in regards to their relationships with both nomological
  partners and measurement indicators."
  colnames(mat_out) <- c("Base Model Loss", "Alt Model Loss", "Diff", "Boot T value", "Boot P Value"    )
  class(mat_out) <- append(class(mat_out), c("table_output"))
  return(mat_out)
}

## Sharma, P.N., Liengaard, B.D., Hair, J.F., Sarstedt, M., Ringle, C.M. (2023)
## "Predictive model assessment and selection in composite-based modeling using
## PLS-SEM: extensions and guidelines for using CVPAT", European Journal of
## Marketing, Vol. 57 No. 6, pp. 1662-1677.
## DOI: 10.1108/EJM-08-2020-0636
#' SEMinR function to compare CV-PAT loss of two models
#'
#' `assess_cvpat` conducts a single model CV-PAT assessment against item average
#' and linear model benchmarks.
#'
#' @param seminr_model The SEMinR model for CV-PAT comparison.
#' @param testtype Either "two.sided" (default) or "greater".
#' @param nboot The number of bootstrap subsamples to execute (defaults to 2000).
#' @param seed The seed for reproducibility (defaults to 123).
#' @param technique predict_EA or predict_DA (default).
#' @param noFolds Mumber of folds for k-fold cross validation.
#' @param reps Number of repetitions for cross validation.
#' @param cores Number of cores for parallelization.
#'
#' @return A matrix of the estimated loss and results of significance testing.
#'
#' @references Sharma, P. N., Liengaard, B. D., Hair, J. F., Sarstedt, M., &
#' Ringle, C. M. (2022). Predictive model assessment and selection in
#' composite-based modeling using PLS-SEM: extensions and guidelines for
#' using CVPAT. European journal of marketing, 57(6), 1662-1677.
#'
#' Liengaard, B. D., Sharma, P. N., Hult, G. T. M., Jensen, M. B.,
#' Sarstedt, M., Hair, J. F., & Ringle, C. M. (2021). Prediction: coveted,
#' yet forsaken? Introducing a cross‐validated predictive ability test in
#' partial least squares path modeling. Decision Sciences, 52(2), 362-392.
#'
#' @examples
#' # Load libraries
#' library(seminr)
#'
#' # Create measurement model ----
#' corp_rep_mm_ext <- constructs(
#'   composite("QUAL", multi_items("qual_", 1:8), weights = mode_B),
#'   composite("PERF", multi_items("perf_", 1:5), weights = mode_B),
#'   composite("CSOR", multi_items("csor_", 1:5), weights = mode_B),
#'   composite("ATTR", multi_items("attr_", 1:3), weights = mode_B),
#'   composite("COMP", multi_items("comp_", 1:3)),
#'   composite("LIKE", multi_items("like_", 1:3))
#' )
#'
#' # Create structural model ----
#' corp_rep_sm_ext <- relationships(
#'   paths(from = c("QUAL", "PERF", "CSOR", "ATTR"), to = c("COMP", "LIKE"))
#' )
#'
#' # Estimate the model ----
#' corp_rep_pls_model_ext <- estimate_pls(
#'   data = corp_rep_data,
#'   measurement_model = corp_rep_mm_ext,
#'   structural_model  = corp_rep_sm_ext,
#'   missing = mean_replacement,
#'   missing_value = "-99")
#'
#' # Assess the base model ----
#' assess_cvpat(seminr_model = corp_rep_pls_model_ext,
#'              testtype = "two.sided",
#'              nboot = 20,
#'              seed = 123,
#'              technique = predict_DA,
#'              noFolds = 5,
#'              reps = 1,
#'              cores = 1)
#'
#' @export
assess_cvpat <- function(seminr_model,
                         testtype = "two.sided",
                         nboot = 2000,
                         seed = 123,
                         technique = predict_DA,
                         noFolds = NULL,
                         reps = NULL,
                         cores = NULL) {

  set.seed(seed)
  # Abort if received a higher-order-model or moderated model
  if (!any(class(seminr_model) == "seminr_model")) {
    message("This function only works with SEMinR models. ")
    return()
  }
  if (!is.null(seminr_model$hoc)) {
    message("There is no published solution for applying PLSpredict to higher-order-models")
    return()
  }
  if (!is.null(seminr_model$interaction)) {
    message("There is no published solution for applying PLSpredict to moderated models")
    return()
  }
  # First we must calculate a IA model which is the "indicator average" model ----
  # we must identify endogenous latents and measures
  endo_lvs <- seminr:::all_endogenous(seminr_model$smMatrix)
  endo_mvs <- unlist(lapply(endo_lvs,
                            function(x) seminr:::items_of_construct(construct = x,
                                                                    model = seminr_model)))
  # Indicator average (IA) from the training model
  IA <- seminr_model$meanData[endo_mvs,drop = F]

  # Calculate IA predictive error
  if (length(endo_mvs) > 1) {
    IA_pred_error <- sweep(seminr_model$data[,endo_mvs,drop = F],2 ,IA)
  }
  if (length(endo_mvs) == 1) {
    IA_pred_error <- seminr_model$data[,endo_mvs,drop = F] - IA
  }

  # Calculate PLS and LM predictions
  pls_predict_model <- predict_pls(model = seminr_model,
                                   technique = technique,
                                   noFolds = noFolds,
                                   reps = reps,
                                   cores = cores)

  pls_predict_error <- as.matrix(pls_predict_model$PLS_out_of_sample_residuals)
  colnames(pls_predict_error) <- endo_mvs
  LM_predict_error <- as.matrix(pls_predict_model$lm_out_of_sample_residuals)
  colnames(LM_predict_error) <- endo_mvs

  PLS_predict_error <- pls_predict_error[,endo_mvs,drop = F]
  LM_predict_error <- LM_predict_error[,endo_mvs,drop = F]

  # Calculate LV-specific losses
  ## for IA model
  LV_losses_IA <- do.call("cbind", lapply(endo_lvs,
                                          function(x) lv_loss(construct = x,
                                                              model = seminr_model,
                                                              error = IA_pred_error)))
  ## for LM model
  LV_losses_LM <- do.call("cbind", lapply(endo_lvs,
                                          function(x) lv_loss(construct = x,
                                                              model = seminr_model,
                                                              error = LM_predict_error)))
  ## for PLS model
  LV_losses_PLS <- do.call("cbind", lapply(endo_lvs,
                                           function(x) lv_loss(construct = x,
                                                               model = seminr_model,
                                                               error = PLS_predict_error)))
  # Name LVs
  colnames(LV_losses_IA) <-  colnames(LV_losses_LM) <- colnames(LV_losses_PLS) <- endo_lvs

  # Calculate overall loss
  ## for IA model
  IA_overall <- overall_loss(LV_losses_IA)
  ## for LM model
  LM_overall <- overall_loss(LV_losses_LM)
  # for PLS model
  PLS_overall <- overall_loss(LV_losses_PLS)

  # CVPAT: PLS vs IA overall
  PLS_v_IA_overall <- bootstrap_cvpat(PLS_overall,
                                      IA_overall,
                                      testtype = testtype,
                                      nboot = nboot)

  # CVPAT: PLS vs LM overall
  PLS_v_LM_overall <- bootstrap_cvpat(LossM1 = PLS_overall,
                                      LossM2 = LM_overall,
                                      testtype = testtype,
                                      nboot = nboot)
  ia_cvpat <- cvpat_per_construct(loss_one = LV_losses_PLS,
                                  loss_two = LV_losses_IA,
                                  testtype = testtype,
                                  nboot = nboot)
  lm_cvpat <- cvpat_per_construct(loss_one = LV_losses_PLS,
                                  loss_two = LV_losses_LM,
                                  testtype = testtype,
                                  nboot = nboot)

  mat_one <- cbind(colMeans(LV_losses_PLS),colMeans(LV_losses_LM),
                   colMeans(LV_losses_PLS) - colMeans(LV_losses_LM),
                   lm_cvpat[,-1,drop = F])


  colnames(mat_one)[1:3] <- c("PLS Loss", "LM Loss", "Diff")
  mat_one <- rbind(mat_one, unlist(c(mean(PLS_overall),
                              mean(LM_overall),
                              mean(PLS_overall) -  mean(LM_overall),
                              PLS_v_LM_overall)))

  mat_one <- apply(mat_one,2,as.numeric)
  rownames(mat_one) <- c(endo_lvs, "Overall")
  mat_one <- mat_one[,c(1,2,3,6,7)]

  mat_two <- cbind(colMeans(LV_losses_PLS),colMeans(LV_losses_IA),
                   colMeans(LV_losses_PLS) - colMeans(LV_losses_IA),
                   ia_cvpat[,-1,drop = F])
  colnames(mat_two)[1:3] <- c("PLS Loss", "IA Loss", "Diff")

  mat_two <- rbind(mat_two, unlist(c(mean(PLS_overall),
                              mean(IA_overall),
                              mean(PLS_overall) -  mean(IA_overall),
                              PLS_v_IA_overall)))
  mat_two <- apply(mat_two,2,as.numeric)

  rownames(mat_two) <- c(endo_lvs, "Overall")
  mat_two <- mat_two[,c(1,2,3,6,7)]
  comment(mat_two) <-comment(mat_one) <- "CV-PAT as per Sharma et al. (2023)."
  class(mat_two) <- class(mat_one) <- append(class(mat_one), c("table_output"))

  return(list(CVPAT_compare_LM = mat_one,
              CVPAT_compare_IA = mat_two))

}

#function to apply bootstrap to every LV
cvpat_per_construct <- function(loss_one,
                                loss_two,
                                testtype = "two.sided",
                                nboot = 2000) {

  index <- colnames(loss_one)
  results <- as.data.frame(matrix(nrow = 0, ncol = 6))
  for (iter in index) {
    results <- rbind(results,c(iter,unlist(bootstrap_cvpat(loss_one[,iter],
                                                    loss_two[,iter],
                                                    testtype = testtype,
                                                    nboot = nboot))))
  }
  colnames(results) <- c("Construct","Std. T value", "Std. P value", "Boot T value", "Boot P Value", "Perc. P Value")
  return(results)
}

lv_loss <- function(construct, model, error) {

  if(length(dim((error))) > 1) {
    loss <- rowMeans(error[,seminr:::items_of_construct(construct = construct,
                                                        model = model),drop = F]^2)
  }
  if (length(dim((error))) == 1) {
    loss <- error[,seminr:::items_of_construct(construct = construct,
                                               model = model),drop = F]^2
  }
  return(loss)
}

overall_loss <- function(error) {
  if(length(dim((error))) > 1) {return(rowMeans(error))}
  return(error)
}

bootstrap_cvpat <- function(LossM1,
                            LossM2,
                            testtype = "two.sided",
                            nboot = 2000) {

  N <- length(LossM1)
  OrgTtest <- t.test(LossM2,
                     LossM1,
                     alternative = testtype,
                     paired=TRUE)$statistic

  # Originial average difference in losses
  OrgDbar <- mean(LossM2-LossM1)

  # Differences in loss functions under the null
  D_0 <- LossM2-LossM1-OrgDbar

  # Differences in loss functions
  D <- LossM2-LossM1

  #Allocating memory to bootrap
  nbootle <- matrix(0,ncol=2,nrow=length(D))
  BootDbar <- rep(0,nboot)
  m_losses<-cbind(LossM1,LossM2)
  tStat <- rep(0,nboot)
  for (b in 1:nboot) {
    nbootle <- m_losses[sample((1:length(D)), length(D), replace=TRUE),]
    tStat[b]<-t.test(nbootle[,2],nbootle[,1],mu=mean(D),alternative=testtype, paired=TRUE)$statistic
    BootDbar[b] <- mean(sample(D_0, length(D_0), replace=TRUE))
  }
  SorttStat<-sort(tStat, decreasing = FALSE)
  SortBootDbar<-sort(BootDbar, decreasing = FALSE)
  # Bootstrap variance on Dbar for t-test
  std<-sqrt(var(BootDbar))
  tstat_boot_Var<-OrgDbar/std
  # Calculating p-values
  if (testtype=="two.sided") {
    p.value_perc_Ttest<-(sum(SorttStat>abs(OrgTtest))+sum(SorttStat<=(-abs(OrgTtest))))/nboot
    p.value_perc_D<-(sum(SortBootDbar>abs(OrgDbar))+sum(SortBootDbar<=(-abs(OrgDbar))))/nboot
    p.value_var_ttest<-2*pt(-abs(tstat_boot_Var),(N-1), lower.tail = TRUE)
  }
  if (testtype=="greater") {
    p.value_perc_Ttest<-1-(head(which(SorttStat>OrgTtest),1)-1)/(nboot+1)
    p.value_perc_D<-1-(head(which(SortBootDbar>OrgDbar),1)-1)/(nboot+1)
    p.value_var_ttest<-pt(tstat_boot_Var,(N-1), lower.tail = FALSE)
    if (length(which(SorttStat>OrgTtest))==0){
      p.value_perc_Ttest=0
      p.value_perc_D=0
    }
  }
  # Load outputs 1
  Results1 <- data.frame("Std. T value"=OrgTtest, "Std. P value"=p.value_perc_Ttest,
                "Boot T value" = as.numeric(tstat_boot_Var), "Boot P Value" = as.numeric(p.value_var_ttest),
                "Perc. P Value" = as.numeric(p.value_perc_D))

  return(Results1)
}
