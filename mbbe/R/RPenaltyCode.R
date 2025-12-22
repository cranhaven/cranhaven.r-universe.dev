# code returns penalty (numeric) that is added to the BIC for selection of the model for a given bootstrap sample
# calculate the absolute fractional difference between simulated and observed Cmax, AUCinf and AUCtau
# each penalty is a linear function of that difference and the maximum (crash_value)
# returns the sum of the three penalties, or crash_value if fails
# function name MUST be MBBE_RPenaltyCode, this is hard code in the package
# file name is user defined, provided as an optionn in the json file.
MBBE_RPenaltyCode <- function(run_dir,
                       this_model,
                       this_samp){

  ngroups <-  4
  NCA_end_time <- 72
  crash_value <- 99999
  # data items are:
  # ID GROUP TRT DV IPRED
  fraction <- 0.1
  tryCatch(
    {
        NMoutFile <- file.path(run_dir, paste0("model", this_model), this_samp, "DATA.TXT")
        if (file.exists(NMoutFile)) {
          group_NCA_results <- data.frame(
            ID = as.integer(), treatment = as.integer(), period = as.integer(), sequence = as.integer(),
            Cmax = as.numeric(), AUCinf = as.numeric(), AUClast = as.numeric()
          )
          All_NCA_results <- data.frame(
            ID = as.integer(), treatment = as.integer(), period = as.integer(), sequence = as.integer(),
            Cmax = as.numeric(), AUCinf = as.numeric(), AUClast = as.numeric()
          )
          data <- read.table(NMoutFile, skip = 1, header = TRUE)
          colnames(data) <- c("ID","TIME","GROUP","TRT","DV","IPRED","EVID","OCC","SEQ")
          data <- data %>%
            dplyr::filter(EVID == 0) %>%
            dplyr::filter(DV > 0, TIME <= NCA_end_time)
          this_group <- 1
          penalty <-  0
          for (this_group in 1:ngroups) {
            group_data <- data %>%
              dplyr::filter(GROUP == this_group)
            # keep period for each subject, for this group
            period_seq <- group_data %>%
              dplyr::group_by(ID) %>%
              dplyr::distinct(ID, .keep_all = TRUE) %>%
              dplyr::select(ID, OCC, SEQ) %>%
              dplyr::arrange(ID)

            # insert conc=0 at time = 0, but remove if duplicated??
            zero_time <- group_data %>%
              dplyr::distinct(ID, .keep_all = TRUE)
            zero_time$TIME <- 0
            zero_time$DV <- 0
            group_data <- rbind(group_data, zero_time) %>%
              dplyr::arrange(ID, TIME)
            # observed
            conc_obj <- PKNCA::PKNCAconc(
              group_data,
              DV ~ TIME | ID
            )
            data_obj <- PKNCA::PKNCAdata(
              data.conc = conc_obj,
              intervals = data.frame(
                start = 0,
                end = NCA_end_time,
                aucinf.obs = TRUE,
                auclast = TRUE,
                cmax = TRUE
              )
            )
            suppressMessages(
              Observed_results_obj <- PKNCA::pk.nca(data_obj, verbose = FALSE)$result
            )
            Observed_AUCinf <- Observed_results_obj %>%
              dplyr::filter(PPTESTCD == "aucinf.obs") %>%
              dplyr::select(ID, PPORRES)
            Observed_AUCinf <- exp(mean(log(Observed_AUCinf$PPORRES)))
            Observed_AUClast <- Observed_results_obj %>%
              dplyr::filter(PPTESTCD == "auclast") %>%
              dplyr::select(ID, PPORRES)
            Observed_AUClast <- exp(mean(log(Observed_AUClast$PPORRES)))
            Observed_CMAX <- Observed_results_obj %>%
              dplyr::filter(PPTESTCD == "cmax") %>%
              dplyr::select(ID, PPORRES)
            Observed_CMAX <- exp(mean(log(Observed_CMAX$PPORRES)))

            # ipredicted
            conc_obj <- PKNCA::PKNCAconc(
              group_data,
              IPRED ~ TIME | ID
            )
            data_obj <- PKNCA::PKNCAdata(
              data.conc = conc_obj,
              intervals = data.frame(
                start = 0,
                end = NCA_end_time,
                aucinf.obs = TRUE,
                auclast = TRUE,
                cmax = TRUE
              )
            )
            suppressMessages(
              Pred_results_obj <- PKNCA::pk.nca(data_obj, verbose = FALSE)$result
            )
            Pred_AUCinf <- Pred_results_obj %>%
              dplyr::filter(PPTESTCD == "aucinf.obs") %>%
              dplyr::select(ID, PPORRES)
            Pred_AUCinf <- exp(mean(log(Pred_AUCinf$PPORRES)))
            penalty <- abs((Observed_AUCinf-Pred_AUCinf)/Observed_AUCinf)*fraction*crash_value/(ngroups)

            Pred_AUClast <- Pred_results_obj %>%
              dplyr::filter(PPTESTCD == "auclast") %>%
              dplyr::select(ID, PPORRES)
            Pred_AUClast <- exp(mean(log(Pred_AUClast$PPORRES)))
            penalty <- penalty + abs((Observed_AUClast-Pred_AUClast)/Observed_AUClast)*fraction*crash_value/(ngroups)
            Pred_CMAX <- Pred_results_obj %>%
              dplyr::filter(PPTESTCD == "cmax") %>%
              dplyr::select(ID, PPORRES)
            Pred_CMAX <- exp(mean(log(Pred_CMAX$PPORRES)))
            penalty <- penalty + abs((Observed_CMAX-Pred_CMAX)/Observed_CMAX)*fraction*crash_value/(ngroups)

          }
        }else{
          message("Error in user defined R code, DATA.TXT file not found for model ", this_model, " sample ", this_samp)
          return(crash_value)
        }
        return(penalty)
    },
    error = function(cond) {
      message("Error in user defined R code",  this_samp, "\n")
      message(cond)
      return(crash_value)
    }
  )
 }

