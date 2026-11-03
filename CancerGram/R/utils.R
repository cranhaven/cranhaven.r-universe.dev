count_longest <- function(x) {
  splitted_x <- strsplit(x = paste0(as.numeric(x > 0.5), collapse = ""),
                         split = "0")[[1]]
  len <- unname(sapply(splitted_x, nchar))
  if (length(len[len > 0]) == 0) {
    0 } else {
      len[len > 0]
    }
}


#' @importFrom stats median
calculate_statistics <- function(pred) {
  data.frame(fraction_true = mean(pred > 0.5),
             pred_mean = mean(pred),
             pred_median = median(pred),
             n_peptide = length(pred),
             n_pos = sum(pred > 0.5),
             pred_min = min(pred),
             pred_max = max(pred), 
             longest_pos = max(count_longest(pred)),
             n_pos_5 = sum(count_longest(pred) >= 5),
             frac_0_0.2 = sum(pred <= 0.2)/length(pred),
             frac_0.2_0.4 = sum(pred > 0.2 & pred <= 0.4)/length(pred),
             frac_0.4_0.6 = sum(pred > 0.4 & pred <= 0.6)/length(pred),
             frac_0.6_0.8 = sum(pred > 0.6 & pred <= 0.8)/length(pred),
             frac_0.8_1 = sum(pred > 0.8 & pred <= 1)/length(pred)) 
}


calculate_statistics_single <- function(mer_preds, group) {

  preds <- mer_preds[, group]
  df <- calculate_statistics(preds)
  nondescriptive_names <- colnames(df)
  colnames(df)[match(colnames(df), table = nondescriptive_names, nomatch = 0) > 0] <- 
    paste0(group, "_", colnames(df)[match(colnames(df), table = nondescriptive_names, nomatch = 0) > 0])
  df
}



calculate_statistics_mc <- function(mer_preds, groups) {
  res <- do.call(cbind,
                 lapply(groups, function(i) {
    calculate_statistics_single(mer_preds, i)
  }))
  stat <- subset(res[,!duplicated(colnames(res))], select = -c(amp_n_peptide, neg_n_peptide))
  stat
} 

#' @importFrom dplyr mutate case_when
get_decision <- function(preds) {
  mutate(preds, decision = case_when(acp > amp & acp > neg  ~ "ACP",
                                     amp > acp & amp > neg ~ "AMP",
                                     neg > amp & neg > acp ~ "neg"))
}


#' @importFrom stringi stri_count
find_ngrams <- function(seq, decoded_ngrams) {
  
  end_pos <- 5L:length(seq)
  start_pos <- end_pos - 4
  
  res <- binarize(do.call(rbind, lapply(1L:length(end_pos), function(ith_mer_id) {
    five_mer <- paste0(seq[start_pos[ith_mer_id]:end_pos[ith_mer_id]], collapse = "")
    stri_count(five_mer, regex = decoded_ngrams)
  })))
  
  res
}

#' Install CancerGramModel package containing model for ACP prediction
#' 
#' Installs CancerGramModel package containing model required for prediction
#' of anticancer peptides. Due to large size of our model and file size 
#' limit on CRAN, it needs to be stored in the external repository.
#' See readme for more information or in case of installation problems. 
#' 
#' @export
#' @importFrom devtools install_github
install_CancerGramModel <- function() {
  install_github("BioGenies/CancerGramModel")
}

is_CancerGramModel_installed <- function() {
  check_CancerGramModel <- try(find.package("CancerGramModel"), silent = TRUE)
  !inherits(check_CancerGramModel, "try-error")
}

#' @importFrom utils menu
require_CancerGramModel <- function() {
  if(interactive()) {
    if (!is_CancerGramModel_installed() && !getOption("CancerGram_suppress_prompt")) {
      response <- menu(c("yes", "no", "no and don't ask me anymore"), 
                       title = "To be able to use CancerGram properly, you should have installed 'CancerGramModel' package available via GitHub. Install?")
      switch (response, 
              tryCatch(install_CancerGramModel(),
                       finally = if (!is_CancerGramModel_installed()) 
                         warning("There was an error during an attempt to install 'CancerGramModel' package.", call. = FALSE)),
              message("You cannot access full functionality of this package without having installed 'CancerGramModel'. You can do it manually by calling 'devtools::install_github('BioGenies/CancerGramModel')'", call. = FALSE),
              {options(CancerGram_suppress_prompt = TRUE)
                cat("Ok, but you cannot access full functionality of this package without having installed 'CancerGramModel'")},
              message("You cannot access full functionality of this package without having installed 'CancerGramModel'. You can do it manually by calling 'devtools::install_github('BioGenies/CancerGramModel')'", call. = FALSE)
      )
    } 
  } else {
    message("To be able to use CancerGram properly, you should have installed 'CancerGramModel' with 'devtools::install_github('BioGenies/CancerGramModel'.")
  }
  
}
