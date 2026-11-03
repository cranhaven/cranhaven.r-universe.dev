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
             n_pos_10 = sum(count_longest(pred) >= 10),
             frac_0_0.2 = sum(pred <= 0.2)/length(pred),
             frac_0.2_0.4 = sum(pred > 0.2 & pred <= 0.4)/length(pred),
             frac_0.4_0.6 = sum(pred > 0.4 & pred <= 0.6)/length(pred),
             frac_0.6_0.8 = sum(pred > 0.6 & pred <= 0.8)/length(pred),
             frac_0.8_1 = sum(pred > 0.8 & pred <= 1)/length(pred)) 
}

find_ngrams <- function(seq, decoded_ngrams) {
  
  end_pos <- 10L:length(seq)
  start_pos <- end_pos - 9
  
  res <- binarize(do.call(rbind, lapply(1L:length(end_pos), function(ith_mer_id) {
    ten_mer <- paste0(seq[start_pos[ith_mer_id]:end_pos[ith_mer_id]], collapse = "")
    stri_count(ten_mer, regex = decoded_ngrams)
  })))
  
  res
}

#' Install AmpGramModel package containing model for AMP prediction
#' 
#' Installs AmpGramModel package containing model required for prediction
#' of antimicrobial peptides. Due to large size of our model and file size 
#' limit on CRAN, it needs to be stored in the external repository.
#' See readme for more information or in case of installation problems. 
#' 
#' @export
#' @importFrom devtools install_github
install_AmpGramModel <- function() {
  install_github("michbur/AmpGramModel")
}

is_AmpGramModel_installed <- function() {
  check_AmpGramModel <- try(find.package("AmpGramModel"), silent = TRUE)
  !inherits(check_AmpGramModel, "try-error")
}

require_AmpGramModel <- function() {
  if(interactive()) {
    if (!is_AmpGramModel_installed() && !getOption("AmpGram_suppress_prompt")) {
      response <- menu(c("yes", "no", "no and don't ask me anymore"), 
                       title = "To be able to use AmpGram properly, you should have installed 'AmpGramModel' package available via GitHub. Install?")
      switch (response, 
              tryCatch(install_AmpGramModel(),
                       finally = if (!is_AmpGramModel_installed()) 
                         warning("There was an error during an attempt to install 'AmpGramModel' package.", call. = FALSE)),
              message("You cannot access full functionality of this package without having installed 'AmpGramModel'. You can do it manually by calling 'devtools::install_github('michbur/AmpGramModel')'", call. = FALSE),
              {options(AmpGram_suppress_prompt = TRUE)
                cat("Ok, but you cannot access full functionality of this package without having installed 'AmpGramModel'")},
              message("You cannot access full functionality of this package without having installed 'AmpGramModel'. You can do it manually by calling 'devtools::install_github('michbur/AmpGramModel')'", call. = FALSE)
      )
    } 
  } else {
    message("To be able to use AmpGram properly, you should have installed 'AmpGramModel' with 'devtools::install_github('michbur/AmpGramModel'.")
  }
  
}
