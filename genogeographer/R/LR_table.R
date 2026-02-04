#' Compute pairwise likelihood ratios
#' 
#' For each pair of a specified vector of profiles the likelihood ratios are computed.
#' The list can include all populations in the data or only a subset.
#' We may for inferral purposes restrict to ratios including at least one "accepted" population.

#' @param result_df The output from \code{genogeo}
#' @param lr_populations A vector of population names (\code{pop} in \code{result_df}). If NULL all populations are used.
#' @param only_accepted Restrict the ratios to include minimum one accepted population.
#' @param CI The level of confidence interval to be computed
#' @param digits If rounding of the output should be performed.
#' @param keep_logP Logical. Should the logP's be returned in output
#' @author Torben Tvedebrink \email{tvede@@math.aau.dk}
#' @return A tibble with numerator and denominator populations with their log10 LR and uncertainty.
#' @export
#' @examples 
#' df_ <- simulate_pops(pop_n = 4, aims_n = 50)
#' df_db <- pops_to_DB(df_)
#' profile <- random_AIMs_profile(df_db, keep_pop = TRUE)
#' profile$pop[1] # The true population
#' result <- genogeo(profile[,c("locus","x0")], df = df_db)
#' LR_table(result)

LR_table <- function(result_df, lr_populations = NULL, only_accepted = TRUE, CI = 0.95, digits = NULL, keep_logP = FALSE){
  ## build fixes : start ##
  meta <- NULL
  pop <- NULL
  logP <- NULL
  varlogP <- NULL
  z_score <- NULL
  p_value <- NULL
  den_pop <- NULL
  den_logP <- NULL
  den_varlogP <- NULL
  den_p_value <- NULL
  logLR <- NULL
  var_logLR <- NULL
  CI_lwr <- NULL
  CI_upr <- NULL
  null_in_CI <- NULL
  . <- NULL
  num_z_score <- NULL
  num_accept <- NULL
  den_z_score <- NULL
  den_accept <- NULL
  ## build fixes : end ##
  z <- qnorm((1-CI)/2, lower.tail = FALSE)
  z_ <- qnorm(CI, lower.tail = TRUE)
  if(names(result_df)[1] == "meta") result_df <- rename(result_df, pop = meta)
  # If empty, make all LRs
  if(is.null(lr_populations)){
    lr_populations <- result_df$pop
  }
  lr_result <- result_df %>% 
    filter(pop %in% lr_populations) %>%
    select(pop, logP, varlogP, z_score, p_value) %>% 
    mutate(pop = factor(pop, levels = lr_populations, ordered = TRUE))
  ## 
  lr_list <- lr_result %>% rowwise() %>% 
    mutate(den = list(.)) %>% 
    ungroup() %>% 
    unnest(.sep = "_") %>% 
    filter(pop < den_pop)
  lr_list <- lr_list %>% 
    mutate(logLR = logP - den_logP,
           var_logLR = varlogP + den_varlogP,
           CI_lwr = logLR - z*sqrt(var_logLR),
           CI_upr = logLR + z*sqrt(var_logLR))
  lr_list <- lr_list %>% 
    mutate(num_accept = p_value > (1-CI),
           num_z_score = z_score,
           den_accept = den_p_value > (1-CI),
           den_z_score = den_z_score)
  if(only_accepted){
    lr_list <- lr_list %>% filter(num_accept | den_accept)
  }
  if(!is.null(digits)){
    lr_list <- lr_list %>% mutate_if(is.double, funs(round(., digits = digits)))
  }
  lr_list <- lr_list %>%
    arrange(pop, den_pop) %>%
    mutate_if(is.factor, .funs = paste)
  if(keep_logP) lr_list <- lr_list %>% select(numerator = pop, denominator = den_pop,logLR:CI_upr, 
                                              num_logP = logP, den_logP, num_accept, num_z_score, den_accept, den_z_score)
  else lr_list <- lr_list %>% select(numerator = pop, denominator = den_pop,logLR:CI_upr)
  lr_list %>% mutate(null_in_CI = sign(CI_lwr) != sign(CI_upr))
}