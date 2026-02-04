scores_pop <- function(profile, CI=0.95, tilt = FALSE, tilt_interval = c(0.001, 0.1), tilt_n = 500, ...){
  ## build fixes : start ##
  lat <- NULL
  lon <- NULL
  out_of_place <- NULL
  logP <- NULL
  varlogP <- NULL
  score <- NULL
  E_score <- NULL
  V_score <- NULL
  z_score <- NULL
  p_value <- NULL
  p_value_tilt <- NULL
  ## build fixes : end ##
  ## profile_df returned by prep_profile
  z_CI <- qnorm(1-(1-CI)/2)
  AIMs_result <- profile %>% 
    summarise(n = median(n, na.rm = TRUE),
              lat = median(lat, na.rm = TRUE),
              lon = median(lon, na.rm = TRUE),
              out_of_place = out_of_place[1],
              logP = sum(logP),
              varlogP = sum(varlogP),
              logP_upr = logP + z_CI*sqrt(varlogP),
              logP_lwr = logP - z_CI*sqrt(varlogP),
              z_score = sum(score-E_score)/sqrt(sum(V_score)),
              p_value = pnorm(z_score, lower.tail = FALSE)
    )
  if(tilt){
    AIMs_result_tilt <- AIMs_result %>% 
      filter(between(p_value, tilt_interval[1], tilt_interval[2]))
    if(nrow(AIMs_result_tilt) == 0){ return(AIMs_result) }## break out of tilting
    profile_tilt <- semi_join(profile, AIMs_result_tilt, by = group_vars(profile)) %>% 
      group_by_(.dots = group_vars(profile)) %>% nest(.key = "data")
    ## if(brow) browser()
    AIMs_result_tilt <- profile_tilt %>% rowwise() %>% 
      mutate(p_value_tilt = with(data, exponent_tilt(x0 = x0, x1 = x1, n = n, B = tilt_n, p_limit = tilt_interval[2]))) %>% 
      select(-data)
    AIMs_result <- full_join(AIMs_result, AIMs_result_tilt, by = group_vars(profile)) %>% 
      mutate(p_value = ifelse(is.na(p_value_tilt), p_value, p_value_tilt)) %>% 
      select(-p_value_tilt)
  }
  AIMs_result
}

