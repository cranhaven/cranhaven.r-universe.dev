rejection <- function(object, alpha = 0.05, stat = names(object[["observed_stats"]]), ...){
  validate_args_rejection(object = object, alpha = alpha, stat = stat)
  output <- data.frame(stat = character(), rate = numeric(), mcse = numeric())
  for(i in seq_along(stat)){
    obs_stat <- object[["observed_stats"]][[stat[i]]]
    mcsim_stat <- object[["mcsim_stats"]][[stat[i]]]
    func <- ecdf(obs_stat)
    q <- quantile(mcsim_stat, probs = 1 - alpha, ...)
    rejection_rate <- 1 - func(q)
    mcse <- unlist(calc_mcse_binom(p = rejection_rate,
                                   B = length(object[["mcsim_stats"]][[1]])))
    output <- rbind(output, data.frame(stat = stat[i],
                                       alpha = alpha,
                                       rate = rejection_rate,
                                       mcse = mcse))
  }
  return(output)
}
