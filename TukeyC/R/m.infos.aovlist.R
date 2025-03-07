m.infos.aovlist <- function(x,
                            my,
                            forminter,
                            which,
                            sig.level,
                            aux_mt,
                            MSE,
                            ...)
{

  aux_m.inf <- aggregate(forminter,
                         data = model.frame(x),
                         function(x) c(min = min(x),
                                       max = max(x),
                                       sd  = sd(x),
                                       se  = sd(x)/length(x),
                                       sepool  = sqrt(MSE/length(x))))

  aux_m.inf1 <- data.frame(aux_m.inf[names(aux_m.inf)!=my],
                           means     = aux_mt[[my]][,1],
                           aux_m.inf[[my]][,1:2],
                           'linf_sd' = aux_mt[[my]][,1] - aux_m.inf[[my]][,3],
                           'lsup_sd' = aux_mt[[my]][,1] + aux_m.inf[[my]][,3],
                           'linf_se' = aux_mt[[my]][,1] - abs(qt(sig.level,aux_mt[[my]][,2]))*aux_m.inf[[my]][,4],
                           'lsup_se' = aux_mt[[my]][,1] + abs(qt(sig.level,aux_mt[[my]][,2]))*aux_m.inf[[my]][,4],
                           'linf_sepool' = aux_mt[[my]][,1] - abs(qt(sig.level,aux_mt[[my]][,2]))*aux_m.inf[[my]][,5],
                           'lsup_sepool' = aux_mt[[my]][,1] + abs(qt(sig.level,aux_mt[[my]][,2]))*aux_m.inf[[my]][,5])

  aux_m.inf2 <- aux_m.inf1[order(aux_m.inf1[['means']],
                                 decreasing = TRUE),]

  m.inf <- list(Means = aux_m.inf2[,c(1:2)],
                mm = aux_m.inf2[,c(1,3:4)],
                sd = aux_m.inf2[,c(1,5:6)],
                ic = aux_m.inf2[,c(1,7:8)],
                icpool = aux_m.inf2[,c(1,9:10)])
}
