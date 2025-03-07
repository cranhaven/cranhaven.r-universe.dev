m.infos.nest.lm <- function(x,
                            my,
                            forminter,
                            which,
                            fl1,
                            fl2,
                            sig.level,
                            aux_mt,
                            ...)
{

  aux_m.inf <- aggregate(forminter,
                         data = x$model,
                         function(x) c(min = min(x),
                                       max = max(x),
                                       sd  = sd(x),
                                       se  = sd(x)/length(x)))

  aux_m.inf1 <- data.frame(aux_m.inf[names(aux_m.inf) != my],
                           means     = aux_mt$coef[,1],
                           aux_m.inf[[my]][,1:2],
                           'linf_sd' = aux_mt$coef[,1] - aux_m.inf[[my]][,3],
                           'lsup_sd' = aux_mt$coef[,1] + aux_m.inf[[my]][,3], 
                           'linf_se' = aux_mt$coef[,1] - abs(qt(sig.level,aux_mt$coef[,3]))*aux_m.inf[[my]][,4],
                           'lsup_se' = aux_mt$coef[,1] + abs(qt(sig.level,aux_mt$coef[,3]))*aux_m.inf[[my]][,4], 
                           'linf_sepool' = aux_mt$coef[,1] - abs(qt(sig.level,aux_mt$coef[,3]))*aux_mt$coef[,2],
                           'lsup_sepool' = aux_mt$coef[,1] + abs(qt(sig.level,aux_mt$coef[,3]))*aux_mt$coef[,2])

  aux_m.inf2 <- aux_m.inf1[order(aux_m.inf1[['means']],
                                 decreasing = TRUE),]

  nf1 <- unlist(strsplit(which,
                         split = ':'))[1] # nome do primeiro fator do which 

  nf2 <- unlist(strsplit(which,
                         split = ':'))[2] # nome do segundo fator do which

  nf3 <- unlist(strsplit(which,
                         split = ':'))[3] # nome do terceiro fator do which 

  if(is.null(fl2)){

    f2 <- levels(x$model[,nf1])[fl1] # corresponde ao fator onde se esta fazendo o desdobramento!

    aux_m.inf21 <- subset(aux_m.inf2, 
                          eval(parse(text = nf1)) == f2) # pegando as medias de interesse

    m.inf <- list(Means = aux_m.inf21[,c(1:3)],
                  mm = aux_m.inf21[,c(1:2,4:5)],
                  sd = aux_m.inf21[,c(1:2,6:7)],
                  ic = aux_m.inf21[,c(1:2,8:9)],
                  icpool = aux_m.inf21[,c(1:2,10:11)]) 

  } else {

    f2 <- levels(x$model[,nf2])[fl2] 

    f3 <- levels(x$model[,nf1])[fl1]

    aux_m.inf21 <- subset(aux_m.inf2, 
                          eval(parse(text = nf1)) == f3 & eval(parse(text = nf2)) == f2) # pegando as medias de interesse

    m.inf <- list(Means = aux_m.inf21[,c(1:4)],
                  mm = aux_m.inf21[,c(1:3,5:6)],
                  sd = aux_m.inf21[,c(1:3,7:8)],
                  ic = aux_m.inf21[,c(1:3,9:10)],
                  icpool = aux_m.inf21[,c(1:3,11:12)]) 

  }
}
