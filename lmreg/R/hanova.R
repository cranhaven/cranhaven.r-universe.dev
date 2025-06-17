hanova <-
function(lm1,lm2) {
  lmanova <- anova(lm1,lm2)
  SS <- c(lmanova$Sum[2],lmanova$RSS[2:1])
  DF <- c(lmanova$Df[2],lmanova$Res.Df[2:1])
  MS <- c(SS[1:2]/DF[1:2],NA)
  Fratio <- c(lmanova$F[2],NA,NA); pvalue = c(lmanova$Pr[2],NA,NA)
  RegAnova <- data.frame(cbind(SS,DF,MS,Fratio,pvalue),
                         row.names = c("Departure from H0","Error","Total"))
  return(RegAnova)
}
