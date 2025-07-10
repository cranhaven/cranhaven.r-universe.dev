# germination speed index
# equation: GI = G1 / T1 + G2 / T2 +.+ Gn / Tn
# Onde, G1, G2, ..., Gn: numero de sementes germinadas na primeira contagem, segunda contagem e assim por diante at? a ?ltima contagem (n), respectivamente, e T1, T2, ..., Tn: n?mero de dias entre a semeadura e a primeira contagem, entre a semeadura e a segunda contagem, e assim por diante at? a ?ltima contagem (n), respectivamente.

GSI <- function(time, nger) {

 n <- length(time)
 result <- 0
 nger_corr <- nger

 for (i in 2:n) {
   nger_corr[i] <- nger[i] - nger[i-1]
 }

 for (i in 1:n) {
   result <- result + nger_corr[[i]]/time[[i]]
 }

 return(result)
}
