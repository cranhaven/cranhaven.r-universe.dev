lrtest <- function(x, mu, lam, cvar, vare, th0, th1, vari){

nume0 <- 0
nume1 <- 0
deno0 <- 0
deno1 <- 0

ni <- size(x)[2]

for (j in 1:ni){
  term01 <- (x[j] - mu[j] - (lam[j] * th0))^2
  term11 <- (x[j] - mu[j] - (lam[j] * th1))^2
  term02 <- lam[j] * lam[j] * (cvar + vare[j])
  term12 <- lam[j] * lam[j] * (vari + vare[j])
  t0 <- term01 / term02
  t1 <- term11 / term12
  nume0 <- nume0 + t0
  nume1 <- nume1 + t1
  term03 <- log(cvar + vare[j])
  term13 <- log(vari + vare[j])
  deno0 <- deno0 + term03
  deno1 <- deno1 + term13

}
provi <- nume0 + deno0 - nume1 - deno1

if (provi < 0){
  chi <- 0.01
}
else if (provi > 15){
  chi <- 15
}
else {
  chi <- provi
}

LR <- exp(-2 * chi)

OUT <- list("LR"=LR, "chi"=chi)
return(OUT)

}
