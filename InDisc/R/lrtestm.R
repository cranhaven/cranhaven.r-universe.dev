lrtestm <- function(x, mu, lam, cvar, vare, th0, th1, vari, nfac){

nume0 <- 0
nume1 <- 0
deno0 <- 0
deno1 <- 0

ni <- size(x)[2]

for (j in 1:ni){

  if (nfac == 2){
    lamtmp <- c(lam[j,1],lam[j,2])
    term01 <- (x[j] - mu[j] - (lam[j,1] * th0[1]) - (lam[j,2] * th0[2]))^2
    term11 <- (x[j] - mu[j] - (lam[j,1] * th1[1]) - (lam[j,2] * th1[2]))^2
  }

  if (nfac == 3){
    lamtmp <- c(lam[j,1],lam[j,2],lam[j,3])
    term01 <- (x[j] - mu[j] - (lam[j,1] * th0[1]) - (lam[j,2] * th0[2]) - (lam[j,3] * th0[3]))^2
    term11 <- (x[j] - mu[j] - (lam[j,1] * th1[1]) - (lam[j,2] * th1[2]) - (lam[j,3] * th1[3]))^2
  }

  if (nfac == 4){
    lamtmp <- c(lam[j,1],lam[j,2],lam[j,3],lam[j,4])
    term01 <- (x[j] - mu[j] - (lam[j,1] * th0[1]) - (lam[j,2] * th0[2]) - (lam[j,3] * th0[3]) - (lam[j,4] * th0[4]))^2
    term11 <- (x[j] - mu[j] - (lam[j,1] * th1[1]) - (lam[j,2] * th1[2]) - (lam[j,3] * th1[3]) - (lam[j,4] * th1[4]))^2
  }

  rlamtmp <- lamtmp %*% transpose(lamtmp)
  term02 <- rlamtmp * (cvar + vare[j])
  term12 <- rlamtmp * (vari + vare[j])
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
