# loss function and its first derivative
l_ld <- function(beta, x, n, Start, Stop, Event){
  fit.cox <- coxph(Surv(Start, Stop, Event) ~ x,
                   init = beta,
                   control = list('iter.max'=0, timefix = FALSE))
  l <- - fit.cox$loglik[1]/n
  ld <- - fit.cox$first/n
  return(c(l, ld))
}
