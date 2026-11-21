.cimehr_hl_laplace <- function(shared) {
  force(shared)
  function(m,nu,sigma2_zeta) {
    a <- as.numeric(sigma2_zeta)
    if(length(a)!=1L||!is.finite(a)||a<0)stop("sigma2_zeta must be one finite nonnegative number.",call.=FALSE)
    dat <- shared$.cimehr_frailty_validate(m,nu)
    n <- length(dat$m)
    if(a==0)return(list(mu=numeric(n),s2=rep(1,n),sigma2_zeta=0,mode=numeric(n),
      curvature=rep(1,n),approximation="Gaussian/Laplace; zero shared-frailty branch"))
    md <- shared$.cimehr_frailty_mode_curvature(dat$m,dat$nu,a)
    invK <- 1/md$curvature
    ratio <- md$omega*invK
    mu <- md$mode_u-.5*sqrt(a)*ratio*invK
    s2 <- invK+a*invK^2*(ratio^2-.5*ratio)
    if(any(!is.finite(mu))||any(!is.finite(s2))||any(s2<=0))
      stop("Higher-order Laplace posterior has nonfinite mean or nonpositive/nonfinite variance.",call.=FALSE)
    list(mu=mu,s2=s2,sigma2_zeta=a,mode=md$mode_u,curvature=md$curvature,
      mode_log_frailty=md$mode_x,maximum_mode_residual=max(md$relative_score_residual),
      approximation="Research Taylor correction: third/fourth local log-density derivatives; Gaussian moment closure")
  }
}
