#Douglas1994 methord for adjust p value
P_douglas1994 <- function(p, e){
    z=stats::qnorm(1-p/2)
    fz=exp(-z*z/2)/sqrt(2*pi)
    fz*(z-1/z)*log((1-e)^2/e^2)+4*fz/z
}
