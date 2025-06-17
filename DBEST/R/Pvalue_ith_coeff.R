Pvalue_ith_coeff <-
function(coef_vec,R,normr,dfSJ,i) {
        
        #VC <- (solve(R)) %*% t(solve(R)) * (normr^2)/dfSJ
        #VC <- (pinv(R)) %*% t(pinv(R)) * (normr^2)/dfSJ
  	VC <- pinv(t(R)%*%R) * (normr^2)/dfSJ

        VCi <- sqrt(VC[i,i])
        statistic <- (coef_vec[i])/VCi
        p <- 2 * (1-pt(abs(statistic),dfSJ))
        return(p)
}
