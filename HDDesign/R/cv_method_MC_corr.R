cv_method_MC_corr <-
function(mu0, p, m, n, alpha_list, nrep, p1=0.5, ss=F, ntest, pcorr, chol.rho, sampling.p=0.5)
{
	temp=sapply(rep(mu0, nrep), cv_method_single_MC_corr, p, m, n, alpha_list, p1, ss, ntest, pcorr, chol.rho,sampling.p)

	if (ss==F){ 
            fromdata= mean(temp[1,]) 
            fromformula = mean(temp[2,])  
            cbind(fromdata,fromformula )
      }
	else{
		 apply(temp,1,mean)
     }
}
