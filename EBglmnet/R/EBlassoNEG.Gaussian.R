EBlassoNEG.Gaussian <-
function(BASIS,Target,a_gamma,b_gamma,verbose = 0){
	N 					= nrow(BASIS);
	K 					= ncol(BASIS);
	Epis = FALSE;
	if (verbose>0) cat("Empirical Bayes Lasso Gaussian Model (Normal + Exponential + Gamma prior), N: ",N,",K: ",K,"\n");
	if(!Epis) {
		N_effect 		= K;
		Beta 			= rep(0,N_effect *4);

		output<-.C("fEBLinearMainEff",
			BASIS 		= as.double(BASIS),
			Target 		= as.double(Target),
			a_gamma 	= as.double(a_gamma),
			b_gamma 	= as.double(b_gamma),
			Beta 		= as.double(Beta),
			WaldScore 	= as.double(0),
			Intercept 	= as.double(0),
			N 			= as.integer(N),
			K 			= as.integer(K),
			ver 		= as.integer(verbose),
			residual 	= as.double(0),
			PACKAGE		="EBglmnet");
	}	
	
	result 				= matrix(output$Beta,N_effect,4);
	ToKeep 				= which(result[,3]!=0);
	if(length(ToKeep)==0) { Blup = matrix(0,1,4)
	}else
	{
		nEff 	= length(ToKeep);
		Blup 		= result[ToKeep,,drop=FALSE];
	}
	
	t 				= abs(Blup[,3])/(sqrt(Blup[,4])+ 1e-20);
	pvalue 			= 2*(1- pt(t,df=(N-1)));
	blup 			= cbind(Blup[,2:4,drop= FALSE],t,pvalue); 			#M x 6
	colnames(blup) = c("predictor","beta","posterior variance","t-value","p-value");

	hyperparameters = c(a_gamma, b_gamma);
	names(hyperparameters) = c("a", "b");
	fEBresult 			<- list(blup,output$WaldScore,output$Intercept,output$residual,hyperparameters);
	rm(list= "output")	
	names(fEBresult)	<-c("fit","WaldScore","Intercept","residual variance","hyperparameters")
	return(fEBresult)
	
}
