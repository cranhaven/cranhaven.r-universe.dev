EBelasticNet.Binomial <-
function(BASIS,Target,lambda,alpha,verbose = 0){
	N 				= nrow(BASIS);
	K 				= ncol(BASIS);
	Epis=FALSE;
	if (verbose>0) 	cat("Empirical Bayes Elastic Net Logistic Model\n");
  if(!Epis) {
		N_effect 		= K;
		Beta 			= rep(0,N_effect *4);


		output<-.C("ElasticNetBinary",
			BASIS 	= as.double(BASIS),
			Target 	= as.double(Target),
			lamda 	= as.double(lambda),
			alpha 	= as.double(alpha),
			logLikelihood = as.double(0),
			Beta 		= as.double(Beta),
			WaldScore 	= as.double(0),
			Intercept 	= as.double(rep(0,2)),
			N 		= as.integer(N),
			K 		= as.integer(K),
			PACKAGE="EBglmnet");

	}
	result 			= matrix(output$Beta,N_effect,4);

	ToKeep 			= which(result[,3]!=0);
	if(length(ToKeep)==0) {  Blup = matrix(0,1,4)
	}else	Blup 	 = result[ToKeep,,drop= FALSE];
	
	
	#t-test:
	t 				= abs(Blup[,3])/(sqrt(Blup[,4])+ 1e-20);
pvalue 			= 2*(1- pt(t,df=(N-1)));
	blup 			= cbind(Blup[,2:4,drop = FALSE],t,pvalue); 			#M x 6
	
	

	colnames(blup) = c("predictor","beta","posterior variance","t-value","p-value");	

	hyperparameters = c(alpha, lambda);
	fEBresult 			<- list(blup,output$logLikelihood,output$WaldScore,output$Intercept[1],hyperparameters);
	rm(list= "output")	
	names(fEBresult)		<-c("fit","logLikelihood","WaldScore","Intercept","hyperparameters")
	return(fEBresult)
	
}
