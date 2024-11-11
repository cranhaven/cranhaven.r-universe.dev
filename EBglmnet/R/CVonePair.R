CVonePair <-function(X,y,nFolds,foldId,hyperpara=c(1,0.1), 
					prior=c("lassoNEG","lasso","elastic net"), family = c("gaussian","binomial"), verbose = 0)
{	
	if(prior=="lassoNEG")
	{
		pr =1;
	}else if(prior=="lasso")
	{
		pr = 2;
	}else
	{
		pr =3;
	}
	if(family =="gaussian")
	{
		model =0;
	}else
	{
		model = 1;
	}
  if(verbose>=1) cat("Empirical Bayes Lasso/Elastic Net Logistic Model: ", nFolds, "fold cross-validation on (",hyperpara, ")\n");
	N = nrow(X);
	K = ncol(X);
	group = 0;
	Epis = FALSE;
	nLogL = rep(0,4);
	output<-.C("cvOnePara",
			BASIS 		= as.double(X),
			y 		= as.double(y),
			foldId  	= as.integer(foldId),
			nfolds 		= as.integer(nFolds),
			n  	= as.integer(N),
			k 		= as.integer(K),
			verbose =as.integer(verbose),
			hyperpara 		= as.double(hyperpara),
			nLogL  	= as.double(nLogL),
			epistasis 		= as.integer(Epis),
			pr  	= as.integer(pr),			
			glm	= as.integer(model),
			group = as.integer(group),
			PACKAGE 	="EBglmnet");
output$nLogL #negative log likelihood
}

