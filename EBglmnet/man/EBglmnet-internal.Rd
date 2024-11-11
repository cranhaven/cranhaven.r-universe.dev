\name{EBglmnet-internal}
\title{Internal EBglmnet functions}
\alias{EBelasticNet.Binomial}
\alias{EBelasticNet.Gaussian}
\alias{EBelasticNet.BinomialCV}
\alias{EBelasticNet.GaussianCV}
\alias{EBlassoNE.BinomialCV}
\alias{EBlassoNE.GaussianCV}
\alias{EBlassoNEG.Binomial}
\alias{EBlassoNEG.Gaussian}
\alias{EBlassoNEG.BinomialCV}
\alias{EBlassoNEG.GaussianCV}
\alias{ijIndex}
\alias{CVonePair}
\alias{lambdaMax}
\description{Internal EBglmnet functions}
\usage{
	EBelasticNet.Binomial(BASIS,Target, lambda, alpha,verbose)
	EBelasticNet.BinomialCV(BASIS,Target, nFolds,foldId, verbose)
	EBelasticNet.Gaussian(BASIS,Target, lambda, alpha,verbose)
	EBelasticNet.GaussianCV(BASIS,Target, nFolds,foldId, verbose)
	EBlassoNE.BinomialCV(BASIS,Target,nFolds,foldId,verbose)
	EBlassoNE.GaussianCV(BASIS,Target, nFolds,foldId,verbose)
	EBlassoNEG.Binomial(BASIS,Target,a_gamma,b_gamma, verbose)
	EBlassoNEG.BinomialCV(BASIS,Target,nFolds,foldId,verbose)
	EBlassoNEG.Gaussian(BASIS,Target,a_gamma, b_gamma,  verbose)
	EBlassoNEG.GaussianCV(BASIS,Target,nFolds,foldId,verbose)
	ijIndex(trueLoc,K)
	CVonePair(X,y,nFolds,foldId,hyperpara,prior,family,verbose)
	lambdaMax(X,y,Epis)
}
\author{Anhui Huang and Dianting Liu \cr Dept of Electrical and Computer Engineering, Univ of Miami, Coral Gables, FL}

\details{These are not intended for use by users. 
	\code{EBelasticNet.Binomial} fits sparse logistic regression using elastic net prior distribution. 
	\code{EBelasticNet.BinomialCV} Cross Validation designed with 20 alpha and 20 lambdas for EBEN binomial model.
	\code{EBelasticNet.Gaussian} fits sparse linear regression using elastic net prior distribution. 
	\code{EBelasticNet.GaussianCV}Cross Validation designed with 20 alpha and 20 lambdas for EBEN Gaussian model.
	\code{EBlassoNE.BinomialCV} Cross Validation designed with 20 lambdas for EBlasso binomial model.
	\code{EBlassoNE.GaussianCV} Cross Validation designed with 20 lambdas for EBlasso Gaussian model.
	\code{EBlassoNEG.Binomial} fits sparse logistic regression using lasso-NEG prior distribution. 
	\code{EBlassoNEG.BinomialCV}Cross Validation designed with 3-step search for EBlasso-NEG binomial model.
	\code{EBlassoNEG.Gaussian} fits sparse linear regression using lasso-NEG prior distribution. 
	\code{EBlassoNEG.GaussianCV}Cross Validation designed with 3-step search for EBlasso-NEG Gaussian model.
	\code{ijIndex}Function for looking at the pair of interaction terms.
	\code{CVonePair}Function performs nFolds CV for the given one pair of hyperparameter.	
	\code{lambdaMax}Function calculate the maximum lambda for EBlasso-NE and EBEN in CV.
}
\keyword{internal}
