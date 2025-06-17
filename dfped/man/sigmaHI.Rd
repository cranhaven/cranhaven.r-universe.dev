\name{sigmaHI}
\alias{sigmaHI}

\title{
Compute the informative prior variance for the adaptive prior.  
}

\description{
Compute the informative prior variance for the adaptive prior based on the assumption that every dose has the same probability to be the maximum tolerated dose (MTD), i.e. uniform distribution. 
}

\usage{
sigmaHI(wm, meanbeta, a = NULL, model, tau, threshold) 
} 

\arguments{
  	\item{wm}{  
		The selected working model; for example the skeleton of toxicity; must be a vector. 
	} 
  	\item{meanbeta}{ 
		The mean value of variable beta. 
	} 
  	\item{a}{
		The variable a; the default value is NULL.
	} 
  	\item{model}{
		A valid model; for example "power_log" model.    
	} 
  	\item{tau}{
		The target of toxicity. 
	}
  	\item{threshold}{
		A threshold of the model. 
	}
}

\references{
	Petit, C., et al, (2016) Unified approach for extrapolation and bridging of adult information in early phase dose-finding paediatric studies, Statistical Methods in Medical Research, <doi:10.1177/0962280216671348>.
	
	Zhang J., Braun T., and J. Taylor. Adaptive prior variance calibration in the bayesian continual reassessment method. Stat. Med., 32:2221-34, 2013.
} 

\author{
	Artemis Toumazi \email{artemis.toumazi@gmail.com},
	Caroline Petit \email{caroline.petit@crc.jussieu.fr},
	Sarah Zohar \email{sarah.zohar@inserm.fr}
}

\seealso{
	\code{\link{sigmaLI}}
}

\examples{
targetTox <- 0.25             # target of toxicity  

####### Skeleton ########### 

skeleton_tox1 <- c(0.10, 0.21, 0.33, 0.55, 0.76)
skeleton_tox2 <- c(0.21, 0.33, 0.55, 0.76, 0.88)
skeleton_tox3 <- c(0.05, 0.10, 0.21, 0.33, 0.55)
skeleton_tox4 <- c(0.025, 0.05, 0.1, 0.21, 0.33)
skeleton_tox5 <- c(0.0125, 0.025, 0.05, 0.1, 0.21)
skeletonTox <-  data.frame(skeleton_tox1, skeleton_tox2, skeleton_tox3, 
                    skeleton_tox4, skeleton_tox5)
mu <- -0.34 
sigmaHI <- sigmaHI(skeletonTox[ ,1], mu, a = NULL, "power_log", targetTox, 0.80)
}

