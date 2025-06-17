\name{waic}
\alias{waic}
\alias{fitDataj}

\title{
Function for the Watanabe-Akaike information criteria (WAIC) 
}

\description{
Model selection can be performed for each working model (WM) using the Watanabe-Akaike
information criteria (WAIC) developed by Watanabe. 
}

\usage{
waic(stanfit, s)            
}

\arguments{ 
	\item{stanfit}{
		Estimates obtained with the STAN fit. You can use the \code{\link{fitDataj}} function which is giving the next fit of the model from STAN.  
	}
	\item{s}{
		Integer specifying the number of models used to compute the WAIC selection.
	}      
}  

\references{ 
	Petit, C., et al, (2016) Unified approach for extrapolation and bridging of adult information in early phase dose-finding paediatric studies, Statistical Methods in Medical Research, <doi:10.1177/0962280216671348>.
	    
	Watanabe S. Asymptotic Equivalence of Bayes cross vallidation and widely applicable information criterion in singular learning theory, volume 11. 2010.
}     

\author{   
	Artemis Toumazi \email{artemis.toumazi@gmail.com},
	Caroline Petit \email{caroline.petit@crc.jussieu.fr},
	Sarah Zohar \email{sarah.zohar@inserm.fr}
}


\examples{
\dontrun{
	for(s in 1:nbDesign){
		fitj <- fitDataj(stan_model, nbPatientsj, nbDoses, tox, eff, given_dose,
						 skeleton_tox, skeleton_eff, mu, sigma, s)
		waicj <- waic(stanfit=fitj, s)
	}
}
}
