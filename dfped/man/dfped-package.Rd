\name{dfped-package}
\alias{dfped-package}
\alias{dfped}
\docType{package}

\title{
Extrapolation and Bridging of Adult Information in Early Phase Dose-Finding Paediatrics Studies
}

\description{
A unified method for designing and analysing dose-finding trials in paediatrics, while bridging information from adults, is proposed in the 'dfped' package. The dose range can be calculated under three extrapolation methods: linear, allometry and maturation adjustment, using pharmacokinetic (PK) data. To do this, it is assumed that target exposures are the same in both populations. The working model and prior distribution parameters of the dose-toxicity and dose-efficacy relationships can be obtained using early phase adult toxicity and efficacy data at several dose levels through 'dfped' package. Priors are used into the dose finding process through a Bayesian model selection or adaptive priors, to facilitate adjusting the amount of prior information to differences between adults and children. This calibrates the model to adjust for misspecification if the adult and paediatric data are very different. User can use his/her own Bayesian model written in Stan code through the 'dfped' package. A template of this model is proposed in the examples of the corresponding R functions in the package. Finally, in this package you can find a simulation function for one trial or for more than one trial. These methods are proposed by Petit et al, (2016) <doi:10.1177/0962280216671348>.
}

\author{ 
Artemis Toumazi \email{artemis.toumazi@gmail.com}, Caroline Petit \email{caroline.petit@crc.jussieu.fr},
Sarah Zohar \email{sarah.zohar@inserm.fr} 

Maintainer: Artemis Toumazi \email{artemis.toumazi@gmail.com} 
}

\references{ 
	Petit, C., et al, (2016) Unified approach for extrapolation and bridging of adult information in early phase dose-finding paediatric studies, Statistical Methods in Medical Research, <doi:10.1177/0962280216671348>.
}

\keyword{package}
