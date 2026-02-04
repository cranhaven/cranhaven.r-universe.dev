\name{cov_simu}
\alias{cov_simu}
\title{Simulate Covariates Matrix Based on User Provided Factor Information}
\usage{
cov_simu(sample_size = sample_size,factors=factors)


}

\arguments{
  \item{sample_size}{Total number of patients }


  \item{factors}{A list contains basic information about the covariate factors. Each element should have factor's name, number of levels and their frequency, as well as the hazard ratio to the reference group. Factors should be categorical data.}

}


\description{
simulate covariates matrix based on user provided factor information
}
\examples{

f1<-list(name='Region', N_level=3, prevalence=c(0.1,0.2,0.7), HR=c(1,1,1), strata=TRUE)
f2<-list(name='Gender', N_level=2, prevalence=c(0.5,0.5), HR=c(1,0.9), strata=TRUE)
factors<-list(f1,f2)
cov_simu(sample_size=300,factors=factors)

}
