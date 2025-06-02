\name{revSupply}

\alias{revSupply}

\title{
Estimate soil nutrients supply
}


\description{
Estimate the apparent base (unfertilized) soil supply of N, P and K based on nutrient omission trial data and a "reverse" QUEFTS approach. The apparent supply is found with optimization. 
}

\usage{
revSupply(obs, crop, soil, Ya, leaf_ratio, stem_ratio, SeasonLength = 120, ...)
}

\arguments{
  \item{obs}{data.frame with observed data from a nutrient omission trial. It must have these four columns: "N", "P", "K" and "Y"; that give the N, P, and K fertilizer application and the crop yield in kg/ha}
  \item{Ya}{numeric. Attainable yield}
  \item{soil}{list with named soil parameters. See \code{\link{quefts_soil}}}
  \item{crop}{list with named crop parameters. See \code{\link{quefts_crop}}}
  \item{leaf_ratio}{positive numeric (typically between 0 and 1) indicating the leaf weight relative to the storage organ weight. For example: 0.46 for maize, 0.17 for potato, and 0.18 for rice}  
  \item{stem_ratio}{positive numeric (typically between 0 and 1) indicating the stem weight relative to the storage organ weight, For example: 0.56 for maize, 0.14 for potato, and 0.67 for rice}  
  \item{SeasonLength}{positive integer}  
  \item{...}{additional arguments supplied to \code{\link[stats]{optim}}}  
}

\value{
numeric vector with the N, P, and K supply in kg/ha
}

\references{
?
}



\examples{
set.seed(777)
trial_data <- data.frame(treat = c("CON", "NPK", "NPK", "PK", "NK", "NP"),
                  N = c(0, 120, 120, 0, 120, 120),
                  P = c(0, 30, 30, 30, 0, 30),
                  K = c(0, 60, 60, 60, 60, 0),
                  Y = c(2000, 6000, 6000, 2500, 4500, 5500) + rnorm(6, 0, 500))
Ya <- max(trial_data$Y) + 1000
crop <- quefts_crop("Potato")
soil <- quefts_soil()

revSupply(trial_data, crop, soil, Ya, leaf_ratio=.17, stem_ratio=.14)
}

