\name{quefts}


\alias{quefts}
\alias{run}
\alias{run,Rcpp_QueftsModel-method}

\alias{soil<-}
\alias{crop<-}
\alias{fert<-}
\alias{biom<-}

\alias{soil<-,Rcpp_QueftsModel,list-method}
\alias{crop<-,Rcpp_QueftsModel,list-method}
\alias{fert<-,Rcpp_QueftsModel,list-method}
\alias{biom<-,Rcpp_QueftsModel,list-method}


\alias{[,Rcpp_QueftsModel,character,missing-method}
\alias{[,Rcpp_QueftsModel,character,character-method}
\alias{[,Rcpp_QueftsCrop,character,missing-method}
\alias{[,Rcpp_QueftsSoil,character,missing-method}

\alias{[<-,Rcpp_QueftsModel,character,missing-method}
\alias{[<-,Rcpp_QueftsModel,character,character-method}
\alias{[<-,Rcpp_QueftsCrop,character,missing-method}
\alias{[<-,Rcpp_QueftsSoil,character,missing-method}


\title{
QUEFTS model
}

\description{
Create a QUEFTS model, set parameters, and run it to compute nutrient requirements and nutrient limited yield.

A number of default crop parameter sets are available from \code{\link{quefts_crop}}, and an example soil from \code{\link{quefts_soil}}. You need to provide attainable or target crop production (in this context that is the maximum production in the absence of nutrient limitation), expressed as dry-matter biomass for leaves, stems and the storage organ (e.g. grain, root or tuber). See \code{\link{quefts_biom}}. Some crops are grown for the stems/leaves, in which case there is no relevant storage organ (e.g. sugarcane, jute). production yield estimates can be obtained with a crop growth model. 

}

\usage{
quefts(soil, crop, fert, biom)
crop(x) <- value
soil(x) <- value
fert(x) <- value
biom(x) <- value
run(x, ...)
}

\arguments{
 \item{soil}{list with named soil parameters. See Details. An example is returned by \code{\link{quefts_soil}}}
 \item{crop}{list with named crop parameters. See Details. An example is returned by \code{\link{quefts_crop}}} 
 \item{fert}{list with named fertilizer parameters (N, P and K). An example is returned by \code{\link{quefts_fert}}} 
 \item{biom}{list with named biomass and growing season length parameters. An example is returned by \code{\link{quefts_biom}}} 
 \item{x}{QueftsModel object}
 \item{value}{list with soil, crop, fertilizer, or biomass parameters as above}
 \item{...}{Additional arguments. None implemented}
}


\details{

For input parameters see \code{\link{quefts_crop}}, \code{\link{quefts_soil}}, \code{\link{quefts_fert}} and \code{\link{quefts_biom}}

\tabular{lll}{

	\tab \bold{Crop yield (biom)} \tab . \cr
	\tab \code{leaf_att, stem_att, store_att} \tab Attainable (in the absence of nutrient limitation), or target crop biomass (dry-matter, kg/ha) for leaves, stems and storage organs.\cr
	\tab \code{SeasonLength} \tab Length of the growing season (days)\cr

	
	\tab . \tab \cr
	\tab . \tab \cr
	\tab \bold{Output Variables} \tab \bold{Explanation} \cr
	\tab \code{N_actual_supply, P_actual_supply, K_actual_supply} \tab nutrient uptake from soil (not fertilizer) (kg/ha) \cr
	\tab \code{leaf_lim, stem_lim, store_lim} \tab nutrient limited biomass of leaves, stems, and storage organ (kg/ha) \cr
	\tab \code{N_gap, P_gap, K_gap} \tab fertilizer required to reach the specified biomass (kg/ha) \cr
}
}


\value{
vector with output variables as described in the Details
}

\references{
Janssen B.H., F.C.T. Guiking, D. van der Eijk, E.M.A. Smaling, J. Wolf and H. van Reuler, 1990. A system for the quantitative evaluation of the fertility of tropical soils (QUEFTS). Geoderma 46: 299-318

Sattari, S.Z., M.K. van Ittersum, A.F. Bouwman, A.L. Smit, and B.H. Janssen, 2014. Crop yield response to soil fertility and N, P, K inputs in different environments: Testing and improving the QUEFTS model. Field Crops Research 157: 35-46
}


\examples{

# create a QUEFTS model
# 1. get parameters
soiltype <- quefts_soil()
barley <- quefts_crop("Barley")
fertilizer <- list(N=0, P=0, K=0)
att_yield <- list(leaf_att=2200, stem_att=2700, store_att=4800, SeasonLength=110)

# 2. create a model
q <- quefts(soiltype, barley, fertilizer, att_yield)

# 3. run the model
run(q)

# change some parameters
q$SeasonLength <- 162
q$leaf_att <- 2651
q$stem_att <- 5053
q$store_att <- 8208

q$N <- 100
q$P <- 50
q$K <- 50

run(q)


## note that Rquefts uses C++ reference classes. 
## This means that if you copy a quefts model, you do not create a 
## new instance of the model, but you point to the same one!
q <- quefts()
q["N"]
k <- q
k["N"] <- 150
k["N"]
# the value of q has also changed!
q["N"]

## different ways of subsetting / replacement
q <- quefts()
q$N
q$N <- 30
q["N"]
q["N"] <- 90
q["model", "N"]
q["model", "N"] <- 60
q$N

q$soil$N_recovery
q["soil$N_recovery"]
q["soil$N_recovery"] <- .6
q["soil", "N_recovery"]
q["soil", "N_recovery"] <- .4
q$soil$N_recovery

}
