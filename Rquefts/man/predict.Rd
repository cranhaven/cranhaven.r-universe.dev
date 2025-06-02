\name{predict}

\alias{predict}
\alias{predict,Rcpp_QueftsModel-method}


\title{
Spatial QUEFTS model predictions
}

\description{
Make spatial predictions with a QUEFTS model. First create a model, then use the model with a SpatRaster of soil properties to make spatial predictions.
}


\usage{
\S4method{predict}{Rcpp_QueftsModel}(object, supply, yatt, leaf_ratio, stem_ratio, 
		var="yield", filename="", overwrite=FALSE, ...)
}


\arguments{
  \item{object}{QUEFTSModel}
  \item{supply}{SpatRaster with nutrient supply data (Ns, Ps, Ks)}  
  \item{yatt}{SpatRaster with attainable yield}  
  \item{leaf_ratio}{positive numeric (typically between 0 and 1) indicating the leaf weight relative to the storage organ weight. For example: 0.46 for maize, 0.17 for potato, and 0.18 for rice}  
  \item{stem_ratio}{positive numeric (typically between 0 and 1) indicating the stem weight relative to the storage organ weight, For example: 0.56 for maize, 0.14 for potato, and 0.67 for rice}  
  \item{var}{character. Output variable name. Either "yield" or "gap"}  
  \item{filename}{character. Output filename. Optional}
  \item{overwrite}{logical. If \code{TRUE}, \code{filename} is overwritten}
  \item{...}{list. Options for writing files as in \code{\link[terra]{writeRaster}}}
}


\value{
SpatRaster
}

\examples{

library(terra)

ff <- list.files(system.file("sp", package="Rquefts"), full.names=TRUE)
r <- rast(ff)

soil <- r[[c("Tavg", "pH", "SOC", "Kex", "Pex", "Ptot")]]
supply <- lapp(soil, nutSupply2)
plot(supply)

yatt <- rast(system.file("sp/Ya.tif", package="Rquefts"))

maize <- quefts_crop("Maize")
fertilizer <- list(N=0, P=0, K=0)
q <- quefts(crop=maize, fert=fertilizer)

p <- predict(q, supply, yatt, 0.46, 0.56)
plot(p)

g <- predict(q, supply, yatt, 0.46, 0.56, "gap")
plot(g)
}

