\name{appendTextureclass}
\alias{appendTextureclass}
\title{
A function for attaching soil textural classes
}
\description{
This function attaches soil textural classes according to different soil texture classification systems
}
\usage{
appendTextureclass(df, method)
}
\arguments{
  \item{df}{
spatial pixel dataframe with columns of soil textural proportions clay, silt, and sand in percentages
}
  \item{method}{
soil texture classification method for calculating soil texture. Default=USDA method
}
}
\details{
df is an output of createTexturedata with spatial reference or similar dataframe with normalized proportions summing to 100%.
method is the texture classification method for textural class calculation. Exanple methods are USDA, FAO, Australian, German, etc.
}
\value{
Output is a soil texture dataframe with textural classes for every row (or pixel) in the dataframe. The output may sometimes return double class such as "SaLo, Lo" implying possibility of a tie for two classes. Such outputs should be edited outside the package for meaningful representation of soil textural classes when necessary
}
\references{
Moyes J. 2018. The soil texture wizard: R functions for plotting, classifying, transforming and exploring soil texture data. https://cran.r-project.org/web/packages/soiltexture/vignettes/soiltexture_vignette.pdf
}
\author{
Christian Thine Omuto
}
\note{
This function can sometimes return double classes such as "SaLo, Lo" implying possibility of a tie for two classes.
}
\seealso{
\code{\link{textureSuit}}, \code{\link{createTexturedata}}
}
\examples{
library(soiltexture)
newtxt=textureinput
\donttest{
texturedata=createTexturedata(newtxt$clay, newtxt$silt, newtxt$sand)
newtxt1=appendTextureclass(as.data.frame(texturedata), method = "USDA")
levels(as.factor(newtxt1$TEXCLASS))
}
}
