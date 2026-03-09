\name{pedoTransfer}
\alias{pedoTransfer}
\title{
A pedotransfer function to predict electrical conductivity or any other soil property using other soil properties
}
\description{
This generic pedo-transfer function is used to approximate EC values from other existing and easy-to-measure soil data
}
\usage{
pedoTransfer(method="linear", df, ...)
}
\arguments{
  \item{method}{
modelling method to link EC and other soil predictors (properties). Default method is linear
}
  \item{df}{
dataframe containing measured EC and predictors of soil properies
}
  \item{...}{
names of measured EC and list of predictors (soil properties) seperated by comma. The names should match the variables in the accompanying dataframe
}
 }
\details{
This generic model can be used even with other soil properties. For example, it can be used to predict porosity from bulk density, carbon, and texture components as long as they are in the database and have known/suspected relationship
}
\value{
model for predicting EC given similar input data
}
\references{
van Looy k,  Bouma  J, Herbst M,  Koestel J, Minasny B,  Mishra U, Montzka C,  Nemes A,  Pachepsky AY,  Padarian J,  Schaap MG,  Tóth B,  Verhoef A, Jan Vanderborght,  van der Ploeg MJ,  Weihermüller L,   Zacharias S,  Zhang Y,  Vereecken H. 2017. Pedotransfer functions in Earth System Science: Challenges and Perspectives. Reviews of Geophysics 55(4): 1199-1256.

Sudduth KA, Kitchen RN, Wiebold WJ, Batchelor W. 2005. Relating apparent electrical conductivity to soil properties across the North-Central USA. Computers and Electronics in Agriculture, 46(1-3):263-283
}
\author{
Christian Thine Omuto
}
\note{
This function can also be used to predict EC from apparent electrical conductivity of bulk soil, texture, and other important soil properties
}
\seealso{
\code{\link{ECconversion4}}, \code{\link{conversion}}
}
\examples{
library(caret)
clay=as.data.frame(runif(120, 1,100))
silt=as.data.frame (runif(120,20,70))
sand=as.data.frame(runif(120,10.1,50.5))
pH=as.data.frame(runif(120,1,14))
EC=as.data.frame(runif(120,0.5,20.5))
OC=as.data.frame(runif(120,0.1,1.25))
soil4=cbind(EC,clay,silt,sand,OC,pH)
names(soil4)=c("EC","clay","silt","sand","OC","pH")
bound <- floor((nrow(soil4)/4)*3)
df.train <- soil4[sample(nrow(soil4)), ][1:bound, ]
df.test <- soil4[sample(nrow(soil4)), ][(bound+1):nrow(soil4[sample(nrow(soil4)), ]), ]
EC1.lm=pedoTransfer("randomforest",df.train,EC, clay,sand,silt,OC,pH)
df.test$EC1=predict(EC1.lm,newdata = df.test)
cor(df.test$EC,df.test$EC1)^2
plot(df.test$EC~df.test$EC1)
abline(1,1)
}
