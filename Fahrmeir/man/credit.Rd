\name{credit}
\alias{credit}
\non_function{}
\concept{credit scoring}
\title{ Credit Score Data From a South German Bank }
\usage{data(credit)}
\description{
The \code{credit} data frame has 1000 rows and 8 columns. This are 
data for 1000 clients of a south german bank, 700 good payers and
300 bad payers. They are used to construct a credit scoring method.
}
\format{
This data frame contains the following columns:
\describe{
\item{Y}{
a factor with levels
\code{buen} 
\code{mal}, the response variable. buen is the good payers.
}
\item{Cuenta}{
a factor with levels
\code{no} 
\code{good running} 
\code{bad running}, quality of the credit clients bank account.
}
\item{Mes}{
a numeric vector, duration of loan in months.
}
\item{Ppag}{
a factor with levels
\code{pre buen pagador} 
\code{pre mal pagador}, if the client previosly have been a 
good or bad payer. 
}
\item{Uso}{
a factor with levels
\code{privado} 
\code{profesional}, the use to which the loan is made. 
}
\item{DM}{
a numeric vector, the size of loan in german marks.
}
\item{Sexo}{
a factor with levels
\code{mujer} 
\code{hombre}, sex of the client.
}
\item{Estc}{
a factor with levels
\code{no vive solo} 
\code{vive solo}, civil state of the client. 
}
}
}
\source{
 Fahrmeier and Tutz, \emph{Multivariate Generalized Linear Models}, 
 Springer Verlag.
}
\examples{
summary(credit)
}
\keyword{datasets}
