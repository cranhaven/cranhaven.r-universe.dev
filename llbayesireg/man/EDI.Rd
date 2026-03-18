\name{EDI}
\alias{EDI}
\docType{data}
\title{Education Development Index}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
EDI data set is the Education Development Index (EDI), Elementary School and High School of the
municipalities of Sergipe state of Brazil.
}
\usage{data("EDI")}
\format{
  A data frame containing 75 observations on 2 variables.
  \describe{
    \item{EDIES}{ The format is:
 num [1:75] 3.6 3.6 3.8 3.1 3.5 3.7 3.8 3 3.2 0 ...}
    \item{EDIHS}{ The format is:
 num [1:75] 3.8 2.9 3 2.8 2.8 1.9 3 2.2 2.6 3.6 ...}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
The quality of education is attributed by a statistical value to educational indicators. This value is assigned by the context economic and social development to which the schools are inserted, not only by the students' performances. The systems educational use mainly of them for the monitoring of educational quality.
}
\references{Fundação Lemann e Meritt (2012): portal QEdu.org.br, acessado em 10/01/2019.
}
\examples{
data(EDI)
## maybe str(EDI) ; plot(EDI) ...
}
\keyword{datasets}
