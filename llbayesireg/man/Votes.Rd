\name{Votes}
\alias{Votes}
\docType{data}
\title{Data of the votes in the presidential elections of the municipalities of Sergipe in the years 1994, 1998, 2002 and 2006}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
Proportion of votes for a political party (Partido dos Trabalhadores) in presidential elections in Brazil by the different municipalities of Sergipe state.
}
\usage{data("Votes")}
\format{
  A data frame containing 75 observations on 4 variables.
  \describe{
    \item{Votes1994}{ The format is:
        num [1:75] 0.228 0.172 0.431 0.105 0.165 ...}
    \item{Votes1998}{ The format is:
        num [1:75] 0.293 0.193 0.427 0.111 0.155 ...}
    \item{Votes2002}{ The format is:
        num [1:75] 0.307 0.278 0.517 0.268 0.223 ...}
    \item{Votes2006}{ The format is:
        num [1:75] 0.492 0.365 0.375 0.426 0.368 ...}
  }
}
\details{
Proportion of votes for a political party (Partido dos Trabalhadores) in presidential elections in Brazil by the different municipalities of Sergipe state in the years 1994, 1998, 2002 and 2006.
%%  ~~ If necessary, more details than the __description__ above ~~
}
\references{Tribunal Superior Eleitoral. Reposit?rio de Dados Eleitorais: TSE website www.tse.jus.br, accessed 10/01/2018.
}
\examples{
data(Votes)
## maybe str(Votes) ; plot(Votes) ...
}
\keyword{datasets}
