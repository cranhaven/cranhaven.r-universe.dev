\name{personality}
\alias{personality}
\docType{data}
\title{ Personality Data From an Experiment in Psychology  }
\description{
  This is a tree-way array showing responses to 15 possible reactions in 
   23 situations for 54 persons. It is used in the book as an example
   for posterior predictive checks.
}
\usage{data(personality)}
\format{
  The format is:
 num [1:15, 1:23, 1:54] 0 0 0 1 0 0 1 2 0 0 ...
 - attr(*, "dimnames")=List of 3
  ..\$ response : chr [1:15] "1" "2" "3" "4" ...
  ..\$ situation: chr [1:23] "1" "2" "3" "4" ...
  ..\$ person   : chr [1:54] "1" "2" "3" "4" ...
}
\examples{
data(personality)
str(personality)
# Following code adapted from file personality3.R on the book's webpage:
nsubjects <- 6
nrep <- 7

test <- function (a){
  output <- as.vector(a)>0
  glm.data.frame <- data.frame (output, response, situation, person)
  glm0 <- glm (output ~
    factor(response) + factor(situation) + factor(person),
    family=binomial(link=logit),
    data=glm.data.frame)
  pred0 <- predict.glm (glm0, type="response")
  mean (ifelse(output, (1-pred0)^2, pred0^2))
}
data <- personality[,,1:nsubjects]
attrs <- attributes(data)
data <- ifelse (data>0, 1, 0)
attributes(data) <- attrs

}
\keyword{datasets}
