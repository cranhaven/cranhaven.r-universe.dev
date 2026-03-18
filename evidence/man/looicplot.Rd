\name{looicplot}
\alias{looicplot}
\title{A dot plot is produced for several related models showing for
  each model its LOOIC-value with its credible interval.}
\description{The LOOIC-value (like the non-Bayesian AIC-value) is a
  useful measure of model performance for model prediction.}
\usage{
looicplot(looiclist, modnames, perc = 90)
}
\arguments{
  \item{looiclist}{
    a list of character-valued names of rstanarm model objects }
  \item{modnames}{
    a character-valued vector of model names for each of the models }
  \item{perc}{
    the percentage credibility for the credible intervals (defaults to
    90\%) }
}
\value{
  None provided, but a printed list of looic-values, their standard
  errors, and credible intervals, and a dot plot with the same
  information are produced. }
\references{
van Hulst, R. 2018. Evaluating Scientific Evidence. ms.
}
\author{
Robert van Hulst
}
\seealso{
\code{\link{dotchart}}
}
\examples{
\dontrun{
data(budworm)
Mbudworm1 <- stan_glm(formula = cbind(dead, 20 - dead) ~ ldose,
                      family = binomial, data = budworm,
                      prior = student_t(df = 7),
                      prior_intercept = student_t(df = 7))
Mbudworm2 <- stan_glm(formula = cbind(dead, 20 - dead) ~ ldose * sex,
                      family = binomial, data = budworm,
                      prior = student_t(df = 7),
                      prior_intercept = student_t(df = 7))
Mbudworm3 <- stan_glm(formula = cbind(dead, 20 - dead) ~ ldose + sex,
                      family = binomial, data = budworm,
                      prior = student_t(df = 7),
                      prior_intercept = student_t(df = 7))
looicplot(looiclist = list("Mbudworm1", "Mbudworm2", "Mbudworm3"),
          modnames = c("~ ldose", "~ ldose + sex", "~ ldose * sex") )
}
}


