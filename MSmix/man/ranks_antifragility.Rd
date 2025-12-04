\name{ranks_antifragility}
\alias{ranks_antifragility}
\docType{data}
\title{Antifragility Data (complete rankings with covariates)}
\description{The Antifragility dataset came up from an on-line survey conducted during spring 2021 by Sapienza University of Rome in collaboration with the Italian incubator Digital Magics, to investigate the construct of antifragility in innovative startups. Antifragility reflects the capacity of a company to adapt and improve its activity in the case of stresses, volatility and disorders triggered by critical and unexpected events, such as the COVID-19 outbreak which motivated the survey. On the basis of their experience and knowledge, a sample of \eqn{N=99} startups provided their complete rankings of \eqn{n=7} desirable antifragility properties in order of importance. The antifragility features are: 1 = Absorption, 2 = Redundancy, 3 = Small stressors, 4 = Non-monotonicity, 5 = Requisite variety, 6 = Emergence and 7 = Uncoupling.}
\usage{data(ranks_antifragility)}
\format{
A data frame gathering \eqn{N=99} complete rankings of the \eqn{n=7} antifragility features in each row (rank 1 = most preferred item). The definition of the antifragility aspects is detailed below:
\describe{
\item{Absorption}{Ability to absorb stress and shocks while remaining in the planned state.}
\item{Redundancy}{Overcapacity to defend from risks and prevent faults.}
\item{Small_stressors}{Ability to exert low levels of stress on the organization.}
\item{Non_monotonicity}{Capacity to learn from failures and errors.}
\item{Requisite_variety}{Need for regulatory agents (i.e., government agency) to monitor and control organization’s outcomes and behaviors.}
\item{Emergence}{Existence of cause-effect relationships between organization’s activity at micro level and its outcomes at macro level.}
\item{Uncoupling}{Existence of strong interconnection between agents inside and outside the organization.}
\item{Industry_sector}{Industry sector of the startup.}
\item{Market}{Market type in which the startup operates.}
\item{Innovation_type}{Main innovation type of the startup.}
\item{Approach_to_crisis}{Main approach implemented by the startup during Covid-19 outbreak.}
\item{Crisis_impact}{Impact of Covid-19 outbreak on the startup.}
\item{Age}{Age of the startup (years).}
\item{N_employees}{Number of employees in the startup.}
\item{Region}{Italian region of the startup.}
\item{Job_title}{Job title of the startup participant in the survey.}
\item{Experience}{Years of job experience of the startup participant in the survey.}
}
}
\references{
Ghasemi A and Alizadeh M (2017). Evaluating organizational antifragility via fuzzy logic. The case of an Iranian company producing banknotes and security paper. \emph{Operations research and decisions}, \bold{27}(2), pages 21--43, DOI: 10.5277/ord170202.
}
\examples{
str(ranks_antifragility)
head(ranks_antifragility)
}
\keyword{datasets}
