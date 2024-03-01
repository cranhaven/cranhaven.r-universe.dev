\name{simfam}
\alias{simfam}
\title{
Generate familial time-to-event data
}
\description{
Generates familial time-to-event data for specified study design, genetic model and source of residual familial correlation; the generated data frame also contains family structure (individual's id, father id, mother id, relationship to proband, generation), gender, current age, genotypes of major or second genes.
}
\usage{
simfam(N.fam, design = "pop", variation = "none", interaction = FALSE, depend = NULL, 
       base.dist = "Weibull", frailty.dist = NULL, base.parms, vbeta, 
       allelefreq = c(0.02, 0.2), dominant.m = TRUE, dominant.s = TRUE,
       mrate = 0, hr = 0, probandage = c(45, 2), agemin = 20, agemax = 100)
% \method{summary}{simulation}(object, \dots)
}

\arguments{
  \item{N.fam}{
Number of families to generate.
}
  \item{design}{
Family based study design used in the simulations. Possible choices are: \code{"pop"}, \code{"pop+"}, \code{"cli"}, \code{"cli+"} or \code{"twostage"}, where \code{"pop"} is for the population-based design that families are ascertained by affected probands, \code{"pop+"} is similar to \code{"pop"} but with mutation carrier probands, \code{"cli"} is for the clinic-based design that includes affected probands with at least one parent and one sib affected, \code{"cli+"} is similar to \code{"cli"} but with mutation carrier probands and \code{"twostage"} for two-stage design that randomly samples families from the population in the first stage and oversamples high risk families in the second stage that include at least two affected members in the family. Default is \code{"pop"}.
}
  \item{variation}{
Source of residual familial correlation.  Possible choices are: \code{"frailty"} for frailty shared within families, \code{"secondgene"} for second gene variation, or \code{"none"} for no residual familial correlation. Default is \code{"none"}.
}
 \item{interaction}{
Logical; if \code{TRUE}, allows the interaction between gender and mutation status. Default is \code{FALSE}.
}
 \item{depend}{
Variance of the frailty distribution. Dependence within families increases with depend value.  Default is \code{NULL}. Value should be specified as a positive real number when \code{variation="frailty"}. 
}

 \item{base.dist}{
Choice of baseline hazard distribution.  Possible choices are: \code{"Weibull"}, \code{"loglogistic"}, \code{"Gompertz"}, \code{"lognormal"} \code{"gamma"}, \code{"logBurr"}. Default is \code{"Weibull"}.
}

 \item{frailty.dist}{
Choice of frailty distribution.  Possible choices are: \code{"gamma"} or \code{"lognormal"} when \code{variation="frailty"}. Default is \code{NULL}.
}
  \item{base.parms}{
Vector of parameter values for the specified baseline hazard function. \code{base.parms=c(lambda, rho)} should be specified for \code{base.dist="Weibull"}, \code{"loglogistic"}, \code{"Gompertz"}, \code{"gamma"}, and \code{"lognormal"}. For \code{base.dist="logBurr"}, three parameters should be specified \code{base.parms = c(lambda, rho, eta)}. }  	
  \item{vbeta}{
Vector of regression coefficients for gender, majorgene, interaction between gender and majorgene (if \code{interaction = TRUE}), and secondgene (if \code{variation = "secondgene"}). }
  \item{allelefreq}{
Vector of population allele frequencies of major and second disease gene alleles. Frequencies must be between 0 and 1. Default frequencies are 0.02 for major gene allele and 0.2 for second gene allele, \code{allelefreq = c(0.02, 0.2)}.}
  \item{dominant.m}{Logical; if \code{TRUE}, the genetic model of major gene is dominant, otherwise recessive.}
  \item{dominant.s}{Logical; if \code{TRUE}, the genetic model of second gene is dominant, otherwise recessive.}
  \item{mrate}{
Proportion of missing genotypes, value between 0 and 1. Default value is 0.}
  \item{hr}{
Proportion of high risk families, which include at least two affected members, to be sampled from the two stage sampling. This value should be specified when \code{design="twostage"}. Default value is 0. Value should lie between 0 and 1.
}
\item{probandage}{
Vector of mean and standard deviation for the proband age. Default values are mean of 45 years and standard deviation of 2 years, \code{probandage = c(45, 2)}.
}
\item{agemin}{
Minimum age of disease onset or minimum age. Default is 20 years of age.
}
\item{agemax}{
Maximum age of disease onset or maximum age. Default is 100 years of age.
}

}
\details{
The \code{design} argument defines the type of family based design to be simulated. Two variants of the population-based and clinic-based design can be chosen: \code{"pop"} when proband is affected, \code{"pop+"} when proband is affected mutation carrier, \code{"cli"} when proband is affected and at least one parent and one sibling are affected, \code{"cli+"} when proband is affected mutation-carrier and at least one parent and one sibling are affected. The two-stage design, \code{"twostage"}, is used to oversample high risk families, where the proportion of high risks families to include in the sample is specified by \code{hr}. High risk families often include multiple (at least two) affected members in the family.

The ages at onset are generated from the following penetrance models depending on the choice of \code{variation = "none", "frailty", "secondgene".}. When \code{variation = "none"}, the ages at onset are independently generated from the proportional hazard model conditional on the gender and carrier status of major gene mutation, \ifelse{html}{ \out{ X = c(x<sub>s</sub>, x<sub>g</sub>)}}{\eqn{X=(x_s, x_g)}{X=c(xs, xg)}}.  
%When \code{variation = "frailty"}, the ages at onset are generated from the shared frailty model that induces the residual familial correlation by a frailty shared within the family. When \code{variation = "secondgene"}, the ages at onset are generated from the two-gene model that induces the residual familial correlation by a second gene shared within the family.
The ages at onset correlated within families are generated from the shared frailty model (code{variation = "frailty"}) or the two-gene model (code{variation = "secondene"}), where the residual familial correlation is induced by a frailty or a second gene, respectively, shared within the family.

\emph{The proportional hazard model}

\ifelse{html}{ \out{<p style="text-align:center"> h(t|X) = h<sub>0</sub>(t - t<sub>0</sub>) exp(&beta;<sub>s</sub> * x<sub>s</sub> + &beta;<sub>g</sub> * x<sub>g</sub>), </p>}}{	
		\deqn{ h(t|X) = h_0(t - t_0) \exp(\beta_s x_s+\beta_g x_g) ,}{ h(g|X) = h0(t - t0) * exp(beta.s * xs + beta.g * xg) ,}}
where \ifelse{html}{\out{h<sub>0</sub>(t)}}{\eqn{h_0(t)}{h0(t)}} is the baseline hazard function, \ifelse{html}{\out{t<sub>0</sub>}}{\eqn{t_0}{t0}} is a minimum age of disease onset, \ifelse{html}{\out{x<sub>x</sub>}}{\eqn{x_s}{xs}} and \ifelse{html}{\out{x<sub>g</sub>}}{\eqn{x_g}{xg}} indicate male (1) or female (0) and carrier (1) or non-carrier (0) of a main gene of interest, respectively.


\emph{The shared frailty model}

\ifelse{html}{ \out{<p style="text-align:center"> h(t|X,Z) = h<sub>0</sub>(t - t<sub>0</sub>) Z exp(&beta;<sub>s</sub> * x<sub>s</sub> + &beta;<sub>g</sub> * x<sub>g</sub>), </p>}}{	
		\deqn{ h(t|X,Z) = h_0(t - t_0) Z \exp(\beta_s x_s+\beta_g x_g) ,}{ h(g|X,Z) = h0(t - t0) * Z * exp(\betas * xs + \betag * xg) ,}}
where \ifelse{html}{\out{h<sub>0</sub>(t)}}{\eqn{h_0(t)}{h0(t)}} is the baseline hazard function, \ifelse{html}{\out{t<sub>0</sub>}}{\eqn{t_0}{t0}} is a minimum age of disease onset, \eqn{Z} represents a frailty shared within families and follows either a gamma or log-normal distribution, \ifelse{html}{\out{x<sub>x</sub>}}{\eqn{x_s}{xs}} and \ifelse{html}{\out{x<sub>g</sub>}}{\eqn{x_g}{xg}} indicate male (1) or female (0) and carrier (1) or non-carrier (0) of a main gene of interest, respectively.

\emph{The two-gene model}

\ifelse{html}{ \out{<p style="text-align:center"> h(t|X) = h<sub>0</sub>(t - t<sub>0</sub>) Z exp(&beta;<sub>s</sub> * x<sub>s</sub> + &beta;<sub>1</sub> * x<sub>2</sub> + &beta;<sub>2</sub> * x<sub>2</sub>), </p>}}{
		\deqn{ h(t|X,Z) = h_0(t - t_0) Z \exp(\beta_s x_s+\beta_1 x_1 + \beta_2 x_2) ,}{ h(g|X,Z) = h0(t - t0) * Z * exp(betas * xs + beta1 * x1 + beta2 * x2) ,}}
where \ifelse{html}{ \out{x<sub>1</sub>, x<sub>2</sub>}}{\eqn{x_1, x_2}{x1,x2}} indicate carriers (1) and non-carriers (0) of a major gene and of second gene mutation, respectively.

The current ages for each generation are simulated assuming normal distributions. However, the probands' ages are generated using a left truncated normal distribution as their ages cannot be less than the minimum age of onset. The average age difference between each generation and their parents is specified as 20 years apart. 

Note that simulating family data under the clinic-based designs (\code{"cli"} or \code{"cli+"}) or the two-stage design can be slower since the ascertainment criteria for the high risk families are difficult to meet in such settings. Especially, \code{"cli"} design could be slower than \code{"cli+"} design since the proband's mutation status is randomly selected from a disease population in \code{"cli"} design, so his/her family members are less likely to be mutation carriers and have less chance to be affected, whereas the probands are all mutation carriers, their family members have higher chance to be carriers and affected by disease. Therefore, \code{"cli"} design requires more iterations to sample high risk families than \code{"cli+"} design. All designs simulations that include \code{variation = "frailty"} could be also slower in order to generate families with specific familial correlations induced by the chosen frailty distribution.

}
\value{
Returns an object of class \code{'simfam'}, a data frame which contains:
\item{famID}{Family identification (ID) numbers.}
\item{indID}{Individual ID numbers.}
\item{gender}{Gender indicators: 1 for males, 0 for females.}
\item{motherID}{Mother ID numbers.}
\item{fatherID}{Father ID numbers.}
\item{proband}{Proband indicators: 1 if the individual is the proband, 0 otherwise.}
\item{generation}{Individuals generation: 1=parents of probands,2=probands and siblings, 3=children of probands and siblings.}
\item{majorgene}{Genotypes of major gene: 1=AA, 2=Aa, 3=aa where A is disease gene.}
\item{secondgene}{Genotypes of second gene: 1=BB, 2=Bb, 3=bb where B is disease gene.}
\item{ageonset}{Ages at disease onset in years.}
\item{currentage}{Current ages in years.}
\item{time}{Ages at disease onset for the affected or ages of last follow-up for the unaffected. }
\item{status}{Disease statuses: 1 for affected, 0 for unaffected (censored).}
\item{mgene}{Major gene mutation indicators: 1 for mutated gene carriers, 0 for mutated gene noncarriers, or \code{NA} if missing.}
\item{relation}{Family members' relationship with the proband:
  \tabular{ll}{
    1  \tab Proband (self) \cr
    2  \tab Brother or sister \cr
    3  \tab Son or daughter \cr
    4  \tab Parent \cr
    5  \tab Nephew or niece \cr
    6  \tab Spouse \cr
    7  \tab Brother or sister in law
  }
  }
\item{fsize}{Family size including parents, siblings and children of the proband and the siblings.}
\item{naff}{Number of affected members in family.}
\item{weight}{Sampling weights.}


}
\references{
Choi, Y.-H., Briollais, L., He, W. and Kopciuk, K. (2021) FamEvent: An R Package for Generating and Modeling Time-to-Event Data in Family Designs, Journal of Statistical Software 97 (7), 1-30. doi:10.18637/jss.v097.i07

Choi, Y.-H., Kopciuk, K. and Briollais, L. (2008) Estimating Disease Risk Associated Mutated Genes in Family-Based Designs, Human Heredity 66, 238-251.

Choi, Y.-H. and Briollais (2011) An EM Composite Likelihood Approach for Multistage Sampling of Family Data with Missing Genetic Covariates, Statistica Sinica 21, 231-253.
}
\author{
Yun-Hee Choi, Wenqing He
}
\seealso{
\code{\link{summary.simfam}, \link{plot.simfam}, \link{penplot}}
}
\examples{

## Example 1: simulate family data from population-based design using
#  a Weibull distribution for the baseline hazard and inducing 
#  residual familial correlation through a shared gamma frailty.

set.seed(4321)
fam <- simfam(N.fam = 10, design = "pop+", variation = "frailty", 
       base.dist = "Weibull", frailty.dist = "gamma", depend=1, 
       allelefreq = 0.02, base.parms = c(0.01, 3), vbeta = c(-1.13, 2.35))

head(fam) 

\dontrun{
  famID indID gender motherID fatherID proband generation majorgene secondgene
1     1     1      1        0        0       0          1         2          0
2     1     2      0        0        0       0          1         2          0
3     1     3      0        2        1       1          2         2          0
4     1     4      1        0        0       0          0         3          0
5     1     9      0        3        4       0          3         2          0
6     1    10      1        3        4       0          3         3          0
   ageonset currentage     time status mgene relation fsize naff weight
1 103.76925   69.19250 69.19250      0     1        4    18    2      1
2  64.88982   67.31119 64.88982      1     1        4    18    2      1
3  45.84891   47.57119 45.84891      1     1        1    18    2      1
4 269.71990   47.37403 47.37403      0     0        6    18    2      1
5  69.78355   27.80081 27.80081      0     1        3    18    2      1
6 192.09392   25.34148 25.34148      0     0        3    18    2      1
}

summary(fam)

plot(fam, famid = c(1:2)) # pedigree plots for families with IDs = 1 and 2

## Example 2: simulate family data from two stage design to include 
#  30\% of high risk families in the sample. 

set.seed(4321)
fam <- simfam(N.fam = 50, design = "twostage", variation = "none", base.dist = "Weibull", 
       base.parms = c(0.01, 3), vbeta = c(-1.13, 2.35), hr = 0.3, allelefreq = 0.02)

summary(fam)

}
\keyword{Simulation}