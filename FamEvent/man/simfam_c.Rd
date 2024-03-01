\name{simfam_c}
\alias{simfam_c}
\title{
Generate familial competing risks data
}
\description{
Generates familial competing risks data for specified study design, genetic model and source of residual familial correlation; the generated data frame also contains family structure (individual's id, father id, mother id, relationship to proband, generation), gender, current age, genotypes of major or second genes. Similar to the simfam function, except with the addition of the \code{competing} option, which provides competing risks data when \code{competing = TURE}.
}
\usage{
simfam_c(N.fam, design = "pop+", variation = "none", interaction = FALSE, depend = NULL, 
       base.dist = c("Weibull", "Weibull"), frailty.dist = "none", 
       base.parms=list(c(0.016, 3),c(0.016, 3)), 
       vbeta=list(c(-1.13, 2.35), c(-1, 2)), competing=TRUE,
       allelefreq = c(0.02, 0.2), dominant.m = TRUE, dominant.s = TRUE,
       mrate = 0, hr = 0, probandage = c(45, 2), agemin = 20, agemax = 100)
% \method{summary}{simulation}(object, \dots)
}

\arguments{
  \item{N.fam}{
Number of families to generate.
}
  \item{design}{
Family based study design used in the simulations. Possible choices are: \code{"pop"}, \code{"pop+"}, \code{"cli"}, \code{"cli+"} or \code{"twostage"}, where \code{"pop"} is for the population-based design that families are ascertained by affected probands, \code{"pop+"} is similar to \code{"pop"} but with mutation carrier probands, \code{"cli"} is for the clinic-based design that includes affected probands with at least one parent and one sib affected, \code{"cli+"} is similar to \code{"cli"} but with mutation carrier probands and \code{"twostage"} for two-stage design that randomly samples families from the population in the first stage and oversamples high risk families in the second stage that include at least two affected members in the family. Default is \code{"pop+"}.
}
  \item{variation}{
Source of residual familial correlation.  Possible choices are: \code{"frailty"} for frailty shared within families, \code{"secondgene"} for second gene variation, or \code{"none"} for no residual familial correlation. Default is \code{"none"}.
}
 \item{interaction}{
Logical; if \code{TRUE}, allows the interaction between gender and mutation status.
For \code{competing=TRUE}, two logical values should be specified for each event; if only one logical value is provided, the same logical value will be assumed for both event.
Default is \code{FALSE}.
}
 \item{depend}{
Inverse of variance for the frailty distribution. Dependence within families decreases with depend value.  Default is \code{NULL}. Positive real values should be specified when \code{variation="frailty"}. With \code{competing = TRUE}, two values shoud be specified for each event when \code{frailty.dist = "gamma"} or \code{frailty.dist = "lognormal"}, three values should be specified with code{frailty.dist = "cgamma"} or \code{frailty.dist = "clognormal"}. The first two values represent the inverse of variance for each event and the thir value represents the correlation between the two events.
}
\item{base.dist}{
Choice of baseline hazard distribution.  Possible choices are: \code{"Weibull"}, \code{"loglogistic"}, \code{"Gompertz"}, \code{"lognormal"} \code{"gamma"}, \code{"logBurr"}. Default is \code{"Weibull"}. When \code{competing = TRUE}, two distributions should be specified for each event. If only one distribution is specified, the same distribution will be assumed for both events.
}
\item{frailty.dist}{
Choice of frailty distribution.  Possible choices are: \code{"gamma"} or \code{"lognormal"} when \code{variation="frailty"}. Default is \code{NULL}. 
When \code{competing = TRUE}, possible choices are \code{"gamma"} for independent gamma, \code{"lognormal"} for independent lognormal, \code{"cgamma"} for correlated gamma, or \code{"clognormal"} for correlated lognormal distribution.
}
  \item{base.parms}{
Vector of parameter values for the specified baseline hazard function. \code{base.parms=c(lambda, rho)} should be specified for \code{base.dist="Weibull"}, \code{"loglogistic"}, \code{"Gompertz"}, \code{"gamma"}, and \code{"lognormal"}. For \code{base.dist="logBurr"}, three parameters should be specified \code{base.parms = c(lambda, rho, eta)}. 
When code{competing = TRUE}, list of two vectors of baseline parameters for each event should be specified. For example, \code{base.parms=list(c(lambda1, rho1), c(lambda2, rho2))} should be specified for \code{base.dist=c("Weibull", "Weibull")}.
}  	
\item{vbeta}{
Vector of regression coefficients for gender, majorgene, interaction between gender and majorgene (if \code{interaction = TRUE}), and secondgene (if \code{variation = "secondgene"}). 
When code{competing = TRUE}, list of two vectors of regression coefficients for each event should be specified. 

}
\item{competing}{
Logical; if \code{TRUE}, allows to generate two competing events. Default is \code{TRUE}.
}
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

\emph{Competing risk model}

Event 1:

\ifelse{html}{
\out{<p style="text-align:center"> h<sub>1</sub>(t|X,Z) = h<sub>01</sub>(t - t<sub>0</sub>) Z<sub>1</sub> exp(&beta;<sub>s1</sub> * x<sub>s</sub> + &beta;<sub>g1</sub> * x<sub>g</sub>), </p>}}{	
		\deqn{ h(t|X,Z_1) = h_{01}(t - t_0) Z \exp(\beta_{s1} x_s+\beta_{g1} x_g) ,}{ h(g|X,Z) = h01(t - t0) * Z1 * exp(\beta s1 * xs + \beta g1 * xg) ,}}

Event 2:

\ifelse{html}{
\out{<p style="text-align:center"> h<sub>2</sub>(t|X,Z) = h<sub>02</sub>(t - t<sub>0</sub>) Z<sub>2</sub> exp(&beta;<sub>s2</sub> * x<sub>s</sub> + &beta;<sub>g2</sub> * x<sub>g</sub>), </p>}}{	
		\deqn{ h(t|X,Z_1) = h_{02}(t - t_0) Z \exp(\beta_{s2} x_s+\beta_{g2} x_g) ,}{ h(g|X,Z) = h02(t - t0) * Z1 * exp(\beta s2 * xs + \beta g2 * xg) ,}}
		
where \ifelse{html}{\out{h<sub>01</sub>(t)}}{\eqn{h_{01}(t)}{h01(t)}} and \ifelse{html}{\out{h<sub>02</sub>(t)}}{\eqn{h_{02}(t)}{h02(t)}} are the baseline hazard functions for event 1 and event 2, respectively, \ifelse{html}{\out{t<sub>0</sub>}}{\eqn{t_0}{t0}} is a minimum age of disease onset, \ifelse{html}{\out{Z<sub>1</sub>}}{\eqn{Z_1}{Z1}} and \ifelse{html}{\out{Z<sub>2</sub>}}{\eqn{Z_2}{Z2}} are frailties shared within families for each event and follow either a gamma, log-normal, correlateg gamma, or correlated log-normal distributions, \ifelse{html}{\out{x<sub>x</sub>}}{\eqn{x_s}{xs}} and \ifelse{html}{\out{x<sub>g</sub>}}{\eqn{x_g}{xg}} indicate male (1) or female (0) and carrier (1) or non-carrier (0) of a main gene of interest, respectively.

\emph{Choice of frailty distributions for competing risk models}

\code{frailty.dist = "gamma"} shares the frailties within families generated from a gamma distribution independently for each competing event, where
\ifelse{html}{\out{Z<sub>j</sub>}}{\eqn{Z_j}{Zj}} follows Gamma(\ifelse{html}{\out{k<sub>j</sub>}}{\eqn{k_j}{kj}}, \ifelse{html}{\out{1/k<sub>j</sub>}}{\eqn{1/k_j}{1/kj}}). 

\code{frailty.dist = "lognormal"} shares the frailties within families generated from a log-normal distribution independently for each competing event, where 
\ifelse{html}{\out{Z<sub>j</sub>}}{\eqn{Z_j}{Zj}} follows log-normal distribution with mean 0 and variance (\ifelse{html}{\out{1/k<sub>j</sub>}}{\eqn{1/k_j}{1/kj}}.

\code{frailty.dist = "cgamma"} shares the frailties within families generated from a correlated gamma distribution to allow the frailties between two events to be correlated, where the correlated gamma frailties (\ifelse{html}{\out{Z<sub>1</sub>, Z<sub>2</sub>}}{\eqn{Z_1, Z_2}{Z1, Z2}}) are generated with three independent gamma frailties (\ifelse{html}{\out{Y<sub>0</sub>, Y<sub>1</sub>, Y<sub>2</sub>}}{\eqn{Y_0, Y_1, Y_2}{Y0, Y1, Y2}}) as follows:

\ifelse{html}{\out{Z<sub>1</sub> = k<sub>0</sub>/(k<sub>0</sub> + k<sub>1</sub>) Y<sub>0</sub> + Y<sub>1</sub>} }{\eqn{Z_1 = (k_0/(k_0 + k_1) Y_0 + Y_1}{Z1 = k0/(k0+k1)Y0 + Y1} }

\ifelse{html}{\out{Z<sub>2</sub> = k<sub>0</sub>/(k<sub>0</sub> + k<sub>2</sub>) Y<sub>0</sub> + Y<sub>2</sub>} }{\eqn{Z_2 = (k_0/(k_0 + k_2) Y_0 + Y_2}{Z2 = k0/(k0+k2)Y0 + Y2} }

where \ifelse{html}{\out{Y<sub>0</sub>}}{\eqn{Y_0}{Y0}} from Gamma(\ifelse{html}{\out{k<sub>0</sub>}}{\eqn{k_0}{k0}}, \ifelse{html}{\out{1/k<sub>0</sub>}}{\eqn{1/k_0}{1/k0}});

\ifelse{html}{\out{Y<sub>1</sub>}}{\eqn{Y_1}{Y1}} from Gamma(\ifelse{html}{\out{k<sub>1</sub>}}{\eqn{k_1}{k1}}, \ifelse{html}{\out{1/(k<sub>0</sub> + k<sub>1</sub>)}}{\eqn{1/(k_0+k_1)}{1/(k0+k1)}});

\ifelse{html}{\out{Y<sub>2</sub>}}{\eqn{Y_2}{Y2}} from Gamma(\ifelse{html}{\out{k<sub>2</sub>}}{\eqn{k_2}{k2}}, \ifelse{html}{\out{1/(k<sub>0</sub> + k<sub>2</sub>)}}{\eqn{1/(k_0+k_2)}{1/(k0+k2)}}).

\code{frailty.dist = "clognormal"} shares the frailties within families generated from a correlated log-normal distribution where 
\ifelse{html}{\out{log(Z<sub>j</sub>)}}{\eqn{log(Z_j)}{log(Zj)}} follows  a normal distribution with mean 0, variance \ifelse{html}{\out{1/k<sub>j</sub>}}{\eqn{1/k_j}{1/kj}} and correlation between two events \ifelse{html}{\out{k<sub>0</sub>}}{\eqn{k_0}{k0}}.

\code{depend} should specify the values of related frailty parameters: \code{c(k1, k2)} with \code{frailty.dist = "gamma"} or \code{frailty.dist = "lognormal"}; \code{c(k1, k2, k0)} for \code{frailty.dist = "cgamma"} or \code{frailty.dist = "clognormal"}. 


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
\item{status}{Disease statuses: 1 for affected, 0 for unaffected (censored).
When \code{competing = TRUE}, 1 for affected by event 1, 2 for affected by event 2, 0 for unaffected (censored).
}
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
\item{naff}{Number of affected members within family. When \code{competing = TRUE}, \code{naff} represents the number of members affected by either event 1 or 2 within family.}
\item{df1}{Number of members affected by event 1 within family when \code{competing = TRUE}.}
\item{df2}{Number of members affected by event 2 within family when \code{competing = TRUE}.}
\item{weight}{Sampling weights.}


}
\references{
Choi, Y.-H., Briollais, L., He, W. and Kopciuk, K. (2021) FamEvent: An R Package for Generating and Modeling Time-to-Event Data in Family Designs, Journal of Statistical Software 97 (7), 1-30. doi:10.18637/jss.v097.i07.

Choi, Y.-H., Jung, H., Buys, S., Daly, M., John, E.M., Hopper, J., Andrulis, I., Terry, M.B., Briollais, L. (2021) A Competing Risks Model with Binary Time Varying Covariates for Estimation of Breast Cancer Risks in BRCA1 Families, Statistical Methods in Medical Research 30 (9), 2165-2183. https://doi.org/10.1177/09622802211008945.

Choi, Y.-H., Kopciuk, K. and Briollais, L. (2008) Estimating Disease Risk Associated Mutated Genes in Family-Based Designs, Human Heredity 66, 238-251.

Choi, Y.-H. and Briollais (2011) An EM Composite Likelihood Approach for Multistage Sampling of Family Data with Missing Genetic Covariates, Statistica Sinica 21, 231-253.
}
\author{
Yun-Hee Choi
}
\seealso{
\code{\link{summary.simfam_c}, \link{plot.simfam_c}, \link{penplot_c}}
}
\examples{

## Example 1: simulate competing risk family data from pop+ design using
#  Weibull distribution for both baseline hazards and inducing 
#  residual familial correlation through a correlated gamma frailty.

set.seed(4321)
fam <- simfam_c(N.fam = 10, design = "pop+", variation = "frailty", 
       base.dist = "Weibull", frailty.dist = "cgamma", depend=c(1, 2, 0.5), 
       allelefreq = 0.02, base.parms = list(c(0.01, 3), c(0.01, 3)), 
       vbeta = list(c(-1.13, 2.35), c(-1, 2)))


head(fam) 

\dontrun{
  famID indID gender motherID fatherID proband generation majorgene secondgene  ageonset
1     1     1      1        0        0       0          1         3          0 124.23752
2     1     2      0        0        0       0          1         2          0  54.66936
3     1     3      0        2        1       1          2         2          0  32.75208
4     1     4      1        0        0       0          0         3          0 136.44926
5     1    11      1        3        4       0          3         3          0  71.53672
6     1    12      1        3        4       0          3         3          0 152.47073
  currentage     time status true_status mgene relation fsize naff df1 df2 weight
1   65.30602 65.30602      0           2     0        4    25    2   1   1      1
2   68.62107 54.66936      1           1     1        4    25    2   1   1      1
3   47.07842 32.75208      2           2     1        1    25    2   1   1      1
4   45.09295 45.09295      0           2     0        6    25    2   1   1      1
5   25.32819 25.32819      0           1     0        3    25    2   1   1      1
6   22.95059 22.95059      0           2     0        3    25    2   1   1      1
}

summary(fam)

plot(fam, famid = 1) # pedigree plots for family with ID = 1

}
\keyword{Simulation}