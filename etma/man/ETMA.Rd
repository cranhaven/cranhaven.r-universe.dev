\name{ETMA}
\alias{ETMA}
\title{
Epistasis Test in Meta-Analysis (ETMA)
}
\description{
This function is a Markov chain Monte Carlo (MCMC) based method, called "Epistasis Test in Meta-Analysis (ETMA)", using the genotype summary data for estimating a consistent estimate of epistasis in meta-analysis.
}
\usage{
ETMA(case.x1.0, case.x1.1, ctrl.x1.0, ctrl.x1.1,
case.x2.0, case.x2.1, ctrl.x2.0, ctrl.x2.1,
data = NULL, sig.level = 0.05, max.step.EM = 20,
iterations.step1 = 20000, iterations.step2 = 200000, start.seed = NULL,
show.detailed.plot = TRUE, show.final.plot = TRUE,
show.p.matrix = FALSE, progress.bar = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{case.x1.0}{the number of wild type of SNP1 in case group.
%%     ~~Describe \code{case.x1.0} here~~
}
  \item{case.x1.1}{the number of mutation type of SNP1 in case group.
%%     ~~Describe \code{case.x1.1} here~~
}
  \item{ctrl.x1.0}{the number of wild type of SNP1 in control group.
%%     ~~Describe \code{ctrl.x1.0} here~~
}
  \item{ctrl.x1.1}{the number of mutation type of SNP1 in control group.
%%     ~~Describe \code{ctrl.x1.1} here~~
}
  \item{case.x2.0}{the number of wild type of SNP2 in case group.
%%     ~~Describe \code{case.x2.0} here~~
}
  \item{case.x2.1}{the number of mutation type of SNP2 in case group.
%%     ~~Describe \code{case.x2.1} here~~
}
  \item{ctrl.x2.0}{the number of wild type of SNP2 in control group.
%%     ~~Describe \code{ctrl.x2.0} here~~
}
  \item{ctrl.x2.1}{the number of mutation type of SNP1 in control group.
%%     ~~Describe \code{ctrl.x2.1} here~~
}
  \item{data}{an optional data frame, list or environment (or object coercible by as.data.frame to a data frame) containing the variables in the model. If not found in data, the variables are taken from environment(formula), typically the environment from which lm is called.
%%     ~~Describe \code{data} here~~
}
  \item{sig.level}{the significance level used to calculate confidence intervals.
%%     ~~Describe \code{sig.level} here~~
}
  \item{max.step.EM}{the maximum number of iterations if convergence is too slow.
%%     ~~Describe \code{max.step.EM} here~~
}
  \item{iterations.step1}{the length of chain to obtain the study-level parameters [p(disease|base),p(SNP1=1),p(SNP2=1)] in step 1.
%%     ~~Describe \code{iterations.step1} here~~
}
  \item{iterations.step2}{the length of chain to obtain the global-level parameters [OR(SNP1),OR(SNP2),OR(interaction)] in step 2.
%%     ~~Describe \code{iterations.step2} here~~
}
  \item{start.seed}{the start seed of this algorithm (if you want your results can be reproduced). A NULL value means a random seed in this algorithm.
%%     ~~Describe \code{start.seed} here~~
}
  \item{show.detailed.plot}{a logical indicating whether showing the MCMC plot in each step.
%%     ~~Describe \code{show.detailed.plot} here~~
}
  \item{show.final.plot}{a logical indicating whether showing the MCMC plot in the last step.
%%     ~~Describe \code{show.final.plot} here~~
}
  \item{show.p.matrix}{a logical indicating whether a p.matrix should be printed.
%%     ~~Describe \code{show.p.matrix} here~~
}
  \item{progress.bar}{a logical indicating whether a progress bar should be presented.
%%     ~~Describe \code{show.p.matrix} here~~
}
}
\value{
  \item{b}{the beta values of each SNP and interaction term (Sequence is SNP1, SNP2, and interaction).}
  \item{vcov}{the variance covariance matrix of beta value.}
  \item{LKK}{the log of likelihood value in the last step.}
  \item{se}{the stardard errors of each SNP and interaction term (Sequence is SNP1, SNP2, and interaction).}
  \item{df}{the degree of freedom in this analysis.}
  \item{OR}{the odds ratios of each SNP and interaction term (Sequence is SNP1, SNP2, and interaction).}
  \item{ci.l}{the lower bounds of confidence interval based on a specific significance level (please see the sig.level).}
  \item{ci.u}{the upper bounds of confidence interval based on a specific significance level (please see the sig.level).}
  \item{t}{the t value of each each SNP and interaction term (Sequence is SNP1, SNP2, and interaction).}
  \item{pval}{the p value of each each SNP and interaction term (Sequence is SNP1, SNP2, and interaction).}
  \item{sig.level}{the significance level for calculating the confidence interval.}
  \item{p.matrix}{the p matrix in iterations process.}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\author{Chin Lin <xup6fup@gmail.com>
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~
\references{
Lin C, Chu CM, Su SL (2016) Epistasis Test in Meta-Analysis: A Multi-Parameter Markov Chain Monte Carlo Model for Consistency of Evidence. PLoS ONE 11(4): e0152891. doi:10.1371/journal.pone.0152891
%%  ~~ possibly secondary sources and usages ~~
}

\seealso{
\code{\link{data.GST}}, \code{\link{data.PAH}}, \code{\link{data.RAS}}
}
\examples{
#Detailed tutorial is shown in website <http://rpubs.com/chinlin/ETMA>
#The simple toy example (just test this algorithm)
#Note: the computing time in this example is about 3-5 secs

data(data.RAS)
ggint.toy=ETMA(case.ACE.0,case.ACE.1,ctrl.ACE.0,ctrl.ACE.1,
                  case.AGT.0,case.AGT.1,ctrl.AGT.0,ctrl.AGT.1,
                  data=data.RAS,iterations.step1=100,iterations.step2=300,
                  start.seed=1,show.detailed.plot=FALSE,show.final.plot=FALSE)
print(ggint.toy)
summary(ggint.toy)

#Following examples are complete examples.
#They need 20,000/200,000 learning time in step 1/step 2, respectively (default).
#Please note they need more than 15 mins, and one of example need about 3 hrs.
#The complete learning time is necessary in real data analysis.
#Please use default setting as following to analysis your data.
#
#Example 1 (Note: the computing time in this example is about 3 hrs)
#
#data(data.GST)
#ggint1=ETMA(case.GSTM1.0,case.GSTM1.1,ctrl.GSTM1.0,ctrl.GSTM1.1,
#                  case.GSTT1.0,case.GSTT1.1,ctrl.GSTT1.0,ctrl.GSTT1.1,
#                  data=data.GST,start.seed=1,show.detailed.plot=TRUE,show.p.matrix=TRUE)
#print(ggint1)
#summary(ggint1)
#
#Example 2 (Note: the computing time in this example is about 15 mins)
#
#data(data.PAH)
#ggint2=ETMA(case.CYP1A1.0,case.CYP1A1.1,ctrl.CYP1A1.0,ctrl.CYP1A1.1,
#                  case.GSTM1.0,case.GSTM1.1,ctrl.GSTM1.0,ctrl.GSTM1.1,
#                  data=data.PAH,start.seed=1,show.detailed.plot=TRUE,show.p.matrix=TRUE)
#
#print(ggint2)
#summary(ggint2)
#
#Example 3 (Note: the computing time in this example is about 15 mins)
#
#data(data.RAS)
#ggint3=ETMA(case.ACE.0,case.ACE.1,ctrl.ACE.0,ctrl.ACE.1,
#                  case.AGT.0,case.AGT.1,ctrl.AGT.0,ctrl.AGT.1,
#                  data=data.RAS,start.seed=1,show.detailed.plot=TRUE,show.p.matrix=TRUE)
#print(ggint3)
#summary(ggint3)
}
