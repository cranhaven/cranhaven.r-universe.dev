\name{mixcov}
\alias{mixcov}

\title{Fitting mixture models with covariates}


\description{
The function mixcov can be used to estimate the parameters of covariate
adjusted mixture models. This is done using the EM algorithm.
The function first performs the necessary data augmentation and then
applies the EM-algorithm. Covariates may be included as fixed and random
effects into the model. \cr

Thus, the EM algorithm for covariate adjusted mixture models implies to
perform  first the necessary data augmentation, and then  based on
starting values for \code{p[j]} and \code{beta[j]} the computation of posterior
probabilities \code{e[ij]}.  This is the E-step. In the M-step new mixing
weights \code{p[j]} and regression coefficients \code{beta[j]} are computed. 
\cr
\cr
Then the algorithm performs as follows:\cr \
Step 0: Let \code{P} be any vector of  starting values \cr
Step 1: Compute the E-step, that is estimate the probability of component 
membership for each observation \cr
Step 2: Compute the M-step, that is compute new mixing weights and model
coefficients \cr
Proceed until convergence is met.           
}


\usage{
mixcov(dep, fixed, random="", data, k, weight=NULL, pop.at.risk=NULL, 
       var.lnOR=NULL, family="gaussian", maxiter=50, 
       acc=10^-7, returnHomogeneousModel = FALSE) 
    }



\arguments{ 
       \item{dep}{
     dependent variable. Vector or colname of \code{data}. Must be specified!
	}
       \item{fixed}{
     fixed effects. Vector or colname of \code{data}. Must be specified!
	}
	   \item{random}{
     random effects. Vector or colname of \code{data}. Must be specified!
	}
	 \item{k}{
   number of components.
  } 
	\item{weight}{
	weights of the data. Vector or colname of \code{data}. Default is \code{NULL}.
	}
  \item{family}{
        the underlying type density function as a character ("gaussian" or "poisson")!
        }
  \item{data}{
        an optional data frame. \code{obs}, \code{weights}, \code{pop.at.risk} and \code{var.lnOR} can be specified as column name of the data frame. 
        }      
  \item{pop.at.risk}{
  	population at risk: An offset that could be used to determine a mixture model for Poisson data from unequally large populations at risk. Vector or colname of \code{data}. Default is\code{NULL}. 
  }
  \item{var.lnOR}{
    variances of the data: These variances might be given when working with meta analyses! Vector or colname of \code{data}. Default is \code{NULL}.
   }
 \item{maxiter}{
  parameter to control the maximal number of iterations in the VEM and EM loops. Default is 5000. 
 }
\item{acc}{
	convergence criterion. VEM and EM loops stop when deltaLL<acc. Default is 10^(-7). 
}
  
  \item{returnHomogeneousModel}{
  boolean to indicate whether the homogeneous model (simple glm) should be returned too. Default is FALSE.
  }
}


\value{
    The function returns a CAMAN.glm.object (S4-class), describing a mixture model with covariates.
	The main information about the mixture model is printed by just typing the <object>. Additional information is given in \code{summary(object)}. 
	Single attributes can be accessed using the \code{@}, e.g. mix@LL.
   
    \item{dat}{(input) data }\cr
    \item{family}{underlying type density function  }
    \item{LL}{Likelihood of the final (best) iteration }
    \item{BIC}{Likelihood of the final (best) iteration }
    \item{num.k}{number of components obtained }
    \item{p}{probability of each component}
    \item{t}{parameter of distribution (normal distr. -> mean, poisson distr. -> lambda, binomial distr. -> prob) }
    \item{coefMatrix}{complete coefficient matrix }
    \item{prob}{probabilies, belonging to each component }
	\item{steps}{number of steps performed (EM). }
	\item{commonEffect}{common effects}
	\item{hetvar}{heterogeneity variance}
	\item{commonEffect}{common effects}
	\item{classification}{classification labels for each observation (\code{which.max} of prob). }
	\item{cl}{the matched call. }
	\item{fittedObs}{predictions of the fitted model, given the observations}
}



\author{
Peter Schlattmann and Johannes Hoehne  
}


\examples{
### Toy data: simulate subjects with a different relationship between age and salariy
  grps = sample(1:3,70, replace=TRUE) #assign each person to one group 
  salary=NULL
  age = round(runif(70) * 47 + 18)
  #random effects: age has a different influence (slope) on the salary
  salary[grps == 1] = 2000 + 12 * age[grps==1]
  salary[grps == 2] = 4000 + 4 * age[grps==2]
  salary[grps == 3] = 3200 + (-15) * age[grps==3]
  salary = salary + rnorm(70)*30  #some noise
  sex =sample(c("m","w"), 70, replace=TRUE)
  salary[sex=="m"] = salary[sex=="m"] * 1.2 #men earn 20 percent more than women
  salaryData = data.frame(salary=salary, age=age, sex=sex)
  tstSalary <- mixcov(dep="salary", fixed="sex", random="age" ,data=salaryData,
                      k=3,family="gaussian", acc=10^-3)
 

### POISSON data:
data(NoP)
ames3 <- mixcov(dep="count",fixed=c("dose", "logd"),random="",data=NoP,
                k=3,family="poisson")

### Gaussian data
data(betaplasma)
beta4 <- mixcov(dep="betacaro", fixed=c("chol","sex","bmi"), random="betadiet",
                data=betaplasma, k=4, family="gaussian")
}

\concept{meta-analysis}
\concept{covariates}
\concept{mixture model}
\keyword{models}
