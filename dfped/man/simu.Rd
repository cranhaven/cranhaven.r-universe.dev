\name{simu}
\alias{simu}
\alias{crmAtj}

\title{
A simulation of a single dose-finding trials in paediatrics.
}

\description{
Simulate a single dose-finding clinical trial with the given scenarios of toxicity and efficacy. 
}

\usage{
    simu(targetTox, targetEff, skeletonTox, skeletonEff, startingDose, 
        nbSubjects, crmModel, cohortSize, scenarioTox, scenarioEff, 
        nbDesign, mu, sd = NULL, lesb, sigmaLI, sigmaHI, adaptivePrior) 
}

\arguments{
    \item{targetTox}{
        Target/threshold of toxicity; must be a integer/double.   
    }
    \item{targetEff}{
        Target/threshold of efficacy; must be a integer/double.   
    }
    \item{skeletonTox}{
        Skeleton of toxicity for the BMA bivariate CRM, or the bivariate CRM. Must be a dataframe with the number of row corresponding to the number of doses and the number of columns corresponding to the number of working models for toxicity.  
    }  
    \item{skeletonEff}{
        Skeleton of efficacy for the BMA bivariate CRM, or the bivariate CRM. Must be a dataframe with the number of row corresponding to the number of doses and the number of columns corresponding to the number of working models for efficacy.
    }
    \item{startingDose}{ 
        First dose to be assigned; must be an integer.    
    }
    \item{nbSubjects}{
        Maximum number of allocated patients; must be an integer.
    }
    \item{crmModel}{
        A model for STAN in C++.    
    } 
    \item{cohortSize}{ 
        The size of the cohorts for the 3+3 based algorithm before kickoff of the CRM; must be an integer.   
    }  
    \item{scenarioTox}{
        Toxicity scenario for the simulations with the probability of toxicity for each dose; must be a vector of length the number of doses.   
    }
    \item{scenarioEff}{
        Efficacy scenario for the simulations; must be a vector of length the number of doses. 
    } 
    \item{nbDesign}{
        The number of different designs for the model selection using the Watanabe-Akaike information criteria (WAIC); must be an integer.
    }
    \item{mu}{
        The mean value which the model is using.
    } 
    \item{sd}{ 
        The standard deviation.
    }
    \item{lesb}{ 
        A vector consisting of the variables b.  
    } 
    \item{sigmaLI}{
        The standard deviation when the model using non-informative prior.
    }
    \item{sigmaHI}{
        The standard deviation when the model using informative prior.  
    }
    \item{adaptivePrior}{
        TRUE if you want to use as a prior an adaptive prior; FALSE otherwise.   
    }  
} 

\references{
    Petit, C., et al, (2016) Unified approach for extrapolation and bridging of adult information in early phase dose-finding paediatric studies, Statistical Methods in Medical Research, <doi:10.1177/0962280216671348>.
}

\author{
    Artemis Toumazi \email{artemis.toumazi@gmail.com}, 
    Caroline Petit \email{caroline.petit@crc.jussieu.fr}, 
    Sarah Zohar \email{sarah.zohar@inserm.fr}
} 

\seealso{
    \code{\link{simulation}}
}  

\examples{ 
\dontrun{ 
    library(rstan)
    adaptivePrior <- TRUE

    ####### Targets  ###########

    targetTox <- 0.25   # target of toxicity
    targetEff <- 0.20   # target of efficacy 

    ####### Skeleton ###########

    skeleton_tox1 <- c(0.10, 0.21, 0.33, 0.55, 0.76) 
    skeleton_tox2 <- c(0.21, 0.33, 0.55, 0.76, 0.88) 
    skeleton_tox3 <- c(0.05, 0.10, 0.21, 0.33, 0.55) 
    skeleton_tox4 <- c(0.025, 0.05,0.1, 0.21, 0.33) 
    skeleton_tox5 <- c(0.0125, 0.025, 0.05,0.1, 0.21) 

    skeleton_eff <- c(0.04937516, 0.20496890, 0.43388003, 0.64409781, 0.79313693) 
    skeleton_tox <-  data.frame(skeleton_tox1, skeleton_tox2, skeleton_tox3,
                        skeleton_tox4, skeleton_tox5)
    skeleton_eff <-  data.frame(skeleton_eff, skeleton_eff, skeleton_eff,
                        skeleton_eff, skeleton_eff)  

    ############# Priors ############
    priorModel  <- list(rep(1/5,5), 0.001)
    sd <- 0.65  
    mu <- -0.34   

    ####### Trial settings  #############

    startingDose <- 1 
    nbSubjects <- 15   
    cohortSize <- 3       

    nbDesign <- length(skeleton_tox[1,])  
    nbDoses <- length(scenario_tox)       

    lesb <- calcul.bi(skeleton_tox[,1], mu, a = NULL, "power_log", targetTox) 
    sigmaLI <- sigmaLI(skeleton_tox[,1],  mu, a = NULL, "power_log", targetTox)
    sigmaHI <- sigmaHI(skeleton_tox[,1],  mu, a = NULL, "power_log", targetTox, 0.80)

    ################## Scenarios ##############

    scenario_tox <- c(0.1301477, 0.2774171, 0.4184642, 0.6486846, 0.8257219)  
    scenario_eff <- c(0.07945205, 0.20000000, 0.33686856, 0.59537737, 0.80996173)  

    stancode <- 'data {
        int <lower = 0> J; //nb of patients
        int <lower = 0> K; // nb of doses and dose reference
        real r[K]; // skeleton for tox - K doses
        real q[K]; // skeleton for efficacy - K doses
        int y[J]; // toxicity of patient j
        int v[J]; // efficacy of patient j
        int d[J]; // dose received by patient j
        real moy; // mean for the normal prior of toxicity
        real standardError; //standard error of the normal prior of toxicity
    }
    parameters {
        real <lower = 0> alpha;
        real  <lower = 0> beta;
    }
    transformed parameters{
        real <lower = 0, upper = 1> varphi[K]; // marginal probability of toxicity for dose k
        real <lower = 0, upper = 1> psi[K]; // marginal probability of efficacy for dose k
        // defining the marginal probabilities for each value of a and b for each dose
        real p01[K]; // tox = 0, eff = 1
        real p10[K]; // tox = 1, eff = 0
        real p11[K]; // tox = 1, eff = 1
        real p00[K]; // tox = 0, eff = 0

        vector[J] logLike;

        for (k in 1:K){
            varphi[k] = exp(alpha*log(r[k]));
            psi[k] = exp(beta*log(q[k]));
        }
        
        
        // computing the marginal probabilities for each dose
            for (k in 1:K){
                p01[k] = (1-varphi[k])*psi[k]; 
                p10[k] = varphi[k]*(1-psi[k]);
                p00[k] = (1-varphi[k])*(1-psi[k]);
                p11[k] = varphi[k]*psi[k];
            }
        // Computing the log-likelihood
            for (j in 1:J){
                logLike[j] = y[j]*v[j]*log(p11[d[j]]) + y[j]*(1-v[j])*log(p10[d[j]]) 
                + (1-y[j])*v[j]*log(p01[d[j]]) + (1-y[j])*(1-v[j])*log(p00[d[j]]);
            }
        
    }
    model {
        // priors
        alpha ~lognormal(moy, standardError);
        beta ~ lognormal(0,sqrt(1.34));              
        increment_log_prob(sum(logLike));
    }'
    
    crm_model <- stan_model(model_code = stancode) 

    ################## Simulation ##############

    simu(targetTox, targetEff, skeleton_tox, skeleton_eff,
        startingDose, nbSubjects, crm_model, cohortSize, scenario_tox,
        scenario_eff, nbDesign, mu, sd = sd, lesb,
        sigmaLI, sigmaHI, adaptivePrior)
}
}
