\name{DHGLMMODELING}
\alias{DHGLMMODELING}

\docType{package}

\title{Defining the Fixed and Random Models for the Mean and Dispersion parameters in DHGLMs}

\description{The DHGLMMODELING specifies a GLM, HGLM, DHGLM model for the mean parameters (mu), and a GLM, HGLM 
model for the overdispersion parameters (phi). GLM for mu, and GLM for phi are specified by adding only fixed terms to the linear predictors 
for the mu and phi, respectively.
These are extended to HGLM for mu and HGLM for phi by adding some random terms. The DHGLM for mu is specified by allowing random effects 
for the variance of random effects in HGLM for mu.
The LMatrix argument allows correlation structures to be defined for random terms. This is done by setting LMatrix to a matrix L
that is used as a post-multiplier for the Z matrix of the corresponding random term.
}

\usage{
DHGLMMODELING(Model="mean",Link=NULL,LinPred="constant",RandDist=NULL,
Offset=NULL,LMatrix=NULL,LinkRandVariance=NULL,LinPredRandVariance=NULL,
RandDistRandVariance="gaussian",
LinkRandVariance2=NULL,LinPredRandVariance2=NULL)}

\arguments{
\item{Model}{This option specifies a GLM, HGLM or DHGLM model for mu when Model="mean" (default), 
and a GLM or HGLM for phi when Model="dispersion".}

\item{Link}{The link function for the linear predictor is
specified by the option Link. For Model="mean", Link can be "identity",
"logit", "probit", "cloglog", "log", or "inverse". 
For Model="dispersion", the choice is either "log" or "inverse". 
The default, specified as NULL link, is "identity" for Model="mean" and
"log" for Model="dispersion".}

\item{LinPred}{The option LinPred specifies the fixed and
random terms for the linear predictor for mu when specified as Model="mean"
or for phi when Model="dispersion". For Model="mean", LinPred=y~x1+x2+(1|id1)+(1|id2)
specifies y as the main response, x1 and x2 as fixed covariates and id1 and id2 as random terms.
For Model="dispersion", the main response should be phi, e.g. phi~x1+x2+(1|id1)+(1|id2). This option
can specify the model without random effects, e.g., LinPred=phi~x1+x2. The default is "constant", which is set 
to intercept only the model for the corresponding linear predictors.}

\item{RandDist}{The option RandDist specifies the
distributions of the random terms represented in the option LinPred.
It is set as a vector of distribution names from "gaussain" (default),
"beta", "gamma", or "inverse-gamma" when Model="mean".
For Model="dispersion", the choice is "gaussian" (default), "gamma", or "inverse-gamma". 
When more than one random terms are specified, e.g., \code{y~x1+x2+(1|id1)+(1|id2)}
in the option LinPred, the different distributions for each random term can be specified, e.g.,
c("gaussian", "gamma"), which assumes normal distribution for
the random term "id1" and gamma distribution for the random term "id2", respectively.}

\item{Offset}{The option Offset can be used to
specify a known component to be included in the linear predictor specified by LinPred
during fitting. This should be the default (NULL) or a numeric
vector of length equal to that of the appropriate data.}

\item{LMatrix}{The option LMatrix sets a matrix that is used as a
post-multiplier for the model matrix of the corresponding random effects. This
option allows correlation structures to be defined for random effects. For
example, when specified as Model="mean" and Lmatrix=L1+L2, the
linear predictor for mu takes X beta + Z1 L1 r1 + Z2 L2 r2, where
Z1 and Z2 are the model matrices for the random effects v1=L1 r1 and
v2=L2 r2, specified in the option LinPred.}

\item{LinkRandVariance}{The option LinkRandVariance
specifies the link function for the linear
predictor of the random-effect variances.
The choice is either "log" (default) or "inverse". When more than two random terms are specified in the option LinPred,
the user can set different link functions, e.g., LinkRandVariance=c("log","inverse") for each random term.}

\item{LinPredRandVariance}{The option LinPredRandVariance specifies the
fixed and random terms for the linear predictor of the random-effect
variances for Model="mean". When y~x1+x2+(1|id1)+(1|id2) is specified in the option LinPred, 

LinPredRandVariance=c(lambda~xx1+(1|id11),lambda~xx2+(1|id12))
specifies xx1 and xx2 as fixed covariates and id11 and id12 as random terms in the lienar predictors for the variances of the random terms id1 and id2, respectively.
For Model="dispersion", the random term is not allowed in the linear predictor of the random-effect variance.
The default (NULL) is set to intercept only model for the corresponding linear predictors.}

\item{RandDistRandVariance}{The option RandDistRandVariance specifies the
distributions for the random terms in the LinPredRandVariance.
The choice is "gaussian" (default), "gamma", or "inverse-gamma".}

\item{LinkRandVariance2}{This option specifies the link function for the linear
predictor of the variance of random effects, which are specified in the option
LinPredRandVariance. The choice is either "log" (default) or "inverse".}

\item{LinPredRandVariance2}{This option specifies the fixed terms for the linear
predictor of the variance of random effects, which is specified in the option LinPredRandVariance. 
For example, when LinPredRandVariance=c(lambda~xx1+(1|id11),lambda~xx1+(1|id12))
is specified, LinPredRandVariance2=c(~xxx1,~xxx2) specifies
xxx1 and xxx2 as fixed covariates for the linear predictor of random-effect variances for id11 and id12, respectively.
The default (NULL) is set to constant variance for the random effects in LinPredRandVariance.}
}

