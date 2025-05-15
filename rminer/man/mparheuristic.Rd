\name{mparheuristic}
\alias{mparheuristic}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function that returns a list of searching (hyper)parameters for a particular model (classification or regression) or for a multiple list of models (automl or ensembles).
}
\description{
Easy to use function that returns a list of searching (hyper)parameters for a particular model (classification or regression) or for a multiple list of models (automl or ensembles). 
The result is to be put in a \code{search} argument, used by \code{\link{fit}} or \code{\link{mining}} functions. Something
like:\cr \code{search=list(search=mparheuristic(...),...)}.
}
\usage{
mparheuristic(model, n = NA, lower = NA, upper = NA, by = NA, exponential = NA, 
              kernel = "rbfdot", task = "prob", inputs = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{model type name. See \code{\link{fit}} for the individual model details (e.g., \code{"ksvm"}). For multiple models use:
               \itemize{
                        \item \code{automl} - 5 individual machine learning algorithms: generalized linear model (GLM, via \code{cv.glmnet}), support vector machine (SVM, via \code{ksvm}), multilayer perceptron (MLP, via \code{mlpe}), random forest (RF, via \code{randomForest}) and extreme gradient boosting (XG, via \code{xgboost}). The \code{n="heuristic"} setting (see below) is assumed for all algorithms, thus just one hyperparameter is tested for each model. This option is thus the fastest automl to run.
                        \item \code{automl2} - same 5 individual machine learning algorithms as \code{automl}. For each algorithm, a grid search is executed with 10 searches (same as:\cr \code{n="heuristic10"}), except for \code{ksvm}, which uses 13 searches of an uniform design (\code{"UD"}).
                        \item \code{automl3} - same as \code{automl2} except that a six extra stacking ensemble (\code{"SE"}) model is performed using the 5 best tuned algorithm versions (GLM, SVM, MLP, RF and XG).
                        \item a character vector with several models - see the example section for a demonstration of this option.
                       }
              }
  \item{n}{number of searches or heuristic or numeric vector (either \code{n} or \code{by} should be used, \code{n} has prevalence over \code{by}). By default, the searches are linear for all models except for SVM several \code{rbfdot} kernel based models (\code{"ksvm"},\code{"rsvm"},\code{"lssvm"}, which can assume \code{2^}search-range; please check the result of this function to confirm if the search is linear or \code{2^}search-range).\cr
If this argument is 1 then the main hyperparameter of the model is set to the value of \code{lower} (if lower is not NA).\cr
If this argument is a numeric vector, then the main hyperparameter of the model is set to this vector.\cr
If this argument is a character type, then it is assumed to be an heuristic. Possible heuristic values are:
\itemize{
           \item \code{heuristic} - only one model is fit, uses default rminer values, same as \code{mparheuristic(model)}.
           \item \code{heuristic5} - 5 hyperparameter searches from lower to upper, only works for the following models: 
\code{ctree}, \code{rpart}, \code{kknn}, \code{ksvm}, \code{lssvm}, \code{mlp}, \code{mlpe}, \code{randomForest}, \code{multinom}, \code{rvm}, \code{xgboost}.
Notes:
\code{rpart} - different \code{cp} values (see \code{\link[rpart]{rpart.control}}); 
\code{ctree} - different \code{mincriterion} values (see \code{\link[party]{ctree_control}}); 
\code{randomForest} -- upper argument is limited by the number of \code{inputs} (\code{mtry} is searched); 
\code{ksvm}, \code{lssvm} or \code{rvm} - the optional \code{kernel} argument can be used.
           \item \code{heuristic10} - same as \code{heuristic5} but with 10 searches from \code{lower} to \code{upper}.
           \item \code{UD} or \code{UD1} - \code{UD} or \code{UD1} uniform design search (only for \code{ksvm} and \code{rbfdof} kernel). This option assumes 2 hyperparameters for classification (sigma, C) and 3 hyperparameters (sigma, C, epsilon) for regression, thus \code{task="reg"} argument needs to be set when regression is used.
           \item \code{xgb9} - 9 searches (3 for \code{eta} and 3 for \code{max_depth}, works only when \code{model=xgboost}.
           \item \code{mlp_t} - heuristic 33 from Delgado 2014 paper, 10 searches, works only when \code{model=mlp} or \code{model=mlpe}.
           \item \code{avNNet_t} - heuristic 34 from Delgado 2014 paper, 9 searches, works only when \code{model=mlpe}.
           \item \code{nnet_t} - heuristic 36 from Delgado 2014 paper, 25 searches, works only when \code{model=mlp} or \code{model=mlpe}.
           \item \code{svm_C} - heuristic 48 from Delgado 2014 paper, 130 searches (may take time), works only when \code{model=ksvm}.
           \item \code{svmRadial_t} - heuristic 52 from Delgado 2014 paper, 25 searches, works only when \code{model=ksvm}.
           \item \code{svmLinear_t} - heuristic 54 from Delgado 2014 paper, 5 searches, works only when \code{model=ksvm}.
           \item \code{svmPoly_t} - heuristic 55 from Delgado 2014 paper, 27 searches, works only when \code{model=ksvm}.
           \item \code{lsvmRadial_t} - heuristic 56 from Delgado 2014 paper, 10 searches, works only when \code{model=lssvm}.
           \item \code{rpart_t} - heuristic 59 from Delgado 2014 paper, 10 searches, works only when \code{model=rpart}.
           \item \code{rpart2_t} - heuristic 60 from Delgado 2014 paper, 10 searches, works only when \code{model=rpart}.
           \item \code{ctree_t} - heuristic 63 from Delgado 2014 paper, 10 searches, works only when \code{model=ctree}.
           \item \code{ctree2_t} - heuristic 64 from Delgado 2014 paper, 10 searches, works only when \code{model=ctree}.
           \item \code{rf_t} - heuristic 131 from Delgado 2014 paper, 10 searches, works only when \code{model=randomForest}.
           \item \code{knn_R} - heuristic 154 from Delgado 2014 paper, 19 searches, works only when \code{model=kknn}.
           \item \code{knn_t} - heuristic 155 from Delgado 2014 paper, 10 searches, works only when \code{model=kknn}.
           \item \code{multinom_t} - heuristic 167 from Delgado 2014 paper, 10 searches, works only when \code{model=multinom}.
}
}
  \item{lower}{lower bound for the (hyper)parameter (if \code{NA} a default value is assumed). If \code{n=1} and \code{!is.na(lower)} then lower is the main hyperparameter value for some models (e.g., "kknn", "mlpe", "ksvm", "xgboost").
}
  \item{upper}{upper bound for the (hyper)parameter (if \code{NA} a default value is assumed).
}
  \item{by}{increment in the sequence (if \code{NA} a default value is assumed depending on \code{n}).
}
  \item{exponential}{if an exponential scale should be used in the search sequence (the \code{NA} is a default value that assumes a linear scale unless \code{model} is a support vector machine).
}
  \item{kernel}{optional kernel type, only used when \code{model="ksvm"}, \code{model="rsvm"} or \code{model="lssvm"}. Currently mapped kernels are \code{"rbfdot"} (Gaussian), 
\code{"polydot"} (polynomial) and \code{"vanilladot"} (linear); see \code{\link[kernlab]{ksvm}} for kernel details.
}
  \item{task}{optional task argument, only used for uniform design (\code{UD} or \code{UD1}) (with \code{"ksvm"} and \code{"rbfdot"}).
}
  \item{inputs}{optional inputs argument: the number of inputs, only used by \code{"randomForest"}.
}
}
\details{
This function facilitates the definition of the \code{search} argument used by \code{\link{fit}} or \code{\link{mining}} functions.
Using simple heuristics, reasonable (hyper)parameter search values are suggested for several rminer models. For models not
mapped in this function, the function returns \code{NULL}, which means that no hyperparameter search is executed (often,
this implies using rminer or R function default values).

The simple usage of \code{heuristic} assumes lower and upper bounds for a (hyper)parameter. If \code{n=1}, then rminer or R defaults are assumed.
Else, a search is created using \code{seq(lower,upper,by)}, where \code{by} was set by the used or computed from \code{n}.
For some \code{model="ksvm"} setups, \code{2^seq(...)} is used for sigma and C, \code{(1/10)^seq(...)} is used for scale. 
Please check the resulting object to inspect the obtained final search values.

This function also allows to easily set multiple model searches, under the: "automl", "automl2", "automl3" or vector character options (see below examples).
}
\value{
A list with one ore more (hyper)parameter values to be searched.
}
\references{
\itemize{
\item To check for more details about rminer and for citation purposes:\cr
P. Cortez.\cr
Data Mining with Neural Networks and Support Vector Machines Using the R/rminer Tool.\cr
In P. Perner (Ed.), Advances in Data Mining - Applications and Theoretical Aspects 10th Industrial Conference on Data Mining (ICDM 2010), Lecture Notes in Artificial Intelligence 6171, pp. 572-583, Berlin, Germany, July, 2010. Springer. ISBN: 978-3-642-14399-1.\cr
@Springer: \url{https://link.springer.com/chapter/10.1007/978-3-642-14400-4_44}\cr
\url{http://www3.dsi.uminho.pt/pcortez/2010-rminer.pdf}\cr

\item The automl is inspired in this work:\cr
L. Ferreira, A. Pilastri, C. Martins, P. Santos, P. Cortez.\cr
An Automated and Distributed Machine Learning Framework for Telecommunications Risk Management.
In J. van den Herik et al. (Eds.), 
Proceedings of 12th International Conference on Agents and Artificial Intelligence -- ICAART 2020, Volume 2, pp. 99-107,
Valletta, Malta, February, 2020, SCITEPRESS, ISBN 978-989-758-395-7.\cr
@INSTICC: \url{https://www.insticc.org/Primoris/Resources/PaperPdf.ashx?idPaper=89528}\cr

\item This tutorial shows additional code examples:\cr
P. Cortez.\cr
A tutorial on using the rminer R package for data mining tasks.\cr
Teaching Report, Department of Information Systems, ALGORITMI Research Centre, Engineering School, University of Minho, Guimaraes, 
Portugal, July 2015.\cr
\url{http://hdl.handle.net/1822/36210}\cr

\item Some lower/upper bounds and heuristics were retrieved from:\cr
M. Fernandez-Delgado, E. Cernadas, S. Barro and D. Amorim.
Do we need hundreds of classifiers to solve real world classification problems?. 
In The Journal of Machine Learning Research, 15(1), 3133-3181, 2014.
}

}
\author{
Paulo Cortez \url{http://www3.dsi.uminho.pt/pcortez/}
}
\note{
See also \url{http://hdl.handle.net/1822/36210} and \url{http://www3.dsi.uminho.pt/pcortez/rminer.html}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{fit}} and \code{\link{mining}}.
}
\examples{
## "kknn"
s=mparheuristic("kknn",n="heuristic")
print(s) 
s=mparheuristic("kknn",n=1) # same thing
print(s) 
s=mparheuristic("kknn",n="heuristic5")
print(s) 
s=mparheuristic("kknn",n=5) # same thing
print(s)
s=mparheuristic("kknn",lower=5,upper=15,by=2)
print(s)
# exponential scale:
s=mparheuristic("kknn",lower=1,upper=5,by=1,exponential=2)
print(s)

## "mlpe"
s=mparheuristic("mlpe")
print(s) # "NA" means set size with min(inputs/2,10) in fit
s=mparheuristic("mlpe",n="heuristic10")
print(s) 
s=mparheuristic("mlpe",n=10) # same thing
print(s) 
s=mparheuristic("mlpe",n=10,lower=2,upper=20) 
print(s) 

# numeric (single number or vector) usage of n:
s=mparheuristic("mlpe",n=NA) #
print(s) 
s=mparheuristic("mlpe",n=1,lower=NA) # same thing
print(s) 
s=mparheuristic("mlpe",n=1,lower=2) # size=2
print(s) 
s=mparheuristic("mlpe",n=1:9) # size=1:9
print(s) 


## "randomForest", upper should be set to the number of inputs = max mtry
s=mparheuristic("randomForest",n=10,upper=6)
print(s) 

## "ksvm"
s=mparheuristic("ksvm",n=10)
print(s) 
s=mparheuristic("ksvm",n=10,kernel="vanilladot")
print(s) 
s=mparheuristic("ksvm",n=10,kernel="polydot")
print(s) 

## lssvm
s=mparheuristic("lssvm",n=10)
print(s) 

## rvm 
s=mparheuristic("rvm",n=5)
print(s) 
s=mparheuristic("rvm",n=5,kernel="vanilladot")
print(s) 

## "rpart" and "ctree" are special cases (see help(fit,package=rminer) examples):
s=mparheuristic("rpart",n=3) # 3 cp values
print(s) 
s=mparheuristic("ctree",n=3) # 3 mincriterion values
print(s) 

### examples with fit
\dontrun{
### classification
data(iris)
# ksvm and rbfdot:
model="ksvm";kernel="rbfdot"
s=mparheuristic(model,n="heuristic5",kernel=kernel)
print(s) # 5 sigma values
search=list(search=s,method=c("holdout",2/3,123))
# task "prob" is assumed, optimization of "AUC":
M=fit(Species~.,data=iris,model=model,search=search,fdebug=TRUE)
print(M@mpar)

# different lower and upper range:
s=mparheuristic(model,n=5,kernel=kernel,lower=-5,upper=1)
print(s) # from 2^-5 to 2^1 
search=list(search=s,method=c("holdout",2/3,123))
# task "prob" is assumed, optimization of "AUC":
M=fit(Species~.,data=iris,model=model,search=search,fdebug=TRUE)
print(M@mpar)

# different exponential scale: 
s=mparheuristic(model,n=5,kernel=kernel,lower=-4,upper=0,exponential=10)
print(s) # from 10^-5 to 10^1 
search=list(search=s,method=c("holdout",2/3,123))
# task "prob" is assumed, optimization of "AUC":
M=fit(Species~.,data=iris,model=model,search=search,fdebug=TRUE)
print(M@mpar)

# "lssvm" Gaussian model, pure classification and ACC optimization, full iris:
model="lssvm";kernel="rbfdot"
s=mparheuristic("lssvm",n=3,kernel=kernel)
print(s)
search=list(search=s,method=c("holdout",2/3,123))
M=fit(Species~.,data=iris,model=model,search=search,fdebug=TRUE)
print(M@mpar)

# test several heuristic5 searches, full iris:
n="heuristic5";inputs=ncol(iris)-1
model=c("ctree","rpart","kknn","ksvm","lssvm","mlpe","randomForest")
for(i in 1:length(model))
 {
  cat("--- i:",i,"model:",model[i],"\n")
  if(model[i]=="randomForest") s=mparheuristic(model[i],n=n,upper=inputs) 
  else s=mparheuristic(model[i],n=n)
  print(s)
  search=list(search=s,method=c("holdout",2/3,123))
  M=fit(Species~.,data=iris,model=model[i],search=search,fdebug=TRUE)
  print(M@mpar)
 }


# test several Delgado 2014 searches (some cases launch warnings):
model=c("mlp","mlpe","mlp","ksvm","ksvm","ksvm",
        "ksvm","lssvm","rpart","rpart","ctree",
        "ctree","randomForest","kknn","kknn","multinom")
n=c("mlp_t","avNNet_t","nnet_t","svm_C","svmRadial_t","svmLinear_t",
    "svmPoly_t","lsvmRadial_t","rpart_t","rpart2_t","ctree_t",
    "ctree2_t","rf_t","knn_R","knn_t","multinom_t")
inputs=ncol(iris)-1
for(i in 1:length(model))
 {
  cat("--- i:",i,"model:",model[i],"heuristic:",n[i],"\n")
  if(model[i]=="randomForest") s=mparheuristic(model[i],n=n[i],upper=inputs) 
  else s=mparheuristic(model[i],n=n[i])
  print(s)
  search=list(search=s,method=c("holdout",2/3,123))
  M=fit(Species~.,data=iris,model=model[i],search=search,fdebug=TRUE)
  print(M@mpar)
 }
} #dontrun

### regression
\dontrun{
data(sa_ssin)
s=mparheuristic("ksvm",n=3,kernel="polydot")
print(s)
search=list(search=s,metric="MAE",method=c("holdout",2/3,123))
M=fit(y~.,data=sa_ssin,model="ksvm",search=search,fdebug=TRUE)
print(M@mpar)

# regression task, predict iris "Petal.Width":
data(iris)
ir2=iris[,1:4]
names(ir2)[ncol(ir2)]="y" # change output name
n=3;inputs=ncol(ir2)-1 # 3 hyperparameter searches
model=c("ctree","rpart","kknn","ksvm","mlpe","randomForest","rvm")
for(i in 1:length(model))
 {
  cat("--- i:",i,"model:",model[i],"\n")
  if(model[i]=="randomForest") s=mparheuristic(model[i],n=n,upper=inputs)
  else s=mparheuristic(model[i],n=n)
  print(s)
  search=list(search=s,method=c("holdout",2/3,123))
  M=fit(y~.,data=ir2,model=model[i],search=search,fdebug=TRUE)
  print(M@mpar)
 }
} #dontrun

### multiple model examples:
\dontrun{
data(iris)
inputs=ncol(iris)-1; task="prob"

# 5 machine learning (ML) algorithms, 1 heuristic hyperparameter per algorithm:
sm=mparheuristic(model="automl",task=task,inputs=inputs)
print(sm)

# 5 ML with 10/13 hyperparameter searches:
sm=mparheuristic(model="automl2",task=task,inputs=inputs)
# note: mtry only has 4 searches due to the inputs limit:
print(sm)

# regression example:
ir2=iris[,1:4]
inputs=ncol(ir2)-1; task="reg"
sm=mparheuristic(model="automl2",task=task,inputs=inputs)
# note: ksvm contains 3 UD hyperparameters (and not 2) since task="reg": 
print(sm)

# 5 ML and stacking:
inputs=ncol(iris)-1; task="prob"
sm=mparheuristic(model="automl3",task=task,inputs=inputs)
# note: $ls only has 5 elements, one for each individual ML 
print(sm)

# other manual design examples: --------------------------------------

# 5 ML and three ensembles:
# the fit or mining functions will search for the best option
# between any of the 5 ML algorithms and any of the three 
# ensemble approaches:
sm2=mparheuristic(model="automl3",task=task,inputs=inputs)
# note: ensembles need to be at the end of the $models field:
sm2$models=c(sm2$models,"AE","WE") # add AE and WE
sm2$smethod=c(sm2$smethod,rep("grid",2)) # add grid to AE and WE
# note: $ls only has 5 elements, one for each individual ML 
print(sm2)

# 3 ML example:
models=c("cv.glmnet","mlpe","ksvm") # just 3 models
# note: in rminer the default cv.glmnet does not have "hyperparameters"
# since the cv automatically sets lambda 
n=c(NA,10,"UD") # 10 searches for mlpe and 13 for ksvm 
sm3=mparheuristic(model=models,n=n)
# note: $ls only has 5 elements, one for each individual ML 
print(sm3)

# usage in sm2 and sm3 for fit (see mining help for usages in mining):
method=c("holdout",2/3,123)
d=iris
names(d)[ncol(d)]="y" # change output name
s2=list(search=sm2,smethod="auto",method=method,metric="AUC",convex=0)
M2=fit(y~.,data=d,model="auto",search=s2,fdebug=TRUE)

s3=list(search=sm3,smethod="auto",method=method,metric="AUC",convex=0)
M3=fit(y~.,data=d,model="auto",search=s3,fdebug=TRUE)
# -------------------------------------------------------------------
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{classif}
\keyword{regression}
\keyword{neural}
\keyword{nonlinear}
