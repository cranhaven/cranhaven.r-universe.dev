\name{mining}
\alias{mining}
\alias{centralpar}
\title{
Powerful function that trains and tests a particular fit model under several runs and a given validation method
}
\description{
Powerful function that trains and tests a particular fit model under several runs and a given validation method.
Since there can be a huge number of models, the fitted models are not stored. Yet, several useful statistics (e.g. predictions) are returned.
}
\usage{
mining(x, data = NULL, Runs = 1, method = NULL, model = "default", 
       task = "default", search = "heuristic", mpar = NULL,
       feature="none", scale = "default", transform = "none", 
       debug = FALSE, ...)
}
\arguments{
  \item{x}{a symbolic description (formula) of the model to be fit. If \code{x} contains the data, then \code{data=NULL} (similar to x in \code{\link[kernlab]{ksvm}}, kernlab package).}

  \item{data}{an optional data frame (columns denote attributes, rows show examples) containing the training data, when using a formula.}

  \item{Runs}{number of runs used (e.g. 1, 5, 10, 20, 30)}

  \item{method}{a vector with c(\emph{vmethod},\emph{vpar},\emph{seed}) or c(\emph{vmethod},\emph{vpar},\emph{window},\emph{increment}), 
where \emph{vmethod} is:
                \itemize{
                         \item \code{all} -- all \emph{NROW} examples are used as both training and test sets (no \emph{vpar} or \emph{seed} is needed).
                         \item \code{holdout} -- standard holdout method. If \emph{vpar}<1 then \emph{NROW}*vpar random samples are used for training and the remaining rows are used for testing. Else, then \emph{NROW}*vpar random samples are used for testing and the remaining are used for training. For classification tasks (\code{prob} or \code{class}) a stratified sampling is assumed (equal to \code{mode="stratified"} in \code{\link{holdout}}).
                         \item \code{holdoutrandom} -- similar to \code{holdout} except that assumes always a random sampling (not stratified).
                         \item \code{holdoutorder} -- similar to \code{holdout} except that instead of a random sampling, the first rows (until the split) are used for training and the remaining ones for testing (equal to \code{mode="order"} in \code{\link{holdout}}).
                         \item \code{holdoutinc} -- incremental holdout retraining (e.g. used for stock market data). Here, \emph{vpar} is the test size,
\emph{window} is the initial window size and \emph{increment} is the number of samples added at each iteration. 
Note: argument \code{Runs} is automatically set when this option is used. See also \code{\link{holdout}}.
                         \item \code{holdoutrol} -- rolling holdout retraining (e.g. used for stock market data). Here, \emph{vpar} is the test size,
\emph{window} is the window size and \emph{increment} is the number of samples added at each iteration. 
Note: argument \code{Runs} is automatically set when this option is used. See also \code{\link{holdout}}.
                         \item \code{kfold} -- K-fold cross-validation method, where \emph{vpar} is the number of folds. For classification tasks (\code{prob} or \code{class}) a stratified split is assumed (equal to \code{mode="stratified"} in \code{\link{crossvaldata}}).
                         \item \code{kfoldrandom} -- similar to \code{kfold} except that assumes always a random sampling (not stratified).
                         \item \code{kfoldorder} -- similar to \code{kfold} except that instead of a random sampling, the order of the rows is used to build the folds.
                        }
                \emph{vpar} -- number used by \emph{vmethod} (optional, if not defined 2/3 for \code{holdout} and 10 for \code{kfold} is assumed);\cr
                and \emph{seed} (optional, if not defined then \code{NA} is assumed) is:
                \itemize{
                          \item \code{NA} -- random seed is adopted (default R method for generating random numbers);
                          \item a vector of size \code{Runs} with fixed seed numbers for each Run;
                          \item a number -- \code{set.seed}(\emph{number}) is applied then a vector of seeds (of size Runs) is generated.
                        }
               }
  \item{model}{See \code{\link{fit}} for details.}

  \item{task}{See \code{\link{fit}} for details.}

  \item{search}{See \code{\link{fit}} for details.}

  \item{mpar}{Only kept for compatibility with previous \code{rminer} versions, as you should use \code{search} instead of \code{mpar}. See \code{\link{fit}} for details.\cr
             }

  \item{feature}{See \code{\link{fit}} for more details about \code{feature="none"}, \code{"sabs"} or \code{"sbs"} options.\cr
                 For the \code{mining} function, additional options are \code{feature=}\emph{fmethod}, where \emph{fmethod} can be one of:
                 \itemize{ 
                           \item \code{sens} or \code{sensg} -- compute the 1-D sensitivity analysis input importances (\code{$sen}), gradient measure. 
                           \item \code{sensv} -- compute the 1-D sensitivity analysis input importances (\code{$sen}), variance measure.
                           \item \code{sensr} -- compute the 1-D sensitivity analysis input importances (\code{$sen}), range measure.
                           \item \code{simp}, \code{simpg} or \code{s} -- equal to \code{sensg} but also computes the 1-D sensitivity responses (\code{$sresponses}, 
                                 useful for \code{graph="VEC"}).
                           \item \code{simpv} -- equal to \code{sensv} but also computes the 1-D sensitivity responses (useful for \code{graph="VEC"}).
                           \item \code{simpr} -- equal to \code{sensr} but also computes the 1-D sensitivity responses (useful for \code{graph="VEC"}).
                      }
                }

  \item{scale}{See \code{\link{fit}} for details.}

  \item{transform}{See \code{\link{fit}} for details.}

  \item{debug}{If TRUE shows some information about each run.}

  \item{\dots}{See \code{\link{fit}} for details.}

}

\details{
Powerful function that trains and tests a particular fit model under several runs and a given validation method
(see [Cortez, 2010] for more details).\cr
Several \code{Runs} are performed. In each run, the same validation method is adopted (e.g. \code{holdout}) and
several relevant statistics are stored. Note: this function can require some computational effort, specially if
a large dataset and/or a high number of \code{Runs} is adopted.
}

\value{
A \code{list} with the components:
\itemize{
\item $object -- fitted object values of the last run (used by multiple model fitting: "auto" mode). For "holdout", it is equal to a \code{fit} object, while for "kfold" it is a list.
\item $time -- vector with time elapsed for each run.
\item $test -- vector list, where each element contains the test (target) results for each run.
\item $pred -- vector list, where each element contains the predicted results for each test set and each run.
\item $error -- vector with a (validation) measure (often it is a error value) according to \code{search$metric} for each run (valid options are explained in \code{\link{mmetric}}).
\item $mpar -- vector list, where each element contains the fit model mpar parameters (for each run).
\item $model -- the \code{model}.
\item $task -- the \code{task}.
\item $method -- the external validation \code{method}.
\item $sen -- a matrix with the 1-D sensitivity analysis input importances. The number of rows is \code{Runs} times \emph{vpar}, if \code{kfold}, else is \code{Runs}.
\item $sresponses -- a vector list with a size equal to the number of attributes (useful for \code{graph="VEC"}). 
Each element contains a list with the 1-D sensitivity analysis input responses
(\code{n} -- name of the attribute; \code{l} -- number of levels; \code{x} -- attribute values; \code{y} -- 1-D sensitivity responses.\cr
                  Important note: sresponses (and "VEC" graphs) are only available if \code{feature="sabs"} or \code{"simp"} related (see \code{feature}).
\item $runs -- the \code{Runs}.
\item $attributes -- vector list with all attributes (features) selected in each run (and fold if \code{kfold}) if a feature selection algorithm is used.
\item $feature -- the \code{feature}.
        }
}
\references{
\itemize{
\item To check for more details about rminer and for citation purposes:\cr
P. Cortez.\cr
Data Mining with Neural Networks and Support Vector Machines Using the R/rminer Tool.\cr
In P. Perner (Ed.), Advances in Data Mining - Applications and Theoretical Aspects 10th Industrial Conference on Data Mining (ICDM 2010), Lecture Notes in Artificial Intelligence 6171, pp. 572-583, Berlin, Germany, July, 2010. Springer. ISBN: 978-3-642-14399-1.\cr
@Springer: \url{https://link.springer.com/chapter/10.1007/978-3-642-14400-4_44}\cr
\url{http://www3.dsi.uminho.pt/pcortez/2010-rminer.pdf}\cr

\item This tutorial shows additional code examples:\cr
P. Cortez.\cr
A tutorial on using the rminer R package for data mining tasks.\cr
Teaching Report, Department of Information Systems, ALGORITMI Research Centre, Engineering School, University of Minho, Guimaraes, 
Portugal, July 2015.\cr
\url{http://hdl.handle.net/1822/36210}\cr

\item For the grid search and other optimization methods:\cr
P. Cortez.\cr
Modern Optimization with R.\cr
Use R! series, Springer, 2nd edition, July 2021, ISBN 978-3-030-72818-2.\cr
\url{https://link.springer.com/book/10.1007/978-3-030-72819-9}\cr
}
}
\author{
Paulo Cortez \url{http://www3.dsi.uminho.pt/pcortez/}
}
\note{
See also \url{http://hdl.handle.net/1822/36210} and \url{http://www3.dsi.uminho.pt/pcortez/rminer.html}
}

\seealso{
\code{\link{fit}}, \code{\link{predict.fit}}, \code{\link{mparheuristic}}, \code{\link{mgraph}}, \code{\link{mmetric}}, \code{\link{savemining}}, \code{\link{holdout}} and \code{\link{Importance}}.
}

\examples{
### dontrun is used when the execution of the example requires some computational effort.

### simple regression example
set.seed(123); x1=rnorm(200,100,20); x2=rnorm(200,100,20)
y=0.7*sin(x1/(25*pi))+0.3*sin(x2/(25*pi))
# mining with an ensemble of neural networks, each fixed with size=2 hidden nodes
# assumes a default holdout (random split) with 2/3 for training and 1/3 for testing:
M=mining(y~x1+x2,Runs=2,model="mlpe",search=2)
print(M)
print(mmetric(M,metric="MAE"))

### more regression examples:
\dontrun{
# simple nonlinear regression task; x3 is a random variable and does not influence y:
data(sin1reg)
# 5 runs of an external holdout with 2/3 for training and 1/3 for testing, fixed seed 12345
# feature selection: sabs method
# model selection: 5 searches for size, internal 2-fold cross validation fixed seed 123
#                  with optimization for minimum MAE metric 
M=mining(y~.,data=sin1reg,Runs=5,method=c("holdout",2/3,12345),model="mlpe",
         search=list(search=mparheuristic("mlpe",n=5),method=c("kfold",2,123),metric="MAE"),
         feature="sabs")
print(mmetric(M,metric="MAE"))
print(M$mpar)
print("median hidden nodes (size) and number of MLPs (nr):")
print(centralpar(M$mpar))
print("attributes used by the model in each run:")
print(M$attributes)
mgraph(M,graph="RSC",Grid=10,main="sin1 MLPE scatter plot")
mgraph(M,graph="REP",Grid=10,main="sin1 MLPE scatter plot",sort=FALSE)
mgraph(M,graph="REC",Grid=10,main="sin1 MLPE REC")
mgraph(M,graph="IMP",Grid=10,main="input importances",xval=0.1,leg=names(sin1reg))
# average influence of x1 on the model:
mgraph(M,graph="VEC",Grid=10,main="x1 VEC curve",xval=1,leg=names(sin1reg)[1])
}

### regression example with holdout rolling windows:
\dontrun{
# simple nonlinear regression task; x3 is a random variable and does not influence y:
data(sin1reg)
# rolling with 20 test samples, training window size of 300 and increment of 50 in each run:
# note that Runs argument is automatically set to 14 in this example:
M=mining(y~.,data=sin1reg,method=c("holdoutrol",20,300,50),
         model="mlpe",debug=TRUE)
}

### regression example with all rminer models:
\dontrun{
# simple nonlinear regression task; x3 is a random variable and does not influence y:
data(sin1reg)
models=c("naive","ctree","rpart","kknn","mlp","mlpe","ksvm","randomForest","mr","mars",
         "cubist","pcr","plsr","cppls","rvm")
for(model in models)
{ 
 M=mining(y~.,data=sin1reg,method=c("holdout",2/3,12345),model=model)
 cat("model:",model,"MAE:",round(mmetric(M,metric="MAE")$MAE,digits=3),"\n")
}
}

### classification example (task="prob")
\dontrun{
data(iris)
# 10 runs of a 3-fold cross validation with fixed seed 123 for generating the 3-fold runs
M=mining(Species~.,iris,Runs=10,method=c("kfold",3,123),model="rpart")
print(mmetric(M,metric="CONF"))
print(mmetric(M,metric="AUC"))
print(meanint(mmetric(M,metric="AUC")))
mgraph(M,graph="ROC",TC=2,baseline=TRUE,Grid=10,leg="Versicolor",
       main="versicolor ROC")
mgraph(M,graph="LIFT",TC=2,baseline=TRUE,Grid=10,leg="Versicolor",
       main="Versicolor ROC")
M2=mining(Species~.,iris,Runs=10,method=c("kfold",3,123),model="ksvm")
L=vector("list",2)
L[[1]]=M;L[[2]]=M2
mgraph(L,graph="ROC",TC=2,baseline=TRUE,Grid=10,leg=c("DT","SVM"),main="ROC")
}

### other classification examples
\dontrun{
### 1st example:
data(iris)
# 2 runs of an external 2-fold validation, random seed
# model selection: SVM model with rbfdot kernel, automatic search for sigma,
#                  internal 3-fold validation, random seed, minimum "AUC" is assumed
# feature selection: none, "s" is used only to store input importance values
M=mining(Species~.,data=iris,Runs=2,method=c("kfold",2,NA),model="ksvm",
         search=list(search=mparheuristic("ksvm"),method=c("kfold",3)),feature="s")

print(mmetric(M,metric="AUC",TC=2))
mgraph(M,graph="ROC",TC=2,baseline=TRUE,Grid=10,leg="SVM",main="ROC",intbar=FALSE)
mgraph(M,graph="IMP",TC=2,Grid=10,main="input importances",xval=0.1,
leg=names(iris),axis=1)
mgraph(M,graph="VEC",TC=2,Grid=10,main="Petal.Width VEC curve",
data=iris,xval=4)
### 2nd example, ordered kfold, k-nearest neigbor:
M=mining(Species~.,iris,Runs=1,method=c("kfoldo",3),model="knn")
# confusion matrix:
print(mmetric(M,metric="CONF"))

### 3rd example, use of all rminer models: 
models=c("naive","ctree","rpart","kknn","mlp","mlpe","ksvm","randomForest","bagging",
         "boosting","lda","multinom","naiveBayes","qda")
for(model in models)
{ 
 M=mining(Species~.,iris,Runs=1,method=c("kfold",3,123),model=model)
 cat("model:",model,"ACC:",round(mmetric(M,metric="ACC")$ACC,digits=1),"\n")
}
}

### multiple models: automl or ensembles 
\dontrun{

data(iris)
d=iris
names(d)[ncol(d)]="y" # change output name
inputs=ncol(d)-1
metric="AUC"

# simple automl (1 search per individual model),
# internal holdout and external holdout:
sm=mparheuristic(model="automl",n=NA,task="prob",inputs=inputs)
mode="auto"

imethod=c("holdout",4/5,123) # internal validation method
emethod=c("holdout",2/3,567) # external validation method

search=list(search=sm,smethod=mode,method=imethod,metric=metric,convex=0)
M=mining(y~.,data=d,model="auto",search=search,method=emethod,fdebug=TRUE)
# 1 single model was selected:
cat("best",emethod[1],"selected model:",M$object@model,"\n")
cat(metric,"=",round(as.numeric(mmetric(M,metric=metric)),2),"\n")

# simple automl (1 search per individual model),
# internal kfold and external kfold: 
imethod=c("kfold",3,123) # internal validation method
emethod=c("kfold",5,567) # external validation method
search=list(search=sm,smethod=mode,method=imethod,metric=metric,convex=0)
M=mining(y~.,data=d,model="auto",search=search,method=emethod,fdebug=TRUE)
# kfold models were selected:
kfolds=as.numeric(emethod[2])
models=vector(length=kfolds)
for(i in 1:kfolds) models[i]=M$object$model[[i]]
cat("best",emethod[1],"selected models:",models,"\n")
cat(metric,"=",round(as.numeric(mmetric(M,metric=metric)),2),"\n")

# example with weighted ensemble:
M=mining(y~.,data=d,model="WE",search=search,method=emethod,fdebug=TRUE)
for(i in 1:kfolds) models[i]=M$object$model[[i]]
cat("best",emethod[1],"selected models:",models,"\n")
cat(metric,"=",round(as.numeric(mmetric(M,metric=metric)),2),"\n")

}


### for more fitting examples check the help of function fit: help(fit,package="rminer")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{classif}
\keyword{regression}
\keyword{neural}
\keyword{nonlinear}
