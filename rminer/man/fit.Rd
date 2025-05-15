\name{fit}
\alias{fit} %- Also NEED an '\alias' for EACH other topic documented here.
\alias{model-class} %- Also NEED an '\alias' for EACH other topic documented here.
\title{
Fit a supervised data mining model (classification or regression) model
}
\description{
Fit a supervised data mining model (classification or regression) model. Wrapper function that allows to fit distinct data mining (16 classification
and 18 regression) methods under the same coherent function structure. 
Also, it tunes the hyperparameters of the models (e.g., \code{kknn}, \code{mlpe} and \code{ksvm}) and performs some feature selection methods.
}
\usage{
fit(x, data = NULL, model = "default", task = "default", 
    search = "heuristic", mpar = NULL, feature = "none", 
    scale = "default", transform = "none", 
    created = NULL, fdebug = FALSE, ...)
}

\arguments{
  \item{x}{a symbolic description (formula) of the model to be fit.\cr 
           If \code{data=NULL} it is assumed that \code{x} contains a formula expression with known variables (see first example below).}
  \item{data}{an optional data frame (columns denote attributes, rows show examples) containing the training data, when using a formula.}
  \item{model}{Typically this should be a character object with the model type name (data mining method, as explained in valid character options).\cr
              \cr
              First usage: individual fit. Valid character options are the typical R base learning functions (individual models), namely one of: 
              \itemize{
              \item \code{naive} most common class (classification) or mean output value (regression)

              \item \code{ctree} -- conditional inference tree (classification and regression, uses \code{\link[party]{ctree}})
              \item \code{cv.glmnet} -- generalized linear model (GLM) with lasso or elasticnet regularization (classification and regression, uses \code{\link[glmnet]{cv.glmnet}}; note: cross-validation is used to automatically set the lambda parameter that is needed to compute the predictions)
              \item \code{rpart} or \code{dt} -- decision tree (classification and regression, uses \code{\link[rpart]{rpart}})
              \item \code{kknn} or \code{knn} -- k-nearest neighbor (classification and regression, uses \code{\link[kknn]{kknn}})
              \item \code{ksvm} or \code{svm} -- support vector machine (classification and regression, uses \code{\link[kernlab]{ksvm}})
              \item \code{lssvm}  -- least squares support vector machine (pure classification only, uses \code{\link[kernlab]{lssvm}})
              \item \code{mlp} -- multilayer perceptron with one hidden layer (classification and regression, uses \code{\link[nnet]{nnet}} (in this version, for both \code{mlp} and \code{mlpe}, the maximum number of weights was increased and fixed to \code{MaxNWts=10000}))
 
              \item \code{mlpe} -- multilayer perceptron ensemble (classification and regression, uses \code{\link[nnet]{nnet}})
              \item \code{randomForest} or \code{randomforest} -- random forest algorithm (classification and regression, uses \code{\link[randomForest]{randomForest}})
              \item \code{xgboost} -- eXtreme Gradient Boosting (Tree) (classification and regression, uses \code{\link[xgboost]{xgboost}}; note: \code{nrounds} parameter is set by default to 2)

              \item \code{bagging} -- bagging from Breiman, 1996 (classification, uses \code{\link[adabag]{bagging}})
              \item \code{boosting} -- adaboost.M1 method from Freund and Schapire, 1996 (classification, uses \code{\link[adabag]{boosting}})
              \item \code{lda} -- linear discriminant analysis (classification, uses \code{\link[MASS]{lda}})
              \item \code{multinom} or \code{lr} -- logistic regression (classification, uses \code{\link[nnet]{multinom}})
              \item \code{naiveBayes} or \code{naivebayes} -- naive bayes (classification, uses \code{\link[e1071]{naiveBayes}})
              \item \code{qda} -- quadratic discriminant analysis (classification, uses \code{\link[MASS]{qda}})

              \item \code{cubist} -- M5 rule-based model (regression, uses \code{\link[Cubist]{cubist}})
              \item \code{lm} -- standard multiple/linear regression (uses \code{\link{lm}})
              \item \code{mr} -- multiple regression (regression, equivalent to \code{\link{lm}} but uses \code{\link[nnet]{nnet}} with zero hidden nodes and linear output function)
              \item \code{mars} -- multivariate adaptive regression splines (regression, uses \code{\link[mda]{mars}})
              \item \code{pcr} -- principal component regression (regression, uses \code{\link[pls]{pcr}})
              \item \code{plsr} -- partial least squares regression (regression, uses \code{\link[pls]{plsr}})
              \item \code{cppls} -- canonical powered partial least squares (regression, uses \code{\link[pls]{cppls}})
              \item \code{rvm} -- relevance vector machine (regression, uses \code{\link[kernlab]{rvm}})
                      }
              Second usage: multiple models. \code{model} can be used to perform Automated Machine Learning (AutoML) or ensembles of several individual models:
              \itemize{
              \item \code{auto} -- first, the best model is automatically set by searching all models defined in \code{search} and selecting the one with the best ``validation'' metric on a validation set (depending on the method defined in \code{search}); then, the selected best model is fit to all training data. When \code{auto} is used, a ranked leaderboard of the models (and their selected hyperparameters) is returned as a new \code{$LB} field of the \code{@mpar} returned slot (e.g., try: \code{print(M@mpar$LB)}, where \code{M} is an object returned by \code{fit}).
              \item \code{AE}, \code{WE} or \code{SE} -- all individual models are first fit to the data; then an ensemble is built by: \code{AE} -- Average Ensemble, majority (if \code{task=="class"}) or average of the predictions; \code{WE}) -- Weighted Ensemble, similar to \code{AE} but each prediction is weighted according to the validation metric 
(for  \code{task=="class"} it is equal to \code{AE}); \code{SE} -- Stacking Ensemble, applies a second-level GLM to weight the individual predictions. For any ensemble, when an individual model produces an error then it is excluded from the ensemble. After excluding invalid models, if there is just a single model then such model is returned (and no ensemble is produced).
                      }
              Third usage: \code{model} can be a \code{\link{list}} with 2 possibilities of fields A) and B).\cr
              A) if you have your one fit function, then you can embed it using:
              \itemize{
              \item \code{$fit} -- a fit function that accepts the arguments \code{x}, \code{data} and \code{...}, the goal is to accept here any R classification or regression model, mainly for its use within the \code{\link{mining}} or \code{\link{Importance}} functions, or to use a hyperparameter search (via \code{search}).
              \item \code{$predict} -- a predict function that accepts the arguments \code{object}, \code{newdata}, this function should behave as any rminer prediction, i.e., return: a factor when \code{task=="class"}; a matrix with \emph{Probabilities x Instances} when \code{task=="prob"}; and a vector when \code{task=="reg"}.
              \item \code{$name} -- optional field with the name of the method.
                      }
              B) automatically produced by some ensemble methods, for the sake of documentation the fields for the ensembles ("AE", "WE" or "SE") are listed here:
               \itemize{
              \item \code{$m} -- a vector character with the fit object model names. 
              \item \code{$f} -- a vector list with several fit objects.
              \item \code{$w} -- a vector with the "weighting" of the individual models.
                      }
              Note: current rminer version emphasizes the use of native fitting functions from their respective packages, since these functions contain several specific hyperparameters that can now be searched or set using the \code{search} or \code{...} arguments. For compatibility with previous rminer versions, older \code{model} options are kept.
              }
  \item{task}{data mining task. Valid options are:
              \itemize{
                       \item \code{prob} (or \code{p}) -- classification with output probabilities (i.e. the sum of all outputs equals 1).
                       \item \code{class} (or \code{c}) -- classification with discrete outputs (\code{\link{factor}}) 
                       \item \code{reg} (or \code{r}) -- regression (numeric output) 
                       \item \code{default} tries to guess the best task (\code{prob} or \code{reg}) given the \code{model} and output variable type (if factor then \code{prob} else \code{reg})
                      }
             }
  \item{search}{used to tune hyperparameter(s) of the model, such as: \code{kknn} -- number of neighbors (k); 
							\code{mlp} or \code{mlpe} -- number of hidden nodes (\emph{size}) or \emph{decay};
							\code{ksvm} -- gaussian kernel parameter (\emph{sigma});
							\code{randomForest} -- \code{mtry} parameter).\cr 
               This is a very flexible argument that can be used under several options: simpler use, complex tuning of an individual model or multiple models. The simpler use is kept for compatibility issues but it is advised to define this argument via the easier \code{\link{mparheuristic}} function.\cr
               Valid options for a simpler \bold{character type} \code{search} use:
              \itemize{
                       \item \code{heuristic} -- simple heuristic, one search parameter (e.g., \emph{size}=inputs/2 for \code{mlp} or 
\emph{size}=10 if classification and inputs/2>10, \emph{sigma} is set using \code{kpar="automatic"} and \code{kernel="rbfdot"} 
of \code{\link[kernlab]{ksvm}}). Important Note: instead of the "heuristic" options, it is advisable to use the explicit \code{\link{mparheuristic}} function
that is designed for a wider option of models (all "heuristic" options were kept due to compatibility issues and work 
only for: \code{kknn}; \code{mlp} or \code{mlpe}; \code{ksvm}, with \code{kernel="rbfdot"}; and \code{randomForest}). 
                       \item \code{heuristic5} -- heuristic with a 5 range grid-search (e.g., \code{seq(1,9,2)} for \code{kknn}, \code{seq(0,8,2)} for \code{mlp} or \code{mlpe}, 
     										 	     \code{2^seq(-15,3,4)} for \code{ksvm}, \code{1:5} for \code{randomRorest})
                       \item \code{heuristic10} -- heuristic with a 10 range grid-search (e.g., \code{seq(1,10,1)} for \code{kknn}, \code{seq(0,9,1)} for \code{mlp} or \code{mlpe}, 
     										 	     \code{2^seq(-15,3,2)} for \code{ksvm}, \code{1:10} for \code{randomRorest})
                       \item \code{UD}, \code{UD1} or \code{UD2} -- uniform design 2-Level with 13 (\code{UD} or \code{UD2}) or 21 (\code{UD1}) searches (only works for \code{ksvm} and \code{kernel="rbfdot"}).
                      } 
                Another simpler use of the \code{search} argument is a \bold{numeric type}:
              \itemize{
                       \item a-\code{\link{vector}} -- numeric vector with all hyperparameter values that will be searched within an internal grid-search (the number of searches is \code{length(search)} when \code{convex=0})
                      }
                      A more complex but advised use of \code{search} is to use a \code{\link{list}}. Non expert users should create this list via the \code{\link{mparheuristic}} function, which is very easy to use. Nevertheless, the fields of the list for a single fit (individual model) are shown here:
              \itemize{
                       \item \code{$smethod} -- type of search method. Valid options are:
                                          \itemize{
                                                   \item \code{none} -- no search is executed, one single fit is performed.
                                                   \item \code{matrix} -- matrix search (tests only n searches, all search parameters are of size n).
                                                   \item \code{grid} -- normal grid search (tests all combinations of search parameters).
                                                   \item \code{2L} - nested 2-Level grid search. First level range is set by \code{$search} and then the 2nd level performs a fine tuning, with \code{length($search)} searches around (original range/2) best value in first level (2nd level is only performed on numeric searches).
                                                   \item \code{UD}, \code{UD1} or \code{UD2} -- uniform design 2-Level with 13 (\code{UD} or \code{UD2}) or 21 (\code{UD1}) searches (note: only works for \code{model="ksvm"} and \code{kernel="rbfdot"}). Under this option,
\code{$search} should contain the first level ranges, such as \code{c(-15,3,-5,15)} for classification (\emph{gamma} min and max, \emph{C} min and max, 
after which a \code{2^} transform is applied) or \code{c(-8,0,-1,6,-8,-1)} for regression (last two values are \emph{epsilon} min and max, after which a \code{2^} transform is applied).
                                                  }
                        \item \code{$search} -- a-\code{\link{list}} with all hyperparameter values to be searched or character with previous described options 
(e.g., "heuristic", "heuristic5", "UD"). If a character, then \code{$smethod} equal to \code{"none"} or \code{"grid"} or \code{"UD"} is automatically assumed.
                        \item \code{$convex} -- number that defines how many searches are performed after a local minimum/maximum is found (if >0, the search can be stopped without testing all grid-search values) 
                        \item \code{$method} -- type of internal (validation) estimation method used during the search (see \code{method} argument of \code{\link{mining}} for details)
                        \item \code{$metric} -- used to compute a metric value during internal estimation. Can be a single character such as \code{"SAD"} or a list with all the arguments used by the \code{mmetric} function except \code{y} and \code{x}, such as:\cr
\code{search$metric=list(metric="AUC",TC=3,D=0.7)}. See \code{\link{mmetric}} for more details.
                      }
                      A more sophisticated definition of \code{search} involves the tuning of several models (used by the \code{model=} \code{auto}, \code{AE}, \code{WE} or \code{SE}). Again, this sophisticated definition should be automatically set using the \code{\link{mparheuristic}} function. The list of fields for the multiple tuning mode are:
		      \itemize{
                               \item \code{$models} - a vector character with LM individual \code{model} values. This field can also include ensembles (\code{"AE"}, \code{"WE"}, \code{"SE"}) provided they appear at the end of this vector. They will work if more than one valid individual model is included. 
                               \item \code{$ls} - a vector list with LM search values (for each individual model, the values are the same as in individual search \code{$search} field). 
                               \item \code{$smethod} - must have the \code{auto} value.
                               \item \code{$smethod} - must have the \code{auto} value.
                               \item \code{$method} - internal (validation) estimation method (equal to the individual search \code{$method} field).
                               \item \code{$metric} - internal (validation) estimation metric (equal to the individual search \code{$metric} field).
                               \item \code{$convex} - equal to the individual search \code{$convex} field.
                              }
                      Note: the \code{mpar} argument only appears due to compatibility issues. If used, then the \code{mpar} values are automatically fed into search. However, a direct use of the 
\code{search} argument is advised instead of \code{mpar}, since \code{search} is more flexible and powerful.
              }

  \item{mpar}{(important note: this argument only is kept in this version due to compatibility with previous rminer versions. Instead of \code{mpar}, you should use the more flexible and powerful \code{search} argument.)\cr
              \cr
              vector with extra default (fixed) model parameters (used for modeling, search and feature selection) with:	
                                    \itemize{ 
                                               \item c(\emph{vmethod},\emph{vpar},\code{metric}) -- generic use of mpar (including most models);
                                               \item c(\emph{C},\emph{epsilon},\emph{vmethod},\emph{vpar},\code{metric}) -- if \code{ksvm} and C and epsilon are explicitly set;
					       \item c(\emph{nr},\emph{maxit},\emph{vmethod},\emph{vpar},\code{metric}) -- if \code{mlp} or \code{mlpe} and nr and maxit are explicitly set; 
                                            }
              \emph{C} and \emph{epsilon} are default values for \code{svm} (if any of these is \code{=NA} then heuristics are used to set the value).\cr
              \emph{nr} is the number of \code{mlp} runs or \code{mlpe} individual models, while \emph{maxit} is the maximum number of epochs (if any of these is \code{=NA} then heuristics are used to set the value).\cr
              For help on \emph{vmethod} and \emph{vpar} see \code{\link{mining}}.\cr
              \code{metric} is the internal error function (e.g., used by search to select the best model), valid options are explained in \code{\link{mmetric}}. 
              When \code{mpar=NULL} then default values are used. If there are \code{NA} values (e.g., \code{mpar=c(NA,NA)}) then default values are used.
             }

  \item{feature}{feature selection and sensitivity analysis control. Valid \code{fit} function options are:
                  \itemize{ 
                           \item \code{none} -- no feature selection;
                           \item a \emph{fmethod} character value, such as \code{sabs} (see below);
                           \item a-\code{\link{vector}} -- vector with c(\emph{fmethod},\emph{deletions},\emph{Runs},\emph{vmethod},\emph{vpar},\emph{defaultsearch})
                           \item a-\code{\link{vector}} -- vector with c(\emph{fmethod},\emph{deletions},\emph{Runs},\emph{vmethod},\emph{vpar})
                          }
                 \emph{fmethod} sets the type. Valid options are: 
                  \itemize{ 
                           \item \code{sbs} -- standard backward selection;
                           \item \code{sabs} -- sensitivity analysis backward selection (faster);
                           \item \code{sabsv} -- equal to \code{sabs} but uses variance for sensitivity importance measure;
                           \item \code{sabsr} -- equal to \code{sabs} but uses range for sensitivity importance measure;
                           \item \code{sabsg} -- equal to \code{sabs} (uses gradient for sensitivity importance measure);
                          }
                 \emph{deletions} is the maximum number of feature deletions (if -1 not used).\cr
                 \emph{Runs} is the number of runs for each feature set evaluation (e.g., 1).\cr
                 For help on \emph{vmethod} and \emph{vpar} see \code{\link{mining}}.\cr
                 \emph{defaultsearch} is one hyperparameter used during the feature selection search, after selecting the best feature set then \code{search} is used (faster). 
                 If not defined, then \code{search} is used during feature selection (may be slow).\cr
                 When feature is a vector then default values are used to fill missing values or \code{NA} values.
                 Note: feature selection capabilities are expected to be enhanced in next rminer versions.
                }

  \item{scale}{if data needs to be scaled (i.e. for \code{mlp} or \code{mlpe}). Valid options are:
                  \itemize{ 
                           \item \code{default} -- uses scaling when needed (i.e. for \code{mlp} or \code{mlpe})
                           \item \code{none} -- no scaling; 
                           \item \code{inputs} -- standardizes (0 mean, 1 st. deviation) input attributes; 
                           \item \code{all} -- standardizes (0 mean, 1 st. deviation) input and output attributes;
                          }
               If needed, the \code{predict} function of rminer performs the inverse scaling.
              }

  \item{transform}{if the output data needs to be transformed (e.g., \code{log} transform). Valid options are:
                  \itemize{ 
                           \item \code{none} -- no transform; 
                           \item \code{log} -- y=(log(y+1)) (the inverse function is applied in the \code{predict} function);
                           \item \code{positive} -- all predictions are positive (negative values are turned into zero);
                           \item \code{logpositive} -- both \code{log} and \code{logpositive}; 
                          }
                  }

  \item{created}{time stamp for the model. By default, the system time is used. Else, you can specify another time.}

  \item{fdebug}{if TRUE show some search details.}

  \item{\dots}{ additional and specific parameters send to each fit function model (e.g., \code{dt}, \code{rpart}, \code{randomforest}, \code{kernlab}). A few examples:\cr
                -- the \code{\link[rpart]{rpart}} function is used for decision trees, thus you can have:\cr
\code{control=rpart.control(cp=.05)} (see \code{\link{crossvaldata}} example).\cr
                -- the \code{\link[kernlab]{ksvm}} function is used for support vector machines, thus you can change the kernel type: \code{kernel="polydot"} (see examples below).\cr
Important note: if you use package functions and get an error, then try to explicitly define the package. For instance, you might
need to use \code{fit(}\emph{several-arguments}\code{,control=Cubist::cubistControl())} instead of\cr
\code{fit(}\emph{several-arguments}\code{,control=cubistControl())}.
              }
}

\details{
Fits a classification or regression model given a data.frame (see [Cortez, 2010] for more details).
The \code{...} optional arguments should be used to fix values used by specific \code{model} functions (see examples).
Notes: \cr
- if there is an error in the fit, then a warning is issued (see example).\cr 
- the new \code{search} argument is very flexible and allows a powerful design of supervised learning models.\cr
- the \code{search} correct use is very dependent on the R learning base functions. For example, if you are tuning \code{model="rpart"} then read
carefully the help of function \code{\link[rpart]{rpart}}.\cr
- \code{mpar} argument is only kept due to compatibility issues and should be avoided; instead, use the more flexible \code{search}.\cr
\cr
Details about some models: 
\itemize{
\item Neural Network: \code{mlp} trains \emph{nr} multilayer perceptrons (with \emph{maxit} epochs, \emph{size} hidden nodes 
and \emph{decay} value according to the \code{\link[nnet]{nnet}} function) and selects the best network according to minimum penalized error (\code{$value}). \code{mlpe} uses an ensemble
of \emph{nr} networks and the final prediction is given by the average of all outputs. To tune \code{mlp} or \code{mlpe} you can use the \code{search} parameter, which performs a grid
search for \emph{size} or \emph{decay}.

\item Support Vector Machine: \code{svm} adopts by default the gaussian (rbfdot) kernel. For classification tasks, you can use \code{search} to tune \emph{sigma} (gaussian kernel parameter) and \emph{C} (complexity parameter). For regression, the epsilon insensitive function is adopted and there is an additional hyperparameter \emph{epsilon}.

\item Other methods: Random Forest -- if needed, you can tune several parameters, including the default \code{mtry} parameter adopted by \code{search} heuristics; k-nearest neighbor -- \code{search} by default tunes \emph{k}. The remaining models can also be tunned but a full definition of \code{search} is
required (e.g., with \code{$smethod}, \code{$search} and other fields); please check \code{\link{mparheuristic}} function for further tuning examples (e.g., \code{rpart}).
}
}

\value{
Returns a model object. You can check all model elements with \code{str(M)}, where \code{M} is a model object. The slots are:
                  \itemize{ 
                           \item \code{@formula} -- the \code{x};
                           \item \code{@model} -- the \code{model};
                           \item \code{@task} -- the \code{task};
                           \item \code{@mpar} -- data.frame with the best model parameters (interpretation depends on \code{model});
                           \item \code{@attributes} -- the attributes used by the model;
                           \item \code{@scale} -- the \code{scale};
                           \item \code{@transform} -- the \code{transform};
                           \item \code{@created} -- the date when the model was created;
                           \item \code{@time} -- computation effort to fit the model;
                           \item \code{@object} -- the R object model (e.g., \code{rpart}, \code{nnet}, ...);
                           \item \code{@outindex} -- the output index (of @attributes);
                           \item \code{@levels} -- if \code{task=="prob"||task=="class"} stores the output levels; 
                           \item \code{@error} -- similarly to \code{mining} this is the "validation" error for some \code{search} options; 
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

\item The automl is inspired in this work:\cr
L. Ferreira, A. Pilastri, C. Martins, P. Santos, P. Cortez.\cr
An Automated and Distributed Machine Learning Framework for Telecommunications Risk Management.
In J. van den Herik et al. (Eds.), 
Proceedings of 12th International Conference on Agents and Artificial Intelligence -- ICAART 2020, Volume 2, pp. 99-107,
Valletta, Malta, February, 2020, SCITEPRESS, ISBN 978-989-758-395-7.\cr
@INSTICC: \url{http://hdl.handle.net/1822/66818}\cr

\item For the sabs feature selection:\cr
P. Cortez, A. Cerdeira, F. Almeida, T. Matos and J. Reis.\cr
Modeling wine preferences by data mining from physicochemical properties.\cr
In Decision Support Systems, Elsevier, 47(4):547-553, 2009.\cr
\doi{10.1016/j.dss.2009.05.016}\cr

\item For the uniform design details:\cr
C.M. Huang, Y.J. Lee, D.K.J. Lin and S.Y. Huang.\cr
Model selection for support vector machines via uniform design,\cr
In Computational Statistics & Data Analysis, 52(1):335-346, 2007.\cr
}
}

\author{
Paulo Cortez \url{https://pcortez.dsi.uminho.pt}
}
\note{
See also \url{http://hdl.handle.net/1822/36210} and \url{http://www3.dsi.uminho.pt/pcortez/rminer.html}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{mparheuristic}},\code{\link{mining}}, \code{\link{predict.fit}}, \code{\link{mgraph}}, \code{\link{mmetric}}, \code{\link{savemining}}, \code{\link{CasesSeries}}, \code{\link{lforecast}},
\code{\link{holdout}} and \code{\link{Importance}}. Check all rminer functions using: \code{help(package=rminer)}.
}

\examples{
### dontrun is used when the execution of the example requires some computational effort.

### simple regression (with a formula) example.
x1=rnorm(200,100,20); x2=rnorm(200,100,20)
y=0.7*sin(x1/(25*pi))+0.3*sin(x2/(25*pi))
M=fit(y~x1+x2,model="mlpe")
new1=rnorm(100,100,20); new2=rnorm(100,100,20)
ynew=0.7*sin(new1/(25*pi))+0.3*sin(new2/(25*pi))
P=predict(M,data.frame(x1=new1,x2=new2,y=rep(NA,100)))
print(mmetric(ynew,P,"MAE"))

### simple classification example.
\dontrun{
data(iris)
M=fit(Species~.,iris,model="rpart")
plot(M@object); text(M@object) # show model
P=predict(M,iris)
print(mmetric(iris$Species,P,"CONF"))
print(mmetric(iris$Species,P,"ALL"))
mgraph(iris$Species,P,graph="ROC",TC=2,main="versicolor ROC",
baseline=TRUE,leg="Versicolor",Grid=10)

M2=fit(Species~.,iris,model="ctree")
plot(M2@object) # show model
P2=predict(M2,iris)
print(mmetric(iris$Species,P2,"CONF"))

# ctree with different setup:
# (ctree_control is from the party package)
M3=fit(Species~.,iris,model="ctree",controls = party::ctree_control(testtype="MonteCarlo"))
plot(M3@object) # show model
}

### simple binary classification example with cv.glmnet and xgboost
\dontrun{
data(sa_ssin_2)
H=holdout(sa_ssin_2$y,ratio=2/3)
# cv.glmnet:
M=fit(y~.,sa_ssin_2[H$tr,],model="cv.glmnet",task="cla") # pure classes
P=predict(M,sa_ssin_2[H$ts,])
cat("1st prediction, class:",as.character(P[1]),"\n")
cat("Confusion matrix:\n")
print(mmetric(sa_ssin_2[H$ts,]$y,P,"CONF")$conf)

M2=fit(y~.,sa_ssin_2[H$tr,],model="cv.glmnet") # probabilities
P2=predict(M2,sa_ssin_2[H$ts,])
L=M2@levels
cat("1st prediction, prob:",L[1],"=",P2[1,1],",",L[2],"=",P2[1,2],"\n")
cat("Confusion matrix:\n")
print(mmetric(sa_ssin_2[H$ts,]$y,P2,"CONF")$conf)
cat("AUC of ROC curve:\n")
print(mmetric(sa_ssin_2[H$ts,]$y,P2,"AUC"))

M3=fit(y~.,sa_ssin_2[H$tr,],model="cv.glmnet",nfolds=3) # use 3 folds instead of 10
plot(M3@object) # show cv.glmnet object
P3=predict(M3,sa_ssin_2[H$ts,])

# xgboost:
M4=fit(y~.,sa_ssin_2[H$tr,],model="xgboost",verbose=1) # nrounds=2, show rounds:
P4=predict(M4,sa_ssin_2[H$ts,])
print(mmetric(sa_ssin_2[H$ts,]$y,P4,"AUC"))
M5=fit(y~.,sa_ssin_2[H$tr,],model="xgboost",nrounds=3,verbose=1) # nrounds=3, show rounds:
P5=predict(M5,sa_ssin_2[H$ts,])
print(mmetric(sa_ssin_2[H$ts,]$y,P5,"AUC"))
}

### classification example with discrete classes, probabilities and holdout
\dontrun{
data(iris)
H=holdout(iris$Species,ratio=2/3)
M=fit(Species~.,iris[H$tr,],model="ksvm",task="class")
M1=fit(Species~.,iris[H$tr,],model="lssvm") # default task="class" is assumed
M2=fit(Species~.,iris[H$tr,],model="ksvm",task="prob")
P=predict(M,iris[H$ts,]) # classes
P1=predict(M1,iris[H$ts,]) # classes
P2=predict(M2,iris[H$ts,]) # probabilities
print(mmetric(iris$Species[H$ts],P,"CONF"))
print(mmetric(iris$Species[H$ts],P1,"CONF"))
print(mmetric(iris$Species[H$ts],P2,"CONF"))
print(mmetric(iris$Species[H$ts],P,"CONF",TC=1))
print(mmetric(iris$Species[H$ts],P2,"CONF",TC=1))
print(mmetric(iris$Species[H$ts],P2,"AUC"))

### exploration of some rminer classification models:
models=c("lda","naiveBayes","kknn","randomForest","cv.glmnet","xgboost")
for(m in models)
 { cat("model:",m,"\n") 
   M=fit(Species~.,iris[H$tr,],model=m)
   P=predict(M,iris[H$ts,])
   print(mmetric(iris$Species[H$ts],P,"AUC")[[1]])
 }
}

### classification example with hyperparameter selection 
###    note: for regression, similar code can be used
### SVM 
\dontrun{
data(iris)
# large list of SVM configurations:
# SVM with kpar="automatic" sigma rbfdot kernel estimation and default C=1:
#  note: each execution can lead to different M@mpar due to sigest stochastic nature:
M=fit(Species~.,iris,model="ksvm")
print(M@mpar) # model hyperparameters/arguments
# same thing, explicit use of mparheuristic:
M=fit(Species~.,iris,model="ksvm",search=list(search=mparheuristic("ksvm")))
print(M@mpar) # model hyperparameters

# SVM with C=3, sigma=2^-7
M=fit(Species~.,iris,model="ksvm",C=3,kpar=list(sigma=2^-7))
print(M@mpar)
# SVM with different kernels:
M=fit(Species~.,iris,model="ksvm",kernel="polydot",kpar="automatic") 
print(M@mpar)
# fit already has a scale argument, thus the only way to fix scale of "tanhdot"
# is to use the special search argument with the "none" method:
s=list(smethod="none",search=list(scale=2,offset=2))
M=fit(Species~.,iris,model="ksvm",kernel="tanhdot",search=s) 
print(M@mpar)
# heuristic: 10 grid search values for sigma, rbfdot kernel (fdebug is used only for more verbose):
s=list(search=mparheuristic("ksvm",10)) # advised "heuristic10" usage
M=fit(Species~.,iris,model="ksvm",search=s,fdebug=TRUE)
print(M@mpar)
# same thing, uses older search="heuristic10"
M=fit(Species~.,iris,model="ksvm",search="heuristic10",fdebug=TRUE)
print(M@mpar)
# identical search under a different and explicit code:
s=list(search=2^seq(-15,3,2))
M=fit(Species~.,iris,model="ksvm",search=2^seq(-15,3,2),fdebug=TRUE)
print(M@mpar)

# uniform design "UD" for sigma and C, rbfdot kernel, two level of grid searches, 
# under exponential (2^x) search scale:
M=fit(Species~.,iris,model="ksvm",search="UD",fdebug=TRUE)
print(M@mpar)
M=fit(Species~.,iris,model="ksvm",search="UD1",fdebug=TRUE)
print(M@mpar)
# now the more powerful search argument is used for modeling SVM:
# grid 3 x 3 search:
s=list(smethod="grid",search=list(sigma=2^c(-15,-5,3),C=2^c(-5,0,15)),convex=0,
            metric="AUC",method=c("kfold",3,12345))
print(s)
M=fit(Species~.,iris,model="ksvm",search=s,fdebug=TRUE)
print(M@mpar)
# identical search with different argument smethod="matrix" 
s$smethod="matrix"
s$search=list(sigma=rep(2^c(-15,-5,3),times=3),C=rep(2^c(-5,0,15),each=3))
print(s)
M=fit(Species~.,iris,model="ksvm",search=s,fdebug=TRUE)
print(M@mpar)
# search for best kernel (only works for kpar="automatic"):
s=list(smethod="grid",search=list(kernel=c("rbfdot","laplacedot","polydot","vanilladot")),
       convex=0,metric="AUC",method=c("kfold",3,12345))
print(s)
M=fit(Species~.,iris,model="ksvm",search=s,fdebug=TRUE)
print(M@mpar)
# search for best parameters of "rbfdot" or "laplacedot" (which use same kpar):
s$search=list(kernel=c("rbfdot","laplacedot"),sigma=2^seq(-15,3,5))
print(s)
M=fit(Species~.,iris,model="ksvm",search=s,fdebug=TRUE)
print(M@mpar)

### randomForest
# search for mtry and ntree
s=list(smethod="grid",search=list(mtry=c(1,2,3),ntree=c(100,200,500)),
            convex=0,metric="AUC",method=c("kfold",3,12345))
print(s)
M=fit(Species~.,iris,model="randomForest",search=s,fdebug=TRUE)
print(M@mpar)

### rpart
# simpler way to tune cp in 0.01 to 0.9 (10 searches):
s=list(search=mparheuristic("rpart",n=10,lower=0.01,upper=0.9),method=c("kfold",3,12345))
M=fit(Species~.,iris,model="rpart",search=s,fdebug=TRUE)
print(M@mpar)

# same thing but with more lines of code
# note: this code can be adapted to tune other rpart parameters,
#       while mparheuristic only tunes cp
# a vector list needs to be used for the search$search parameter
lcp=vector("list",10) # 10 grid values for the complexity cp
names(lcp)=rep("cp",10) # same cp name 
scp=seq(0.01,0.9,length.out=10) # 10 values from 0.01 to 0.18
for(i in 1:10) lcp[[i]]=scp[i] # cycle needed due to [[]] notation
s=list(smethod="grid",search=list(control=lcp),
            convex=0,metric="AUC",method=c("kfold",3,12345))
M=fit(Species~.,iris,model="rpart",search=s,fdebug=TRUE)
print(M@mpar)

### ctree 
# simpler way to tune mincriterion in 0.1 to 0.98 (9 searches):
mint=c("kfold",3,123) # internal validation method
s=list(search=mparheuristic("ctree",n=8,lower=0.1,upper=0.99),method=mint)
M=fit(Species~.,iris,model="ctree",search=s,fdebug=TRUE)
print(M@mpar)
# same thing but with more lines of code
# note: this code can be adapted to tune other ctree parameters,
#       while mparheuristic only tunes mincriterion
# a vector list needs to be used for the search$search parameter
lmc=vector("list",9) # 9 grid values for the mincriterion
smc=seq(0.1,0.99,length.out=9)
for(i in 1:9) lmc[[i]]=party::ctree_control(mincriterion=smc[i]) 
s=list(smethod="grid",search=list(controls=lmc),method=mint,convex=0)
M=fit(Species~.,iris,model="ctree",search=s,fdebug=TRUE)
print(M@mpar)

### some MLP fitting examples:
# simplest use:
M=fit(Species~.,iris,model="mlpe")  
print(M@mpar)
# same thing, with explicit use of mparheuristic:
M=fit(Species~.,iris,model="mlpe",search=list(search=mparheuristic("mlpe")))
print(M@mpar) # hidden nodes and number of ensemble mlps
# setting some nnet parameters:
M=fit(Species~.,iris,model="mlpe",size=3,decay=0.1,maxit=100,rang=0.9) 
print(M@mpar) # mlpe hyperparameters
# MLP, 5 grid search fdebug is only used to put some verbose in the console:
s=list(search=mparheuristic("mlpe",n=5)) # 5 searches for size
print(s) # show search
M=fit(Species~.,iris,model="mlpe",search=s,fdebug=TRUE)
print(M@mpar)
# previous searches used a random holdout (seed=NULL), now a fixed seed (123) is used:
s=list(smethod="grid",search=mparheuristic("mlpe",n=5),convex=0,metric="AUC",
            method=c("holdout",2/3,123))
print(s)
M=fit(Species~.,iris,model="mlpe",search=s,fdebug=TRUE)
print(M@mpar)
# faster and greedy grid search:
s$convex=1;s$search=list(size=0:9)
print(s)
M=fit(Species~.,iris,model="mlpe",search=s,fdebug=TRUE)
print(M@mpar)
# 2 level grid with total of 5 searches 
#  note of caution: some "2L" ranges may lead to non integer (e.g., 1.3) values at
#  the 2nd level search. And some R functions crash if non integer values are used for
#  integer parameters.
s$smethod="2L";s$convex=0;s$search=list(size=c(4,8,12))
print(s)
M=fit(Species~.,iris,model="mlpe",search=s,fdebug=TRUE)
print(M@mpar)

# testing of all 17 rminer classification methods:
model=c("naive","ctree","cv.glmnet","rpart","kknn","ksvm","lssvm","mlp","mlpe",
 "randomForest","xgboost","bagging","boosting","lda","multinom","naiveBayes","qda")
inputs=ncol(iris)-1
ho=holdout(iris$Species,2/3,seed=123) # 2/3 for training and 1/3 for testing
Y=iris[ho$ts,ncol(iris)]
for(i in 1:length(model))
 {
  cat("i:",i,"model:",model[i],"\n")
  search=list(search=mparheuristic(model[i])) # rminer default values
  M=fit(Species~.,data=iris[ho$tr,],model=model[i],search=search,fdebug=TRUE)
  P=predict(M,iris[ho$ts,])
  cat("predicted ACC:",round(mmetric(Y,P,metric="ACC"),1),"\n")
 }

}

### example of an error (warning) generated using fit:
\dontrun{
data(iris)
# size needs to be a positive integer, thus 0.1 leads to an error:
M=fit(Species~.,iris,model="mlp",size=0.1)  
print(M@object)
}

### exploration of some rminer regression models:
\dontrun{
data(sa_ssin)
H=holdout(sa_ssin$y,ratio=2/3,seed=12345)
models=c("lm","mr","ctree","mars","cubist","cv.glmnet","xgboost","rvm")
for(m in models)
 { cat("model:",m,"\n") 
   M=fit(y~.,sa_ssin[H$tr,],model=m)
   P=predict(M,sa_ssin[H$ts,])
   print(mmetric(sa_ssin$y[H$ts],P,"MAE"))
 }
}

# testing of all 18 rminer regression methods:
\dontrun{
model=c("naive","ctree","cv.glmnet","rpart","kknn","ksvm","mlp","mlpe",
 "randomForest","xgboost","cubist","lm","mr","mars","pcr","plsr","cppls","rvm")
# note: in this example, default values are considered for the hyperparameters.
# better results can be achieved by tuning hyperparameters via improved usage
# of the search argument (via mparheuristic function or written code)
data(iris)
ir2=iris[,1:4] # predict iris "Petal.Width"
names(ir2)[ncol(ir2)]="y" # change output name
inputs=ncol(ir2)-1
ho=holdout(ir2$y,2/3,seed=123) # 2/3 for training and 1/3 for testing
Y=ir2[ho$ts,ncol(ir2)]
for(i in 1:length(model))
 {
  cat("i:",i,"model:",model[i],"\n")
  search=list(search=mparheuristic(model[i])) # rminer default values
  M=fit(y~.,data=ir2[ho$tr,],model=model[i],search=search,fdebug=TRUE)
  P=predict(M,ir2[ho$ts,])
  cat("predicted MAE:",round(mmetric(Y,P,metric="MAE"),1),"\n")
 }
}

### regression example with hyperparameter selection:
\dontrun{
data(sa_ssin)
# some SVM experiments:
# default SVM:
M=fit(y~.,data=sa_ssin,model="svm")
print(M@mpar)
# SVM with (Cherkassy and Ma, 2004) heuristics to set C and epsilon:
M=fit(y~.,data=sa_ssin,model="svm",C=NA,epsilon=NA)
print(M@mpar)
# SVM with Uniform Design set sigma, C and epsilon:
M=fit(y~.,data=sa_ssin,model="ksvm",search="UD",fdebug=TRUE)
print(M@mpar)

# sensitivity analysis feature selection
M=fit(y~.,data=sa_ssin,model="ksvm",search=list(search=mparheuristic("ksvm",n=5)),feature="sabs") 
print(M@mpar)
print(M@attributes) # selected attributes (1, 2 and 3 are the relevant inputs)

# example that shows how transform works:
M=fit(y~.,data=sa_ssin,model="mr") # linear regression
P=predict(M,data.frame(x1=-1000,x2=0,x3=0,x4=0,y=NA)) # P should be negative
print(P)
M=fit(y~.,data=sa_ssin,model="mr",transform="positive")
P=predict(M,data.frame(x1=-1000,x2=0,x3=0,x4=0,y=NA)) # P is not negative
print(P)
}

### pure classification example with a generic R (not rminer default) model ###
\dontrun{
### nnet is adopted here but virtually ANY fitting function/package could be used:

# since the default nnet prediction is to provide probabilities, there is
# a need to create this "wrapping" function:
predictprob=function(object,newdata)
{ predict(object,newdata,type="class") }
# list with a fit and predict function:
# nnet::nnet (package::function)
model=list(fit=nnet::nnet,predict=predictprob,name="nnet")
data(iris)
# note that size is not a fit parameter and it is sent directly to nnet:
M=fit(Species~.,iris,model=model,size=3,task="class") 
P=predict(M,iris)
print(P)
} 

### multiple models: automl and ensembles 
\dontrun{
data(iris)
d=iris
names(d)[ncol(d)]="y" # change output name
inputs=ncol(d)-1
metric="AUC"

# consult the help of mparheuristic for more automl and ensemble examples:
#
# automatic machine learining (automl) with 5 distinct models and "SE" ensemble.
# the single models are tuned with 10 internal hyperparameter searches, 
# except ksvm that uses 13 searches via "UD".
# fit performs an internal validation 
sm=mparheuristic(model="automl3",n=NA,task="prob", inputs= inputs )
method=c("kfold",3,123)
search=list(search=sm,smethod="auto",method=method,metric=metric,convex=0)
M=fit(y~.,data=d,model="auto",search=search,fdebug=TRUE)
P=predict(M,d)
# show leaderboard:
cat("> leaderboard models:",M@mpar$LB$model,"\n")
cat(">  validation values:",round(M@mpar$LB$eval,4),"\n")
cat("best model is:",M@model,"\n")
cat(metric,"=",round(mmetric(d$y,P,metric=metric),2),"\n")


# average ensemble of 5 distinct models
# the single models are tuned with 1 (heuristic) hyperparameter search 
sm2=mparheuristic(model="automl",n=NA,task="prob", inputs= inputs )
method=c("kfold",3,123)
search2=list(search=sm2,smethod="auto",method=method,metric=metric,convex=0)
M2=fit(y~.,data=d,model="AE",search=search2,fdebug=TRUE)
P2=predict(M,d)
cat("best model is:",M2@model,"\n")
cat(metric,"=",round(mmetric(d$y,P2,metric=metric),2),"\n")

# example with an invalid model exclusion: 
# in this case, randomForest produces an error and warning
# thus it is excluded from the leaderboard
sm=mparheuristic(model="automl3",n=NA,task="prob", inputs= inputs )
method=c("holdout",2/3,123)
search=list(search=sm,smethod="auto",method=method,metric=metric,convex=0)
d2=d
#
d2[,2]=as.factor(1:150) # force randomForest error
M=fit(y~.,data=d2,model="auto",search=search,fdebug=TRUE)
P=predict(M,d2)
# show leaderboard:
cat("> leaderboard models:",M@mpar$LB$model,"\n")
cat(">  validation values:",round(M@mpar$LB$eval,4),"\n")
cat("best model is:",M@model,"\n")
cat(metric,"=",round(mmetric(d$y,P,metric=metric),2),"\n")

}

}

\keyword{classif}
\keyword{regression}
\keyword{neural}
\keyword{nonlinear}
