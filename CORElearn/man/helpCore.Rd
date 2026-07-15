\name{helpCore}
\alias{helpCore}
\alias{help.Core}
\title{ Description of parameters. }
\description{
  The behavior of CORElearn is controlled by several parameters. This is a short overview.  
}
\details{
There are many different parameters available. Some are general and can be used in many
learning, or feature evaluation algorithms. All the values actually used by 
the classifier / regressor can be written to file (or read from it) using
\code{\link{paramCoreIO}}.
The parameters for the methods are split into several groups and documented below.
}
\section{Attribute/feature evaluation}{
The parameters in this group may be used inside model construction 
via \code{\link{CoreModel}} and feature evaluation in \code{\link{attrEval}}. See \code{\link{attrEval}}
for description of relevant evaluation methods. 

Parameters \code{attrEvaluationInstances}, \code{binaryEvaluation}, \cr
\code{binarySplitNumericAttributes}
are applicable to all attribute evaluation methods. In models which need feature evaluation (e.g., trees,
random forests) they affect the selection of splits in the nodes.
Other parameters may be used only in context sensitive measures, i.e., ReliefF in classification
and RReliefF in regression and their variants.  
\describe{
    \item{binaryEvaluation}{type: logical, default value: FALSE \cr
          Shall we treat all attributes as binary and binarize them before evaluation if necessary.
          If \code{TRUE}, then for all multivalued discrete and all numeric features a search for the
          best binarization is performed. The evaluation of the best binarization found is reported.
          If \code{FALSE}, then multivalued discrete features are evaluated "as is" with multivalued versions
          of estimators. With ReliefF-type measures, numeric features are also evaluated "as is". For evaluation
          of numeric features with other (non-ReliefF-type) measures, they are first binarized or discretized.
          The choice between binarization and discretization is controlled by 
          \code{binaryEvaluateNumericAttributes}. Due to performance reasons it is recommended that 
          \code{binaryEvaluation=FALSE} is used.
          See also \code{discretizationSample}. }
    \item{binaryEvaluateNumericAttributes}{type: logical, default value: TRUE \cr
          ReliefF like measures can evaluate numeric attributes intrinsically, others
          have to discretize or binarize them before evaluation; for those measures
          this parameter selects binarization (default) or discretization (computationally more demanding). }
    \item{multiclassEvaluation}{type: integer, default value: 1, value range: 1, 4 \cr 
               multi-class extension for two-class-only evaluation measures 
               (1-average of all-pairs, 2-best of all-pairs, 
               3-average of one-against-all, 4-best of one-against-all). }
    \item{attrEvaluationInstances}{type: integer, default value: 0, value range: 0, Inf \cr
          number of instances for attribute evaluation (0=all available). }
    \item{minNodeWeightEst}{type: numeric, default value: 2, value range: 0, Inf \cr
        minimal number of instances (weight) in resulting split to take it in consideration. }          
    \item{ReliefIterations}{type: integer, default value: 0, value range: -2, Inf \cr 
              number of iterations for all variants of Relief (0=DataSize, -1=ln(DataSize) -2=sqrt(DataSize)). }
    \item{numAttrProportionEqual}{type: numeric, default value: 0.04, value range: 0, 1 \cr
           used in ramp function, proportion of numerical attribute's range to consider two values equal. }
    \item{numAttrProportionDifferent}{type: numeric, default value: 0.1, value range: 0, 1 \cr
           used in ramp function, proportion of numerical attribute's range to consider two values different. }
    \item{kNearestEqual}{type: integer, default value: 10, value range: 0, Inf \cr
        number of neighbors to consider in equal k-nearest attribute evaluation. }
    \item{kNearestExpRank}{type: integer, default value: 70, value range: 0, Inf \cr
        number of neighbors to consider in exponential rank distance attribute evaluation. }
    \item{quotientExpRankDistance}{type: numeric, default value: 20, value range: 0, Inf \cr
         quotient in exponential rank distance attribute evaluation. }
}
}
\section{Decision/regression tree construction}{
There are several parameters controlling a construction of the tree model. Some are described here,
but also attribute evaluation, stop building, model, constructive induction, discretization, 
and pruning options described in this document are applicable.    
Splits in trees are always binary, however, the option \code{binaryEvaluation} has influence on the
feature selection for the split. Namely, selecting the best feature for the split is done with the given
value of \code{binaryEvaluation}. If \code{binaryEvaluation=FALSE}, the features are first evaluated and
the best one is finally binarized. If \code{binaryEvaluation=TRUE}, the features are binarized before
selection. In this case, a search for the best binarization for all considered features is performed and
the best binarizations found are used for splits. The latter option is computationally more intensive,
but typically does not produce better trees. 
\describe{
    \item{selectionEstimator}{type: character, default value: "MDL", possible values: all from \code{\link{attrEval}}, section classification \cr
        estimator for selection of attributes and binarization in classification. }
    \item{selectionEstimatorReg}{type: character, default value: "RReliefFexpRank", possible values: all from \code{\link{attrEval}}, section regression \cr
        estimator for selection of attributes and binarization in regression. }
    \item{minReliefEstimate}{type: numeric, default value: 0, value range: -1, 1 \cr
      for all variants of Relief attribute estimator: the minimal evaluation of attribute to consider the attribute useful in further processing. }
    \item{minInstanceWeight}{type: numeric, default value: 0.05, value range: 0, 1 \cr  minimal weight of an instance to use it further in splitting. }
}
}
\section{Stop tree building}{
During tree construction the node is recursively split, until certain condition is fulfilled.
\describe{
    \item{minNodeWeightTree}{type: numeric, default value: 5, value range: 0, Inf \cr
        minimal number of instances (weight) of a leaf in the decision or regression tree model. }
    \item{minNodeWeightRF}{type: numeric, default value: 2, value range: 0, Inf \cr
        minimal number of instances (weight) of a leaf in the random forest tree. }
    \item{relMinNodeWeight}{type: numeric, default value: 0, value range: 0, 1 \cr
       minimal proportion of training instances in a tree node to split it further. }
    \item{majorClassProportion}{type: numeric, default value: 1, value range: 0, 1 \cr
        proportion of majority class in a classification tree node to stop splitting it. }
    \item{rootStdDevProportion}{type: numeric, default value: 0, value range: 0, 1 \cr
         proportion of root's standard deviation in a regression tree node to stop splitting it. }
%   \item{minNonMajorityWeight}{type: numeric, default value: 0, value range: 0, Inf \cr
%        minimal number of non-majority class instances (weight) in a node to allow further splitting. The default value 0 disables this stopping criterion. }
 }
}
\section{Models in the tree leaves}{
In leaves of the tree model there can be various prediction models controlling prediction. For example instead of classification with 
majority of class values one can use naive Bayes in classification, or a linear model in regression, thereby expanding 
expressive power of the tree model. 
\describe{
    \item{modelType}{type: integer, default value: 1, value range: 1, 4 \cr
        type of models used in classification tree leaves (1=majority class, 2=k-nearest neighbors, 3=k-nearest neighbors with kernel, 4=naive Bayes). }
    \item{modelTypeReg}{type: integer, default value: 5, value range: 1, 8 \cr
        type of models used in regression tree leaves (1=mean predicted value, 2=median predicted value, 3=linear by MSE, 
        4=linear by MDL, 5=linear reduced as in M5, 6=kNN, 7=Gaussian kernel regression, 8=locally weighted linear regression). }
    \item{kInNN}{type: integer, default value: 10, value range: 0, Inf \cr
         number of neighbors in k-nearest neighbors models (0=all). }
    \item{nnKernelWidth}{type: numeric, default value: 2, value range: 0, Inf \cr  kernel width in k-nearest neighbors models. }
    \item{bayesDiscretization}{type: integer, default value: 2, value range: 1, 3 \cr
       type of discretization for naive Bayesian models (1=greedy with selection estimator, 2=equal frequency, 3=equal width). }
    \item{discretizationIntervals}{type: integer, default value: 4, value range: 1, Inf \cr
        number of intervals in equal frequency or equal width discretizations. }
 }
 }
 \section{Constructive induction aka. feature construction}{
 The expressive power of tree models can be increased by incorporating additional types of splits. Operator based
 constructive induction is implemented in both classification and regression. The best construct is searched with beam search. 
 At each step new constructs are evaluated with selected feature evaluation measure.
 With different types of operators one can control expressions in the interior tree nodes.
\describe{
    \item{constructionMode}{type: integer, default value: 15, value range: 1, 15 \cr 
        sum of constructive operators (1=single attributes, 2=conjunction, 4=addition, 8=multiplication); all=1+2+4+8=15 }
    \item{constructionDepth}{type: integer, default value: 0, value range: 0, Inf \cr
         maximal depth of the tree for constructive induction (0=do not do construction, 1=only at root, ...). }
    \item{noCachedInNode}{type: integer, default value: 5, value range: 0, Inf \cr
       number of cached attributes in each node where construction was performed. }
    \item{constructionEstimator}{type: character, default value: "MDL", possible values: all from \code{\link{attrEval}}, section classification \cr
           estimator for constructive induction in classification. }
    \item{constructionEstimatorReg}{type: character, default value: "RReliefFexpRank", possible values: all from \code{\link{attrEval}}, section regression \cr
           estimator for constructive induction in regression. }
    \item{beamSize}{type: integer, default value: 20, value range: 1, Inf \cr     size of the beam in search for best feature in constructive induction. }
    \item{maxConstructSize}{type: integer, default value: 3, value range: 1, Inf \cr
        maximal size of constructs in constructive induction. }
        }
        }
 \section{Attribute discretization and binarization}{
 Some algorithms cannot deal with numeric attributes directly, so we have to discretize them. Also the tree models use
 binary splits in nodes. The discretization  algorithm  evaluates split candidates and forms intervals of values. 
 Note that setting \code{discretizationSample=1} will force random selection of splitting point, which will speed-up the algorithm
 and may be perfectly acceptable for random forest ensembles.
 
 CORElearn builds binary trees so multivalued  discrete attributes have to be binarized i.e., values have to be split into 
 twoa subset, one going left and the other going right in a node. The method used depends on the parameters 
 and the number of attribute values. Possible methods are exhaustive (if the number of attribute values is less or equal  
 \code{maxValues4Exhaustive}), greedy ((if the number of attribute values is less or equal \code{maxValues4Greedy})
 and random ((if the number of attribute values is more than \code{maxValues4Exhaustive}). 
 Setting \code{maxValues4Greedy=2} will always randomly selet splitting point. 
 
\describe{
     \item{discretizationLookahead}{type: integer, default value: 3, value range: 0, Inf \cr
     Discretization is performed with a greedy algorithm which adds a new boundary, until there is no
     improvement in evaluation function for \code{discretizationLookahead} number of times
     (0=try all possibilities). Candidate boundaries are chosen from a random sample of boundaries,
     whose size is \code{discretizationSample}. }
     \item{discretizationSample}{type: integer, default value: 50, value range: 0, Inf \cr
     Maximal number of points to try discretization (0=all sensible). 
     For ReliefF-type measures, binarization of numeric features is performed with \code{discretizationSample} randomly
     chosen splits. For other measures, the split is searched among all possible splits.
     }
    \item{maxValues4Exhaustive}{type: integer, default value: 7, value range: 2, Inf \cr
     Maximal number of values of a discrete attribute to try finding split exhaustively.
     If the attribute has more values the split will be searched greedily or selected ranomly based on the 
     value of parameter \code{maxValues4Greedy}.
     }
   \item{maxValues4Greedy}{type: integer, default value: 30, value range: 2, Inf \cr
     Maximal number of values of a discrete attribute to try finding split greedily.
     If the attribute has more values the split will be selected ranomly. Setting this parameter to 2 will
     force random but balanced selection of splits which may be acceptable for random forest ensembles and will greatly
     speed-up tree construction.
     }     
     }
     }
\section{Tree pruning}{
After the tree is constructed, to reduce noise it is beneficial to prune it.
\describe{
    \item{selectedPruner}{type: integer, default value: 1, value range: 0, 1 \cr  decision tree pruning method used (0=none, 1=with m-estimate). }
    \item{selectedPrunerReg}{type: integer, default value: 2, value range: 0, 4 \cr
        regression tree pruning method used (0=none, 1=MDL, 2=with m-estimate, 3=as in M5, 4=error complexity as in CART (fixed alpha)). }
    \item{mdlModelPrecision}{type: numeric, default value: 0.1, value range: 0, Inf \cr precision of model coefficients in MDL tree pruning. }
    \item{mdlErrorPrecision}{type: numeric, default value: 0.01, value range: 0, Inf \cr precision of errors in MDL tree pruning. }
    \item{mEstPruning}{type: numeric, default value: 2, value range: 0, Inf \cr m-estimate for pruning with m-estimate. }
    \item{alphaErrorComplexity}{type: numeric, default value: 0, value range: 0, Inf \cr alpha for error complexity pruning. }
    }
    }
\section{Prediction}{
For some models (decision trees, random forests, naive Bayes, and regression trees) one can smoothe the output predictions.
In classification models output probabilities are smoothed and in case of regression prediction value is smoothed. 
\describe{
    \item{smoothingType}{type: integer, default value: 0, value range: 0, 4 \cr default value 0
      means no smoothing  (in case classification one gets relative frequencies),
      value 1 stands for additive smoothing, 2 is pure Laplace's smoothing, 3 is m-estimate smoothing, and 4 means
      Zadrozny-Elkan type of m-estimate smoothing where \code{smoothingValue} is interpreted as 
      \eqn{m\cdot p_c}{m * Pc} and \eqn{p_c}{Pc} is the prior probability of
      the least probable class value; 
      for regression \code{smoothingType} has no effect, as the smoothing is controlled solely by \code{smoothingValue}.
      }
    \item{smoothingValue}{type: numeric, default value: 0, value range: 0, Inf \cr 
      additional parameter for some sorts of smoothing; in classification it is needed for additive, m-estimate, 
      and Zadrozny-Elkan type of smoothing; 
      in case of regression trees 0 means no smoothing and values larger than 0 change prediction value towards
      the prediction of the models in ascendant nodes.
      }
    }
}
\section{Random forests}{
Random forest is quite complex model, whose construction one can control with several parameters.
Momentarily only classification version of the algorithm is implemented.
Besides parameters in this section one can apply majority of parameters for control of decision trees (except constructive induction and tree pruning).  
\describe{
    \item{rfNoTrees}{type: integer, default value: 100, value range: 1, Inf \cr number of trees in the random forest. }
    \item{rfNoSelAttr}{type: integer, default value: 0, value range: -2, Inf \cr 
       number of randomly selected attributes in the node (0=sqrt(numOfAttr), -1=log2(numOfAttr)+1, -2=all). }
    \item{rfMultipleEst}{type: logical, default value: FALSE \cr
       use multiple attribute estimators in the forest? If TRUE the algorithm uses some preselected attribute evaluation measures on different trees. }
    \item{rfkNearestEqual}{type: integer, default value: 30, value range: 0, Inf \cr
        number of nearest intances for weighted random forest classification (0=no weighing). }
    \item{rfPropWeightedTrees}{type: numeric, default value: 0, value range: 0, 1 \cr
      Proportion of trees where attribute probabilities are weighted with their quality. As attribute weighting might reduce the variance between the models, 
      the default value switches the weighing off. }
    \item{rfPredictClass}{type: logical, default value: FALSE \cr
      shall individual trees predict with majority class (otherwise with class distribution). }     
      }
      }
 \section{General tree ensembles}{
 In the same manner as random forests more general tree ensembles can be constructed. Additional options control sampling,
 tree size and regularization. 
\describe{
    \item{rfSampleProp}{type: numeric, default value: 0, value range: 0, 1 \cr
        proportion of the training set to be used in learning (0=bootstrap replication). }
    \item{rfNoTerminals}{type: integer, default value: 0, value range: 0, Inf \cr  maximal number of leaves in each tree (0=build the whole tree). }
    \item{rfRegType}{type: integer, default value: 2, value range: 0, 2 \cr
       type of regularization (0=no regularization, 1=global regularization, 2=local regularization). }
    \item{rfRegLambda}{type: numeric, default value: 0, value range: 0, Inf \cr regularization parameter lambda (0=no regularization). }
    }
    }
 \section{Read data directly from files}{
 In case of very large data sets it is useful to bypass \R and read data directly from files as the standalone learning system CORElearn 
 does. Supported file formats are C4.5, M5, and native format of CORElearn. See documentation at \url{http://lkm.fri.uni-lj.si/rmarko/software/}.
\describe{
    \item{domainName}{type: character, \cr name of a problem to read from files with suffixes .dsc, .dat, .names, .data, .cm, and .costs }
    \item{dataDirectory}{type: character, \cr folder where data files are stored. }   
    \item{NAstring}{type: character, default value: "?" \cr
         character string which represents missing and NA values in the data files. }     
}  
}
\section{Miscellaneous}{
\describe{
    \item{maxThreads}{type: integer, default value: 0, value range: 0, Inf \cr maximal number of active threads (0=allow OpenMP to set its defaults).
         \cr As side effect, this parameter changes the number of active threads in all subsequent execution (till \code{maxThreads} is set again). }
    }
    }
\references{
B. Zadrozny, C. Elkan. Learning and making decisions when costs and probabilities are both unknown.
In Proceedings of the Seventh International Conference on Knowledge Discovery and Data Mining, 2001.
}
\author{ Marko Robnik-Sikonja, Petr Savicky }

\keyword{ models }
\keyword{ regression }
\keyword{ nonlinear }
\keyword{ tree }
\keyword{ classif }

\seealso{
\code{\link{CORElearn}},
\code{\link{CoreModel}},
\code{\link{predict.CoreModel}},
\code{\link{attrEval}},
\code{\link{ordEval}},
\code{\link{paramCoreIO}}.
}

