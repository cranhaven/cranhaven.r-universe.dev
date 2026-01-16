##' @title S4 class for representing DB-CSP (Distance-Based Common Spatial Patterns)
#'
#' @description An object of class dbcsp. 'dbcsp' stands for Distance-Based Common Spatial Patterns.
#' The object includes the Common Spatial Patterns filter obtained with the input lists and
#' using the distance method indicated.
#'
#' @seealso \code{\link[=dbcsp-package]{dbcsp}}, \code{\link[=print.dbcsp]{print}}, \code{\link[=summary.dbcsp]{summary}}, \code{\link[=train.dbcsp]{train}}, \code{\link{selectQ}}, \code{\link[=predict.dbcsp]{predict}}, \code{\link[=plot.dbcsp]{plot}}, \code{\link[=boxplot.dbcsp]{boxplot}}
#' @name dbcsp-class
#' @rdname dbcsp-class
#' @exportClass dbcsp
#' @importFrom graphics matplot par plot boxplot
#' @importFrom methods new
#' @importFrom grDevices rainbow
#' @importFrom stats as.dist
#' @importFrom caret train
#' @import TSdist
#' @export selectQ
#'
#'
#' @slot X1 list of matrices for data class 1.
#' @slot X2 list of matrices for data class 2.
#' @slot q integer value indicating the number of vectors used in the projection, by default \code{q=15}.
#' @slot labels vector of two strings indicating labels names, by default names of variables X1 and X2.
#' @slot type string which sets the type of distance to be considered, by default \code{type='EUCL'}. See details section.
#' @slot w weight for the distances mixture D_mixture = w*D_euclidean + (1-w)*D_type, by default \code{w=0.5}.
#' @slot mixture logical value indicating whether to use distances mixture or not (EUCL + other), by default \code{mixture=FALSE}.
#' @slot training logical value indicating whether to perform the training or not.
#' @slot fold integer value, by default \code{fold=10}. It controls the number of partitions when training.
#' If \code{fold==1} a train/test split is performed, with p=0.2 for test indices.
#' @slot seed numeric value, by default \code{seed=NULL}. Set a seed to ensure reproducible results.
#' @slot eig.tol numeric value, by default \code{eig.tol=1e-06}, tolerance to convert distance matrix to be definite positive.
#' @slot verbose logical
#' @slot more list, additional parameters to be passed to the distance methods. See details section.
#' @slot out list containing the output.
#' @details If the lists of matrices \code{X1} or \code{X2} contain NA values, these are automatically interpolated by a linear interpolation
#' using \code{\link[zoo]{na.approx}} function. These new interpolated matrices are saved in the X1 and X2 slots of the object.
#'
#' The supported distances for \code{type} are these ones:
#'
#' - Included in \code{\link[TSdist]{TSDatabaseDistances}}: infnorm, ccor, sts, lb.keogh, edr, erp, lcss, fourier, tquest, dissim, acf, pacf, ar.lpc.ceps, ar.mah,
#' ar.mah.statistic, ar.mah.pvalue, ar.pic, cdm, cid, cor, cort, int.per, per, mindist.sax, ncd, pred, spec.glk, spec.isd,
#' spec.llr, pdc, frechet, tam.
#'
#' - Included in \code{\link[parallelDist]{parDist}}: bhjattacharyya, bray, canberra, chord, divergence, dtw, euclidean, fJaccard, geodesic, hellinger,
#' kullback, mahalanobis, manhattan, maximum, minkowski, podani, soergel, wave, whittaker.
#'
#' - It is possible to use a custom distance. The name of the custom distance function is passed as character to the \code{type} parameter.
#'  In order to use the \code{\link[parallelDist]{parDist}} custom distance option, the custom function must be defined as
#'  explained in "\code{Details: User-defined distance functions}" part of \code{\link[parallelDist]{parDist}} documentation.
#'  See Examples section below.
#'
#' The additional parameters for the selected distance (see \code{\link[TSdist]{TSDatabaseDistances}}, \code{\link[parallelDist]{parDist}}) can be passed
#' as parameters when creating the object, which will be saved in \code{more} slot. See Examples section below.
#'
#' The output is a list containing this information (\code{object@out}):
#' - \code{vectors} The projection vectors obtained after applying CSP.
#' - \code{eig} The eigenvalues obtained after applying CSP.
#' - \code{proy} The variance values of the projected signals obtained after applying CSP.
#'
#' And if \code{training=TRUE} the following values are also saved:
#' - \code{acc} The mean accuracy value obtained for training data applying cross validation.
#' - \code{used_folds} List of the folds used in the cross validation.
#' - \code{folds_acc} Accuracy values for each of the folds of the cross validation.
#' - \code{model} The trained LDA classifier.
#' - \code{selected_q} The number of vectors used when training.
#' @examples
#' # To create an instance of a class dbcsp given data from 2 classes
#' x <- AR.data$come[1:20]
#' y <- AR.data$five[1:20]
#' mydbcsp <- new("dbcsp", X1 = x, X2 = y)
#'
#' # CUSTOM DISTANCE
#' x <- AR.data$come[1:10]
#' y <- AR.data$five[1:10]
#' fn <- function(x, y, eps=1) mean(1 - cos(x - y))*eps
#' mydbcsp <- new("dbcsp", X1 = x, X2 = y, type="fn", eps=0.9)
#'
setClassUnion("numericOrNULL", c("numeric", "NULL"))
dbcsp <- setClass("dbcsp",
                  slots = c(X1="list", X2="list", q="integer", labels="vector", mixture="logical",
                            type="character", w="numeric", training="logical", fold="integer",
                            seed="numericOrNULL", eig.tol = "numeric", verbose="logical",more="list",out="list"))
                  # prototype = list(X1=list(), X2=list(), q=integer(), labels=vector(), mixture=logical(),
                  #                  type=character(), w=numeric(), training=logical(), fold=integer(),
                  #                  seed=numOrNUL(), verbose=logical(), out=list()))



setMethod("initialize", "dbcsp", function(.Object, X1, X2, q=15, labels = c(deparse(substitute(X1)),deparse(substitute(X2))), mixture=FALSE,
                                          type='EUCL', w=0.5, training=FALSE, fold=10, seed=NULL, eig.tol=1e-06, verbose = FALSE,...) {

  if(mixture && (type=='EUCL' || type=='eucl' || type=='euclidean')){
    message('As in mixture option EUCL distance is already used, distance type is set to COR.')
    type='COR'
  }

  # GET EXTRA PARAMETERS
  params <- grabFunctionParameters()
  params_names <- c(".Object","X1","X2","labels","q","mixture","type","w","training","fold","seed","eig.tol","verbose")
  extra_params <- subset(params,!(names(params) %in% params_names))

  # REMOVE NANS
  # class 1
  countNA_X1 <- sum(sapply(X1,is.na))
  if(countNA_X1>0){
    warning('Encountered ', countNA_X1, ' NA values in ', labels[1],' class. A linear interpolation is used to interpolate them.')
    X1 <- lapply(X1, function(x){
      x <- t(apply(x, 1, remove_NA))
    })
  }
  # class 2
  countNA_X2 <- sum(sapply(X2,is.na))
  if(countNA_X2>0){
    warning('Encountered ', countNA_X2, ' NA values in ', labels[2],' class. A linear interpolation is used to interpolate them.')
    X2 <- lapply(X2, function(x){
      x <- t(apply(x, 1, remove_NA))
    })
  }
  .Object@X1 <- X1
  .Object@X2 <- X2
  .Object@labels<- labels
  .Object@q <- as.integer(q)
  .Object@mixture <- mixture
  .Object@type <- type
  .Object@w <- w
  .Object@training <- training
  .Object@fold <- as.integer(fold)
  .Object@seed <- seed
  .Object@eig.tol <- eig.tol
  .Object@verbose <- verbose
  .Object@more <- extra_params

  .Object@out <- compute.dbcsp(.Object)

  .Object
})

compute.dbcsp <-  function(object) {

  data <- c(object@X1,object@X2)
  data_labels <- c(rep(object@labels[1],length(object@X1)),rep(object@labels[2],length(object@X2)))

  if(object@verbose){cat(length(object@X1),'instances for class',object@labels[1],'and',length(object@X2),'instances for class',object@labels[2],'.\n')}

  # standardize
  X1 <- plyr::llply(object@X1, estandarizar)
  X2 <- plyr::llply(object@X2, estandarizar)

  # CSP
  if(object@verbose){
    if(object@mixture){cat('Applying DB-CSP with mixture EUCL +',object@type,'distances.\n')}
    else{cat('Applying DB-CSP with',object@type,'distance.\n')}
  }

  if(object@training){
    sol <- train.dbcsp(object,verbose=FALSE)
    out <- sol@out
  }
  else{
    csp_out <- CSP(X1,X2,q=object@q,mixture=object@mixture,type=object@type,w=object@w,eig.tol=object@eig.tol,more=object@more)
    out <- list("vectors"=csp_out$vectors,"proy"=csp_out$proy,"eig"=csp_out$eig)
  }
  object@out <- out
}


#' @name print.dbcsp
#' @title Print function implemented by dbcsp class
#' @description This function prints information about \code{\link[=dbcsp-class]{dbcsp}} class.
#' @param x object of class \code{\link[=dbcsp-class]{dbcsp}}.
#' @param ... not currently used.
#' @return No return value, called for side effects.
#' @details It provides information about the object and the class.
#' @seealso \code{\link[=dbcsp-class]{dbcsp}}, \code{\link[=summary.dbcsp]{summary}}, \code{\link[=train.dbcsp]{train}}, \code{\link{selectQ}}, \code{\link[=predict.dbcsp]{predict}}, \code{\link[=plot.dbcsp]{plot}}, \code{\link[=boxplot.dbcsp]{boxplot}}
#' @examples
#' # Read data from 2 classes
#' x <- AR.data$come[1:30]
#' y <- AR.data$five[1:30]
#' mydbcsp <- new("dbcsp", X1 = x, X2 = y)
#' print(mydbcsp)
#' @usage \method{print}{dbcsp}(x, ...)
#' @rdname print.dbcsp
#' @exportS3Method print dbcsp
#'
setGeneric("print.dbcsp", function(x,...) standardGeneric("print.dbcsp"))

setMethod("print.dbcsp",
          signature = "dbcsp",
          definition = function(x,...){
            cat('dbcsp object\n')
            name_object <- deparse(substitute(x))
            cat('Object summary:',paste('summary(',name_object,')',sep=''),'\n')
            cat('Documentation: help("dbcsp-class")\n')
            cat('See also: "dbcsp", "summary", "train", "predict", "selectQ", "plot", "boxplot"\n')
            # helpfile <- utils:::.getHelpFile(help("dbcsp-class"))
            # hs <- tools:::Rd2txt(helpfile)
            # cat(hs, sep="\n")
            # invisible(hs)
          }
)

setMethod("show",
          signature = "dbcsp",
          definition = function(object){
            print(object)
          })


#' @name summary.dbcsp
#' @title Summary function implemented by dbcsp class
#' @description This function provides a summary of the dbcsp object and information about the performed process.
#' @param object object of class \code{\link[=dbcsp-class]{dbcsp}}.
#' @param ... not currently used.
#' @return No return value, called for side effects.
#' @details It prints the following information:
#' - Length and shape of the list of matrices of each class.
#' - The number of vectors (dimensions) used in the CSP projection.
#' - Distance used when performing the Common Spatial Patterns algorithm.
#' - If the training process has already been performed, the obtained training accuracy value.
#' @seealso \code{\link[=dbcsp-class]{dbcsp}}, \code{\link[=print.dbcsp]{print}}, \code{\link[=train.dbcsp]{train}}, \code{\link{selectQ}}, \code{\link[=predict.dbcsp]{predict}}, \code{\link[=plot.dbcsp]{plot}}, \code{\link[=boxplot.dbcsp]{boxplot}}
#' @examples
#' # Read data from 2 classes
#' x <- AR.data$come[1:30]
#' y <- AR.data$five[1:30]
#' mydbcsp <- new("dbcsp", X1 = x, X2 = y)
#' summary(mydbcsp)
#' @usage \method{summary}{dbcsp}(object, ...)
#' @rdname summary.dbcsp
#' @exportS3Method summary dbcsp
#'
setGeneric("summary.dbcsp", function(object,...) standardGeneric("summary.dbcsp"))

setMethod("summary.dbcsp",
          signature = "dbcsp",
          definition = function(object,...){
            cat('There are',length(object@X1),'instances of class', object@labels[1], 'with',paste('[', dim(object@X1[[1]])[1],'x',dim(object@X1[[1]])[2],']',sep=""),'dimension.\n')
            cat('There are',length(object@X2),'instances of class', object@labels[2], 'with',paste('[', dim(object@X2[[1]])[1],'x',dim(object@X2[[1]])[2],']',sep=""),'dimension.\n')
            cat('The DB-CSP method has used',object@q,'vectors for the projection.\n')
            if(object@mixture) cat('Distances mixture,',paste('EUCL + ',object@type,',',sep=""),'has been used with', object@w, 'weight for EUCL distance and', 1-object@w, 'for',object@type, 'distance.\n')
            else cat(object@type,'distance has been used.\n')

            if(object@training) cat('An accuracy of',object@out$acc,'has been obtained with',object@fold,'fold cross validation and using', object@out$selected_q,'vectors when training.\n')
            else cat('Training has not been performed yet.\n')
          }
)





#' @name train.dbcsp
#' @title Training process of a dbcsp object, using LDA classifier.
#' @description This function applies DB-CSP to the instances and perform the training of a Linear Discriminant
#' Analysis (LDA) classifier using the object data.
#' @param x object of class \code{\link[=dbcsp-class]{dbcsp}}.
#' @param selected_q integer value indicating the number of vectors to use when training the model, by default \code{selected_q=x@q}.
#' @param fold integer value, by default \code{fold=x@fold}. It controls the number of partitions. If \code{fold==1} a train/test
#' split is performed, with p=0.2 for test indices.
#' @param seed numeric value, by default \code{fold=x@seed}. Set a seed to ensure reproducible results.
#' @param verbose logical
#' @param ... not currently used.
#' @return The \code{\link[=dbcsp-class]{dbcsp}} object with the training results saved as list in \code{x@out}:
#' - \code{vectors} The projection vectors obtained after applying CSP.
#' - \code{eig} The eigenvalues obtained after applying CSP.
#' - \code{proy} The variance values of the projected signals obtained after applying CSP.
#' - \code{acc} The mean accuracy value obtained for training data applying cross validation.
#' - \code{used_folds} List of the folds used in the cross validation.
#' - \code{folds_acc} Accuracy values for each of the folds of the cross validation.
#' - \code{model} The trained LDA classifier.
#' - \code{selected_q} The number of vectors used when training.
#' @seealso \code{\link[=dbcsp-class]{dbcsp}}, \code{\link[=print.dbcsp]{print}}, \code{\link[=summary.dbcsp]{summary}}, \code{\link{selectQ}}, \code{\link[=predict.dbcsp]{predict}}, \code{\link[=plot.dbcsp]{plot}}, \code{\link[=boxplot.dbcsp]{boxplot}}
#' @examples
#' # Read data from 2 classes
#' x <- AR.data$come[1:20]
#' y <- AR.data$five[1:20]
#' mydbcsp <- new("dbcsp", X1 = x, X2 = y)
#' mydbcsp <- train(mydbcsp,fold=3)
#' print(mydbcsp@out$acc)
#' @usage \method{train}{dbcsp}(x, selected_q=x@q, fold=x@fold, seed=x@seed, verbose=TRUE,...)
#' @rdname train.dbcsp
#' @exportS3Method train dbcsp
#'
setGeneric("train.dbcsp", function(x, selected_q=x@q, fold=x@fold, seed=x@seed, verbose=TRUE,...) standardGeneric("train.dbcsp"))

setMethod("train.dbcsp",
          signature = "dbcsp",
          definition = function(x, selected_q=x@q, fold=x@fold, seed=x@seed, verbose=TRUE,...){
            folds <- fold
            data <- c(x@X1,x@X2)
            data_labels <- c(rep(x@labels[1],length(x@X1)),rep(x@labels[2],length(x@X2)))
            selected_q <- as.integer(selected_q)
            if(selected_q>x@q){
              message(paste('Incorrect value for selected_q: 1 <= selected_q <= q (q=',x@q,'). select_q=q will be used.',sep=''))
              selected_q <- x@q
            }
            selected_dim <- c(1:selected_q, (x@q+1):(x@q+selected_q))

            if(verbose){
              cat(length(x@X1),'instances for',x@labels[1],'class and',length(x@X2),'instances for',x@labels[2],'class.\n')
              if(x@mixture) {
                cat('Distances mixture,',paste('EUCL + ',x@type,',',sep=""), 'has been used with', x@w, 'weight for EUCL distance and', 1-x@w, 'for',x@type,'distance.\n')
                cat(x@q,'projections are being used when applying CSP.\n')
              }else cat('Applying DB-CSP with',x@type,'distance and',x@q,'projections.\n')

              cat(selected_q,'projections are used when training the LDA classifier.\n')
            }

            # standardize
            data <- plyr::llply(data, estandarizar)
            if(folds==1){
              set.seed(seed)
              test_folds <- caret::createDataPartition(data_labels,p=0.2)
            }
            else{
              set.seed(seed)
              test_folds <- caret::createFolds(data_labels,k=folds)
            }
            total_acc <- c()
            for(i in 1:folds){

              X_train <- data[-test_folds[[i]]]
              X_test <- data[test_folds[[i]]]
              y_train <- data_labels[-test_folds[[i]]]
              y_test <- data_labels[test_folds[[i]]]

              # TRAIN DATA + CSP
              new_X1 <- X_train[y_train==x@labels[1]]
              new_X2 <- X_train[y_train==x@labels[2]]

              out <- CSP(new_X1, new_X2, q=x@q, mixture=x@mixture, type=x@type, w=x@w, eig.tol=x@eig.tol, getWarning=FALSE,more=x@more)

              proy <- out$proy
              proy <- as.matrix(proy[,1:(2*x@q)])
              actual_train <- data.frame(log(proy))
              # selected q
              actual_train <- actual_train[,selected_dim]

              actual_train$labels <- y_train

              # PREPARE TEST DATA
              TX1 <- plyr::ldply(X_test, calcVarianzas, W=out$vectors)
              actual_test <- data.frame(log(TX1))
              actual_test$labels <- y_test

              # PERFORM CLASSIFICATION
              actual_train$labels <- as.factor(actual_train$labels)
              actual_test$labels <- factor(actual_test$labels, levels=levels(actual_train$labels))

              model <- MASS::lda(labels ~ ., data = actual_train)
              predictions <- stats::predict(model,actual_test[,-length(actual_test)])
              accuracy <- sum(diag(table(predictions$class, actual_test$labels))) / length(actual_test$labels)
              total_acc <- append(total_acc, accuracy)

              if(verbose){cat('Fold',i,'-',length(X_train),'training instances,', length(X_test),'test instances - Acc:',accuracy,'\n')}
            }

            if(verbose){cat(paste('Final accuracy:', mean(total_acc),'\n',sep=" "))}

            # CSP using every instances
            X1 <- plyr::llply(x@X1, estandarizar)
            X2 <- plyr::llply(x@X2, estandarizar)
            csp_out <- CSP(X1, X2, q=x@q, mixture=x@mixture, type=x@type, w=x@w, eig.tol=x@eig.tol,more=x@more)
            # train model with all instances
            proy <- csp_out$proy
            proy <- as.matrix(proy[,1:(2*x@q)])
            train_data <- data.frame(log(proy))
            # selected q
            train_data <- train_data[,selected_dim]

            train_data$labels <- data_labels
            model <- MASS::lda(labels ~ ., data = train_data)

            solution <- list("vectors"=csp_out$vectors,"proy"=csp_out$proy,"eig"=csp_out$eig,"acc"=mean(total_acc),"used_folds"=test_folds,"folds_acc"=total_acc,"model"=model,"selected_q"=selected_q)
            x@out <- solution
            x@training <- TRUE
            x@fold <- as.integer(fold)
            x@seed <- seed

            return(x)
          }
)

#' @name predict.dbcsp
#' @title Predict function implemented by dbcsp class
#' @description This function returns the labels predicted for the input instances. If \code{true_targets} are passed as parameter,
#' the accuracy obtained is printed too.
#' @usage \method{predict}{dbcsp}(object, X_test, true_targets=NULL, ...)
#' @param object object of class \code{\link[=dbcsp-class]{dbcsp}}.
#' @param X_test list of matrices for test data.
#' @param true_targets vector of true labels of the instances. Note that they must match the names of the labels used when training the model.
#' @param ... not currently used.
#' @details It gives the predictions for the test data using the model saved in the object, which has been previously trained with
#' the \code{\link{train.dbcsp}} function. If the \code{true_targets} are indicated, the confusion matrix and obtained accuracy value are
#' returned too.
#' @return The values returned by the LDA \code{\link[MASS]{predict.lda}} function, a list with these components:
#' - \code{class} The MAP classification (a factor)
#' - \code{posterior} Posterior probabilities for the classes
#' - \code{x} The scores of test cases on up to dimen discriminant variables
#'
#' If the \code{true_targets} are indicated, two more items are added to the output list:
#' - \code{confusion_matrix} The confusion matrix obtained with predicted labels and true labels.
#' - \code{acc} The accuracy value obtained for the test instances.
#' @seealso \code{\link[=dbcsp-class]{dbcsp}}, \code{\link[=print.dbcsp]{print}}, \code{\link[=summary.dbcsp]{summary}}, \code{\link[=train.dbcsp]{train}}, \code{\link{selectQ}}, \code{\link[=plot.dbcsp]{plot}}, \code{\link[=boxplot.dbcsp]{boxplot}}
#' @examples
#' # Read data from 2 classes
#' x <- AR.data$come[1:20]
#' y <- AR.data$five[1:20]
#' mydbcsp <- new("dbcsp", X1 = x, X2 = y)
#' mydbcsp <- train(mydbcsp,fold=3)
#' test_data <- c(AR.data$come[20:24], AR.data$five[20:24])
#' test_labels <- c(rep('x',5),rep('y',5))
#' predictions <- predict(mydbcsp,test_data,test_labels)
#' # Predicted classes
#' print(predictions$class)
#' # Confusion matrix
#' print(predictions$confusion_matrix)
#' # Accuracy
#' print(predictions$acc)
#' @rdname predict.dbcsp
#' @exportS3Method predict dbcsp
#'
setGeneric("predict.dbcsp", function(object, X_test, true_targets=NULL,...) standardGeneric("predict.dbcsp"))

setMethod("predict.dbcsp",
          signature = "dbcsp",
          definition = function(object, X_test, true_targets=NULL,...){

            if(object@training){
              model <- object@out$model
              typeof(model)

              # Transform test with CSP output
              vectors <- object@out$vectors

              X_test <- plyr::llply(X_test, estandarizar)
              TX1 <- plyr::ldply(X_test, calcVarianzas, W=vectors)
              actual_test <- data.frame(log(TX1))
              # take just dimensions with which it has been trained the model
              selected_dim <- c(1:object@out$selected_q, (object@q+1):(object@q+object@out$selected_q))
              actual_test <- actual_test[,selected_dim]

              # Predict
              predictions <- stats::predict(model, actual_test)

              if(!is.null(true_targets)){
                real_label <- factor(true_targets, levels=model$lev)
                predicted_label <- predictions$class
                tab <- table(predicted_label, real_label)
                accuracy <- sum(diag(tab)) / length(true_targets)
                predictions$confusion_matrix <- tab
                predictions$acc <- accuracy
              }
              return(predictions)
            }else{
              message('Use train() function to train the model first.')
            }
          }
)


#' @name selectQ
#' @param object object of class \code{\link[=dbcsp-class]{dbcsp}}.
#' @param Q list of integers which represents the dimensions to use, by default \code{Q=c(1,2,3,5,10,15)}.
#' @param train_size float between 0.0 and 1.0 representing the proportion of the dataset to include in the train split, by default \code{train_size=0.75}.
#' @param CV logical indicating if a cross validation must be performed or not (if TRUE, train_size is not used), by default \code{CV=FALSE}.
#' @param folds integer, number of folds to use if CV is performed.
#' @param seed numeric value, by default \code{seed=NULL}. Set a seed to ensure reproducible results.
#' @rdname selectQ
#' @docType methods
setGeneric("selectQ", function(object, Q=c(1, 2, 3, 5, 10, 15), train_size = 0.75, CV=FALSE, folds=10, seed=NULL) standardGeneric("selectQ"))
#' @title Select Q best dimension
#'
#' @description  This function applies DB-CSP and classification with different dimensions to see which gets the best outcomes.
#'
#' @param object object of class \code{\link[=dbcsp-class]{dbcsp}}.
#' @param Q list of integers which represents the dimensions to use, by default \code{Q=c(1,2,3,5,10,15)}.
#' @param train_size float between 0.0 and 1.0 representing the proportion of the dataset to include in the train split, by default \code{train_size=0.75}.
#' @param CV logical indicating if a cross validation must be performed or not (if TRUE, train_size is not used), by default \code{CV=FALSE}.
#' @param folds integer, number of folds to use if CV is performed.
#' @param seed numeric value, by default \code{seed=NULL}. Set a seed to ensure reproducible results.
#' @return A \code{data.frame} including the dimensions and their corresponding accuracy values.
#' If \code{CV=TRUE}, for each dimension, the standard deviation of the accuracy values of the folds is also included in the data frame.
#' @seealso \code{\link[=dbcsp-class]{dbcsp}}, \code{\link[=print.dbcsp]{print}}, \code{\link[=summary.dbcsp]{summary}}, \code{\link[=train.dbcsp]{train}}, \code{\link[=predict.dbcsp]{predict}}, \code{\link[=plot.dbcsp]{plot}}, \code{\link[=boxplot.dbcsp]{boxplot}}
#' @examples
#' # Read data from 2 classes
#' x <- AR.data$come
#' y <- AR.data$five
#' mydbcsp <- new("dbcsp", X1 = x, X2 = y)
#' result <- selectQ(mydbcsp)
#' print(result)
#' @rdname selectQ
#' @docType methods
#' @export

setMethod("selectQ",
          signature = "dbcsp",
          definition = function(object, Q=c(1, 2, 3, 5, 10, 15), train_size = 0.75, CV=FALSE, folds=10, seed=NULL)
          {

            if(CV){
              data <- c(object@X1,object@X2)
              # standardize
              data <- plyr::llply(data, estandarizar)

              labels <- rep(object@labels, times = c(length(object@X1), length(object@X2)))
              #res.parcial <- rep(0,  length(Q))
              res.parcial <- matrix(data=0,nrow=length(Q),ncol=folds)
              set.seed(seed)
              k_folds <- caret::createFolds(labels,folds)
              getWarning <- TRUE
              for(l in 1:folds){
                if(l>1) getWarning <- FALSE
                X_train <- data[-k_folds[[l]]]
                X_test <- data[k_folds[[l]]]
                y_train <- labels[-k_folds[[l]]]
                y_test <- labels[k_folds[[l]]]

                # TRAIN DATA + CSP
                x1 <- X_train[y_train==object@labels[1]]
                x2 <- X_train[y_train==object@labels[2]]

                q <- max(Q)
                out <- CSP(x1, x2, q=q, mixture=object@mixture, type=object@type, w=object@w, eig = TRUE, vectors = TRUE, eig.tol=object@eig.tol, getWarning=getWarning, more=object@more)
                proy <- as.matrix(out$proy[, 1:(2 * q)])
                TX <- plyr::ldply(X_test, calcVarianzas , W = out$vectors)[,1:(2*q)]
                dat <- data.frame(rbind(log(proy), log(TX)), klasea = c(y_train,y_test))

                train <- 1:(length(x1) + length(x2))
                k <- 2 * q + 1
                for (i in 1:length(Q))
                {
                  m <- Q[i]
                  v <- c(1:m, (q + 1):(q + m), k)
                  out1 <- MASS::lda(klasea ~ ., data = dat[, v], subset = train)
                  res.parcial[i,l] <- (sum(diag(table(dat$klasea[-train], stats::predict(out1, dat[-train, v])$class))) / sum(length(X_test)))
                }
              }
              acc <- rowMeans(res.parcial)
              sd <- apply(res.parcial,1,sd)
              results <- data.frame('Q'=Q, 'acc'=acc, 'sd'=sd)
            }
            else{
              num_instances1 <- length(object@X1)
              set.seed(seed)
              X_index_class1 <- sample.int(num_instances1, round(num_instances1*train_size))
              num_instances2 <- length(object@X2)
              set.seed(seed)
              X_index_class2 <- sample.int(num_instances2, round(num_instances2*train_size))

              x1 <- object@X1[X_index_class1]
              x2 <- object@X2[X_index_class2]
              t1 <- object@X1[-X_index_class1]
              t2 <- object@X2[-X_index_class2]

              # standardize
              x1 <- plyr::llply(x1, estandarizar)
              x2 <- plyr::llply(x2, estandarizar)
              t1 <- plyr::llply(t1, estandarizar)
              t2 <- plyr::llply(t2, estandarizar)

              q <- max(Q)
              out <- CSP(x1, x2, q=q, mixture=object@mixture, type=object@type, w=object@w, eig = TRUE, vectors = TRUE, eig.tol=object@eig.tol,more=object@more)
              proy <- as.matrix(out$proy[, 1:(2 * q)])
              TX1 <- plyr::ldply(t1, calcVarianzas , W = out$vectors)[,1:(2*q)]
              TX2 <- plyr::ldply(t2, calcVarianzas , W = out$vectors)[,1:(2*q)]
              dat <- data.frame(rbind(log(proy), log(TX1), log(TX2)),
                                klasea = rep(c(1:2, 1:2), times = c(length(x1), length(x2), length(t1), length(t2))))
              res.parcial <- rep(NA,  length(Q))
              train <- 1:(length(x1) + length(x2))
              k <- 2 * q + 1
              for (i in 1:length(Q))
              {
                m <- Q[i]
                v <- c(1:m, (q + 1):(q + m), k)
                out1 <- MASS::lda(klasea ~ ., data = dat[, v], subset = train)
                res.parcial[i] <- sum(diag(table(dat$klasea[-train], stats::predict(out1, dat[-train, v])$class))) / sum(c(length(t1), length(t2)))
              }
              results <- data.frame('Q'=Q, 'acc'=res.parcial)
            }
            return(results)
          }
)

#' @name plot.dbcsp
#' @title Plot function implemented by dbcsp class
#' @description This function plots an instance before and/or after its DB-CSP projection.
#' @param x object of class \code{\link[=dbcsp-class]{dbcsp}}.
#' @param class integer, which of both classes to access (1 or 2), by default \code{class=1}
#' @param index an integer, representing which instance of the class to plot, by default \code{index=1}.
#' @param vectors an integer or vector of integers, representing the vectors to plot after the projection, by default all the vectors used in the projection are plotted \code{vectors=1:(x@q*2)}.
#' @param pairs logical, if TRUE the pairs of the indicated vectors are also shown, by default \code{pairs=TRUE}.
#' @param before logical, if TRUE the original signals are plotted, by default \code{before=TRUE}.
#' @param after logical, if TRUE the signals after projection are plotted, by default \code{after=TRUE}.
#' @param legend logical, if true the legend of the transformed signals is shown, by default \code{legend=FALSE}. When plotting more than 15 pairs of signals (15*2=30 signals), the legend is not shown. If \code{before=TRUE} legends are not displayed.
#' @param getsignals logical, if TRUE the projected signals for the selected class, instance and vectors are returned, by default \code{getsignals=FALSE}.
#' @param ... optional arguments inherited from the \code{\link{matplot}} method.
#' @return Displays a plot of the selected instance before and/or after the DB-CSP filter projection.
#' The vectors shown after the projection are differentiated by the q first and q last vectors,
#' since the former maximize the variance of one class and minimize the variance of the other, while the latter do the opposite.
#' If \code{getsignals=TRUE}, a matrix with the projected signals shown in the plot is returned.
#' @details It plots an instance before and/or after being projected with the DB-CSP filter.
#' Vectors values must lie between 1 and 2*q, being q the number of dimensions used to perform the DB-CSP
#' algorithm when creating the \code{\link[=dbcsp-class]{dbcsp}} object. The following should be
#' taken into account when plotting:
#' - The first q values (1,...,q) are indicated as a1...aq, and are plotted with solid lines.
#' - The last q values (q+1,...,2*q) are indicated as b1...bq, and are plotted with dashed lines.
#'
#' If \code{pairs=TRUE}, it is recommended that \code{vectors<q} for better understanding,
#' since their pairs are plotted as well. In case that \code{vectors>q}, it should be noted
#' that the values are displayed from b1 to bq, where b1 and bq represent q+1 vector and 2*q vector,
#' respectively. The paired vectors (a1-b1, a2-b2, ...) are plotted with the same color, but different line type.
#'
#' For example if \code{q=15} and \code{plot(object, vectors=16, pairs=FALSE)}, b1 (16-q=1) vector is
#' shown.
#'
#' The number of rows and columns of the layout (mfrow, mfcol) can not be modified, as the function select them
#' according to \code{before} and \code{after} parameters.
#'
#' @examples
#' # Read data from 2 classes
#' x <- AR.data$come
#' y <- AR.data$five
#' mydbcsp <- new("dbcsp", X1 = x, X2 = y)
#' plot(mydbcsp)
#' plot(mydbcsp,class=2,index=30,vectors=1:5,before=FALSE, legend=TRUE)
#' pSignals <- plot(mydbcsp,class=2,index=30,vectors=1:5,before=FALSE, legend=TRUE,getsignals=TRUE)
#' @usage \method{plot}{dbcsp}(x, class = 1, index = 1, vectors = 1:(x@q*2), pairs=TRUE,
#'      before = TRUE, after = TRUE, legend = FALSE, getsignals = FALSE, ...)
#' @seealso \code{\link[=dbcsp-class]{dbcsp}}, \code{\link[=print.dbcsp]{print}}, \code{\link[=summary.dbcsp]{summary}}, \code{\link[=train.dbcsp]{train}}, \code{\link{selectQ}}, \code{\link[=predict.dbcsp]{predict}}, \code{\link[=boxplot.dbcsp]{boxplot}}
#' @rdname plot.dbcsp
#' @exportS3Method plot dbcsp
#'
setGeneric("plot.dbcsp", function(x, class=1, index=1, vectors=1:(x@q*2), pairs = TRUE, before=TRUE, after=TRUE, legend=FALSE, getsignals=FALSE,...) standardGeneric("plot.dbcsp"))

setMethod("plot.dbcsp",
          signature = "dbcsp",
          definition = function(x, class=1, index=1, vectors=1:(x@q*2), pairs = TRUE, before=TRUE, after=TRUE, legend=FALSE, getsignals=FALSE,...){
            # reset par settings on exit
            oldpar <- par(no.readonly = TRUE)
            on.exit(par(oldpar))

            if(max(vectors)>(x@q*2)){
              message(paste('Please indicate a correct value (or values) for vectors: 1 <= index <= 2*q (2*q=',2*x@q,').',sep=''))
            }else{
              if(class==1){instances <- x@X1}
              else{
                if(class!=2){
                  message("Incorrect class value (must be 1 or 2), class=2 will be plotted.")
                  class <- 2
                }
                instances <- x@X2
              }
              # standardize
              std_instances <- plyr::llply(instances, estandarizar)

              if(index > length(instances)){
                message('The selected index ',paste("(",index,")",sep=''),' exceeds the amount of \"',x@labels[class],'\" instances ',
                    paste("(",length(instances),").",sep=''),' The last instance is going to be plotted, ',
                    paste('index=',length(instances),'.',sep=''))
                index <- length(instances)
              }
              not_std_actualinstance <- instances[[index]]
              actual_instance <- std_instances[[index]]
              title1 <- paste('Original signals: instance ',index,' of class ',x@labels[class],'.',sep='')
              # Show selected vectors in pairs
              if(pairs){
                unique_vectors <- unique(c(vectors[vectors<=x@q],vectors[vectors>x@q]-x@q))
                chosen_vectors <- c(unique_vectors,unique_vectors+x@q)
                legend_names <- c(paste('a',unique_vectors,sep=''),paste('b',unique_vectors,sep=''))
                lines_type <- rep(1:2,each=length(unique_vectors))
                lines_col <- rainbow(length(unique_vectors))
              }else{
                unique_vectors <- unique(c(vectors[vectors<=x@q],vectors[vectors>x@q]-x@q))
                chosen_vectors <- unique(vectors)
                legend_names <- ifelse(chosen_vectors <= x@q, paste('a',chosen_vectors,sep=''),paste('b',chosen_vectors-x@q,sep=''))
                lines_type <- ifelse(chosen_vectors <= x@q, 1, 2)
                optional_col <- rainbow(length(unique_vectors))
                vectors_col <- ifelse(vectors <= x@q, vectors, vectors-x@q)
                names(optional_col) <- unique(vectors_col)
                lines_col <- optional_col[match(vectors_col,names(optional_col))]
              }

              projected_instance <- t(actual_instance)%*%x@out$vectors[,chosen_vectors]
              title2 <- paste('Projected signals: instance ',index,' of class ',x@labels[class],'.',sep ='')
              if(length(chosen_vectors) > 30 && legend) cat('There are too many signals to plot, the legend will not be shown')

              ########################################################################################
              # EXTRA ARGUMENTS ...
              args_todo <-sys.call()
              defaultArgs <- c("class","index","vectors","pairs","before","after","legend","getsignals")
              argsNames <- names(args_todo[-1])
              argsValues <- args_todo[-1]
              types <- unlist(lapply(argsValues, typeof))

              argsValues_before <- argsValues
              argsValues_after <- argsValues
              extra_after <-""
              extra_before <- ""
              # Do not use default arguments
              for(i in 1:length(defaultArgs)){
                argsValues_before[defaultArgs[i]] <- NULL
                argsValues_after[defaultArgs[i]] <- NULL
              }

              if(!("lty" %in% argsNames)){
                extra_after <- paste(extra_after,",lty=lines_type",sep="")
                extra_before <- paste(extra_before,",lty=1",sep="")
                legends_lty <- lines_type
              }
              else{
                if(types["lty"]=="character") legends_lty <-as.character(unname(argsValues["lty"]))
                else legends_lty <- eval(parse(text=unname(argsValues["lty"])))
                legends_lty <- rep(legends_lty,length.out=length(lines_type))
              }
              if(!("type" %in% argsNames)){
                argsValues_before["type"] <- "l"
                argsValues_after["type"] <- "l"
              }
              if(!("col" %in% argsNames)){
                extra_after <- paste(extra_after, ",col=lines_col", sep="")
                legends_col <- lines_col
              }
              else{
                if(types["col"]=="character") legends_col <- as.character(unname(argsValues["col"]))
                else legends_col <- eval(parse(text=unname(argsValues["col"])))
              }
              if(!("main" %in% argsNames)){
                argsValues_before["main"] <- title1
                argsValues_after["main"] <- title2
              }else{
                argsValues_before["main"] <- paste(argsValues_before["main"]," - Original Signals",sep="")
                argsValues_after["main"] <- paste(argsValues_after["main"]," - Projected Signals",sep="")
              }
              if(!("xlab" %in% argsNames)){
                argsValues_before["xlab"] <- "time"
                argsValues_after["xlab"] <- "time"
              }
              if(!("ylab" %in% argsNames)){
                argsValues_before["ylab"] <- "value"
                argsValues_after["ylab"] <- "value"
              }

              argsValues_before <- format(argsValues_before)
              argsValues_before <- gsub("  ","",paste(argsValues_before,collapse=""))
              argsValues_before <- sub("^.*?\\((.*)\\)[^)]*$", "\\1", argsValues_before) #strsplit(argsValues,"[()]")[[1]][2]
              added_args_before <- argsValues_before

              argsValues_after <- format(argsValues_after)
              argsValues_after <- gsub("  ","",paste(argsValues_after,collapse=""))
              argsValues_after <- sub("^.*?\\((.*)\\)[^)]*$", "\\1", argsValues_after) #strsplit(argsValues,"[()]")[[1]][2]
              added_args_after <- argsValues_after

              ########################################################################################

              if(before && after){
                par(mfrow=c(2,1))
                if(legend) cat('Set before=FALSE to display legends')
                eval(parse(text = paste("matplot(t(not_std_actualinstance),",added_args_before,extra_before,")")))
                eval(parse(text = paste("matplot(projected_instance,", added_args_after, extra_after,")")))
              }else if(before){
                par(mfrow=c(1,1))
                if(legend) cat('Legends are only shown for transformed signals. Set before=FALSE and after=TRUE to display legends.')
                eval(parse(text = paste("matplot(t(not_std_actualinstance),", added_args_before,extra_before,")")))
              }else if(after){
                # Poner leyenda máximo con 15 vectores, más no que no se ve
                if(length(chosen_vectors) <= 30 && legend) par(mfrow=c(1,1),mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
                eval(parse(text = paste("matplot(projected_instance,", added_args_after, extra_after,")")))
                if(length(chosen_vectors) <= 30 && legend) legend("right", legend_names,col=legends_col,cex=0.8,inset=c(-0.2,0),lty=legends_lty)
              }else{
                message("Set 'before' or/and 'after' TRUE to plot the original signals and the signals after the projection, respectively.")
              }
              if(getsignals) return(projected_instance)
            }
          }
)

#' @name boxplot.dbcsp
#' @title Boxplot for dbcsp object
#' @description This function plots the variance of the selected vectors in a boxplot.
#' @param x object of class \code{\link[=dbcsp-class]{dbcsp}}.
#' @param vectors integer or array of integers, indicating the index of the projected vectors to plot, by default \code{vectors=1}.
#' @param pairs logical, if TRUE the pairs of the indicated vectors are also shown, by default \code{pairs=TRUE}.
#' @param ordered_pairs logical, if TRUE the pairs are plotted next to each other, else pairs are plotted at the end, by default \code{ordered_pairs=TRUE}.
#' @param show_log logical, if TRUE the logarithms of the variances are displayed, else the variances are displayed, by default \code{show_log=TRUE}.
#' @param ... not currently used.
#' @return Displays the boxplot of the variances of the selected vectors.
#' @details A boxplot with the variances of the selected vectors.
#' Vectors values must lie between 1 and 2*q, being q the number of dimensions used to perform the DB-CSP
#' algorithm when creating the \code{\link[=dbcsp-class]{dbcsp}} object. The following should be
#' taken into account when plotting:
#' - The first q values (1,...,q) are indicated as a1...aq
#' - The last q values (q+1,...,2*q) are indicated as b1...bq.
#'
#' If \code{pairs=TRUE}, it is recommended that \code{vectors<q} for better understanding,
#' since their pairs are plotted as well. In case that \code{vectors>q}, it should be noted
#' that the values are displayed from b1 to bq, where b1 and bq represent q+1 vector and 2*q vector,
#' respectively.
#'
#' For example if \code{q=15} and \code{boxplot(object, vectors=16, pairs=FALSE)}, b1 (16-q=1) vector is
#' shown.
#'
#' Among the selected boxplots, the largest whiskers are obtained and just the outliers within these whiskers are shown, the rest are not displayed.
#' With the outliers which are outside the whiskers it is not possible to get a good visualization.
#'
#' @examples
#' # Read data from 2 classes
#' x <- AR.data$come
#' y <- AR.data$five
#' mydbcsp <- new("dbcsp", X1 = x, X2 = y)
#' boxplot(mydbcsp)
#' boxplot(mydbcsp,vectors=1:4,pairs=FALSE)
#' boxplot(mydbcsp, vectors=c(1,4,7),ordered_pairs=FALSE)
#' @usage \method{boxplot}{dbcsp}(x, vectors=1, pairs=TRUE, ordered_pairs=TRUE, show_log=TRUE,...)
#' @seealso \code{\link[=dbcsp-class]{dbcsp}}, \code{\link[=print.dbcsp]{print}}, \code{\link[=summary.dbcsp]{summary}}, \code{\link[=train.dbcsp]{train}}, \code{\link{selectQ}}, \code{\link[=predict.dbcsp]{predict}}, \code{\link[=plot.dbcsp]{plot}}
#' @rdname boxplot.dbcsp
#' @exportS3Method boxplot dbcsp
#'

setGeneric("boxplot.dbcsp", function(x, vectors=1, pairs=TRUE, ordered_pairs=TRUE, show_log=TRUE,...) standardGeneric("boxplot.dbcsp"))

setMethod("boxplot.dbcsp",
          signature = "dbcsp",
          definition = function(x, vectors=1, pairs=TRUE, ordered_pairs=TRUE, show_log=TRUE,...){

            if(max(vectors) > (x@q*2) || min(vectors)==0){
              message(paste('Please indicate a correct vectors value (or values): 1 <= vectors <= 2*q (2*q=',2*x@q,').',sep=''))
            }else{
              if(pairs){
                # Get the first half vectors (if given vectors are of the second half, pass it to the fist to facilitate the process)
                new_ind <- vectors%%x@q
                new_ind[new_ind==0]<-x@q
                unique_ind <- unique(new_ind) # in case they are duplicated
                if(ordered_pairs){ # If ordered, put pairs together
                  proj_names <- c(rbind(paste('a',unique_ind,sep=''),paste('b',unique_ind,sep='')))
                  ind_par <- c(rbind(unique_ind, unique_ind+x@q))
                }else{ # else, the given vectors first and then the pairs
                  proj_names <- c(paste('a',unique_ind,sep=''),paste('b',unique_ind,sep=''))
                  ind_par <- c(unique_ind, unique_ind+x@q)
                }
                num_ind <- length(unique_ind)*2 # in pairs
              }else{ # just the selected vectors, no pairs
                proj_names <- sapply(vectors, function(vec_actual){
                  if(vec_actual<=x@q) paste('a',vec_actual,sep='')
                  else paste('b',vec_actual-x@q,sep='')
                })
                ind_par <- vectors
                num_ind <- length(vectors)
              }

              # create a data frame
              variety <- rep(proj_names, each=length(x@out$proy$group)) # vectors names q1,q2...
              labels <- rep(x@labels, times=c(length(x@X1),length(x@X2)))
              treatment <- rep(labels,times=num_ind) # class, split boxplots by class
              note <- unname(unlist(x@out$proy[,ind_par])) # values for the selected indices (ind_par)

              y_name <- 'variance'
              if(show_log){
                note <- log(note) # Show log of variances
                y_name <- 'log(variance)'
              }

              data <- data.frame(variety, treatment ,  note)
              data$variety <- factor(data$variety , levels=proj_names)

              # compute lower and upper whiskers
              # Among the selected boxplots, get with the largest whiskers
              # Outliers within whiskers are shown, the rest are not displayed
              # This is done because with the outliers which are outside the whiskers is not possible to get a good visualization
              ylim_up <- -Inf
              ylim_bottom <- Inf
              for(i in unique(variety)){
                actual_data1 <- data[data$variety==i,]
                for(class in unique(actual_data1$treatment)){
                  actual_data <- actual_data1[actual_data1$treatment==class,]
                  ylim1 <- grDevices::boxplot.stats(actual_data$note)$stats[1]
                  ylim2 <- grDevices::boxplot.stats(actual_data$note)$stats[5]
                  if(ylim1 < ylim_bottom) ylim_bottom <- ylim1
                  if(ylim2 > ylim_up) ylim_up <- ylim2
                }
              }

              # Put pairs into groups, adding borders to distinguish them
              # Group them only when there are pairs and we want them ordered, otherwise they will go individually
              if(pairs && ordered_pairs){
                groups <- rep(paste('Pair: a', unique_ind, " - b", unique_ind, sep=''),each=(length(x@out$proy$group)*2))
              }
              else{
                ind_name <- sapply(ind_par, function(ind_actual){
                  if(ind_actual<=x@q) paste('a',ind_actual,sep='')
                  else paste('b',ind_actual-x@q,sep='')
                })
                groups <- rep(ind_name,each=length(x@out$proy$group))
              }
              data$groups <- factor(groups,levels=unique(groups))

              # Grouped BOXPLOT
              ggplot2::ggplot(data, ggplot2::aes(x=variety, y=note, fill=treatment )) +
                ggplot2::geom_boxplot(outlier.shape = 1) + ggplot2::coord_cartesian(ylim = c(ylim_bottom, ylim_up)*1.05) +
                ggplot2::labs(title='Boxplot of different projection vectors, by class',
                     x ='projection vector', y = y_name, fill='class') +
                ggplot2::facet_grid(~groups, scales='free_x')

            }
          }
)



