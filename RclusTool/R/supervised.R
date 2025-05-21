# RclusTool: clustering of items in datasets
#
# Copyright 2013 Guillaume Wacquet, Pierre-Alexandre Hebert, Emilie Poisson-Caillault
#                
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.

#' computeSupervised performs supervised classification based on the use of a training set
#' @title Supervised classification 
#' @description Perform supervised classification based on the use of a training set.
#' @param data.sample list containing features, profiles and clustering results.
#' @param prototypes data.frame containing the features of each prototype associated to a class.
#' @param method.name character vector specifying the supervised algorithm to use. Must be 'K-NN' (K-Nearest Neighbor by default), 'MLP' (MultiLayer Perceptron), 'SVM' (Support Vector Machine) or 'RF' (Random Forest).
#' @param RclusTool.env environment in which all global parameters, raw data and results are stored.
#' @param model option to predict directly from model
#' @importFrom class knn
#' @importFrom nnet class.ind nnet
#' @importFrom stats predict
#' @importFrom e1071 svm
#' @importFrom randomForest randomForest
#' @return The function returns a list containing:
#' \item{label}{vector of labels.}
#' \item{summary}{data.frame containing classes summaries (min, max, sum, average, sd).}
#' \item{nbItems}{number of observations.}
#' \item{prototypes}{data.frame containing the features of each prototype associated to a class.}
#' @seealso \code{\link{readTrainSet}}
#'
#' @examples
#'
#' rep <- system.file("extdata", package="RclusTool")
#' featuresFile <- file.path(rep, "sample_example_features.csv")
#' features <- read.csv(featuresFile, header = TRUE)
#' features$ID <- NULL
#' traindir <- file.path(rep, "train_example")
#' tf <- tempfile()
#' write.table(features, tf, sep=",", dec=".")
#'
#' x <- importSample(file.features=tf, dir.save=dirname(tf))
#'
#' train <- readTrainSet(traindir)
#' 
#' res <- computeSupervised(x, prototypes=train)
#'
#' plot(features[,3], features[,4], type = "p", xlab = "x", ylab = "y", 
#' col = res$label, main = "K-Nearest-Neighbor classification")
#' 
#'
#' @export 
#' 
computeSupervised <- function(data.sample, prototypes, method.name = "K-NN", model = NULL, RclusTool.env=initParameters()) {
    train.label <- prototypes[, "Class"]

    #select features from config
    numFeat <- colnames(data.sample$features[["preprocessed"]]$x)[sapply(data.sample$features[["preprocessed"]]$x, is.numeric)]
    selected.var <- intersect(data.sample$config$selectFeat, numFeat)  
    if (!length(selected.var))
        selected.var <- numFeat

    selected.var <- intersect(selected.var, names(prototypes))

    if (length(selected.var)==0)
        stop("Training aborted: there isn't any common features between training set and sample set.")

    train <- prototypes[, selected.var, drop=FALSE]

    # pas de calcul ici : a faire en amont, avec tous ceux necessaires ! (preprocessing)
    #logscale <- data.sample$config$logFeat[data.sample$config$logFeat %in% selected.var] #keep element names
    ##apply log10
    #message("computeSupervised: to be tested!")
    #if (length(logscale)) {
    #    z <- train[, logscale, drop=FALSE]
    #    logscale <- logscale[apply(z, 2, function(x) { all(x>=-RclusTool.env$param$preprocess$zero.threshold)}) ]
    #    if (length(logscale)) {
    #        train[,logscale] <- log10(removeZeros(train[,logscale], threshold=RclusTool.env$param$preprocess$zero.threshold, positive=TRUE))
    #        idxLog <- match(colnames(train), logscale)
    #        colnames(train) <- mapply(function(id,col) {if (!is.na(id)) names(logscale)[id] else col}, idxLog, colnames(train))
    #    }
    #}

    test <- data.sample$features[["preprocessed"]]$x[data.sample$id.clean, selected.var, drop=FALSE]
    test.label <- NULL
    if (method.name=="K-NN") {
        test.label <- class::knn(train, test, cl=train.label, k=min(RclusTool.env$param$classif$unsup$nb.neighbours, min(table(train.label)))) 
    }

    if (method.name=="MLP") {
    	if (is.null(model)) {
        	train.matrix <- nnet::class.ind(train.label)
        	model <- do.call(nnet::nnet, c(list(train, train.matrix), RclusTool.env$param$classif$sup[["nnet"]]))
        }
        model.mlp <- model
        train.levels <- levels(as.factor(train.label))
        res.mlp <- stats::predict(model.mlp, test)
        ind <- apply(res.mlp, MARGIN=1, nnet::which.is.max)
        test.label <- factor(train.levels[ind], levels=train.levels)
    }

    if (method.name=="SVM") {    	
    	if (is.null(model)){
        	model <- do.call(e1071::svm, c(list(train, train.label), RclusTool.env$param$classif$sup[["svm"]]))
        }
        model.svm <- model
        res.svm <- stats::predict(model.svm, test)
        test.label <- unlist(res.svm)
    }

    if (method.name=="RF") {
    	if (is.null(model)) {
        	train.label <- factor(train.label)
        	model <- do.call(randomForest::randomForest, c(list(train, train.label), RclusTool.env$param$classif$sup[["rf"]]))
        }
        model.rf <- model
        res.rf <- stats::predict(model.rf, test)
        test.label <- unlist(res.rf)
    }

    label <- formatLabelSample(test.label, data.sample, new.labels=FALSE, noise.cluster=RclusTool.env$param$preprocess$noise.cluster)

    summary <- clusterSummary(data.sample, label, summary.functions=RclusTool.env$param$analysis$summary.functions)

    nbItems <- rep(1, length(label))
    names(nbItems) <- names(label)

    list(label=label, summary=summary, nbItems=nbItems,
         prototypes = prototypes, model=model)
}
