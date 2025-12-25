##############################################
## Functions to evaluate HEMDAG performance ##
##############################################

#' @name auprc
#' @aliases auprc.single.class
#' @aliases auprc.single.over.classes
#' @title AUPRC measures
#' @description Compute the Area under the Precision Recall Curve (AUPRC) through \pkg{precrec} package.
#' @details The AUPRC (for a single class or for a set of classes) is computed either one-shot or averaged across stratified folds.
#' @details \code{auprc.single.class} computes the AUPRC just for a given class.
#' @details \code{auprc.single.over.classes} computes the AUPRC for a set of classes, returning also the averaged values across the classes.
#' @details For all those classes having zero annotations, the AUPRC is set to 0. These classes are discarded in the computing of the AUPRC
#' averaged across classes, both when the AUPRC is computed one-shot or averaged across stratified folds.
#' @details Names of rows and columns of \code{labels} and \code{predicted} matrix must be provided in the same order, otherwise a stop message is returned.
#' @param folds number of folds on which computing the AUPRC. If \code{folds=NULL} (\code{def.}), the AUPRC is computed one-shot,
#' otherwise the AUPRC is computed averaged across folds.
#' @param seed initialization seed for the random generator to create folds. Set \code{seed} only if \code{folds}\eqn{\neq}\code{NULL}.
#' If \code{seed=NULL} and \code{folds}\eqn{\neq}\code{NULL}, the AUPRC averaged across folds is computed without seed initialization.
#' @param labels vector of the true labels (0 negative, 1 positive examples).
#' @param target matrix with the target multilabel: rows correspond to examples and columns to classes.
#' \eqn{target[i,j]=1} if example \eqn{i} belongs to class \eqn{j}, \eqn{target[i,j]=0} otherwise.
#' @param scores a numeric vector of the values of the predicted labels (scores).
#' @param predicted a numeric matrix with predicted values (scores): rows correspond to examples and columns to classes.
#' @return \code{auprc.single.class} returns a numeric value corresponding to the AUPRC for the considered class;
#' \code{auprc.single.over.classes} returns a list with two elements:
#' \enumerate{
#'  \item average: the average AUPRC across classes;
#'  \item per.class: a named vector with AUPRC for each class. Names correspond to classes.
#' }
#' @export
#' @examples
#' data(labels);
#' data(scores);
#' data(graph);
#' root <- root.node(g);
#' L <- L[,-which(colnames(L)==root)];
#' S <- S[,-which(colnames(S)==root)];
#' prc.single.class <- auprc.single.class(L[,3], S[,3], folds=5, seed=23);
#' prc.over.classes <- auprc.single.over.classes(L, S, folds=5, seed=23);
auprc.single.class <- function(labels, scores, folds=NULL, seed=NULL){
    if(is.matrix(labels) || is.matrix(scores))
        stop("labels or scores must be a vector");
    if(length(scores)!=length(labels))
        stop("length of true and predicted labels does not match");
    if(any(names(labels)!=names(scores)))
        stop("names of labels and scores are not in the same order");
    if(any((labels!=0) & (labels!=1)))
        stop("labels variable must take values 0 or 1");
    if(is.null(folds) && !is.null(seed))
        seed <- NULL;
    if(!is.null(folds) && is.null(seed))
        warning("folds are generated without seed initialization");
    ## degenerate case when all labels are equals
    if((all(labels==0)) || (all(labels==1))){
        if(!is.null(folds)){
            prc.avg <- 0;
            prc.fold <- rep(0,folds);
            prc <- list(average=prc.avg, across.fold=prc.fold);
        }else{
            prc <- 0;
            names(prc) <- "one.shoot";
        }
        return(prc);
    }
    ## compute PRC averaged across folds
    if(!is.null(folds)){
        indices <- 1:length(labels);
        positives <- which(labels==1);
        foldIndex <- stratified.cv.data.single.class(indices, positives, kk=folds, seed=seed);
        testIndex <- mapply(c, foldIndex$fold.positives, foldIndex$fold.negatives, SIMPLIFY=FALSE); ## index of examples used for test set..
        fold.check <- unlist(lapply(testIndex,length));
        if(any(fold.check==0))
            stop("number of folds selected too high: some folds have no examples. Please reduce the number of folds");
        prc.fold <- numeric(folds);
        for(k in 1:folds){
            labels.test <- labels[testIndex[[k]]];
            scores.test <- scores[testIndex[[k]]];
            if(sum(labels.test) > 0){
                if(all(labels.test==1)){ ## degenerate case when all labels in the k fold are equals to 1
                    prc.fold[k] <- 0;
                }else{
                    res <- evalmod(scores=scores.test, labels=labels.test);
                    aucs <- auc(res);
                    prc <- subset(aucs, curvetypes=="PRC");
                    prc.fold[k] <- prc$aucs;
                }
            }else{
                prc.fold[k] <- 0;
            }
        }
        if(all(prc.fold==0)){
            prc.avg <- 0; ## degenerate case in which for each fold all the PRC computed by precrec are 0
        }else{
            classIndex <- which(prc.fold!=0);
            classPerfs <- prc.fold[classIndex];
            prc.avg <- mean(classPerfs);
        }
        prc <- list(average=prc.avg, across.fold=prc.fold);
        return(prc);
    }
    ## compute PRC one-shoot
    res <- evalmod(scores=scores, labels=labels);
    aucs <- auc(res);
    prc <- subset(aucs, curvetypes=="PRC")$aucs;
    names(prc) <- "one.shoot";
    return(prc);
}

#' @rdname auprc
#' @export
auprc.single.over.classes <- function(target, predicted, folds=NULL, seed=NULL){
    if(!is.matrix(target) || !is.matrix(predicted))
        stop("target or predicted must be a matrix");
    n.examples <- nrow(target);
    n.classes <- ncol(target);
    if((n.examples!=nrow(predicted)) || (n.classes!=ncol(predicted)))
        stop ("number of rows or columns do not match between target and predicted classes");
    if(any(colnames(target)!=colnames(predicted)) || any(rownames(target)!=rownames(predicted)))
        stop("rows or columns names of target and predicted are not in the same order");
    if(any((target!=0) & (target!=1)))
        stop("target variable must take values 0 or 1");
    if(is.null(folds) && !is.null(seed))
        seed <- NULL;
    ## AUPRC averaged across folds and classes
    if(!is.null(folds)){
        prc.class <- rep(0,ncol(predicted));
        names(prc.class) <- colnames(predicted);
        for(i in 1:ncol(predicted)){
            prc.class[i] <- auprc.single.class(target[,i], predicted[,i], folds=folds, seed=seed)$average;
        }
        if(all(prc.class==0)){
            prc.avg <- 0;
        }else{
            classIndex <- which(prc.class!=0);
            classPerfs <- prc.class[classIndex];
            prc.avg <- mean(classPerfs);
        }
        prc.res <- list(average=prc.avg, per.class=prc.class);
        return(prc.res);
    }
    ## AUPRC one-shot over classes
    ## if there are classes with zero annotations, we remove them..
    target.names <- colnames(target);
    sample.names <- rownames(target);
    class.ann <- apply(target,2,sum);
    class.noann <- which(class.ann==0);
    check <- length(class.noann)!=0;
    check.degen <- length(class.noann)!=ncol(target); ## degenerate case when all the classes have zero annotations
    if(check & check.degen){
        target <- target[,-class.noann];
        predicted <- predicted[,-class.noann];
    }
    ## degenerate case when target and predicted are vectors: just one class has an annotation. Might happen in cross validation..
    if(!is.matrix(target)){
        selected <- which(class.ann!=0);
        target <- matrix(target, nrow=n.examples, dimnames=list(sample.names, names(selected)));
    }
    if(!is.matrix(predicted)){
        selected <- which(class.ann!=0);
        predicted <- matrix(predicted, nrow=n.examples, dimnames=list(sample.names, names(selected)));
    }
    ## compute PRC considering only those class with non-zero annotations
    prc.class <- rep(0,ncol(predicted));
    names(prc.class) <- colnames(predicted);
    for(i in 1:ncol(predicted)){
        prc.class[i] <- auprc.single.class(target[,i], predicted[,i], folds=NULL, seed=NULL);
    }
    ## if there are classes with zero annotations, set the prc of those classes to zero and restore the start classes order
    if(check & check.degen){
        prc.class <- prc.class[target.names];
        prc.class[is.na(prc.class)] <- 0;
        names(prc.class) <- target.names;
    }
    if(all(prc.class==0)){
            prc.avg <- 0;
    }else{
        classIndex <- which(prc.class!=0);
        classPerfs <- prc.class[classIndex];
        prc.avg <- mean(classPerfs);
    }
    prc.res <- list(average=prc.avg, per.class=prc.class);
    return(prc.res);
}

#' @name auroc
#' @aliases auroc.single.class
#' @aliases auroc.single.over.classes
#' @title AUROC measures
#' @description Compute the Area under the ROC Curve (AUROC) through \pkg{precrec} package.
#' @details The AUROC (for a single class or for a set of classes) is computed either one-shot or averaged across stratified folds.
#' @details \code{auroc.single.class} computes the AUROC just for a given class.
#' @details \code{auroc.single.over.classes} computes the AUROC for a set of classes, including their average values across all the classes.
#' @details For all those classes having zero annotations, the AUROC is set to 0.5. These classes are included in the computing of the AUROC
#' averaged across classes, both when the AUROC is computed one-shot or averaged across stratified folds.
#' @details The AUROC is set to 0.5 to all those classes having zero annotations.
#' Names of rows and columns of \code{labels} and \code{predicted} must be provided in the same order, otherwise a stop message is returned.
#' @param folds number of folds on which computing the AUROC. If \code{folds=NULL} (\code{def.}), the AUROC is computed one-shot,
#' otherwise the AUROC is computed averaged across folds.
#' @param seed initialization seed for the random generator to create folds. Set \code{seed} only if \code{folds}\eqn{\neq}\code{NULL}.
#' If \code{seed=NULL} and \code{folds}\eqn{\neq}\code{NULL}, the AUROC averaged across folds is computed without seed initialization.
#' @param labels vector of the true labels (0 negative, 1 positive examples).
#' @param target annotation matrix: rows correspond to examples and columns to classes.
#' \eqn{target[i,j]=1} if example \eqn{i} belongs to class \eqn{j}, \eqn{target[i,j]=0} otherwise.
#' @param scores a numeric vector of the values of the predicted labels (scores).
#' @param predicted a numeric matrix with predicted values (scores): rows correspond to examples and columns to classes.
#' @return \code{auroc.single.class} returns a numeric value corresponding to the AUROC for the considered class;
#' \code{auprc.single.over.classes} returns a list with two elements:
#' \enumerate{
#'  \item average: the average AUROC across classes;
#'  \item per.class: a named vector with AUROC for each class. Names correspond to classes.
#' }
#' @export
#' @examples
#' data(labels);
#' data(scores);
#' data(graph);
#' root <- root.node(g);
#' L <- L[,-which(colnames(L)==root)];
#' S <- S[,-which(colnames(S)==root)];
#' auc.single.class <- auroc.single.class(L[,3], S[,3], folds=5, seed=23);
#' auc.over.classes <- auroc.single.over.classes(L, S, folds=5, seed=23);
auroc.single.class <- function(labels, scores, folds=NULL, seed=NULL){
    if(is.matrix(labels) || is.matrix(scores))
        stop("labels or scores must be a vector");
    if(length(scores)!=length(labels))
        stop("length of true and predicted labels does not match");
    if(any(names(labels)!=names(scores)))
        stop("names of labels and scores are not in the same order");
    if(any((labels!=0) & (labels!=1)))
        stop("labels variable must take values 0 or 1");
    if(is.null(folds) && !is.null(seed))
        seed <- NULL;
    if(!is.null(folds) && is.null(seed))
        warning("folds are generated without seed initialization");
    ## degenerate case when all labels are equals
    if((all(labels==0)) || (all(labels==1))){
        if(!is.null(folds)){
            auc.avg <- 0.5;
            auc.fold <- rep(0.5,folds);
            auc <- list(average=auc.avg, across.fold=auc.fold);
        }else{
            auc <- 0.5;
            names(auc) <- "one.shoot";
        }
        return(auc);
    }
    ## compute AUC averaged across folds
    if(!is.null(folds)){
        indices <- 1:length(labels);
        positives <- which(labels==1);
        foldIndex <- stratified.cv.data.single.class(indices, positives, kk=folds, seed=seed);
        testIndex <- mapply(c, foldIndex$fold.positives, foldIndex$fold.negatives, SIMPLIFY=FALSE); ## index of examples used for test set..
        fold.check <- unlist(lapply(testIndex,length));
        if(any(fold.check==0))
            stop("number of folds selected too high: some folds have no examples. Please reduce the number of folds");
        auc.fold <- numeric(folds);
        for(k in 1:folds){
            labels.test <- labels[testIndex[[k]]];
            scores.test <- scores[testIndex[[k]]];
            if(sum(labels.test) > 0){
                if(all(labels.test==1)){ ## degenerate case when all labels in the k fold are equals to 1
                    auc.fold[k] <- 0.5;
                }else{
                    res <- evalmod(scores=scores.test, labels=labels.test);
                    aucs <- auc(res);
                    auc <- subset(aucs, curvetypes=="ROC");
                    auc.fold[k] <- auc$aucs;
                }
            }else{
                auc.fold[k] <- 0.5;
            }
        }
        auc.avg <- mean(auc.fold);
        auc <- list(average=auc.avg, across.fold=auc.fold);
        return(auc);
    }
    ## compute AUC one-shoot
    res <- evalmod(scores=scores, labels=labels);
    aucs <- auc(res);
    auc <- subset(aucs, curvetypes=="ROC")$aucs;
    names(auc) <- "one.shoot";
    return(auc);
}

#' @rdname auroc
#' @export
auroc.single.over.classes <- function(target, predicted, folds=NULL, seed=NULL){
    if(!is.matrix(target) || !is.matrix(predicted))
        stop("target or predicted must be a matrix");
    n.examples <- nrow(target);
    n.classes <- ncol(target);
    if((n.examples!=nrow(predicted)) || (n.classes!=ncol(predicted)))
        stop ("number of rows or columns do not match between target and predicted classes");
    if(any(colnames(target)!=colnames(predicted)) || any(rownames(target)!=rownames(predicted)))
        stop("rows or columns names of target and predicted are not in the same order");
    if(any((target!=0) & (target!=1)))
        stop("target variable must take values 0 or 1");
    if(is.null(folds) && !is.null(seed))
        seed <- NULL;
    ## AUROC averaged across folds and classes
    if(!is.null(folds)){
        auc.class <- rep(0,ncol(predicted));
        names(auc.class) <- colnames(predicted);
        for(i in 1:ncol(predicted)){
            auc.class[i] <- auroc.single.class(target[,i], predicted[,i], folds=folds, seed=seed)$average;
        }
        auc.avg <- mean(auc.class);
        auc.res <- list(average=auc.avg, per.class=auc.class);
        return(auc.res);
    }
    ## if there are classes with zero annotations, we remove them..
    target.names <- colnames(target);
    sample.names <- rownames(target);
    class.ann <- apply(target,2,sum);
    class.noann <- which(class.ann==0);
    check <- length(class.noann)!=0;
    check.degen <- length(class.noann)!=ncol(target); ## degenerate case when all the classes have zero annotation
    if(check & check.degen){
        target <- target[,-class.noann];
        predicted <- predicted[,-class.noann];
    }
    ## degenerate case when target and predicted are vectors: just one class has an annotation. Might happen in cross validation..
    if(!is.matrix(target)){
        selected <- which(class.ann!=0);
        target <- matrix(target, nrow=n.examples, dimnames=list(sample.names, names(selected)));
    }
    if(!is.matrix(predicted)){
        selected <- which(class.ann!=0);
        predicted <- matrix(predicted, nrow=n.examples, dimnames=list(sample.names, names(selected)));
    }
    ## compute AUC considering only those class with non-zero annotations
    auc.class <- rep(0,ncol(predicted));
    names(auc.class) <- colnames(predicted);
    for(i in 1:ncol(predicted)){
        auc.class[i] <- auroc.single.class(target[,i],predicted[,i], folds=NULL, seed=NULL);
    }
    ## if there are classes with zero annotations, set the AUC of those classes to zero and restore the start classes order
    if(check & check.degen){
        auc.class <- auc.class[target.names];
        auc.class[is.na(auc.class)] <- 0.5;
        names(auc.class) <- target.names;
    }
    auc.avg <- mean(auc.class);
    auc.res <- list(average=auc.avg, per.class=auc.class);
    return(auc.res);
}

#' @name multilabel.F.measure
#' @aliases F.measure.multilabel
#' @title multilabel F-measure
#' @description Method for computing Precision, Recall, Specificity, Accuracy and F-measure for multiclass and multilabel classification.
#' @details Names of rows and columns of \code{target} and \code{predicted} matrix must be provided in the same order, otherwise a stop message is returned.
#' @param target matrix with the target multilabel: rows correspond to examples and columns to classes.
#' \eqn{target[i,j]=1} if example \eqn{i} belongs to class \eqn{j}, \eqn{target[i,j]=0} otherwise.
#' @param predicted a numeric matrix with discrete predicted values: rows correspond to examples and columns to classes.
#' \eqn{predicted[i,j]=1} if example \eqn{i} is predicted belonging to class \eqn{j}, \eqn{target[i,j]=0} otherwise.
#' @param b.per.example a boolean value.
#' \itemize{
#'  \item \code{TRUE}: results are returned for each example;
#'  \item \code{FALSE}: only the average results are returned;
#' }
#' @return Two different outputs respect to the input parameter \code{b.per.example}:
#' \itemize{
#'  \item \code{b.per.example==FALSE}: a list with a single element average. A named vector with average precision (P), recall (R),
#'   specificity (S), F-measure (F), average F-measure (avF) and Accuracy (A) across examples. F is the F-measure computed as the
#'   harmonic mean between the average precision and recall; av.F is the F-measure computed as average across examples;
#'  \item \code{b.per.example==FALSE}: a list with two elements:
#'   \enumerate{
#'     \item average: a named vector with average precision (P), recall (R), specificity (S), F-measure (F), average F-measure (avF)
#'      and Accuracy (A) across examples;
#'     \item per.example: a named matrix with the Precision (P), Recall (R), Specificity (S), Accuracy (A), F-measure (F) and
#'      av.F-measure (av.F) for each example. Row names correspond to examples, column names correspond respectively to Precision (P), Recall (R),
#'      Specificity (S), Accuracy (A), F-measure (F) and av.F-measure (av.F);
#'   }
#' }
#' @examples
#' data(labels);
#' data(scores);
#' data(graph);
#' root <- root.node(g);
#' L <- L[,-which(colnames(L)==root)];
#' S <- S[,-which(colnames(S)==root)];
#' S[S>0.7] <- 1;
#' S[S<0.7] <- 0;
#' fscore <- F.measure.multilabel(L,S);
#' @export
#' @docType methods
setGeneric("F.measure.multilabel",
    function(target, predicted, b.per.example=FALSE) standardGeneric("F.measure.multilabel"));
#' @rdname multilabel.F.measure
setMethod("F.measure.multilabel", signature(target="matrix", predicted="matrix"),
    function(target, predicted, b.per.example=FALSE){
        n.examples <- nrow(target);
        n.classes <- ncol(target);
        if((n.examples!=nrow(predicted)) || (n.classes!=ncol(predicted)))
            stop ("number of rows or columns do not match between target and predicted classes");
        if(any(colnames(target)!=colnames(predicted)) || any(rownames(target)!=rownames(predicted)))
            stop("rows or columns names of target and predicted are not in the same order");
        if(any((target!=0) & (target!=1)) || any((predicted!=0) & (predicted!=1)))
            stop("target and predicted variables must take values 0 or 1");
        z <- target + predicted;
        TP <- apply(z, 1, function(x){
            return(sum(x==2));
            });
        TN <- apply(z, 1, function(x){
            return(sum(x==0));
        });
        z <- predicted - target;
        FP <- apply(z, 1, function(x){
            return(sum(x==1));
        });
        FN <- apply(z, 1, function(x){
            return(sum(x== -1));
        });
        rm(z);
        ## lines below were useful in debugging :)
        # n <- sum(TP)+sum(TN)+sum(FN)+sum(FP);
        # if(n != (n.examples*n.classes)){
        #     cat("n = ", n, "\n n.examples = ", n.examples, "\n n.classes = ", n.classes, "\n");
        #     cat (" sum(TP) = ", sum(TP), "\n sum(TN) = ", sum(TN), "\n sum(FN) = ", sum(FN), "\n sum(FP) = ", sum(FP), "\n");
        #     warning("something went wrong in F-measure");
        # }
        P <- TP+FP;
        P[which(P==0)] <- 1;  # to avoid division by 0 in precision
        sum.TP.FN <- TP+FN;
        sum.TN.FP <- TN+FP;
        sum.TP.FN[which(sum.TP.FN==0)] <- 1;  # to avoid division by 0 in recall
        sum.TN.FP[which(sum.TN.FP==0)] <- 1;  # to avoid division by 0 in specificity
        precision <- TP/P;
        recall <- TP/sum.TP.FN;
        specificity <- TN/sum.TN.FP;
        prec.rec <- precision+recall;
        prec.rec[which(prec.rec==0)] <- 1;  # to avoid division by 0 for f.measure
        f.measure <- (2*precision*recall)/prec.rec;
        accuracy <- (TP+TN)/n.classes;
        av.precision <- sum(precision)/n.examples;
        av.recall <- sum(recall)/n.examples;
        av.specificity <- sum(specificity)/n.examples;
        av.prec.rec <- av.precision+av.recall;
        if(av.prec.rec == 0){
            av.prec.rec <- 1;
        }
        overall.av.f.measure <- (2*av.precision*av.recall)/av.prec.rec;
        av.f.measure <- sum(f.measure)/n.examples;
        av.accuracy  <- sum(accuracy)/n.examples;
        average <- c(av.precision, av.recall, av.specificity, overall.av.f.measure, av.f.measure, av.accuracy);
        names(average) <- c("P", "R", "S", "F", "avF", "A");
       if(b.per.example){
            per.example <- cbind(precision, recall, specificity, f.measure, accuracy);
            colnames(per.example) <- c("P", "R", "S", "F","A");
            return (list(average=average, per.example=per.example))
        }else{
            return (list(average=average));
        }
    }
)

#' @title Best hierarchical F-score
#' @description Select the best hierarchical F-score by choosing an appropriate threshold in the scores.
#' @details All the examples having no positive annotations are discarded. The predicted scores matrix (\code{predicted}) is rounded
#' according to parameter \code{n.round} and all the values of \code{predicted} are divided by \code{max(predicted)}.
#' Then all the thresholds corresponding to all the different values included in \code{predicted} are attempted, and the threshold
#' leading to the maximum F-measure is selected.
#' @details Names of rows and columns of \code{target} and \code{predicted} matrix must be provided in the same order, otherwise a stop message is returned.
#' @param target matrix with the target multilabel: rows correspond to examples and columns to classes.
#' \eqn{target[i,j]=1} if example \eqn{i} belongs to class \eqn{j}, \eqn{target[i,j]=0} otherwise.
#' @param predicted a numeric matrix with continuous predicted values (scores): rows correspond to examples and columns to classes.
#' @param n.round number of rounding digits to be applied to predicted (\code{default=3}).
#' @param verbose a boolean value. If \code{TRUE} (def.) the number of iterations are printed on stdout.
#' @param b.per.example a boolean value.
#' \itemize{
#'  \item \code{TRUE}: results are returned for each example;
#'  \item \code{FALSE}: only the average results are returned;
#' }
#' @return Two different outputs respect to the input parameter \code{b.per.example}:
#' \itemize{
#'  \item \code{b.per.example==FALSE}: a list with a single element average. A named vector with 7 elements relative to the best result in terms
#'   of the F.measure: Precision (P), Recall (R), Specificity (S), F.measure (F), av.F.measure (av.F), Accuracy (A) and the best selected Threshold (T).
#'   F is the F-measure computed as the harmonic mean between the average precision and recall; av.F is the F-measure computed as the average across
#'   examples and T is the best selected threshold;
#'  \item \code{b.per.example==FALSE}: a list with two elements:
#'   \enumerate{
#'    \item average: a named vector with with 7 elements relative to the best result in terms of the F.measure: Precision (P), Recall (R),
#'     Specificity (S), F.measure (F), av.F.measure (av.F), Accuracy (A) and the best selected Threshold (T);
#'    \item per.example: a named matrix with the Precision (P), Recall (R), Specificity (S), Accuracy (A), F-measure (F), av.F-measure (av.F)
#'     and the best selected Threshold (T) for each example. Row names correspond to examples, column names correspond respectively
#'     to Precision (P), Recall (R), Specificity (S), Accuracy (A), F-measure (F), av.F-measure (av.F) and the best selected Threshold (T);
#'   }
#' }
#' @export
#' @examples
#' data(graph);
#' data(labels);
#' data(scores);
#' root <- root.node(g);
#' L <- L[,-which(colnames(L)==root)];
#' S <- S[,-which(colnames(S)==root)];
#' fscore <- find.best.f(L, S, n.round=3, verbose=TRUE, b.per.example=TRUE);
find.best.f <- function(target, predicted, n.round=3, verbose=TRUE, b.per.example=FALSE){
    if(!is.matrix(target) || !is.matrix(predicted))
        stop("target or predicted must be a matrix");
    n.examples <- nrow(target);
    n.classes <- ncol(target);
    if((n.examples!=nrow(predicted)) || (n.classes!=ncol(predicted)))
        stop("number of rows or columns do not match between target and predicted classes");
    if(any(colnames(target)!=colnames(predicted)) || any(rownames(target)!=rownames(predicted)))
        stop("rows or columns names of target and predicted are not in the same order");
    if(any((target!=0) & (target!=1)))
        stop("labels variable must take values 0 or 1");
    x <- apply(target,1,sum);
    selected <- which(x>0);
    ##  degenerate case when target is a full-zero matrix (all genes without annotations)
    if(length(selected)==0){
        selected <- which(x==0);
    }
    target <- target[selected,];
    ## degenerate case when target is a vector (just one annotated gene)
    if(!is.matrix(target)){
        target <- t(as.matrix(target));
        rownames(target) <- names(selected);
    }
    predicted <- predicted[selected,];
    ## degenerate case when predicted is a vector (just one annotated gene)
    if(!is.matrix(predicted)){
        predicted <- t(as.matrix(predicted));
        rownames(predicted) <- names(selected);
    }
    predicted <- predicted/max(predicted);
    predicted <- round(predicted,n.round);
    n.examples <- nrow(predicted);
    n.classes <- ncol(predicted);
    thresh <- unique(as.numeric(predicted));
    thresh <- sort(thresh);
    best.res <- best <- best.thresh <- 0;
    i <- 0;
    for(t in thresh){
        predicted.labels <- matrix(numeric(n.examples*n.classes), nrow=n.examples);
        predicted.labels[predicted>=t] <- 1;
        res <- F.measure.multilabel(target, predicted.labels, b.per.example);
        if(res$average["F"] > best){
            best <- res$average["F"];
            best.res <- res;
            best.thresh <- t;
        }
        i <- i+1;
        if(i%%100 == 0  && verbose){
            cat("iteration ", i,  "\n");
        }
    }
    ## degenerate case when target is a full-zero matrix: by.def F-score is zero
    if(!is.list(best.res)){
        best.res <- res;
    }
    if(b.per.example){
        best.res$average <- c(best.res$average, best.thresh);
        names(best.res$average)[7] <- "T";
        return(best.res);
    }else{
        best.res <- c(best.res$average, best.thresh);
        names(best.res)[7] <- "T";
        return(best.res);
    }
}

#' @name fmax
#' @title Compute Fmax
#' @description Compute the best hierarchical Fmax either one-shot or averaged across folds
#' @details Names of rows and columns of \code{target} and \code{predicted} matrix must be provided in the same order, otherwise a stop message is returned.
#' @param target matrix with the target multilabel: rows correspond to examples and columns to classes.
#' \eqn{target[i,j]=1} if example \eqn{i} belongs to class \eqn{j}, \eqn{target[i,j]=0} otherwise.
#' @param predicted a numeric matrix with predicted values (scores): rows correspond to examples and columns to classes.
#' @param n.round number of rounding digits to be applied to predicted (\code{default=3}).
#' @param verbose a boolean value. If \code{TRUE} (def.) the number of iterations are printed on stdout.
#' @param b.per.example a boolean value.
#' \itemize{
#'  \item \code{TRUE}: results are returned for each example;
#'  \item \code{FALSE}: only the average results are returned;
#' }
#' @param folds number of folds on which computing the Fmax If \code{folds=NULL} (\code{def.}), the Fmax is computed one-shot,
#' otherwise the Fmax is computed averaged across folds.
#' @param seed initialization seed for the random generator to create folds. Set \code{seed} only if \code{folds}\eqn{\neq}\code{NULL}.
#' If \code{seed=NULL} and \code{folds}\eqn{\neq}\code{NULL}, the Fmax averaged across folds is computed without seed initialization.
#' @return Two different outputs respect to the input parameter \code{b.per.example}:
#' \itemize{
#'  \item \code{b.per.example==FALSE}: a list with a single element average. A named vector with 7 elements relative to the best result in terms
#'   of the F.measure: Precision (P), Recall (R), Specificity (S), F.measure (F), av.F.measure (av.F), Accuracy (A) and the best selected Threshold (T).
#'   F is the F-measure computed as the harmonic mean between the average precision and recall; av.F is the F-measure computed as the average across
#'   examples and T is the best selected threshold;
#'  \item \code{b.per.example==FALSE}: a list with two elements:
#'   \enumerate{
#'    \item average: a named vector with with 7 elements relative to the best result in terms of the F.measure: Precision (P), Recall (R),
#'     Specificity (S), F.measure (F), av.F.measure (av.F), Accuracy (A) and the best selected Threshold (T);
#'    \item per.example: a named matrix with the Precision (P), Recall (R), Specificity (S), Accuracy (A), F-measure (F), av.F-measure (av.F)
#'     and the best selected Threshold (T) for each example. Row names correspond to examples, column names correspond respectively
#'     to Precision (P), Recall (R), Specificity (S), Accuracy (A), F-measure (F), av.F-measure (av.F) and the best selected Threshold (T);
#'   }
#' }
#' @export
#' @examples
#' data(graph);
#' data(labels);
#' data(scores);
#' root <- root.node(g);
#' L <- L[,-which(colnames(L)==root)];
#' S <- S[,-which(colnames(S)==root)];
#' fmax <- compute.fmax(L, S, n.round=3, verbose=TRUE, b.per.example=TRUE, folds=5, seed=23);
compute.fmax <- function(target, predicted, n.round=3, verbose=TRUE, b.per.example=FALSE, folds=NULL, seed=NULL){
    if(!is.matrix(target) || !is.matrix(predicted))
        stop("target or predicted must be a matrix");
    n.examples <- nrow(target);
    n.classes <- ncol(target);
    if((n.examples!=nrow(predicted)) || (n.classes!=ncol(predicted)))
        stop("number of rows or columns do not match between target and predicted classes");
    if(any(colnames(target)!=colnames(predicted)) || any(rownames(target)!=rownames(predicted)))
        stop("rows or columns names of target and predicted are not in the same order");
    if(any((target!=0) & (target!=1)))
        stop("target variable must take values 0 or 1");
    if(is.null(folds) && !is.null(seed))
        seed <- NULL;
    if(!is.null(folds) && is.null(seed))
        warning("folds are generated without seed initialization");
    ## Fmax averaged across folds
    if(!is.null(folds)){
        testIndex <- unstratified.cv.data(predicted, kk=folds, seed=seed);
        avg.res.list <- list();
        res.per.example <- c();
        for(k in 1:folds){
            fold.res <- find.best.f(target[testIndex[[k]],], predicted[testIndex[[k]],], n.round=n.round, verbose=verbose, b.per.example=TRUE);
            avg.res.list[[k]] <- fold.res$average;
            res.per.example <- rbind(res.per.example, fold.res$per.example);
        }
        Fcv <- Reduce("+", avg.res.list)/folds;
        Fmeas <- apply(res.per.example,2,mean);
        names(Fmeas)[4] <- "avF";
        ## degenerate case when both precision and recall are zero..
        if((Fmeas[["P"]] && Fmeas[["R"]]) == 0){     ## sum(res.per.example[,"F"])==0
            Fmax <- 0;
        }else{
            Fmax <- 2 * (Fmeas[["P"]] * Fmeas[["R"]])/((Fmeas[["P"]] + Fmeas[["R"]]));
        }
        names(Fmax) <- "F";
        Fmax.avg <- append(Fmeas, Fmax, after=3);
        Fmax.avg <- append(Fmax.avg, Fcv["T"]);
        res <- list(average=Fmax.avg, per.example=res.per.example);
        if(b.per.example){
            return(res);
        }else{
            res <- res$average;
            return(res);
        }
    }
    ## Fmax one-shot
    res <- find.best.f(target=target, predicted=predicted, n.round=n.round, verbose=verbose, b.per.example=b.per.example);
    return(res);
}

#' @name pxr
#' @aliases precision.at.all.recall.levels.single.class
#' @aliases precision.at.given.recall.levels.over.classes
#' @title Precision-Recall curves
#' @description Compute the Precision-Recall (PxR) values through \pkg{precrec} package.
#' @details \code{precision.at.all.recall.levels.single.class} computes the precision at all recall levels just for a single class.
#' @details \code{precision.at.given.recall.levels.over.classes} computes the precision at fixed recall levels over classes.
#' @param labels vector of the true labels (0 negative, 1 positive examples).
#' @param target matrix with the target multilabel: rows correspond to examples and columns to classes.
#' \eqn{target[i,j]=1} if example \eqn{i} belongs to class \eqn{j}, \eqn{target[i,j]=0} otherwise.
#' @param scores a numeric vector of the values of the predicted labels (scores).
#' @param predicted a numeric matrix with predicted values (scores): rows correspond to examples and columns to classes.
#' @param folds number of folds on which computing the PXR. If \code{folds=NULL} (\code{def.}), the PXR is computed one-shot,
#' otherwise the PXR is computed averaged across folds.
#' @param seed initialization seed for the random generator to create folds. Set \code{seed} only if \code{folds}\eqn{\neq}\code{NULL}.
#' If \code{seed=NULL} and \code{folds}\eqn{\neq}\code{NULL}, the PXR averaged across folds is computed without seed initialization.
#' @param recall.levels a vector with the desired recall levels (\code{def:} \code{from:0.1}, \code{to:0.9}, \code{by:0.1}).
#' @return \code{precision.at.all.recall.levels.single.class} returns a two-columns matrix, representing a pair of precision and recall values.
#' The first column is the precision, the second the recall;
#' \code{precision.at.given.recall.levels.over.classes} returns a list with two elements:
#' \enumerate{
#'  \item average: a vector with the average precision at different recall levels across classes;
#'  \item fixed.recall: a matrix with the precision at different recall levels: rows are classes, columns precision at different recall levels;
#' }
#' @export
#' @examples
#' data(labels);
#' data(scores);
#' data(graph);
#' root <- root.node(g);
#' L <- L[,-which(colnames(L)==root)];
#' S <- S[,-which(colnames(S)==root)];
#' labels <- L[,1];
#' scores <- S[,1];
#' rec.levels <- seq(from=0.25, to=1, by=0.25);
#' pxr.single <- precision.at.all.recall.levels.single.class(labels, scores);
#' pxr <- precision.at.given.recall.levels.over.classes(L, S, folds=5, seed=23,
#'          recall.levels=rec.levels);
precision.at.all.recall.levels.single.class <- function(labels, scores){
    if(is.matrix(labels) || is.matrix(scores))
        stop("labels or scores must be a vector");
    if(length(scores)!=length(labels))
        stop("length of true and predicted labels does not match");
    if(any(names(labels)!=names(scores)))
        stop("names of labels and scores are not in the same order");
    if(any((labels!=0) & (labels!=1)))
        stop("labels variable must take values 0 or 1");
    if(all(labels==0) || all(labels==1)){
        recall <- seq(from=0.0, to=1, by=0.25);
        precision <- rep(0, length(recall));
        pxr <- cbind(precision=precision, recall=recall);
        return(pxr);
    }else{
        res <- evalmod(mode="basic", labels=labels, scores=scores);
        df <- data.frame(res, stringsAsFactors=TRUE);
        precision <- subset(df, df$type=="precision")$y;
        recall <- subset(df, df$type=="sensitivity")$y;
        pxr <- cbind(precision=precision, recall=recall);
        return(pxr);
    }
}

#' @rdname pxr
#' @export
precision.at.given.recall.levels.over.classes <- function(target, predicted, folds=NULL, seed=NULL, recall.levels=seq(from=0.1, to=1, by=0.1)){
    if(!is.matrix(target) || !is.matrix(predicted))
        stop("target or predicted must be a matrix");
    n.examples <- nrow(target);
    n.classes <- ncol(target);
    if((n.examples!=nrow(predicted)) || (n.classes!=ncol(predicted)))
        stop ("number of rows or columns do not match between target and predicted classes");
    if(any(colnames(target)!=colnames(predicted)) || any(rownames(target)!=rownames(predicted)))
        stop("rows or columns names of target and predicted are not in the same order");
    if(any((target!=0) & (target!=1)))
        stop("target variable must take values 0 or 1");
    if(is.null(folds) && !is.null(seed))
        seed <- NULL;
    if(!is.null(folds) && is.null(seed))
        warning("folds are generated without seed initialization");
    n.classes <- ncol(predicted);
    classes.names <- colnames(predicted);
    len.level <- length(recall.levels);
    pxr <- c();
    ## PXR cross-validated
    if(!is.null(folds)){
        for(i in 1:n.classes){
            labels <- target[,i];
            scores <- predicted[,i];
            df <- create.stratified.fold.df(labels=labels, scores=scores, folds=folds, seed=seed);
            nfold <- format_nfold(nfold_df=df,  score_cols="scores", lab_col="labels", fold_col="folds");
            prec2rec <- c(); ## storing the higher precisions at fixed recall level in the k_th fold
            pxr.fold <- c(); ## storing the higher precisions at fixed recall level average across the k_th folds
            for(k in 1:folds){
                for(j in 1:len.level){
                    ## degenerate case in which labels in the fold are all positive or all negative
                    if(all(nfold$labels[[k]]==1) || all(nfold$labels[[k]]==2)){
                        prec2rec[j] <- 0;
                    }else{
                        res <- evalmod(mode="basic", scores=nfold$scores[k], labels=nfold$labels[k], modnames="m1", dsids=k);
                        df <- data.frame(res, stringsAsFactors=TRUE);
                        precision <- subset(df, df$type=="precision")$y;
                        recall <- subset(df, df$type=="sensitivity")$y;
                        ## we take the higher precision value at the given recall level. NB: recall is monotone...
                        prec2rec[j] <- precision[which(recall - recall.levels[j]>=0)[1]];
                    }
                }
                pxr.fold <- c(pxr.fold, prec2rec);
            }
            if(all(pxr.fold==0)){
                prec2rec <- rep(0,len.level);
            }else{
                for(j in 1:len.level){
                    tmp <- pxr.fold[seq(j,len.level*folds,len.level)];
                    if(all(tmp==0)){
                        prec2rec[j] <- 0;
                    }else{
                        prec2rec[j] <- mean(tmp[which(tmp!=0)]);
                    }
                }
            }
            pxr <- rbind(pxr, prec2rec);
        }
        dimnames(pxr) <- list(classes.names, recall.levels);
        avgpxr <- apply(pxr, 2, mean);
        names(avgpxr) <- recall.levels;
        res <- list(average=avgpxr, fixed.recall=pxr);
        return(res);
    }
    # PXR one-shot
    for(i in 1:n.classes){
        labels <- target[,i];
        scores <- predicted[,i];
        prec2rec <- c();
        if(all(labels==0) || all(labels==1)){
            prec2rec <- rep(0,len.level);
        }else{
            for(j in 1:len.level){
                res <- evalmod(mode="basic", labels=labels, scores=scores);
                df <- data.frame(res, stringsAsFactors=TRUE);
                precision <- subset(df, df$type=="precision")$y;
                recall <- subset(df, df$type=="sensitivity")$y;
                ## we take the higher precision value at the given recall level. NB: recall is monotone...
                prec2rec[j] <- precision[which(recall - recall.levels[j]>=0)[1]];
            }
        }
        pxr <- rbind(pxr, prec2rec);
    }
    dimnames(pxr) <- list(classes.names, recall.levels);
    avgpxr <- apply(pxr, 2, mean);
    names(avgpxr) <- recall.levels;
    res <- list(average=avgpxr, fixed.recall=pxr);
    return(res);
}

#' @name stratified.cross.validation
#' @title Stratified cross validation
#' @description Generate data for the stratified cross-validation.
#' @details Folds are \emph{stratified}, i.e. contain the same amount of positive and negative examples.
#' @param labels labels matrix. Rows are genes and columns are classes. Let's denote \eqn{M} the labels matrix.
#' If \eqn{M[i,j]=1}, means that the gene \eqn{i} is annotated with the class \eqn{j}, otherwise \eqn{M[i,j]=0}.
#' @param examples indices or names of the examples. Can be either a vector of integers or a vector of names.
#' @param positives vector of integers or vector of names. The indices (or names) refer to the indices (or names) of 'positive' examples.
#' @param kk number of folds (\code{def. kk=5}).
#' @param seed seed of the random generator (\code{def. seed=NULL}). If is set to \code{NULL} no initialization is performed.
#' @return \code{stratified.cv.data.single.class} returns a list with 2 two component:
#' \itemize{
#'  \item fold.non.positives: a list with \eqn{k} components. Each component is a vector with the indices (or names) of the non-positive elements.
#'     Indexes (or names) refer to row numbers (or names) of a data matrix;
#'     \item fold.positives: a list with \eqn{k} components. Each component is a vector with the indices (or names) of the positive elements.
#'     Indexes (or names) refer to row numbers (or names) of a data matrix;
#' }
#' @examples
#' data(labels);
#' examples.index <- 1:nrow(L);
#' examples.name <- rownames(L);
#' positives <- which(L[,3]==1);
#' x <- stratified.cv.data.single.class(examples.index, positives, kk=5, seed=23);
#' y <- stratified.cv.data.single.class(examples.name, positives, kk=5, seed=23);
#' z <- stratified.cv.data.over.classes(L, examples.index, kk=5, seed=23);
#' k <- stratified.cv.data.over.classes(L, examples.name, kk=5, seed=23);
#' @export
stratified.cv.data.single.class <- function(examples, positives, kk=5, seed=NULL){
    set.seed(seed);
    if(is.numeric(examples) && length(names(positives))!=0)
        positives <- unname(positives);
    if(is.character(examples) && length(names(positives))!=0)
        positives <- names(positives);
    ## degenerate case when labels have only one positive
    if(length(positives)==1){
        positives <- positives;
    }else{
        positives <- sample(positives);
    }
    ## degenerate case when labels have only one negative
    negatives <- setdiff(examples,positives);
    if(length(negatives)==1){
        negatives <- negatives;
    }else{
        negatives <- sample(negatives);
    }
    n <- length(positives);
    m <- length(negatives);
    set.pos <- list();
    set.neg <- list();
    for (k in 1:kk) {
        ## folds of indices of positive examples
        last.pos <- (k * n) %/% kk;
        first.pos  <- ((k - 1) * n) %/% kk;
        size.pos <-  last.pos - first.pos;
        if(size.pos>1){subset.pos <- positives[1:size.pos];}
        if(size.pos==1){subset.pos <- positives[size.pos];}
        if(size.pos==0){subset.pos <- integer(0);}
        set.pos[[k]] <- subset.pos;
        positives <- setdiff(positives, subset.pos);
        ## folds of indices of negatives examples
        last.neg <- (k * m) %/% kk;
        first.neg  <- ((k - 1) * m) %/% kk;
        size.neg <-  last.neg - first.neg;
        if(size.neg>1){subset.neg <- negatives[1:size.neg];}
        if(size.neg==1){subset.neg <- negatives[size.neg];}
        if(size.neg==0){subset.neg <- integer(0);}
        set.neg[[k]] <- subset.neg;
        negatives <- setdiff(negatives, subset.neg);
    }
    return(list(fold.positives=set.pos, fold.negatives=set.neg));
}

#' @rdname stratified.cross.validation
#' @return \code{stratified.cv.data.over.classes} returns a list with \eqn{n} components, where \eqn{n} is the number of classes of the labels matrix.
#' Each component \eqn{n} is in turn a list with \eqn{k} elements, where \eqn{k} is the number of folds.
#' Each fold contains an equal amount of positives and negatives examples.
#' @export
stratified.cv.data.over.classes <- function(labels, examples, kk=5, seed=NULL){
    set.seed(seed);
    folds <- list();
    for(class in colnames(labels)){
        folds[[class]] <- list();
        positives <- which(labels[,class]==1);
        strfold <- stratified.cv.data.single.class(examples,positives, kk=kk, seed=seed);
        for(k in 1:kk){
            folds[[class]][[k]] <- list();
            names(folds[[class]])[k] <- paste0("fold",k);
            folds[[class]][[k]] <- append(strfold$fold.positives[[k]], strfold$fold.negatives[[k]]);
        }
    }
    return(folds);
}

#' @title DataFrame for stratified cross validation
#' @description Create a data frame for stratified cross-validation.
#' @details Folds are \emph{stratified}, i.e. contain the same amount of positive and negative examples.
#' @param labels vector of the true labels (0 negative, 1 positive).
#' @param scores a numeric vector of the values of the predicted labels.
#' @param seed initialization seed for the random generator to create folds (\code{def. seed=23}).
#' If \code{seed=NULL}, the stratified folds are generated without seed initialization.
#' @param folds number of folds of the cross validation (\code{def. folds=5}).
#' @return A data frame with three columns:
#' \itemize{
#'  \item \code{scores}: contains the predicted scores;
#'    \item \code{labels}: contains the labels as \code{pos} or \code{neg};
#'  \item \code{folds}: contains the index of the fold in which the example falls.
#'    The index can range from 1 to the number of folds.
#' }
#' @export
#' @examples
#' data(labels);
#' data(scores);
#' df <- create.stratified.fold.df(L[,3], S[,3], folds=5, seed=23);
create.stratified.fold.df <- function(labels, scores, folds=5, seed=23){
    if(is.matrix(labels) || is.matrix(scores))
        stop("labels or scores must be a vector");
    if(length(scores)!=length(labels))
        stop("length of true and predicted labels does not match");
    if(any((labels!=0) & (labels!=1)))
        stop("labels variable must take values 0 or 1");
    if(!(abs(folds - round(folds)) < .Machine$double.eps^0.5))
        stop("folds must be an integer number");
    if(is.null(seed))
        warning("folds are generated without seed initialization");
    indices <- 1:length(labels);
    positives <- which(labels==1);
    foldIndex <- stratified.cv.data.single.class(indices, positives, kk=folds, seed=seed);
    testIndex <- mapply(c, foldIndex$fold.positives, foldIndex$fold.negatives, SIMPLIFY=FALSE);
    fold.check <- unlist(lapply(testIndex,length));
    if(any(fold.check==0))
        stop("number of folds selected too high: some folds have no examples. Please reduce the number of folds");
    fold.len <- sapply(testIndex,length);
    tmp.list <- vector(mode="list", length=folds);
    for(k in 1:folds)
        tmp.list[[k]] <- rep(k, length(testIndex[[k]]));
    foldcol <- numeric(length(scores));
    labelschar <- ifelse(labels==1, "pos", "neg");
    df <- data.frame(scores, labelschar, foldcol, stringsAsFactors=TRUE);
    for(i in 1:length(tmp.list))
        df$foldcol[testIndex[[i]]] <- tmp.list[[i]];
    names(df) <- c("scores","labels","folds");
    return(df);
}
