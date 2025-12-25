library(HEMDAG);
source("make.test.data.R");

context("test perfmeas");

test_that("auprc works", {
    S <- make.scores();
    ann <- make.ann();
    S.degen <- S[,c("A","B","C")];
    ann.degen <- ann[,c("A","B","C")];
    tmp <- tempfile();

    ## auprc one-shot
    auprc <- auprc.single.over.classes(ann, S, folds=NULL, seed=NULL);
    write.table(data.frame(auprc), row.names=TRUE, col.names=TRUE, quote=FALSE, file=tmp);
    auprc.tmp <- read.table(tmp);
    auprc.check <- list(average=unique(auprc.tmp$average), per.class=auprc.tmp$per.class);
    names(auprc.check$per.class) <- colnames(ann);
    expect_equal(auprc, auprc.check);

    ## auprc cross-validated
    auprc <- auprc.single.over.classes(ann, S, folds=2, seed=23);
    write.table(data.frame(auprc), row.names=TRUE, col.names=TRUE, quote=FALSE, file=tmp);
    auprc.tmp <- read.table(tmp);
    auprc.check <- list(average=unique(auprc.tmp$average), per.class=auprc.tmp$per.class);
    names(auprc.check$per.class) <- colnames(ann);
    expect_equal(auprc, auprc.check);

    ## degenerate case: one class is annotated
    ann.degen[,c("B","C")] <- 0;
    auprc <- auprc.single.over.classes(ann.degen, S.degen, folds=NULL, seed=NULL);
    write.table(data.frame(auprc), row.names=TRUE, col.names=TRUE, quote=FALSE, file=tmp);
    auprc.tmp <- read.table(tmp);
    auprc.check <- list(average=unique(auprc.tmp$average), per.class=auprc.tmp$per.class);
    names(auprc.check$per.class) <- colnames(ann.degen);
    expect_equal(auprc, auprc.check);

    ## degenerate case (single class across folds): all labels in the k fold are equals to 1
    labels.degen <- ann[,"I"];
    scores.degen <- S[,"I"];
    labels.degen[2] <- 1;
    auprc <- auprc.single.class(labels.degen, scores.degen, folds=2, seed=23);
    write.table(data.frame(auprc), row.names=TRUE, col.names=TRUE, quote=FALSE, file=tmp);
    auprc.tmp <- read.table(tmp);
    auprc.check <- list(average=unique(auprc.tmp$average), across.fold=auprc.tmp$across.fold);
    expect_equal(auprc, auprc.check);

    ## degenerate case (across folds): all classes have zero annotations
    ## classes without annotations are discarded from average AUPRC
    ann.degen[ann.degen==1] <- 0;
    auprc <- auprc.single.over.classes(ann.degen, S.degen, folds=2, seed=23);
    write.table(data.frame(auprc), row.names=TRUE, col.names=TRUE, quote=FALSE, file=tmp);
    auprc.tmp <- read.table(tmp);
    auprc.check <- list(average=unique(auprc.tmp$average), per.class=auprc.tmp$per.class);
    names(auprc.check$per.class) <- colnames(ann.degen);
    expect_equal(auprc, auprc.check);

    ## stop messages
    labels <- ann[,"I"];
    scores <- S[,"I"];
    labels.flip <- labels[order(length(labels):1)];
    labels.trunk <- labels[-c(1,2,3,5)];
    S.flip <- S[order(nrow(S):1),order(ncol(S):1)];
    S.trunk <- S[,-c(1,2,3,5)];
    expect_error(auprc.single.over.classes(labels, scores), "target or predicted must be a matrix");
    expect_error(auprc.single.over.classes(ann, S.trunk), "number of rows or columns do not match between target and predicted classes");
    expect_error(auprc.single.over.classes(ann, S.flip), "rows or columns names of target and predicted are not in the same order");
    expect_error(auprc.single.over.classes(S, ann), "target variable must take values 0 or 1");
    expect_error(auprc.single.class(ann, S), "labels or scores must be a vector");
    expect_error(auprc.single.class(labels.trunk, scores), "length of true and predicted labels does not match");
    expect_error(auprc.single.class(labels.flip, scores), "names of labels and scores are not in the same order");
    expect_error(auprc.single.class(scores, labels), "labels variable must take values 0 or 1");
    expect_error(auprc.single.class(labels, scores, folds=10, seed=23),
        "number of folds selected too high: some folds have no examples. Please reduce the number of folds");

    ## warning messages
    expect_warning(auprc.single.class(labels, scores, folds=2, seed=NULL), "folds are generated without seed initialization");

    ## seed autoset to NULL (single class)
    auprc <- auprc.single.class(labels, scores, folds=NULL, seed=1);
    write.table(data.frame(auprc), row.names=TRUE, col.names=TRUE, quote=FALSE, file=tmp);
    auprc.tmp <- read.table(tmp);
    auprc.check <- auprc.tmp[,1];
    names(auprc.check) <- "one.shoot";
    expect_equal(auprc, auprc.check);

    ## seed autoset to NULL (over classes)
    auprc <- auprc.single.over.classes(ann, S, folds=NULL, seed=1);
    write.table(data.frame(auprc), row.names=TRUE, col.names=TRUE, quote=FALSE, file=tmp);
    auprc.tmp <- read.table(tmp);
    auprc.check <- list(average=unique(auprc.tmp$average), per.class=auprc.tmp$per.class);
    names(auprc.check$per.class) <- colnames(S);
    expect_equal(auprc, auprc.check);
})

test_that("auroc works", {
    S <- make.scores();
    ann <- make.ann();
    ann.degen <- ann[,c("A","B","C")];
    S.degen <- S[,c("A","B","C")];
    tmp <- tempfile();

    ## auroc one-shot
    auroc <- auroc.single.over.classes(ann, S, folds=NULL, seed=NULL);
    write.table(data.frame(auroc), row.names=TRUE, col.names=TRUE, quote=FALSE, file=tmp);
    auroc.tmp <- read.table(tmp);
    auroc.check <- list(average=unique(auroc.tmp$average), per.class=auroc.tmp$per.class);
    names(auroc.check$per.class) <- colnames(ann);
    expect_equal(auroc, auroc.check);

    ## auroc cross-validated
    auroc <- auroc.single.over.classes(ann, S, folds=2, seed=23);
    write.table(data.frame(auroc), row.names=TRUE, col.names=TRUE, quote=FALSE, file=tmp);
    auroc.tmp <- read.table(tmp);
    auroc.check <- list(average=unique(auroc.tmp$average), per.class=auroc.tmp$per.class);
    names(auroc.check$per.class) <- colnames(ann);
    expect_equal(auroc, auroc.check);

    ## degenerate case: only one class is annotated
    ann.degen[,c("B","C")] <- 0;
    auroc <- auroc.single.over.classes(ann.degen, S.degen, folds=NULL, seed=NULL);
    write.table(data.frame(auroc), row.names=TRUE, col.names=TRUE, quote=FALSE, file=tmp);
    auroc.tmp <- read.table(tmp);
    auroc.check <- list(average=unique(auroc.tmp$average), per.class=auroc.tmp$per.class);
    names(auroc.check$per.class) <- colnames(ann.degen);
    expect_equal(auroc, auroc.check);

    ## degenerate case (single class across folds): all labels in the k fold are equals to 1
    labels.degen <- ann[,"I"];
    scores.degen <- S[,"I"];
    labels.degen[2] <- 1;
    auroc <- auroc.single.class(labels.degen, scores.degen, folds=2, seed=23);
    write.table(data.frame(auroc), row.names=TRUE, col.names=TRUE, quote=FALSE, file=tmp);
    auroc.tmp <- read.table(tmp);
    auroc.check <- list(average=unique(auroc.tmp$average), across.fold=auroc.tmp$across.fold);
    expect_equal(auroc, auroc.check);

    ## stop messages
    labels <- ann[,"I"];
    scores <- S[,"I"];
    labels.flip <- labels[order(length(labels):1)];
    labels.trunk <- labels[-c(1,2,3,5)];
    S.flip <- S[order(nrow(S):1),order(ncol(S):1)];
    S.trunk <- S[,-c(1,2,3,5)];
    expect_error(auroc.single.over.classes(labels, scores), "target or predicted must be a matrix");
    expect_error(auroc.single.over.classes(ann, S.trunk), "number of rows or columns do not match between target and predicted classes");
    expect_error(auroc.single.over.classes(ann, S.flip), "rows or columns names of target and predicted are not in the same order");
    expect_error(auroc.single.over.classes(S, ann), "target variable must take values 0 or 1");
    expect_error(auroc.single.class(ann, S), "labels or scores must be a vector");
    expect_error(auroc.single.class(labels.trunk, scores), "length of true and predicted labels does not match");
    expect_error(auroc.single.class(labels.flip, scores), "names of labels and scores are not in the same order");
    expect_error(auroc.single.class(scores, labels), "labels variable must take values 0 or 1");
    expect_error(auroc.single.class(labels, scores, folds=10, seed=23),
        "number of folds selected too high: some folds have no examples. Please reduce the number of folds");

    ## warning messages
    expect_warning(auroc.single.class(labels, scores, folds=2, seed=NULL), "folds are generated without seed initialization");

    ## seed autoset to NULL (single class)
    auroc <- auroc.single.class(labels, scores, folds=NULL, seed=1);
    write.table(data.frame(auroc), row.names=TRUE, col.names=TRUE, quote=FALSE, file=tmp);
    auroc.tmp <- read.table(tmp);
    auroc.check <- auroc.tmp[,1];
    names(auroc.check) <- "one.shoot";
    expect_equal(auroc, auroc.check);

    ## seed autoset to NULL (over classes)
    auroc <- auroc.single.over.classes(ann, S, folds=NULL, seed=1);
    write.table(data.frame(auroc), row.names=TRUE, col.names=TRUE, quote=FALSE, file=tmp);
    auroc.tmp <- read.table(tmp);
    auroc.check <- list(average=unique(auroc.tmp$average), per.class=auroc.tmp$per.class);
    names(auroc.check$per.class) <- colnames(S);
    expect_equal(auroc, auroc.check);
})

test_that("fmax works", {
    S <- make.scores();
    ann <- make.ann();
    tmp.av <- tempfile();
    tmp.ex <- tempfile();

    ## fmax one-shot
    fmax <- compute.fmax(ann, S, n.round=3, verbose=FALSE, b.per.example=TRUE, folds=NULL, seed=NULL);
    write.table(fmax$average, row.names=TRUE, col.names=FALSE, quote=FALSE, file=tmp.av);
    write.table(fmax$per.example, row.names=TRUE, col.names=TRUE, quote=FALSE, file=tmp.ex);
    fmax.tmp <- read.table(tmp.av, stringsAsFactor=FALSE);
    fmax.av <- fmax.tmp[,2];
    names(fmax.av) <- fmax.tmp[,1];
    fmax.ex <- as.matrix(read.table(tmp.ex));
    fmax.check <- list(average=fmax.av, per.example=fmax.ex);
    expect_equal(fmax, fmax.check);

    fmax <- compute.fmax(ann, S, n.round=3, verbose=FALSE, b.per.example=FALSE, folds=NULL, seed=NULL);
    write.table(fmax, row.names=TRUE, col.names=FALSE, quote=FALSE, file=tmp.av);
    fmax.tmp <- read.table(tmp.av, stringsAsFactor=FALSE);
    fmax.check <- fmax.tmp[,2];
    names(fmax.check) <- fmax.tmp[,1];
    expect_equal(fmax, fmax.check);

    ## famx cross-validated
    fmax <- compute.fmax(ann, S, n.round=3, verbose=FALSE, b.per.example=TRUE, folds=2, seed=23);
    write.table(fmax$average, row.names=TRUE, col.names=FALSE, quote=FALSE, file=tmp.av);
    write.table(fmax$per.example, row.names=TRUE, col.names=TRUE, quote=FALSE, file=tmp.ex);
    fmax.tmp <- read.table(tmp.av, stringsAsFactor=FALSE);
    fmax.av <- fmax.tmp[,2];
    names(fmax.av) <- fmax.tmp[,1];
    fmax.ex <- as.matrix(read.table(tmp.ex));
    fmax.check <- list(average=fmax.av, per.example=fmax.ex);
    expect_equal(fmax, fmax.check);

    ## degenerate case
    ann.degen <- ann[,c("B","C")];
    ann.degen[,c("B","C")] <- 0; ## all examples not annotated
    S.degen <- S[,c("B","C")];
    fmax <- compute.fmax(ann.degen, S.degen, folds=2, seed=23, b.per.example=TRUE);
    write.table(fmax$average, row.names=TRUE, col.names=FALSE, quote=FALSE, file=tmp.av);
    write.table(fmax$per.example, row.names=TRUE, col.names=TRUE, quote=FALSE, file=tmp.ex);
    fmax.tmp <- read.table(tmp.av, stringsAsFactor=FALSE);
    fmax.av <- fmax.tmp[,2];
    names(fmax.av) <- fmax.tmp[,1];
    fmax.ex <- as.matrix(read.table(tmp.ex));
    fmax.check <- list(average=fmax.av, per.example=fmax.ex);
    expect_equal(fmax, fmax.check);

    ann.degen <- ann[,c("A","B","C")];
    ann.degen[c("pr2","pr3","pr4"),] <- 0; ## only one example annotated
    S.degen <- S[,c("A","B","C")];
    fmax <- compute.fmax(ann.degen, S.degen, folds=2, seed=23, b.per.example=TRUE);
    write.table(fmax$average, row.names=TRUE, col.names=FALSE, quote=FALSE, file=tmp.av);
    write.table(fmax$per.example, row.names=TRUE, col.names=TRUE, quote=FALSE, file=tmp.ex);
    fmax.tmp <- read.table(tmp.av, stringsAsFactor=FALSE);
    fmax.av <- fmax.tmp[,2];
    names(fmax.av) <- fmax.tmp[,1];
    fmax.ex <- as.matrix(read.table(tmp.ex));
    fmax.check <- list(average=fmax.av, per.example=fmax.ex);
    expect_equal(fmax, fmax.check);

    ## stop messages
    labels <- ann[1,];
    scores <- S[1,];
    S.flip <- S[order(nrow(S):1),order(ncol(S):1)];
    S.trunk <- S[,-c(1,2,3,5)];
    expect_error(compute.fmax(labels, scores), "target or predicted must be a matrix");
    expect_error(compute.fmax(ann, S.flip), "rows or columns names of target and predicted are not in the same order");
    expect_error(compute.fmax(ann, S.trunk), "number of rows or columns do not match between target and predicted classes");
    expect_error(compute.fmax(S, ann), "target variable must take values 0 or 1");
    expect_error(find.best.f(labels, scores), "target or predicted must be a matrix");
    expect_error(find.best.f(ann, S.flip), "rows or columns names of target and predicted are not in the same order");
    expect_error(find.best.f(ann, S.trunk), "number of rows or columns do not match between target and predicted classes");
    expect_error(find.best.f(S, ann), "labels variable must take values 0 or 1");
    expect_error(F.measure.multilabel(ann, S.trunk), "number of rows or columns do not match between target and predicted classes");
    expect_error(F.measure.multilabel(ann, S.flip), "rows or columns names of target and predicted are not in the same order");
    expect_error(F.measure.multilabel(S, ann), "target and predicted variables must take values 0 or 1");

    ## warning messages
    expect_warning(compute.fmax(ann, S, folds=2, seed=NULL), "folds are generated without seed initialization");

    ## seed autoset to NULL
    fmax <- compute.fmax(ann, S, folds=NULL, seed=1);
    write.table(fmax, row.names=TRUE, col.names=FALSE, quote=FALSE, file=tmp.av);
    fmax.tmp <- read.table(tmp.av, stringsAsFactor=FALSE);
    fmax.check <- fmax.tmp[,2];
    names(fmax.check) <- fmax.tmp[,1];
    expect_equal(fmax, fmax.check);
})

test_that("pxr works", {
    S <- make.scores();
    ann <- make.ann();
    label <- ann[,"E"];
    score <- S[,"E"];
    label.deg <- ann[,"A"]; ## all labels==1
    score.deg <- S[,"A"];
    rec.lev <- seq(from=0.1, to=1, by=0.1);
    tmp <- tempfile();

    ## pxr one-shot
    pxr <- precision.at.all.recall.levels.single.class(label, score);
    write.table(pxr, row.names=FALSE, col.names=TRUE, quote=FALSE, file=tmp);
    pxr.check <- as.matrix(read.table(tmp, header=TRUE));
    expect_equal(pxr, pxr.check);

    pxr <- precision.at.given.recall.levels.over.classes(ann, S, folds=NULL, seed=NULL, recall.levels=rec.lev);
    write.table(data.frame(pxr), row.names=TRUE, col.names=TRUE, quote=FALSE, file=tmp);
    pxr.tmp <- read.table(tmp);
    pxr.check <- list(average=pxr.tmp$average, fixed.recall=as.matrix(pxr.tmp[,2:ncol(pxr.tmp)]));
    names(pxr.check$average) <- rec.lev;
    dimnames(pxr.check$fixed.recall) <- list(colnames(ann), rec.lev);
    expect_equal(pxr, pxr.check);

    ## pxr cross-validated
    pxr <- precision.at.given.recall.levels.over.classes(ann, S, folds=2, seed=23, recall.levels=rec.lev);
    write.table(data.frame(pxr), row.names=TRUE, col.names=TRUE, quote=FALSE, file=tmp);
    pxr.tmp <- read.table(tmp);
    pxr.check <- list(average=pxr.tmp$average, fixed.recall=as.matrix(pxr.tmp[,2:ncol(pxr.tmp)]));
    names(pxr.check$average) <- rec.lev;
    dimnames(pxr.check$fixed.recall) <- list(colnames(ann), rec.lev);
    expect_equal(pxr, pxr.check);

    ## pxr degenerate case
    pxr <- precision.at.all.recall.levels.single.class(label.deg, score.deg);
    write.table(pxr, row.names=FALSE, col.names=TRUE, quote=FALSE, file=tmp);
    pxr.check <- as.matrix(read.table(tmp, header=TRUE));
    expect_equal(pxr, pxr.check);

    ## stop message
    labels <- ann[,"A"];
    scores <- S[,"A"];
    labels.flip <- labels[order(length(labels):1)];
    labels.trunk <- labels[-c(1,2,3,5)];
    S.flip <- S[order(nrow(S):1),order(ncol(S):1)];
    S.trunk <- S[,-c(1,2,3,5)];
    expect_error(precision.at.all.recall.levels.single.class(ann, S), "labels or scores must be a vector");
    expect_error(precision.at.all.recall.levels.single.class(labels.trunk, scores), "length of true and predicted labels does not match");
    expect_error(precision.at.all.recall.levels.single.class(labels.flip, scores), "names of labels and scores are not in the same order");
    expect_error(precision.at.all.recall.levels.single.class(scores, labels), "labels variable must take values 0 or 1");
    expect_error(precision.at.given.recall.levels.over.classes(labels, scores), "target or predicted must be a matrix");
    expect_error(precision.at.given.recall.levels.over.classes(ann, S.trunk), "number of rows or columns do not match between target and predicted classes");
    expect_error(precision.at.given.recall.levels.over.classes(ann, S.flip), "rows or columns names of target and predicted are not in the same order");
    expect_error(precision.at.given.recall.levels.over.classes(S, ann), "target variable must take values 0 or 1");

    ## warning messages
    expect_warning(precision.at.given.recall.levels.over.classes(ann, S, folds=2, seed=NULL), "folds are generated without seed initialization");

    ## seed autoset to NULL
    pxr <- precision.at.given.recall.levels.over.classes(ann, S, folds=NULL, seed=1);
    write.table(data.frame(pxr), row.names=TRUE, col.names=TRUE, quote=FALSE, file=tmp);
    pxr.tmp <- read.table(tmp);
    pxr.check <- list(average=pxr.tmp$average, fixed.recall=as.matrix(pxr.tmp[,2:ncol(pxr.tmp)]));
    names(pxr.check$average) <- rec.lev;
    dimnames(pxr.check$fixed.recall) <- list(colnames(ann), rec.lev);
    expect_equal(pxr, pxr.check);
})

test_that("stratified data works", {
    ann <- make.ann();
    ann <- ann[,c("I","J")];
    examples.index <- 1:nrow(ann);
    examples.name <- rownames(ann);
    positives <- which(ann[,"I"]==1);
    x <- stratified.cv.data.single.class(examples.index, positives, kk=2, seed=23);
    y <- stratified.cv.data.single.class(examples.name, positives, kk=2, seed=23);
    z <- stratified.cv.data.over.classes(ann, examples.index, kk=2, seed=23);

    idx.pos <- list(1,4);
    idx.neg <- list(3,2);
    idx.lis <- list(f1=c(1,3), f2=c(4,2), f3=1, f4=c(3,2,4));
    chr.pos <- list("pr1","pr4");
    chr.neg <- list("pr3","pr2");

    x.test <- list(fold.positives=idx.pos, fold.negatives=idx.neg);
    y.test <- list(fold.positives=chr.pos, fold.negatives=chr.neg);
    z.test <- list();
    count <- 0;
    for(class in colnames(ann)){
        z.test[[class]] <- list();
        for(k in 1:2){
            count <- count + 1;
            z.test[[class]][[k]] <- list();
            names(z.test[[class]])[k] <- paste0("fold",k);
            z.test[[class]][[k]] <- idx.lis[[count]];
        }
    }
    expect_equal(x, x.test);
    expect_equal(y, y.test);
    expect_equal(z, z.test);

    ## degenerate case when labels have only one negative
    examples <- 1:4;
    positives <- 1:3;
    p <- stratified.cv.data.single.class(examples, positives, kk=2, seed=23);
    p.test <- list(fold.positives=list(1,c(2,3)), fold.negatives=list(integer(),4));
    expect_equal(p, p.test);
})

test_that("stratified fold works", {
    ann <- make.ann();
    S <- make.scores();
    labels <- ann[,"I"];
    scores <- S[,"I"];
    labels.flip <- labels[order(length(labels):1)];
    labels.trunk <- labels[-c(1,2,3,5)];

    ## stop messages
    expect_error(create.stratified.fold.df(ann, S, folds=5, seed=23), "labels or scores must be a vector");
    expect_error(create.stratified.fold.df(labels.trunk, scores, folds=5, seed=23), "length of true and predicted labels does not match");
    expect_error(create.stratified.fold.df(scores, labels, folds=5, seed=23), "labels variable must take values 0 or 1");
    expect_error(create.stratified.fold.df(labels, scores, folds=1.5, seed=23), "folds must be an integer number");
    expect_error(create.stratified.fold.df(labels, scores, folds=NULL, seed=23), "non-numeric argument to mathematical function");
    expect_error(create.stratified.fold.df(labels, scores, folds='"fold', seed=23), "non-numeric argument to mathematical function");
    expect_error(create.stratified.fold.df(labels, scores, folds=15, seed=23),
        "number of folds selected too high: some folds have no examples. Please reduce the number of folds");

    ## warning messages
    expect_warning(create.stratified.fold.df(labels, scores, folds=2, seed=NULL), "folds are generated without seed initialization");
})
