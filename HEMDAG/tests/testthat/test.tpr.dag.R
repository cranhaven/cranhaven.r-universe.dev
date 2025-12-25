library(HEMDAG);
source("make.test.data.R");

context("test tpr-dag methods");

test_that("tpr.dag works",{
    S <- make.scores();
    g <- make.graph();
    root <- root.node(g);
    S.noroot <- S[,-which(colnames(S)==root)];
    tmp <- tempfile();

    ## all 18 tpr-dag variants
    S.tprTF         <- tpr.dag(S, g, root, positive="children", bottomup="threshold.free", topdown="htd");
    S.tprT          <- tpr.dag(S, g, root, positive="children", bottomup="threshold", topdown="htd", t=0.5);
    S.tprW          <- tpr.dag(S, g, root, positive="children", bottomup="weighted.threshold.free", topdown="htd", w=0.5);
    S.tprWT         <- tpr.dag(S, g, root, positive="children", bottomup="weighted.threshold", topdown="htd", t=0.5, w=0.5);

    S.descensTF     <- tpr.dag(S, g, root, positive="descendants", bottomup="threshold.free", topdown="htd");
    S.descensT      <- tpr.dag(S, g, root, positive="descendants", bottomup="threshold", topdown="htd", t=0.5);
    S.descensW      <- tpr.dag(S, g, root, positive="descendants", bottomup="weighted.threshold.free", topdown="htd", w=0.5);
    S.descensWT     <- tpr.dag(S, g, root, positive="descendants", bottomup="weighted.threshold", topdown="htd", t=0.5, w=05);
    S.descensTAU    <- tpr.dag(S, g, root, positive="descendants", bottomup="tau", topdown="htd", t=0.5);

    S.isotprTF      <- tpr.dag(S, g, root, positive="children", bottomup="threshold.free", topdown="gpav");
    S.isotprT       <- tpr.dag(S, g, root, positive="children", bottomup="threshold", topdown="gpav", t=0.5);
    S.isotprW       <- tpr.dag(S, g, root, positive="children", bottomup="weighted.threshold.free", topdown="gpav", w=0.5);
    S.isotprWT      <- tpr.dag(S, g, root, positive="children", bottomup="weighted.threshold", topdown="gpav", t=0.5, w=0.5);

    S.isodescensTF  <- tpr.dag(S, g, root, positive="descendants", bottomup="threshold.free", topdown="gpav");
    S.isodescensT   <- tpr.dag(S, g, root, positive="descendants", bottomup="threshold", topdown="gpav", t=0.5);
    S.isodescensW   <- tpr.dag(S, g, root, positive="descendants", bottomup="weighted.threshold.free", topdown="gpav", w=0.5);
    S.isodescensWT  <- tpr.dag(S, g, root, positive="descendants", bottomup="weighted.threshold", topdown="gpav", t=0.5, w=0.5);
    S.isodescensTAU <- tpr.dag(S, g, root, positive="descendants", bottomup="tau", topdown="gpav", t=0.5);

    write.table(S.tprTF, row.names=TRUE, col.names=TRUE, quote=FALSE, file=tmp);
    S.check <- as.matrix(read.table(tmp));
    expect_equal(S.tprTF, S.check);

    write.table(S.tprT, row.names=TRUE, col.names=TRUE, quote=FALSE, file=tmp);
    S.check <- as.matrix(read.table(tmp));
    expect_equal(S.tprT, S.check);

    write.table(S.tprW, row.names=TRUE, col.names=TRUE, quote=FALSE, file=tmp);
    S.check <- as.matrix(read.table(tmp));
    expect_equal(S.tprW, S.check);

    write.table(S.tprWT, row.names=TRUE, col.names=TRUE, quote=FALSE, file=tmp);
    S.check <- as.matrix(read.table(tmp));
    expect_equal(S.tprWT, S.check);


    write.table(S.descensTF, row.names=TRUE, col.names=TRUE, quote=FALSE, file=tmp);
    S.check <- as.matrix(read.table(tmp));
    expect_equal(S.descensTF, S.check);

    write.table(S.descensT, row.names=TRUE, col.names=TRUE, quote=FALSE, file=tmp);
    S.check <- as.matrix(read.table(tmp));
    expect_equal(S.descensT, S.check);

    write.table(S.descensW, row.names=TRUE, col.names=TRUE, quote=FALSE, file=tmp);
    S.check <- as.matrix(read.table(tmp));
    expect_equal(S.descensW, S.check);

    write.table(S.descensWT, row.names=TRUE, col.names=TRUE, quote=FALSE, file=tmp);
    S.check <- as.matrix(read.table(tmp));
    expect_equal(S.descensWT, S.check);

    write.table(S.descensTAU, row.names=TRUE, col.names=TRUE, quote=FALSE, file=tmp);
    S.check <- as.matrix(read.table(tmp));
    expect_equal(S.descensTAU, S.check);


    write.table(S.isotprTF, row.names=TRUE, col.names=TRUE, quote=FALSE, file=tmp);
    S.check <- as.matrix(read.table(tmp));
    expect_equal(S.isotprTF, S.check);

    write.table(S.isotprT, row.names=TRUE, col.names=TRUE, quote=FALSE, file=tmp);
    S.check <- as.matrix(read.table(tmp));
    expect_equal(S.isotprT, S.check);

    write.table(S.isotprW, row.names=TRUE, col.names=TRUE, quote=FALSE, file=tmp);
    S.check <- as.matrix(read.table(tmp));
    expect_equal(S.isotprW, S.check);

    write.table(S.isotprWT, row.names=TRUE, col.names=TRUE, quote=FALSE, file=tmp);
    S.check <- as.matrix(read.table(tmp));
    expect_equal(S.isotprWT, S.check);


    write.table(S.isodescensTF, row.names=TRUE, col.names=TRUE, quote=FALSE, file=tmp);
    S.check <- as.matrix(read.table(tmp));
    expect_equal(S.isodescensTF, S.check);

    write.table(S.isodescensT, row.names=TRUE, col.names=TRUE, quote=FALSE, file=tmp);
    S.check <- as.matrix(read.table(tmp));
    expect_equal(S.isodescensT, S.check);

    write.table(S.isodescensW, row.names=TRUE, col.names=TRUE, quote=FALSE, file=tmp);
    S.check <- as.matrix(read.table(tmp));
    expect_equal(S.isodescensW, S.check);

    write.table(S.isodescensWT, row.names=TRUE, col.names=TRUE, quote=FALSE, file=tmp);
    S.check <- as.matrix(read.table(tmp));
    expect_equal(S.isodescensWT, S.check);

    write.table(S.isodescensTAU, row.names=TRUE, col.names=TRUE, quote=FALSE, file=tmp);
    S.check <- as.matrix(read.table(tmp));
    expect_equal(S.isodescensTAU, S.check);


    ## no root testing
    S.hier.noroot <- tpr.dag(S.noroot, g, root, positive="children", bottomup="threshold.free", topdown="htd");

    ## class mismatch
    S.error <- S[,-which(colnames(S) %in% c("D","H"))];
    expect_error(tpr.dag(S.error, g, root, positive="children", bottomup="threshold.free", topdown="htd"),
        "mismatch between the number of nodes of the graph g and the number of classes of the scores matrix S");

    ## stop/warnings messages
    expect_error(tpr.dag(S, g, root, positive="child", bottomup="threshold.free", topdown="htd"),
        "positive or bottomup or topdown value misspelled");
    expect_error(tpr.dag(S, g, root, positive="children", bottomup="tau", topdown="htd"),
        "tau is a descens variant. Please set positive to descendants");
    expect_warning(tpr.dag(S, g, root, positive="descendants", bottomup="threshold", topdown="htd", t=1),
        "when t or w is equal to 1, tpr-dag is reduced to htd-dag");
    expect_warning(tpr.dag(S, g, root, positive="children", bottomup="threshold.free", topdown="gpav", parallel=TRUE, ncores=1),
        "increase number of cores to exploit the gpav parallel version");
    expect_warning(tpr.dag(S, g, root, positive="children", bottomup="threshold.free", topdown="gpav", parallel=FALSE, ncores=3),
        "set parallel to TRUE to exploit the gpav parallel version");
    expect_warning(tpr.dag(S, g, root, positive="children", bottomup="threshold.free", topdown="htd", parallel=TRUE, ncores=3),
        "does not exist a parallel version of htd");
})

test_that("tpr.dag.cv works",{
    S <- make.scores()
    g <- make.graph();
    ann <- make.ann();
    root <- root.node(g);

    S.noroot <- S[,-which(colnames(S)%in%root)];
    ## degenerate case: train and test made of one row/example
    S.degen <- S[1:2,];
    ann.degen <- ann[1:2,];

    threshold <- seq(0.1, 0.9, 0.2);
    weight <- seq(0.1, 0.9, 0.2);

    expect_output(tpr.dag.cv(S, g, ann=NULL, norm=FALSE, norm.type=NULL, positive="children", bottomup="threshold.free", topdown="gpav", W=NULL, parallel=FALSE,
        ncores=1, threshold=0, weight=0, kk=NULL, seed=NULL, metric=NULL, n.round=NULL), "tpr-dag correction done");

    expect_output(tpr.dag.cv(S, g, ann=ann, norm=FALSE, norm.type=NULL, positive="children", bottomup="threshold", topdown="gpav", W=NULL, parallel=FALSE,
        ncores=1, threshold=threshold, weight=0, kk=3, seed=23, metric="auprc", n.round=NULL),
        "training fold:\\t1\\ttop auprc avg found:\\t0.236183\\tbest threshold:\\t0.1\\t\\ntraining fold:\\t1\\ttop auprc avg found:\\t0.3748124\\tbest threshold:\\t0.5\\t\\ntraining fold:\\t2\\ttop auprc avg found:\\t0.2832962\\tbest threshold:\\t0.1\\t\\ntpr-dag correction done");

    expect_output(tpr.dag.cv(S, g, ann=ann, norm=FALSE, norm.type=NULL, positive="children", bottomup="weighted.threshold.free", topdown="gpav", W=NULL,
        parallel=FALSE, ncores=1, threshold=0, weight=weight, kk=3, seed=23, metric="fmax", n.round=3),
    "training fold:\\t1\\ttop fmax avg found:\\t0.8113208\\tbest weight:\\t0.1\\t\\ntraining fold:\\t2\\ttop fmax avg found:\\t0.7933067\\tbest weight:\\t0.1\\t\\ntraining fold:\\t3\\ttop fmax avg found:\\t0.776699\\tbest weight:\\t0.1\\t\\ntpr-dag correction done");

    expect_output(tpr.dag.cv(S, g, ann=ann, norm=FALSE, norm.type=NULL, positive="children", bottomup="weighted.threshold", topdown="gpav", W=NULL, parallel=FALSE,
        ncores=1, threshold=threshold, weight=weight, kk=3, seed=23, metric="fmax", n.round=3),
    "training fold:\\t1\\ttop fmax avg found:\\t0.8113208\\tbest threshold:\\t0.1\\tbest weight:\\t0.1\\t\\ntraining fold:\\t2\\ttop fmax avg found:\\t0.8219138\\tbest threshold:\\t0.1\\tbest weight:\\t0.1\\t\\ntraining fold:\\t3\\ttop fmax avg found:\\t0.8224138\\tbest threshold:\\t0.1\\tbest weight:\\t0.1\\t\\ntpr-dag correction done");

    expect_output(tpr.dag.cv(S, g, ann=NULL, norm=TRUE, norm.type="qnorm", positive="children", bottomup="threshold.free", topdown="htd", W=NULL, parallel=FALSE,
        ncores=1, threshold=0, weight=0, kk=NULL, seed=NULL, metric=NULL, n.round=NULL),
        "qnorm normalization: done\\ntpr-dag correction done");

    expect_output(tpr.dag.cv(S.degen, g, ann=ann.degen, norm=FALSE, norm.type=NULL, positive="children", bottomup="threshold", topdown="gpav", W=NULL,
        parallel=FALSE, ncores=1, threshold=threshold, weight=0, kk=2, seed=23, metric="auprc", n.round=NULL), "tpr-dag correction done");

    expect_output(tpr.dag.cv(S.degen, g, ann=ann.degen, norm=FALSE, norm.type=NULL, positive="children", bottomup="threshold", topdown="gpav", W=NULL,
        parallel=FALSE, ncores=1, threshold=threshold, weight=0, kk=2, seed=23, metric="fmax", n.round=3), "tpr-dag correction done");

    expect_output(tpr.dag.cv(S.noroot, g, ann=NULL, norm=FALSE, norm.type=NULL, positive="children", bottomup="threshold.free", topdown="gpav", W=NULL,
        parallel=FALSE, ncores=1, threshold=0, weight=0, kk=NULL, seed=NULL, metric=NULL, n.round=NULL), "tpr-dag correction done");

    ## n.round autoset to NULL
    expect_output(tpr.dag.cv(S, g, ann=ann, norm=FALSE, norm.type=NULL, positive="children", bottomup="threshold", topdown="gpav", W=NULL, parallel=FALSE,
        ncores=1, threshold=threshold, weight=0, kk=3, seed=23, metric="auprc", n.round=3),
        "training fold:\\t1\\ttop auprc avg found:\\t0.236183\\tbest threshold:\\t0.1\\t\\ntraining fold:\\t1\\ttop auprc avg found:\\t0.3748124\\tbest threshold:\\t0.5\\t\\ntraining fold:\\t2\\ttop auprc avg found:\\t0.2832962\\tbest threshold:\\t0.1\\t\\ntpr-dag correction done");

    ## error messages
    expect_error(tpr.dag.cv(S, g, ann=NULL, norm=TRUE, norm.type="maxnorm", positive="descendantss", bottomup="threshold.free", topdown="htd", W=NULL,
        parallel=FALSE, ncores=1, threshold=0, weight=0, kk=NULL, seed=NULL, metric=NULL, n.round=NULL),
        "positive or bottomup or topdown value misspelled");

    expect_error(tpr.dag.cv(S, g, ann=ann, norm=TRUE, norm.type="maxnorm", positive="children", bottomup="tau", topdown="htd", W=NULL, parallel=FALSE,
        ncores=1, threshold=0, weight=0, kk=NULL, seed=NULL, metric=NULL, n.round=NULL),
        "tau is a descens variant. Please set positive to descendants");

    expect_error(tpr.dag.cv(S, g, ann=NULL, norm=FALSE, norm.type="maxnorm", positive="children", bottomup="threshold.free", topdown="htd", W=NULL, parallel=FALSE,
        ncores=1, threshold=0, weight=0, kk=NULL, seed=NULL, metric=NULL, n.round=NULL),
        "do you wanna or not normalize the matrix S\\? norm and norm.type inconsistent");

    expect_error(tpr.dag.cv(S, g, ann=NULL, norm=TRUE, norm.type=NULL, positive="children", bottomup="threshold.free", topdown="htd", W=NULL, parallel=FALSE,
        ncores=1, threshold=0, weight=0, kk=NULL, seed=NULL, metric=NULL, n.round=NULL),
        "choose a normalization methods among those available");

    expect_error(tpr.dag.cv(S, g, ann=NULL, norm=FALSE, norm.type=NULL, positive="children", bottomup="threshold", topdown="htd", W=NULL, parallel=FALSE,
        ncores=1, threshold=0, weight=0, kk=3, seed=23, metric="auprc", n.round=NULL),
        "the annotation matrix must be provided to maximize the hyper-parameter\\(s\\) of the chosen tpr-dag ensemble variant");

    expect_error(tpr.dag.cv(S, g, ann=ann, norm=FALSE, norm.type=NULL, positive="children", bottomup="threshold", topdown="htd", W=NULL, parallel=FALSE,
        ncores=1, threshold=threshold, weight=0, kk=1, seed=23, metric="auprc", n.round=NULL),
        "smallest number of folds to define test and training set is 2. Set kk larger or equal to 2");

    expect_error(tpr.dag.cv(S, g, ann=ann, norm=FALSE, norm.type=NULL, positive="children", bottomup="weighted.threshold", topdown="htd", W=NULL, parallel=FALSE,
        ncores=1, threshold=threshold, weight=weight, kk=3, seed=23, metric=NULL, n.round=NULL),
        "the bottom-up approach weighted.threshold is parametric. Select the metric on which maximize according to those available");

    expect_error(tpr.dag.cv(S, g, ann=ann, norm=FALSE, norm.type=NULL, positive="children", bottomup="weighted.threshold", topdown="htd", W=NULL, parallel=FALSE,
        ncores=1, threshold=threshold, weight=weight, kk=3, seed=23, metric="auc", n.round=NULL),
        "value of parameter metric misspelled");

    expect_error(tpr.dag.cv(S, g, ann=ann, norm=FALSE, norm.type=NULL, positive="children", bottomup="threshold", topdown="htd", W=NULL, parallel=FALSE,
        ncores=1, threshold=threshold, weight=0, kk=3, seed=NULL, metric="auprc", n.round=NULL),
        "set seed to create folds");

    expect_error(tpr.dag.cv(S.degen, g, ann=ann.degen, norm=FALSE, norm.type=NULL, positive="children", bottomup="threshold", topdown="gpav", W=NULL,
        parallel=FALSE, ncores=1, threshold=threshold, weight=0, kk=3, seed=23, metric="fmax", n.round=3),
        "number of folds selected too high: some folds have no examples. Please reduce the number of folds")

    expect_error(tpr.dag.cv(S, g, ann=ann, norm=FALSE, norm.type=NULL, positive="children", bottomup="weighted.threshold", topdown="gpav", W=NULL, parallel=FALSE,
        ncores=1, threshold=threshold, weight=weight, kk=3, seed=23, metric="fmax", n.round=NULL), "set n.round properly");
})

test_that("tpr.dag.holdout works",{
    S <- make.scores()
    g <- make.graph();
    ann <- make.ann();
    root <- root.node(g);
    testIndex <- 4;

    S.noroot <- S[,-which(colnames(S)%in%root)];
    ## degenerate case: train and test made of one row/example
    S.degen <- S[1:2,];
    ann.degen <- ann[1:2,];
    testIndex.degen <- 1;

    threshold <- seq(0.1, 0.9, 0.2);
    weight <- seq(0.1, 0.9, 0.2);

    expect_output(tpr.dag.holdout(S, g, ann=NULL, testIndex=testIndex, norm=FALSE, norm.type=NULL, positive="children", bottomup="threshold.free",
        topdown="gpav", W=NULL, parallel=FALSE, ncores=1, threshold=0, weight=0, kk=NULL, seed=NULL, metric=NULL, n.round=NULL),
    "tpr-dag holdout correction done");

    expect_output(tpr.dag.holdout(S.degen, g, ann=NULL, testIndex=testIndex.degen, norm=FALSE, norm.type=NULL, positive="children", bottomup="threshold.free",
        topdown="gpav", W=NULL, parallel=FALSE, ncores=1, threshold=0, weight=0, kk=NULL, seed=NULL, metric=NULL, n.round=NULL),
    "tpr-dag holdout correction done");

    expect_output(tpr.dag.holdout(S, g, ann=ann, testIndex=testIndex, norm=FALSE, norm.type=NULL, positive="descendants",
        bottomup="weighted.threshold.free", topdown="gpav", W=NULL, parallel=FALSE, ncores=1, threshold=0, weight=weight, kk=2, seed=23,
        metric="fmax", n.round=3),
        "training fold:\\t1\\ttop fmax avg found:\\t0.7272727\\tbest weight:\\t0.1\\t\\ntraining fold:\\t2\\ttop fmax avg found:\\t0.8244898\\tbest weight:\\t0.1\\t\\nacross\\t2 training folds\\tbest fmax avg found:\\t0.8244898\\tbest weight:\\t0.1\\t\\ntpr-dag holdout correction done");

    expect_output(tpr.dag.holdout(S, g, ann=ann, testIndex=testIndex, norm=FALSE, norm.type=NULL, positive="children", bottomup="threshold", topdown="htd",
        W=NULL, parallel=FALSE, ncores=1, threshold=threshold, weight=0, kk=2, seed=23, metric="auprc", n.round=NULL),
        "training fold:\\t2\\ttop auprc avg found:\\t0.4801396\\tbest threshold:\\t0.1\\t\\ntraining fold:\\t2\\ttop auprc avg found:\\t0.6534264\\tbest threshold:\\t0.9\\t\\nacross\\t2 training folds\\tbest auprc avg found:\\t0.6534264\\tbest threshold:\\t0.9\\t\\ntpr-dag holdout correction done");

    expect_output(tpr.dag.holdout(S.noroot, g, ann=NULL, testIndex=testIndex, norm=FALSE, norm.type=NULL, positive="children", bottomup="threshold.free",
        topdown="gpav", W=NULL, parallel=FALSE, ncores=1, threshold=0, weight=0, kk=NULL, seed=NULL, metric=NULL, n.round=NULL),
        "tpr-dag holdout correction done");

    expect_output(tpr.dag.holdout(S, g, ann=ann, testIndex=testIndex, norm=FALSE, norm.type=NULL, positive="children", bottomup="weighted.threshold",
        topdown="gpav", W=NULL, parallel=FALSE, ncores=1, threshold=threshold, weight=weight, kk=2, seed=23, metric="fmax", n.round=3),
        "training fold:\\t1\\ttop fmax avg found:\\t0.8\\tbest threshold:\\t0.1\\tbest weight:\\t0.1\\t\\ntraining fold:\\t2\\ttop fmax avg found:\\t0.8244898\\tbest threshold:\\t0.1\\tbest weight:\\t0.1\\t\\nacross\\t2 training folds\\tbest fmax avg found:\\t0.8244898\\tbest threshold:\\t0.1\\tbest weight:\\t0.1\\t\\ntpr-dag holdout correction done");

    expect_output(tpr.dag.holdout(S, g, ann=NULL, testIndex=testIndex, norm=TRUE, norm.type="maxnorm", positive="children", bottomup="threshold.free",
        topdown="gpav", W=NULL, parallel=FALSE, ncores=1, threshold=0, weight=0, kk=NULL, seed=NULL, metric=NULL, n.round=NULL),
        "maxnorm normalization done\\ntpr-dag holdout correction done");

    ## n.round autoset to NULL
    expect_output(tpr.dag.holdout(S, g, ann=ann, testIndex=testIndex, norm=FALSE, norm.type=NULL, positive="children", bottomup="threshold", topdown="gpav",
        W=NULL, parallel=FALSE, ncores=1, threshold=threshold, weight=0, kk=2, seed=23, metric="auprc", n.round=3),
    "training fold:\\t2\\ttop auprc avg found:\\t0.3068528\\tbest threshold:\\t0.1\\t\\ntraining fold:\\t2\\ttop auprc avg found:\\t0.4801396\\tbest threshold:\\t0.5\\t\\nacross\\t2 training folds\\tbest auprc avg found:\\t0.4801396\\tbest threshold:\\t0.5\\t\\ntpr-dag holdout correction done");

    ## error messages
    expect_error(tpr.dag.holdout(S.degen, g, ann=ann.degen, testIndex=testIndex.degen, norm=FALSE, norm.type=NULL, positive="children",
        bottomup="threshold", topdown="gpav", W=NULL, parallel=FALSE, ncores=1, threshold=threshold, weight=0, kk=2, seed=23,
        metric="auprc", n.round=NULL),
        "training matrix too small \\(only one example/row\\) for hyper-parameters tuning. Please use threshold.free strategy instead");

    expect_error(tpr.dag.holdout(S, g, ann=NULL, testIndex=testIndex, norm=TRUE, norm.type="maxnorm", positive="descendantss", bottomup="threshold.free",
        topdown="htd", W=NULL, parallel=FALSE, ncores=1, threshold=0, weight=0, kk=NULL, seed=NULL, metric=NULL, n.round=NULL),
        "positive or bottomup or topdown value misspelled");

    expect_error(tpr.dag.holdout(S, g, ann=ann, testIndex=testIndex, norm=TRUE, norm.type="maxnorm", positive="children", bottomup="tau", topdown="htd",
        W=NULL, parallel=FALSE, ncores=1, threshold=0, weight=0, kk=NULL, seed=NULL, metric=NULL, n.round=NULL),
        "tau is a descens variant. Please set positive to descendants");

    expect_error(tpr.dag.holdout(S, g, ann=NULL, testIndex=testIndex, norm=FALSE, norm.type="maxnorm", positive="children", bottomup="threshold.free",
        topdown="htd", W=NULL, parallel=FALSE, ncores=1, threshold=0, weight=0, kk=NULL, seed=NULL, metric=NULL, n.round=NULL),
        "do you wanna or not normalize the matrix S\\? norm and norm.type inconsistent");

    expect_error(tpr.dag.holdout(S, g, ann=NULL, testIndex=testIndex, norm=TRUE, norm.type=NULL, positive="children", bottomup="threshold.free",
        topdown="htd", W=NULL, parallel=FALSE, ncores=1, threshold=0, weight=0, kk=NULL, seed=NULL, metric=NULL, n.round=NULL),
        "choose a normalization methods among those available");

    expect_error(tpr.dag.holdout(S, g, ann=NULL,testIndex=testIndex, norm=FALSE, norm.type=NULL, positive="children", bottomup="threshold", topdown="htd",
        W=NULL, parallel=FALSE, ncores=1, threshold=0, weight=0, kk=3, seed=23, metric="auprc", n.round=NULL),
        "the annotation matrix must be provided to maximize the hyper-parameter\\(s\\) of the chosen tpr-dag ensemble variant");

    expect_error(tpr.dag.holdout(S, g, ann=ann,testIndex=testIndex, norm=FALSE, norm.type=NULL, positive="children", bottomup="threshold", topdown="htd",
        W=NULL, parallel=FALSE, ncores=1, threshold=threshold, weight=0, kk=1, seed=23, metric="auprc", n.round=NULL),
        "smallest number of folds to define test and training set is 2. Set kk larger or equal to 2");

    expect_error(tpr.dag.holdout(S, g, ann=ann,testIndex=testIndex, norm=FALSE, norm.type=NULL, positive="children", bottomup="weighted.threshold",
        topdown="htd", W=NULL, parallel=FALSE, ncores=1, threshold=threshold, weight=weight, kk=3, seed=23, metric=NULL, n.round=NULL),
        "the bottom-up approach weighted.threshold is parametric. Select the metric on which maximize according to those available");

    expect_error(tpr.dag.holdout(S, g, ann=ann,testIndex=testIndex, norm=FALSE, norm.type=NULL, positive="children", bottomup="weighted.threshold",
        topdown="htd", W=NULL, parallel=FALSE, ncores=1, threshold=threshold, weight=weight, kk=3, seed=23, metric="auc", n.round=NULL),
        "value of parameter metric misspelled");

    expect_error(tpr.dag.holdout(S, g, ann=ann, testIndex=testIndex, norm=FALSE, norm.type=NULL, positive="children", bottomup="threshold", topdown="htd",
        W=NULL, parallel=FALSE, ncores=1, threshold=threshold, weight=weight, kk=3, seed=NULL, metric="auprc", n.round=NULL),
        "set seed to create folds");

    expect_error(tpr.dag.holdout(S, g, ann=ann, testIndex=testIndex, norm=FALSE, norm.type=NULL, positive="children", bottomup="weighted.threshold",
        topdown="gpav", W=NULL, parallel=FALSE, ncores=1, threshold=threshold, weight=weight, kk=3, seed=23, metric="fmax", n.round=NULL),
        "set n.round properly");
})
