library(HEMDAG);
source("make.test.data.R");

context("test htd-dag methods");

test_that("htd works",{
    S <- make.scores();
    g <- make.graph();
    root <- root.node(g);
    S.noroot <- S[,-which(colnames(S) %in% root)];
    tmp <- tempfile(); ## store hierarchical matrix

    S.htd  <- htd(S, g, root);
    write.table(S.htd, row.names=TRUE, col.names=TRUE, quote=FALSE, file=tmp);
    S.check <- as.matrix(read.table(tmp));
    expect_equal(S.htd, S.check);

    S.htd.noroot <-  htd(S.noroot, g, root);
    write.table(S.htd.noroot, row.names=TRUE, col.names=TRUE, quote=FALSE, file=tmp);
    S.check <- as.matrix(read.table(tmp));
    expect_equal(S.htd.noroot, S.check);

    ## test class mismatch
    S.error <- S[,-which(colnames(S) %in% c("D","H"))];
    expect_error(htd(S.error, g, root), "mismatch between the number of nodes of the graph g and the number of classes of the scores matrix S");
})

test_that("htd.vanilla works",{
    S <- make.scores();
    g <- make.graph();
    root <- root.node(g);

    expect_output(htd.vanilla(S, g, norm=FALSE, norm.type=NULL), "htd-dag correction: done");
    expect_output(htd.vanilla(S, g, norm=TRUE, norm.type="maxnorm"), "maxnorm normalization: done\\nhtd-dag correction: done");
    expect_error(htd.vanilla(S, g, norm=TRUE, norm.type="maxnorms"), "the chosen normalization method is not among those available or it was misspelled");
    expect_error(htd.vanilla(S, g, norm=TRUE, norm.type=NULL), "choose a normalization methods among those available");
    expect_error(htd.vanilla(S, g, norm=FALSE, norm.type="maxnorm"), "do you wanna or not normalize the matrix S\\? norm and norm.type are inconsistent");
})

test_that("htd.holdout works",{
    S <- make.scores();
    g <- make.graph();
    root <- root.node(g);

    expect_output(htd.holdout(S, g, testIndex=1:3, norm=FALSE, norm.type=NULL), "htd-dag correction: done");
    expect_output(htd.holdout(S, g, testIndex=1:3, norm=TRUE, norm.type="maxnorm"), "maxnorm normalization: done\\nhtd-dag correction: done");
    expect_error(htd.holdout(S, g, testIndex=1:3, norm=TRUE, norm.type="maxnorms"),
        "the chosen normalization method is not among those available or it was misspelled");
    expect_error(htd.holdout(S, g, testIndex=1:3, norm=TRUE, norm.type=NULL), "choose a normalization methods among those available");
    expect_error(htd.holdout(S, g, testIndex=1:3, norm=FALSE, norm.type="maxnorm"),
        "do you wanna or not normalize the matrix S\\? norm and norm.type are inconsistent");
})
