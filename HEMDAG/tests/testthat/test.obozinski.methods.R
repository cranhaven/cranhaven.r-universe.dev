library(HEMDAG);
source("make.test.data.R");

context("test obozinski methods");

test_that("obozinski.max works", {
    S  <- make.scores();
    g <- make.graph();
    root <- root.node(g);
    S.noroot <- S[,-which(colnames(S) %in% root)];
    tmp <- tempfile(); ## store hierarchical matrix

    S.max  <- obozinski.max(S, g, root);
    write.table(S.max, row.names=TRUE, col.names=TRUE, quote=FALSE, file=tmp);
    S.check <- as.matrix(read.table(tmp));
    expect_equal(S.max, S.check);

    S.max.noroot <- obozinski.max(S.noroot, g, root);
    write.table(S.max.noroot, row.names=TRUE, col.names=TRUE, quote=FALSE, file=tmp);
    S.check <- as.matrix(read.table(tmp));
    expect_equal(S.max.noroot, S.check);

    ## test class mismatch
    S.error <- S[,-which(colnames(S) %in% c("D","H"))];
    expect_error(obozinski.max(S.error, g, root), "mismatch between the number of nodes of the graph g and the number of classes of the scores matrix S");
})

test_that("obozinski.and works", {
    S  <- make.scores();
    g <- make.graph();
    root <- root.node(g);
    S.noroot <- S[,-which(colnames(S) %in% root)];
    tmp <- tempfile(); ## store hierarchical matrix

    S.and  <- obozinski.and(S, g, root);
    write.table(S.and, row.names=TRUE, col.names=TRUE, quote=FALSE, file=tmp);
    S.check <- as.matrix(read.table(tmp));
    expect_equal(S.and, S.check);

    S.and.noroot <- obozinski.and(S.noroot, g, root);
    write.table(S.and.noroot, row.names=TRUE, col.names=TRUE, quote=FALSE, file=tmp);
    S.check <- as.matrix(read.table(tmp));
    expect_equal(S.and.noroot, S.check);

    ## test class mismatch
    S.error <- S[,-which(colnames(S) %in% c("D","H"))];
    expect_error(obozinski.and(S.error, g, root), "mismatch between the number of nodes of the graph g and the number of classes of the scores matrix S");
})

test_that("obozinski.or works", {
    S  <- make.scores();
    g <- make.graph();
    root <- root.node(g);
    S.noroot <- S[,-which(colnames(S) %in% root)];
    tmp <- tempfile(); ## store hierarchical matrix

    S.or  <- obozinski.or(S, g, root);
    write.table(S.or, row.names=TRUE, col.names=TRUE, quote=FALSE, file=tmp);
    S.check <- as.matrix(read.table(tmp));
    expect_equal(S.or, S.check);

    S.or.noroot <- obozinski.or(S.noroot, g, root);
    write.table(S.or.noroot, row.names=TRUE, col.names=TRUE, quote=FALSE, file=tmp);
    S.check <- as.matrix(read.table(tmp));
    expect_equal(S.or.noroot, S.check);

    ## test class mismatch
    S.error <- S[,-which(colnames(S) %in% c("D","H"))];
    expect_error(obozinski.or(S.error, g, root), "mismatch between the number of nodes of the graph g and the number of classes of the scores matrix S");
})

test_that("obozinski.methods works", {
    g <- make.graph();
    S  <- make.scores();
    expect_output(S.hier <- obozinski.methods(S, g, heuristic="max", norm=FALSE, norm.type=NULL), "Obozinski's heuristic max correction: done");
    expect_output(S.hier <- obozinski.methods(S, g, heuristic="and", norm=FALSE, norm.type=NULL), "Obozinski's heuristic and correction: done");
    expect_output(S.hier <- obozinski.methods(S, g, heuristic="or", norm=FALSE, norm.type=NULL), "Obozinski's heuristic or correction: done");
    expect_output(S.hier <- obozinski.methods(S, g, heuristic="and", norm=TRUE, norm.type="maxnorm"),
        "maxnorm normalization: done\\nObozinski's heuristic and correction: done");
    expect_error(S.hier  <- obozinski.methods(S, g, heuristic="max", norm=FALSE, norm.type="maxnorm"),
        "do you wanna or not normalize the matrix S\\? norm and norm.type are inconsistent");
    expect_error(S.hier  <- obozinski.methods(S, g, heuristic="and", norm=TRUE, norm.type=NULL),
        "choose a normalization methods among those available");
    expect_error(S.hier  <- obozinski.methods(S, g, heuristic="o", norm=FALSE, norm.type=NULL),
        "the chosen heuristic method is not among those available or it has been misspelled");
})

test_that("obozinski.holdout works", {
    g <- make.graph();
    S  <- make.scores();
    expect_output(S.hier <- obozinski.holdout(S, g, testIndex=1:2, heuristic="max", norm=FALSE, norm.type=NULL), "Obozinski's heuristic max correction: done");
    expect_output(S.hier <- obozinski.holdout(S, g, testIndex=1:2, heuristic="and", norm=FALSE, norm.type=NULL), "Obozinski's heuristic and correction: done");
    expect_output(S.hier <- obozinski.holdout(S, g, testIndex=1:2, heuristic="or", norm=FALSE, norm.type=NULL), "Obozinski's heuristic or correction: done");
    expect_output(S.hier <- obozinski.holdout(S, g, testIndex=1:2, heuristic="and", norm=TRUE, norm.type="maxnorm"),
        "maxnorm normalization: done\\nObozinski's heuristic and correction: done");
    expect_error(S.hier  <- obozinski.holdout(S, g, testIndex=1:2, heuristic="max", norm=FALSE, norm.type="maxnorm"),
        "do you wanna or not normalize the matrix S\\? norm and norm.type are inconsistent");
    expect_error(S.hier  <- obozinski.holdout(S, g, testIndex=1:2, heuristic="and", norm=TRUE, norm.type=NULL),
        "choose a normalization methods among those available");
    expect_error(S.hier  <- obozinski.holdout(S, g, testIndex=1:2, heuristic="o", norm=FALSE, norm.type=NULL),
        "the chosen heuristic method is not among those available or it has been misspelled");
})
