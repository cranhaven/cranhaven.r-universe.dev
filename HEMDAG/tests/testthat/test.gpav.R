library(HEMDAG);
source("make.test.data.R");

context("test gpav");

test_that("adj.upper.tri works",{
    g <- make.graph();
    tmp <- tempfile(); ## store adj

    adj <- adj.upper.tri(g);
    write.table(adj, row.names=TRUE, col.names=TRUE, quote=FALSE, file=tmp);
    adj.check <- as.matrix(read.table(tmp));
    expect_equal(adj, adj.check);
})

test_that("gpav works", {
    S <- make.scores();
    Y <- S["pr2",];
    W <- rep(1,length(Y));
    g <- make.graph();
    tmp <- tempfile();

    adj <- adj.upper.tri(g);
    Y.gpav  <- gpav(Y, W=NULL, adj); ## if W=NULL it is assumed that W is unitary
    Y.gpav2 <- gpav(Y, W=W, adj);

    write.table(Y.gpav, row.names=TRUE, col.names=FALSE, quote=FALSE, file=tmp);
    Y.tmp <- read.table(tmp, stringsAsFactor=FALSE);
    Y.check <- Y.tmp[,2];
    names(Y.check) <- Y.tmp[,1];
    expect_equal(Y.gpav,  Y.check);
    expect_equal(Y.gpav2, Y.check);

    Y.trim <- Y[-which(names(Y) %in% c("D","H"))];
    W.trim <- rep(1,length(Y.trim));
    expect_error(gpav(Y.trim, W=NULL, adj), "mismatch between the number of classes between Y and adj");
    expect_error(gpav(Y, W=W.trim, adj), "mismatch between the number of classes between Y and adj");
})

test_that("gpav.over.examples works", {
    S <- make.scores();
    g <- make.graph();
    tmp <- tempfile();

    S.gpav <- gpav.over.examples(S, g, W=NULL);
    write.table(S.gpav, row.names=TRUE, col.names=TRUE, quote=FALSE, file=tmp);
    S.check <- as.matrix(read.table(tmp));
    expect_equal(S.gpav, S.check);

    S.error <- S[,-which(colnames(S) %in% c("D","H"))];
    expect_error(gpav.over.examples(S.error, g, W=NULL), "mismatch between the number of nodes of the graph g and the number of classes of the scores matrix S");
})

test_that("gpav.parallel works", {
    S <- make.scores();
    g <- make.graph();
    tmp <- tempfile();

    if(Sys.info()['sysname']!="Windows"){
        S.gpav1core <- gpav.parallel(S, W=NULL, g, ncores=1);
        S.gpav2core <- gpav.parallel(S, W=NULL, g, ncores=2);

        write.table(S.gpav1core, row.names=TRUE, col.names=TRUE, quote=FALSE, file=tmp);
        S.check <- as.matrix(read.table(tmp));
        expect_equal(S.gpav2core, S.check);
        expect_equal(S.gpav1core, S.check);
    }

    S.error <- S[,-which(colnames(S) %in% c("D","H"))];
    expect_error(gpav.parallel(S.error, g, W=NULL), "mismatch between the number of nodes of the graph g and the number of classes of the scores matrix S");

    ## gpav.parallel with ncores>2
    ## R CMD check allows a maximum of two cores -> see a 'NB:...' in the R Packages Book (http://r-pkgs.had.co.nz/check.html)
    ## skip on CRAN -- avoid fail message 'simultaneous processes spawned'
    ## not_on_cran allows to execute test locally (with covr) but not on CRAN check
    not_on_cran <- function(){identical(Sys.getenv("NOT_CRAN"), "TRUE");}  ## set NOT_CRAN environment variable to TRUE
    if(not_on_cran()){
        S.gpav0core <- gpav.parallel(S, W=NULL, g, ncores=0);
        expect_equal(S.gpav0core, S.check);
    }else{
        skip_on_cran(); ## skip test on CRAN
    }
})

test_that("gpav vanilla works", {
    g <- make.graph();
    root <- root.node(g);
    nd <- graph::nodes(g);
    S <- make.scores();
    S.noroot <- S[,-which(colnames(S)==root)];

    expect_output(S.gpav <- gpav.vanilla(S, g, parallel=FALSE, ncores=1, norm=FALSE, norm.type=NULL), "gpav correction: done");
    expect_output(S.gpav <- gpav.vanilla(S.noroot, g, parallel=FALSE, ncores=1, norm=FALSE, norm.type=NULL), "gpav correction: done");
    expect_output(S.gpav <- gpav.vanilla(S, g, parallel=FALSE, ncores=1, norm=TRUE, norm.type="maxnorm"),
        "maxnorm normalization: done\\ngpav correction: done");
    expect_output(S.gpav <- gpav.vanilla(S, g, parallel=FALSE, ncores=1, norm=TRUE, norm.type="qnorm"),
        "qnorm normalization: done\\ngpav correction: done");
     expect_output(expect_warning(S.gpav <- gpav.vanilla(S, g, parallel=TRUE, ncores=1, norm=FALSE, norm.type=NULL),
        "increase number of cores to exploit the gpav parallel version"),
        "gpav correction: done");
    expect_output(expect_warning(S.gpav <- gpav.vanilla(S, g, parallel=FALSE, ncores=2, norm=FALSE, norm.type=NULL),
        "set parallel to TRUE to exploit the gpav parallel version"),
        "gpav correction: done");
    expect_error(S.gpav  <- gpav.vanilla(S, g, parallel=FALSE, ncores=1, norm=FALSE, norm.type="maxnorm"),
        "do you wanna or not normalize the matrix S\\? norm and norm.type are inconsistent");
    expect_error(S.gpav  <- gpav.vanilla(S, g, parallel=FALSE, ncores=1, norm=TRUE, norm.type=NULL), "choose a normalization methods among those available");
})

test_that("gpav holdout works", {
    g <- make.graph();
    root <- root.node(g);
    nd <- graph::nodes(g);
    S <- make.scores();
    S.noroot <- S[,-which(colnames(S)==root)];

    expect_output(S.gpav <- gpav.holdout(S, g, testIndex=1:2, parallel=FALSE, ncores=1, norm=FALSE, norm.type=NULL), "gpav correction: done");
    expect_output(S.gpav <- gpav.holdout(S.noroot, g,  testIndex=1:2, parallel=FALSE, ncores=1, norm=FALSE, norm.type=NULL), "gpav correction: done");
    expect_output(S.gpav <- gpav.holdout(S, g,  testIndex=1:2, parallel=FALSE, ncores=1, norm=TRUE, norm.type="maxnorm"),
        "maxnorm normalization: done\\ngpav correction: done");
    expect_output(S.gpav <- gpav.holdout(S, g,  testIndex=1:2, parallel=FALSE, ncores=1, norm=TRUE, norm.type="qnorm"),
        "qnorm normalization: done\\ngpav correction: done");
   expect_output(expect_warning(S.gpav <- gpav.holdout(S, g,  testIndex=1:2, parallel=TRUE, ncores=1, norm=FALSE, norm.type=NULL),
        "increase number of cores to exploit the gpav parallel version"),
        "gpav correction: done");
    expect_output(expect_warning(S.gpav <- gpav.holdout(S, g,  testIndex=1:2, parallel=FALSE, ncores=2, norm=FALSE, norm.type=NULL),
        "set parallel to TRUE to exploit the gpav parallel version"),
        "gpav correction: done");
    expect_error(S.gpav  <- gpav.holdout(S, g, testIndex=1:2, parallel=FALSE, ncores=1, norm=FALSE, norm.type="maxnorm"),
        "do you wanna or not normalize the matrix S\\? norm and norm.type are inconsistent");
    expect_error(S.gpav  <- gpav.holdout(S, g, testIndex=1:2, parallel=FALSE, ncores=1, norm=TRUE, norm.type=NULL),
        "choose a normalization methods among those available");
})
