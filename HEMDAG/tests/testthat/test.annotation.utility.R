library(HEMDAG);
source("make.test.data.R");

context("test annotation utility functions");

test_that("specific.annotation.matrix works", {
    ann.prname <- "pr1 A|C\npr2 B";
    ann.entrex <- "1 A|C\n2 B";

    file.txt <- paste0(tempdir(),"/","annfile.txt");
    file.zip <- paste0(tempdir(),"/","annfile.txt.gz");
    file.etx <- paste0(tempdir(),"/","annfile.entrez");
    writeLines(ann.prname, con=file.txt);
    writeLines(ann.prname, con=file.zip);
    writeLines(ann.entrex, con=file.etx);

    pr1 <- c(1,0,1);
    pr2 <- c(0,1,0);
    ann.goldID <- ann.gold <- rbind(pr1,pr2);
    colnames(ann.gold) <- c("A","B","C");
    rownames(ann.goldID) <- 1:nrow(ann.gold);
    colnames(ann.goldID) <- colnames(ann.gold);

    ann.txt <- specific.annotation.matrix(file=file.txt);
    ann.zip <- specific.annotation.matrix(file=file.zip);
    ann.etx <- specific.annotation.matrix(file=file.etx);
    expect_equal(ann.txt, ann.gold);
    expect_equal(ann.zip, ann.gold);
    expect_equal(ann.etx, ann.goldID);
})

test_that("specific.annotation.list works", {
    spec.ann <- make.spec.ann();
    ann.list <- specific.annotation.list(spec.ann);

    expect_equal(ann.list[["pr1"]], "I");
    expect_equal(ann.list[["pr2"]], "H");
    expect_equal(ann.list[["pr3"]], "J");
    expect_equal(ann.list[["pr4"]], "I");
})

test_that("transitive.closure.annotations works", {
    g <- make.graph();
    spec.ann <- make.spec.ann();

    anc <- build.ancestors(g);
    ann <- transitive.closure.annotations(spec.ann, anc);

    ann.check <- make.ann();
    expect_equal(ann, ann.check);
})

test_that("full.annotation.matrix works", {
    g <- make.graph();
    spec.ann <- make.spec.ann();
    ann <- make.ann();
    W <- make.scores();
    tmp <- tempfile();

    anc <- build.ancestors(g);
    W <- W[1:3,];

    full.ann <- full.annotation.matrix(W, anc, spec.ann); ## tca performed
    write.table(full.ann, row.names=TRUE, col.names=TRUE, quote=FALSE, file=tmp);
    full.check <- as.matrix(read.table(tmp));
    expect_equal(full.ann, full.check);
})

test_that("build.submatrix works", {
    ann <- make.ann();
    tmp <- tempfile();

    subann <- build.submatrix(ann,2);
    write.table(subann, row.names=TRUE, col.names=TRUE, quote=FALSE, file=tmp);
    subann.check <- as.matrix(read.table(tmp));
    expect_equal(subann, subann.check);
    expect_equal(subann, subann.check);
})

test_that("check.annotation.matrix.integrity works", {
    g <- make.graph();
    spec.ann <- make.spec.ann();

    anc <- build.ancestors(g);
    ann <- transitive.closure.annotations(spec.ann, anc);
    ann.broken <- ann;
    ann.broken[1,1] <- 0;

    expect_output(check.annotation.matrix.integrity(anc, spec.ann, ann.broken), "check.annotation.matrix: NOTOK. Transitive closure NOT RESPECTED");
    expect_output(check.annotation.matrix.integrity(anc, spec.ann, ann), "check.annotation.matrix: OK");
})
