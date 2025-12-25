library(HEMDAG);
source("make.test.data.R");

context("test graph utility functions");

test_that("root.node works", {
    g <- make.graph();
    root <- root.node(g);

    expect_equal(root, "A");
    expect_equal(length(root), 1);
})

test_that("compute.flipped.graph works", {
    if (!check.graph()){ skip("graph package cannot be loaded"); }

    g <- make.graph();
    ndg <- graph::numNodes(g);
    edg <- graph::numEdges(g);

    og <- compute.flipped.graph(g);
    ndog <- numNodes(og);
    edog <- numEdges(og);

    expect_s4_class(og,"graphNEL");
    expect_equal(ndg, 10);
    expect_equal(edg, 16);
    expect_equal(ndog, 10);
    expect_equal(edog, 16);
})

test_that("graph.levels works", {
    g <- make.graph();
    root <- root.node(g);
    lev <- graph.levels(g, root=root);

    expect_equal(lev[["level_0"]], "A");
    expect_equal(lev[["level_1"]], c("B","C","G"));
    expect_equal(lev[["level_2"]], c("D","E"));
    expect_equal(lev[["level_3"]], "F");
    expect_equal(lev[["level_4"]], c("H","I","J"));
    expect_equal(length(lev), 5);

    expect_error(graph.levels(g, root="R"), "root node not found in g. Insert the root node");
    expect_error(graph.levels(g, root="B"), "root is not the right root node of g. Use the function root.node\\(g\\) to find the root node of g");
})

test_that("build.parents works", {
    g <- make.graph();
    nd <- nodes(g);
    root <- root.node(g)
    parents <- build.parents(g, root=root);
    lev <- graph.levels(g, root=root);
    parents.tod <- build.parents.top.down(g, lev, root=root);
    parents.bup <- build.parents.bottom.up(g, lev, root=root);
    parents.tsort <- build.parents.topological.sorting(g, root=root);

    expect_equal(length(parents), 9);
    expect_equal(length(parents.tod), 9);
    expect_equal(length(parents.bup), 9);
    expect_equal(names(parents), c("B","C","D","E","F","G","H","I","J"));
    expect_equal(names(parents.tod), c("B","C","G","D","E","F","H","I","J"));
    expect_equal(names(parents.bup), c("J","I","H","F","E","D","G","C","B"));
    expect_equal(names(parents.tsort), c("G","E","C","B","D","F","J","I","H"));
    expect_equal(parents[["B"]], "A");
    expect_equal(parents[["C"]], "A");
    expect_equal(parents[["D"]], "B");
    expect_equal(parents[["E"]], c("A","G"));
    expect_equal(parents[["F"]], c("B","C","D"));
    expect_equal(parents[["G"]], "A");
    expect_equal(parents[["H"]], c("C","E","F","G"));
    expect_equal(parents[["I"]], c("D","F"));
    expect_equal(parents[["J"]], "F");

    expect_error(build.parents(g, root="R"), "root node not found in g. Insert the root node");
    expect_error(build.parents(g, root="B"), "root is not the right root node of g. Use the function root.node\\(g\\) to find the root node of g");
    expect_error(build.parents.top.down(g, root="R"), "root node not found in g. Insert the root node");
    expect_error(build.parents.top.down(g, root="B"), "root is not the right root node of g. Use the function root.node\\(g\\) to find the root node of g");
    expect_error(build.parents.bottom.up(g,root="R"), "root node not found in g. Insert the root node");
    expect_error(build.parents.bottom.up(g,root="B"), "root is not the right root node of g. Use the function root.node\\(g\\) to find the root node of g");
    expect_error(build.parents.topological.sorting(g,root="R"), "root node not found in g. Insert the root node");
    expect_error(build.parents.topological.sorting(g,root="B"),
        "root is not the right root node of g. Use the function root.node\\(g\\) to find the root node of g");
})

test_that("build.children works", {
    g <- make.graph();
    nd <- nodes(g);
    root <- root.node(g)
    children <- build.children(g);
    lev <- graph.levels(g, root=root);
    children.tod <- build.children.top.down(g,lev);
    children.bup <- build.children.bottom.up(g,lev);

    expect_equal(length(children), 10);
    expect_equal(length(children.tod), 10);
    expect_equal(length(children.bup), 10);
    expect_equal(names(children), c("A","B","C","D","E","F","G","H","I","J"));
    expect_equal(names(children.tod), c("A","B","C","G","D","E","F","H","I","J"));
    expect_equal(names(children.bup), c("J","I","H","F","E","D","G","C","B","A"));

    expect_equal(children[["A"]], c("B","C","G","E"));
    expect_equal(children[["B"]], c("D","F"));
    expect_equal(children[["C"]], c("F","H"));
    expect_equal(children[["D"]], c("F","I"));
    expect_equal(children[["E"]], "H");
    expect_equal(children[["F"]], c("H","I","J"));
    expect_equal(children[["G"]], c("H","E"));
    expect_equal(children[["H"]], character(0));
    expect_equal(children[["I"]], character(0));
    expect_equal(children[["J"]], character(0));
})

test_that("build.ancestors works", {
    g <- make.graph();
    nd <- nodes(g);
    root <- root.node(g);
    lev <- graph.levels(g, root=root);
    anc <- build.ancestors(g);
    anc.tod <- build.ancestors.per.level(g,lev);
    anc.bup <- build.ancestors.bottom.up(g,lev);

    expect_equal(length(anc), 10);
    expect_equal(length(anc.tod), 10);
    expect_equal(length(anc.bup), 10);
    expect_equal(names(anc), nd);
    expect_equal(names(anc.tod), c("A","B","C","G","D","E","F","H","I","J"));
    expect_equal(names(anc.bup), c("J","I","H","F","E","D","G","C","B","A"));
    expect_equal(anc[["A"]], "A");
    expect_equal(anc[["B"]], c("A","B"));
    expect_equal(anc[["C"]], c("A","C"));
    expect_equal(anc[["D"]], c("B","A","D"));
    expect_equal(anc[["E"]], c("A","G","E"));
    expect_equal(anc[["F"]], c("D","B","A","C","F"));
    expect_equal(anc[["G"]], c("A","G"));
    expect_equal(anc[["H"]], c("F","D","B","A","E","G","C","H"));
    expect_equal(anc[["I"]], c("F","D","B","A","C","I"));
    expect_equal(anc[["J"]], c("F","D","B","A","C","J"));
})

test_that("build.descendants works", {
    g <- make.graph();
    nd <- nodes(g);
    root <- root.node(g);
    desc <- build.descendants(g);
    lev <- graph.levels(g, root=root);
    desc.tod <- build.descendants.per.level(g,lev);
    desc.bup <- build.descendants.bottom.up(g,lev);

    expect_equal(length(desc), 10);
    expect_equal(length(desc.tod), 10);
    expect_equal(length(desc.bup), 10);
    expect_equal(names(desc), nd);
    expect_equal(names(desc.tod), c("A","B","C","G","D","E","F","H","I","J"));
    expect_equal(names(desc.bup), c("J","I","H","F","E","D","G","C","B","A"));
    expect_equal(desc[["A"]], c("G","E","H","C","F","J","B","D","I","A"));
    expect_equal(desc[["B"]], c("H","F","J","D","I","B"));
    expect_equal(desc[["C"]], c("H","F","J","I","C"));
    expect_equal(desc[["D"]], c("H","F","J","I","D"));
    expect_equal(desc[["E"]], c("H","E"));
    expect_equal(desc[["F"]], c("H","J","I","F"));
    expect_equal(desc[["G"]], c("E","H","G"));
    expect_equal(desc[["H"]], c("H"));
    expect_equal(desc[["I"]], c("I"));
    expect_equal(desc[["J"]], c("J"));
})

test_that("constraints.matrix works", {
    g <- make.graph();
    tmp <- tempfile();

    m <- constraints.matrix(g);
    write.table(m, row.names=TRUE, col.names=TRUE, quote=FALSE, file=tmp);
    m.tmp <- read.table(tmp, row.names=NULL);
    m.check <- as.matrix(m.tmp[,c("child", "parent")]);
    rownames(m.check) <- m.tmp[,1];
    expect_equal(constraints.matrix(g), m);
})

test_that("lexicographical.topological.sort works", {
    g <- make.graph();
    gL1 <- graph::addEdge(from="A",to="A",g);
    gL2 <- graph::addEdge(from="C",to="C",g);

    expect_equal(lexicographical.topological.sort(g), c("A","B","C","D","F","G","E","H","I","J"));
    expect_error(lexicographical.topological.sort(gL1), "input graph g contains self-loop");
    expect_error(lexicographical.topological.sort(gL2), "input graph g contains self-loop");
})

test_that("build.consistent.graph works", {
    g <- make.graph();
    root <- root.node(g);
    G <- graph::addNode("Z",g);

    expect_s4_class(build.consistent.graph(g, root=root),"graphNEL");
    expect_output(build.consistent.graph(G, root=root), "removed nodes not accessible from root:\\n1 \\t Z");
})

test_that("check.dag.integrity works", {
    g <- make.graph();
    root <- root.node(g);
    G <- graph::addNode("Z",g);
    G <- graph::addEdge(from="Z",to="C",G);
    G <- graph::addEdge(from="Z",to="Z",G);
    expect_output(check.dag.integrity(g, root=root), "dag is ok");
    expect_output(check.dag.integrity(G, root=root), "not all nodes accessible from root\nnodes not accessible from root:\nZ");
    expect_error(check.dag.integrity(g, root="R"), "root node not found in g. Insert the root node");
    expect_error(check.dag.integrity(g, root="B"),
        "the supplied root node is not the right root node of g. Use the function root.node\\(g\\) to find the root node of g");
})

test_that("find.leaves works", {
    g <- make.graph();
    expect_equal(find.leaves(g), c("H","I","J"));
})

test_that("distances.from.leaves works", {
    g <- make.graph();
    dist.leaves <- distances.from.leaves(g);

    nd <- c("A","B","C","D","E","F","G","H","I","J");
    dist <- c(2,2,1,1,1,1,1,0,0,0);
    names(dist) <- nd;
    expect_equal(dist.leaves, dist);
})

test_that("check.hierarchy.single.sample works", {
    S  <- make.scores();
    g <- make.graph();

    adj <- adj.upper.tri(g);
    Y <- S["pr1",];
    Y.gpav  <- gpav(Y, W=NULL, adj);
    root <- root.node(g);
    nd <- graph::nodes(g);
    nd.noroot <- nd[-which(nd==root)];

    ## hierarchy constraints satisfied
    broken <- rep(FALSE, length(nd.noroot));
    names(broken) <- nd.noroot;
    satisfied <- sum(broken==FALSE);
    names(satisfied) <- FALSE;
    check <- list(status="OK", hierarchy.constraints.broken=broken, hierarchy.constraints.satisfied=satisfied);

    ## check that gpav hierarchical algorithm respects hierarchical constraints
    S.gpav.check <- check.hierarchy.single.sample(Y.gpav, g, root);
    expect_equal(S.gpav.check, check);

    ## check that random flat scores violates hierarchical constraints
    Y.flat <- Y[nd.noroot];
    check.flat <- check.hierarchy.single.sample(Y.flat, g, root);
    broken <- c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE);
    names(broken) <- nd.noroot;
    satisfied <- c(7,2);
    names(satisfied) <- c(FALSE,TRUE);
    check <- list(status="NOTOK", hierarchy.constraints.broken=broken, hierarchy.constraints.satisfied=satisfied);
    expect_equal(check.flat, check);
})

test_that("check.hierarchy works", {
    S  <- make.scores();
    g <- make.graph();
    root <- root.node(g);
    nd <- graph::nodes(g);
    nd.noroot <- nd[-which(nd==root)];

    ## hierarchy constraints satisfied
    broken <- rep(FALSE, length(nd.noroot));
    names(broken) <- nd.noroot;
    satisfied <- sum(broken==FALSE);
    names(satisfied) <- FALSE;
    check <- list(status="OK", hierarchy.constraints.broken=broken, hierarchy.constraints.satisfied=satisfied);

    ## check that all hierarchical algorithms respect hierarchical constraints
    S.htd  <- htd(S, g, root);
    S.htd.check <- check.hierarchy(S.htd, g, root);
    expect_equal(S.htd.check, check);

    S.max  <- obozinski.max(S, g, root);
    S.max.check <- check.hierarchy(S.max, g, root);
    expect_equal(S.max.check, check);

    S.and  <- obozinski.and(S, g, root);
    S.and.check <- check.hierarchy(S.and, g, root);
    expect_equal(S.and.check, check);

    S.or  <- obozinski.or(S, g, root);
    S.or.check <- check.hierarchy(S.or, g, root);
    expect_equal(S.or.check, check);

    S.tprTF <- tpr.dag(S, g, root, positive="children", bottomup="threshold.free", topdown="htd");
    S.tprTF.check <- check.hierarchy(S.tprTF, g, root);
    expect_equal(S.tprTF.check, check);

    S.tprT <- tpr.dag(S, g, root, positive="children", bottomup="threshold", topdown="htd", t=0.5);
    S.tprT.check <- check.hierarchy(S.tprT, g, root);
    expect_equal(S.tprT.check, check);

    S.tprW <- tpr.dag(S, g, root, positive="children", bottomup="weighted.threshold.free", topdown="htd", w=0.5);
    S.tprW.check <- check.hierarchy(S.tprW, g, root);
    expect_equal(S.tprW.check, check);

    S.tprWT <- tpr.dag(S, g, root, positive="children", bottomup="weighted.threshold", topdown="htd", t=0.5, w=0.5);
    S.tprWT.check <- check.hierarchy(S.tprWT, g, root);
    expect_equal(S.tprWT.check, check);

    S.descensTF <- tpr.dag(S, g, root, positive="descendants", bottomup="threshold.free", topdown="htd");
    S.descensTF.check <- check.hierarchy(S.descensTF, g, root);
    expect_equal(S.descensTF.check, check);

    S.descensT <- tpr.dag(S, g, root, positive="descendants", bottomup="threshold", topdown="htd", t=0.5);
    S.descensT.check <- check.hierarchy(S.descensT, g, root);
    expect_equal(S.descensT.check, check);

    S.descensW <- tpr.dag(S, g, root, positive="descendants", bottomup="weighted.threshold.free", topdown="htd", w=0.5);
    S.descensW.check <- check.hierarchy(S.descensW, g, root);
    expect_equal(S.descensW.check, check);

    S.descensWT <- tpr.dag(S, g, root, positive="descendants", bottomup="weighted.threshold", topdown="htd", t=0.5, w=05);
    S.descensWT.check <- check.hierarchy(S.descensWT, g, root);
    expect_equal(S.descensWT.check, check);

    S.descensTAU <- tpr.dag(S, g, root, positive="descendants", bottomup="tau", topdown="htd", t=0.5);
    S.descensTAU.check <- check.hierarchy(S.descensTAU, g, root);
    expect_equal(S.descensTAU.check, check);

    S.ISOtprTF <- tpr.dag(S, g, root, positive="children", bottomup="threshold.free", topdown="gpav");
    S.ISOtprTF.check <- check.hierarchy(S.ISOtprTF, g, root);
    expect_equal(S.ISOtprTF.check, check);

    S.ISOtprT <- tpr.dag(S, g, root, positive="children", bottomup="threshold", topdown="gpav", t=0.5);
    S.ISOtprT.check <- check.hierarchy(S.ISOtprT, g, root);
    expect_equal(S.ISOtprT.check, check);

    S.ISOtprW <- tpr.dag(S, g, root, positive="children", bottomup="weighted.threshold.free", topdown="gpav", w=0.5);
    S.ISOtprW.check <- check.hierarchy(S.ISOtprW, g, root);
    expect_equal(S.ISOtprW.check, check);

    S.ISOtprWT <- tpr.dag(S, g, root, positive="children", bottomup="weighted.threshold", topdown="gpav", t=0.5, w=0.5);
    S.ISOtprWT.check <- check.hierarchy(S.ISOtprWT, g, root);
    expect_equal(S.ISOtprWT.check, check);

    S.ISOdescensTF <- tpr.dag(S, g, root, positive="descendants", bottomup="threshold.free", topdown="gpav");
    S.ISOdescensTF.check <- check.hierarchy(S.ISOdescensTF, g, root);
    expect_equal(S.ISOdescensTF.check, check);

    S.ISOdescensT <- tpr.dag(S, g, root, positive="descendants", bottomup="threshold", topdown="gpav", t=0.5);
    S.ISOdescensT.check <- check.hierarchy(S.ISOdescensT, g, root);
    expect_equal(S.ISOdescensT.check, check);

    S.ISOdescensW   <- tpr.dag(S, g, root, positive="descendants", bottomup="weighted.threshold.free", topdown="gpav", w=0.5);
    S.ISOdescensW.check <- check.hierarchy(S.ISOdescensW, g, root);
    expect_equal(S.ISOdescensW.check, check);

    S.ISOdescensWT  <- tpr.dag(S, g, root, positive="descendants", bottomup="weighted.threshold", topdown="gpav", t=0.5, w=0.5);
    S.ISOdescensWT.check <- check.hierarchy(S.ISOdescensWT, g, root);
    expect_equal(S.ISOdescensWT.check, check);

    S.ISOdescensTAU <- tpr.dag(S, g, root, positive="descendants", bottomup="tau", topdown="gpav", t=0.5);
    S.ISOdescensTAU.check <- check.hierarchy(S.ISOdescensTAU, g, root);
    expect_equal(S.ISOdescensTAU.check, check);

    ## check that random flat scores violates hierarchical constraints
    S <- S[,nd.noroot];
    check.flat <- check.hierarchy(S, g, root);
    broken <- c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE);
    names(broken) <- nd.noroot;
    satisfied <- c(3,6);
    names(satisfied) <- c(FALSE,TRUE);
    check <- list(status="NOTOK", hierarchy.constraints.broken=broken, hierarchy.constraints.satisfied=satisfied);
    expect_equal(check.flat, check);
})

test_that("weighted.adjacency.matrix works", {
    file.zip <- system.file("extdata/edges.txt.gz", package="HEMDAG");
    file.txt <- paste0(tempdir(),"/","edges.txt");
    m.tupla <- read.table(gzfile(file.zip), colClasses="character", stringsAsFactors=FALSE);
    write.table(m.tupla, file=file.txt, row.names=FALSE, col.names=FALSE, quote=FALSE);

    m <- weighted.adjacency.matrix(file=file.zip);
    pr.names <- rownames(m);

    tmp <- tempfile();
    write.table(m, file=tmp, row.names=FALSE, col.names=FALSE, quote=FALSE);
    m.check <- as.matrix(read.table(tmp));
    dimnames(m.check) <- list(pr.names, pr.names);

    expect_equal(weighted.adjacency.matrix(file=file.txt), m.check);
    expect_equal(weighted.adjacency.matrix(file=file.zip), m.check);

    ## replace rows and columns names with random integer number (entrez gene-ID)
    entrez <- 1:length(pr.names);
    for(i in 1:length(pr.names)){
        m.tupla[which(m.tupla[,1]==pr.names[i]),1] <- entrez[i];
        m.tupla[which(m.tupla[,2]==pr.names[i]),2] <- entrez[i];
    }
    write.table(m.tupla, file=file.txt, row.names=FALSE, col.names=FALSE, quote=FALSE);

    m <- weighted.adjacency.matrix(file=file.txt);

    tmp <- tempfile();
    write.table(m, file=tmp, row.names=FALSE, col.names=FALSE, quote=FALSE);
    m.check <- as.matrix(read.table(tmp));
    dimnames(m.check) <- list(entrez, entrez);

    expect_equal(weighted.adjacency.matrix(file=file.txt), m.check);
})

test_that("tupla.matrix works", {
    ## store tupla matrix
    tmp <- tempfile();
    zip <- paste0(tempdir(),"/","tmp.gz");

    ## scores matrix
    m <- make.scores();
    tupla.matrix(m, output.file=tmp);
    m.check <- read.table(tmp);
    expect_equal(m.check, read.table(tmp));

    ## square symmetric matrix
    file <- system.file("extdata/edges.txt.gz", package="HEMDAG");
    j <- read.table(gzfile(file), colClasses="character", stringsAsFactors=FALSE);
    write.table(j, file=tmp, row.names=FALSE, col.names=FALSE, quote=FALSE);
    w <- weighted.adjacency.matrix(file=tmp);

    tupla.matrix(w, output.file=tmp);
    w.check <- read.table(tmp);
    expect_equal(w.check, read.table(tmp));

    ## degenerate case when w is symmetric and some interactions are zero
    w <- matrix(0, ncol=3, nrow=3);
    dimnames(w) <- list(LETTERS[1:3], LETTERS[1:3]);
    diag(w)[-1] <- 1;

    tupla.matrix(w, output.file=zip);
    w.check <- read.table(gzfile(zip));
    expect_equal(w.check, read.table(gzfile(zip)));
})

test_that("build.scores.matrix works", {
    tmp <- tempfile();

    ## read scores matrix from list
    file.list  <- system.file("extdata/scores.list.txt.gz", package="HEMDAG");
    S <- build.scores.matrix.from.list(file.list, split="[(\t,|)]");
    write.table(S, file=tmp, row.names=TRUE, col.names=TRUE, quote=FALSE);
    S.check <- read.table(tmp);
    expect_equal(S.check, read.table(tmp));

    ## name of rows are entrez-geneID (ie integer number)
    file.list.entrez <- tempfile();
    con <- gzfile(file.list);
    line <- readLines(con);
    close(con);
    entrez <- c(1, 10, 100, 1000);
    out <- c();
    li <- strsplit(line, split="\t");
    for(i in 1:length(li)){
        li[[i]][1] <- entrez[i];
        out <- c(out, paste0(li[[i]], collapse="\t"));
    }
    writeLines(out, file.list.entrez);
    S <- build.scores.matrix.from.list(file.list.entrez, split="[(\t,|)]");
    write.table(S, file=tmp, row.names=TRUE, col.names=TRUE, quote=FALSE);
    S.check <- read.table(tmp);
    expect_equal(S.check, read.table(tmp));

    ## read scores matrix from tupla
    file.tupla <- system.file("extdata/scores.tupla.txt.gz", package="HEMDAG");
    S <- build.scores.matrix.from.tupla(file.tupla);
    write.table(S, file=tmp, row.names=TRUE, col.names=TRUE, quote=FALSE);
    S.check <- read.table(tmp);
    expect_equal(S.check, read.table(tmp));

    ## name of rows are entrez geneID (integer number)
    file.tupla.entrez <- tempfile();
    m <- read.table(gzfile(file.tupla), stringsAsFactors=FALSE);
    entrez <- c(1, 10, 100, 1000);
    prs <- unique(m[,1]);
    for(i in 1:length(prs))
        m[which(m[,1]==prs[i]),1] <- entrez[i];
    write.table(m, file=file.tupla.entrez, row.names=TRUE, col.names=TRUE, quote=FALSE);
    S <- build.scores.matrix.from.tupla(file.tupla.entrez);
    write.table(S, file=tmp, row.names=TRUE, col.names=TRUE, quote=FALSE);
    S.check <- read.table(tmp);
    expect_equal(S.check, read.table(tmp));
})

test_that("build.edges.from.hpo.obo works", {
    ## take a while and require internet connection ...
    not_on_cran <- function(){identical(Sys.getenv("NOT_CRAN"), "TRUE");}  ## set NOT_CRAN environment variable to TRUE
    if(not_on_cran()){
        ## ## read and save plain hp.obo file
        tmp <- tempfile();
        hpobo <- "http://purl.obolibrary.org/obo/hp.obo";

        build.edges.from.hpo.obo(obofile=hpobo, file=tmp);
        obo.check <- read.table(tmp);
        expect_equal(obo.check, read.table(tmp));

        ## read and save zipped hp.obo file
        lines <- readLines(hpobo);
        obozip <- paste0(tempdir(),"/","hp.obo.gz")
        con <- gzfile(obozip, "w");
        writeLines(lines, con);
        close(con);
        edgzip <- paste0(tempdir(),"/","hp.edges.txt.gz");

        build.edges.from.hpo.obo(obofile=obozip, file=edgzip);
        obo.check <- read.table(gzfile(edgzip));
        expect_equal(obo.check, read.table(gzfile(edgzip)));
    }else{
        skip_on_cran(); ## skip test on CRAN
    }
})

test_that("write.graph works", {
    tmp <- tempfile();
    zip <- paste0(tempdir(),"/","tmp.gz");

    g <- make.graph();
    write.graph(g, file=tmp);
    write.graph(g, file=zip);

    tmp.check <- read.table(tmp);
    zip.check <- read.table(gzfile(zip));

    expect_equal(tmp.check, read.table(tmp));
    expect_equal(zip.check, read.table(gzfile(zip)));
})

test_that("read.graph works", {
    ## read from zipped file
    zip <- system.file("extdata/graph.edges.txt.gz", package= "HEMDAG");
    g <- read.graph(file=zip);
    expect_s4_class(g, "graphNEL");

    ## read from plain file
    tmp <- tempfile();
    write.table(read.table(gzfile(zip)), tmp);
    g <- read.graph(file=tmp);
    expect_s4_class(g, "graphNEL");
})

test_that("read.undirected.graph works", {
    ## read from zipped file
    zip <- system.file("extdata/edges.txt.gz", package="HEMDAG");
    g <- read.undirected.graph(file=zip);
    expect_s4_class(g, "graphNEL");

    ## read from plain file
    tmp <- tempfile();
    write.table(read.table(gzfile(zip)), tmp);
    g <- read.undirected.graph(file=tmp);
    expect_s4_class(g, "graphNEL");
})
