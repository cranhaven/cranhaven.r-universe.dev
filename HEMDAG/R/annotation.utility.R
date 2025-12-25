################################################################
## Utility functions to process and analyze annotation matrix ##
################################################################

#' @title Specific annotation matrix
#' @description Build the annotation matrix of the most specific functional terms.
#' @details The input plain text file (representing the associations gene-OBO terms) can be obtained by cloning the GitHub repository
#' \href{https://github.com/marconotaro/obogaf-parser}{obogaf-parser}, a perl5 module specifically designed to handle HPO and GO obo file and
#' their gene annotation file (gaf file).
#' @param file text file representing the associations gene-OBO terms. The file must be written as sequence of rows.
#' Each row represents a gene/protein and all its associations with an ontology term (pipe separated), i.e. in the form \emph{e.g.: gene1 |obo1|obo2|...|oboN}.
#' @return The annotation matrix of the most specific annotations (0/1): rows are genes and columns are functional terms (such as GO or HPO).
#' Let's denote \eqn{M} the labels matrix. If \eqn{M[i,j]=1}, means that the gene \eqn{i} is annotated with the class \eqn{j}, otherwise \eqn{M[i,j]=0}.
#' @export
#' @examples
#' gene2pheno <- system.file("extdata/gene2pheno.txt.gz", package="HEMDAG");
#' spec.ann <- specific.annotation.matrix(file=gene2pheno);
specific.annotation.matrix <- function(file="gene2pheno.txt.gz"){
    tmp <- strsplit(file, "[.,/,_]")[[1]];
    if(any(tmp %in% "gz")){
        con <- gzfile(file);
        line <- readLines(con);
        close(con);
    }else{
        line <- readLines(file);
    }
    tmp <- strsplit(line, split="[(\\s+,\t)|]", perl=TRUE); #nb: space (or tab) before pipe useful to separate gene from obo terms
    genenames <- c();
    for(i in 1:length(tmp)) genenames <- c(genenames,tmp[[i]][1]);
    ann.list <- list();
    for(i in 1:length(tmp)) ann.list[[i]] <- unique(tmp[[i]])[-1];
    names(ann.list) <- genenames;
    oboID <- unique(unlist(ann.list));
    n.genes <- length(genenames);
    n.oboID <- length(oboID);
    m <- matrix(integer(n.genes * n.oboID), nrow=n.genes);
    rownames(m) <- genenames;
    colnames(m) <- oboID;
    for (i in genenames){
        spec.ann <- ann.list[[i]];
        m[i, spec.ann] <- 1;
    }
    charcheck <- any(suppressWarnings(is.na(as.numeric(genenames))));
    if(charcheck){
        m <- m[sort(rownames(m)),sort(colnames(m))];
    }else{
        rname <- as.character(sort(as.numeric(genenames)));
        m <- m[rname, sort(colnames(m))];
    }
    return(m);
}

#' @title Specific annotations list
#' @description Build the annotation list starting from the matrix of the most specific annotations.
#' @param ann an annotation matrix (0/1). Rows are examples and columns are the most specific functional terms. It must be a named matrix.
#' @return A named list, where names of each component correspond to examples (genes) and elements of each component are the associated functional terms.
#' @export
#' @examples
#' data(labels);
#' spec.list <- specific.annotation.list(L);
specific.annotation.list <- function(ann){
 ann.list <- apply(ann, 1, function(gene){
        terms <- which(gene==1);
        return(names(gene[terms]));
    });
    ## when ann has one positive per row ann.list is a vector -> force to list
    if(sum(ann) == nrow(ann))
        ann.list <- as.list(ann.list);
    return(ann.list);
}

#' @title Transitive closure of annotations
#' @description Perform the transitive closure of the annotations using ancestors and the most specific annotation matrix.
#' The annotations are propagated from bottom to top, enriching the most specific annotations table. Rows correspond to genes and columns to functional terms.
#' @param ann.spec the annotation matrix of the most specific annotations (0/1): rows are genes and columns are functional terms.
#' @param anc the ancestor list.
#' @return The annotation table T: rows correspond to genes and columns to OBO terms. \eqn{T[i,j]=1} means that gene \eqn{i} is annotated for the term \eqn{j},
#' \eqn{T[i,j]=0} means that gene \eqn{i} is not annotated for the term \eqn{j}.
#' @export
#' @examples
#' data(graph);
#' data(labels);
#' anc <- build.ancestors(g);
#' tca <- transitive.closure.annotations(L, anc);
transitive.closure.annotations <- function(ann.spec, anc){
    ## build annotation list
    ann.list <- specific.annotation.list(ann.spec);
    ## construct the full empty annotation matrix
    genes <- rownames(ann.spec);
    n.genes <- length(genes);
    oboIDs <- names(anc);
    n.oboID <- length(anc);
    obo.ann <- matrix(numeric(n.oboID * n.genes), nrow=n.genes, ncol=n.oboID);    #empty label matrix
    dimnames(obo.ann) <- list(genes,oboIDs);
    ## fill the full empty annotation matrix with the most specific annotation
    obo.spec.term <- colnames(ann.spec); # the most specific obo terms
    # might happen that there are same obo IDs that are classified as "obsolete" in obo file, but that still exist in the annotation file
    obo.spec.term.sel <- oboIDs[oboIDs  %in% obo.spec.term]; # removing obsolete obo terms...
    obo.ann[genes,obo.spec.term.sel] <- ann.spec[,obo.spec.term.sel];
    ## transitive closure: annotation propagation from the most specific nodes to all its ancestors
    for (i in genes){
        spec.ann <- ann.list[[i]];
        all.anc <- lapply(spec.ann, function(x) return(anc[[x]]));
        all.anc <- unique(unlist(all.anc));
        obo.ann[i, all.anc] <- 1;  # setting the annotations derived by transitive closure
    }
    ## remove obo empty terms
    obo.ann <- obo.ann[,colSums(obo.ann)!=0];
    return(obo.ann);
}

#' @title Full annotation matrix
#' @description Build a full annotations matrix using the ancestor list and the most specific annotations matrix w.r.t. a given weighted adjacency matrix (wadj).
#' The rows of the full annotation matrix correspond to all the examples of the given weighted adjacency matrix and the columns to the class/terms.
#' The transitive closure of the annotations is performed.
#' @details The examples present in the annotation matrix (\code{ann.spec}) but not in the adjacency weighted matrix (\code{W}) are purged.
#' @param W a symmetric adjacency weighted matrix of the graph.
#' @param anc the ancestor list.
#' @param ann.spec the annotation matrix of the most specific annotations (0/1): rows are genes and columns are terms.
#' @return A full annotation table T, that is a matrix where the transitive closure of annotations is performed.
#' Rows correspond to genes of the weighted adjacency matrix and columns to terms.
#' \eqn{T[i,j]=1} means that gene \eqn{i} is annotated for the term \eqn{j}, \eqn{T[i,j]=0} means that gene \eqn{i} is not annotated for the term \eqn{j}.
#' @export
#' @examples
#' data(wadj);
#' data(graph);
#' data(labels);
#' anc <- build.ancestors(g);
#' full.ann <- full.annotation.matrix(W, anc, L);
full.annotation.matrix <- function(W, anc, ann.spec){
    ## construction of annotation list
    ann.list <- specific.annotation.list(ann.spec);
    ## construction the full empty annotation matrix
    genes <- rownames(W);
    n.genes <- length(genes);
    oboIDs <- names(anc);
    n.oboID <- length(anc);
    obo.ann <- matrix(numeric(n.oboID * n.genes), nrow=n.genes, ncol=n.oboID);    #empty label matrix
    dimnames(obo.ann) <- list(genes,oboIDs);
    ## fill the full empty annotation matrix with the most specific annotation
    genes2obo <- rownames(ann.spec);              # all genes that are associated with obo terms
    genes.sel <- genes[genes %in% genes2obo];     # genes 2 obo terms 2 entrez id of wadj
    obo.spec.term <- colnames(ann.spec);          # the most specific obo terms
    #might happen that there are same obo IDs that are classified as "obsolete" in obo file, but that still exist in the annotation file (e.g. build 1233)
    obo.spec.term.sel <- oboIDs[oboIDs  %in% obo.spec.term]; # removing obsolete obo terms...
    obo.ann[genes.sel,obo.spec.term.sel] <- ann.spec[genes.sel,obo.spec.term.sel];    # setting the most specific annotations
    ## transitive closure: annotation propagation from the most specific nodes to all its ancestors
    for (i in genes){
        spec.ann <- ann.list[[i]];
        all.anc <- lapply(spec.ann, function(x) return(anc[[x]]));
        all.anc <- unique(unlist(all.anc));
        obo.ann[i, all.anc] <- 1;  # setting the annotations derived by transitive closure
    }
    ## remove obo empty terms
    obo.ann <- obo.ann[,colSums(obo.ann)!=0];
    return(obo.ann);
}

#' @title Build submatrix
#' @title Build an annotation matrix containing only those terms having more than n annotations.
#' @description Terms having less than n annotations are pruned. Terms having exactly n annotations are discarded as well.
#' @param ann the annotation matrix (0/1). Rows are examples and columns are functional terms.
#' @param n an integer number representing the number of annotations to be pruned.
#' @return An annotation matrix having only those terms with more than n annotations.
#' @export
#' @examples
#' data(labels);
#' subm <- build.submatrix(L,5);
build.submatrix <- function(ann,n){
    ann.sel <- ann[,colSums(ann)>n];
    return(ann.sel);
}

#' @title Annotation matrix checker
#' @description Assess the integrity of an annotation matrix where a transitive closure of annotations was performed.
#' @param anc the ancestor list.
#' @param ann.spec the annotation matrix of the most specific annotations (0/1): rows are genes and columns are terms.
#' @param ann the full annotation matrix (0/1), i.e. the matrix where the transitive closure of the annotation was performed.
#' Rows are examples and columns are terms.
#' @return If the transitive closure of the annotations is performed correctly, \code{OK} is returned, otherwise an error message is printed on the stdout.
#' @export
#' @examples
#' data(graph);
#' data(labels);
#' anc <- build.ancestors(g);
#' tca <- transitive.closure.annotations(L, anc);
#' check.annotation.matrix.integrity(anc, L, tca);
check.annotation.matrix.integrity <- function(anc, ann.spec, ann){
    ## construction of annotation list
    ann.list <- specific.annotation.list(ann.spec);
    genes <- rownames(ann);
    check <- c();
    for (i in genes){
        spec.ann <- which(ann[i,]==1);
        len.ann <- length(spec.ann);
        all.anc <- lapply(ann.list[[i]], function(x) return(anc[[x]]));
        all.anc <- unique(unlist(all.anc));
        len.anc <- length(all.anc);
        cmp <- len.anc == len.ann;
        if(cmp==TRUE){
            check <- c(check,"OK");
        } else {
            check <- c(check,"NOTOK");
        }
    }
    names(check) <- genes;
    violated <- any(check!="OK");
    if(violated){
        n <- names(check)[check=="NOTOK"];
        cat("check.annotation.matrix: NOTOK. Transitive closure NOT RESPECTED\n");
    }else{
        cat("check.annotation.matrix: OK\n");
    }
}
