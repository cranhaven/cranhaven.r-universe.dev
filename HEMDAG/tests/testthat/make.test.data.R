## create data for testing
check.graph <- function(){
    if(requireNamespace("graph", quietly=TRUE)){
        TRUE
    }else{
        FALSE
    }
}

make.graph <- function(){
    if (!check.graph()){ skip("graph package cannot be loaded"); }
    V <- LETTERS[1:10];
    edL <- vector("list", length=length(V));
    names(edL) <- V;
    edL[[1]] <- list(edges=c(2,3,7,5));
    edL[[2]] <- list(edges=c(4,6));
    edL[[3]] <- list(edges=c(6,8));
    edL[[4]] <- list(edges=c(6,9));
    edL[[5]] <- list(edges=c(8));
    edL[[6]] <- list(edges=c(8,9,10));
    edL[[7]] <- list(edges=c(8,5));
    edL[[8]] <- list(edges=c());
    edL[[9]] <- list(edges=c());
    g <- graph::graphNEL(nodes=V, edgeL=edL, edgemode="directed");
    return(g);
}

make.scores <- function(){
    set.seed(12345);
    S <- matrix(round(runif(40),2), nrow=4);
    dimnames(S) <- list(paste0("pr",1:4), LETTERS[1:ncol(S)]);
    return(S);
}

make.spec.ann <- function(){
    pr1 <- c(1,0,0);
    pr2 <- c(0,1,0);
    pr3 <- c(0,0,1);
    pr4 <- c(1,0,0);
    spec.ann <- rbind(pr1,pr2,pr3,pr4);
    colnames(spec.ann) <- c("I","H","J");
    return(spec.ann);
}

make.ann <- function(){
    g <- make.graph();
    spec.ann <- make.spec.ann();
    anc <- build.ancestors(g);
    ann <- transitive.closure.annotations(spec.ann, anc);
    return(ann);
}
