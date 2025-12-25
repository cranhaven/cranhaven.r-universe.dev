#############
## TPR-DAG ##
#############

#' @title TPR-DAG ensemble variants
#' @description Collection of the true-path-rule-based hierarchical learning ensemble algorithms and its variants.
#'
#' \code{TPR-DAG} is a family of algorithms on the basis of the choice of the \strong{bottom-up} step adopted for the selection of
#' \emph{positive} children (or descendants) and of the \strong{top-down} step adopted to assure ontology-based predictions.
#' Indeed, in their more general form the \code{TPR-DAG} algorithms adopt a two step learning strategy:
#' \enumerate{
#'  \item in the first step they compute a \emph{per-level bottom-up} visit from leaves to root to propagate \emph{positive} predictions across the hierarchy;
#'  \item in the second step they compute a \emph{per-level top-down} visit from root to leaves in order to assure the consistency of the predictions.
#' }
#' It is worth noting that levels (both in the first and second step) are defined in terms of the maximum distance from
#' the root node (see \code{\link{graph.levels}}).
#' @details The \emph{vanilla} \code{TPR-DAG} adopts a per-level bottom-up traversal of the DAG to correct the flat predictions
#' \eqn{\hat{y}_i} according to the following formula:
#' \deqn{
#'  \bar{y}_i := \frac{1}{1 + |\phi_i|} (\hat{y}_i + \sum_{j \in \phi_i} \bar{y}_j)
#' }
#' where \eqn{\phi_i} are the positive children of \eqn{i}.
#' Different strategies to select the positive children \eqn{\phi_i} can be applied:
#' \enumerate{
#'  \item \strong{threshold-free} strategy: the positive nodes are those children that can increment the score of the node \eqn{i}, that is those nodes
#'   that achieve a score higher than that of their parents:
#'   \deqn{
#'      \phi_i := \{ j \in child(i) | \bar{y}_j > \hat{y}_i \}
#'   }
#'  \item \strong{threshold} strategy: the positive children are selected on the basis of a threshold that can be selected in two different ways:
#'   \enumerate{
#'    \item for each node a constant threshold \eqn{\bar{t}} is a priori selected:
#'     \deqn{
#'      \phi_i := \{ j \in child(i) | \bar{y}_j > \bar{t} \}
#'     }
#'   For instance if the predictions represent probabilities it could be meaningful to a priori select \eqn{\bar{t}=0.5}.
#'    \item the threshold is selected to maximize some performance metric \eqn{\mathcal{M}} estimated on the training data, as for instance
#'     the Fmax or the AUPRC. In other words the threshold is selected to maximize some measure of accuracy of the predictions
#'     \eqn{\mathcal{M}(j,t)} on the training data for the class \eqn{j} with respect to the threshold \eqn{t}.
#'     The corresponding set of positives \eqn{\forall i \in V} is:
#'     \deqn{
#'      \phi_i := \{ j \in child(i) | \bar{y}_j > t_j^*,  t_j^* = \arg \max_{t} \mathcal{M}(j,t) \}
#'     }
#'   For instance \eqn{t_j^*} can be selected from a set of \eqn{t \in (0,1)} through internal cross-validation techniques.
#'  }
#' }
#'
#' The weighted \code{TPR-DAG} version can be designed by adding a weight \eqn{w \in [0,1]} to balance between the
#' contribution of the node \eqn{i} and that of its positive children \eqn{\phi}, through their convex combination:
#' \deqn{
#'  \bar{y}_i := w \hat{y}_i + \frac{(1 - w)}{|\phi_i|} \sum_{j \in \phi_i} \bar{y}_j
#' }
#' If \eqn{w=1} no weight is attributed to the children and the \code{TPR-DAG} reduces to the \code{HTD-DAG} algorithm, since in this
#' way only the prediction for node \eqn{i} is used in the bottom-up step of the algorithm. If \eqn{w=0} only the predictors
#' associated to the children nodes vote to predict node \eqn{i}. In the intermediate cases we attribute more importance to the predictor for the
#' node \eqn{i} or to its children depending on the values of \eqn{w}.
#' By combining the weighted and the threshold variant, we design the weighted-threshold variant.
#'
#' Since the contribution of the descendants of a given node decays exponentially with their distance from the node itself, to enhance the
#' contribution of the most specific nodes to the overall decision of the ensemble we design the ensemble variant \code{DESCENS}.
#' The novelty of \code{DESCENS} consists in strongly considering the contribution of all the descendants of each node instead of
#' only that of its children. Therefore \code{DESCENS} predictions are more influenced by the information embedded in the leaves nodes,
#' that are the classes containing the most informative and meaningful information from a biological and medical standpoint.
#' For the choice of the ``positive'' descendants we use the same strategies adopted for the selection of the ``positive''
#' children shown above. Furthermore, we designed a variant specific only for \code{DESCENS}, that we named \code{DESCENS}-\eqn{\tau}.
#' The \code{DESCENS}-\eqn{\tau} variant balances the contribution between the ``positives'' children of a node \eqn{i}
#' and that of its ``positives'' descendants excluding its children by adding a weight \eqn{\tau \in [0,1]}:
#' \deqn{
#'  \bar{y}_i := \frac{\tau}{1+|\phi_i|}(\hat{y}_i + \sum_{j \in \phi_i} \bar{y}_j) + \frac{1-\tau}{1+|\delta_i|}(\hat{y}_i + \sum_{j\in \delta_i} \bar{y}_j)
#' }
#' where \eqn{\phi_i} are the ``positive'' children of \eqn{i} and \eqn{\delta_i=\Delta_i \setminus \phi_i} the descendants of \eqn{i} without its children.
#' If \eqn{\tau=1} we consider only the contribution of the ``positive'' children of \eqn{i}; if \eqn{\tau=0} only the descendants that are not
#' children contribute to the score, while for intermediate values of \eqn{\tau} we can balance the contribution of \eqn{\phi_i} and
#' \eqn{\delta_i} positive nodes.
#'
#' Simply by replacing the \code{HTD-DAG} top-down step (\code{\link{htd}}) with the \code{GPAV} approach (\code{\link{gpav}}) we design the \code{ISO-TPR} variant.
#' The most important feature of \code{ISO-TPR} is that it maintains the hierarchical constraints by construction and it selects the closest
#' solution (in the least square sense) to the bottom-up predictions that obeys the \emph{True Path Rule}.
#' @seealso \code{\link{gpav}}, \code{\link{htd}}
#' @param S a named flat scores matrix with examples on rows and classes on columns.
#' @param g a graph of class \code{graphNEL}. It represents the hierarchy of the classes.
#' @param root name of the class that it is on the top-level of the hierarchy (\code{def. root="00"}).
#' @param positive choice of the \emph{positive} nodes to be considered in the bottom-up strategy. Can be one of the following values:
#' \itemize{
#'  \item \code{children} (\code{def.}): positive children are are considered for each node;
#'  \item \code{descendants}: positive descendants are are considered for each node;
#' }
#' @param bottomup strategy to enhance the flat predictions by propagating the positive predictions from leaves to root. It can be one of the following values:
#' \itemize{
#'  \item \code{threshold.free} (\code{def.}): positive nodes are selected on the basis of the \code{threshold.free} strategy;
#'  \item \code{threshold}: positive nodes are selected on the basis of the \code{threshold} strategy;
#'  \item \code{weighted.threshold.free}: positive nodes are selected on the basis of the \code{weighted.threshold.free} strategy;
#'  \item \code{weighted.threshold}: positive nodes are selected on the basis of the \code{weighted.threshold} strategy;
#'  \item \code{tau}: positive nodes are selected on the basis of the \code{tau} strategy.
#'   NOTE: \code{tau} is only a \code{DESCENS} variant. If you select \code{tau} strategy you must set \code{positive=descendants};
#' }
#' @param topdown strategy to make scores ``hierarchy-aware''. It can be one of the following values:
#' \itemize{
#'  \item \code{htd}: \code{HTD-DAG} strategy is applied (\code{\link{htd}});
#'  \item \code{gpav} (\code{def.}): \code{GPAV} strategy is applied (\code{\link{gpav}});
#' }
#' @param t threshold for the choice of positive nodes (\code{def. t=0}). Set \code{t} only for the variants requiring a threshold for the
#' selection of the positive nodes, otherwise set \code{t=0}.
#' @param w weight to balance between the contribution of the node \eqn{i} and that of its positive nodes. Set \code{w} only for
#' the \emph{weighted} variants, otherwise set \code{w=0}.
#' @param W vector of weight relative to a single example. If \code{W=NULL} (def.) it is assumed that \code{W} is a unitary vector of the
#' same length of the columns' number of the matrix \code{S} (root node included). Set \code{W} only if \code{topdown=gpav}.
#' @param parallel a boolean value:
#' \itemize{
#'  \item \code{TRUE}: execute the parallel implementation of GPAV (\code{\link{gpav.parallel}});
#'  \item \code{FALSE} (def.): execute the sequential implementation of GPAV (\code{\link{gpav.over.examples}});
#' }
#' Use \code{parallel} only if \code{topdown=GPAV}; otherwise set \code{parallel=FALSE}.
#' @param ncores number of cores to use for parallel execution. Set \code{ncores=1} if \code{parallel=FALSE}, otherwise set \code{ncores} to
#' the desired number of cores. Set \code{ncores} if and only if \code{topdown=GPAV}; otherwise set \code{ncores=1}.
#' @return A named matrix with the scores of the classes corrected according to the chosen \code{TPR-DAG} ensemble algorithm.
#' @export
#' @examples
#' data(graph);
#' data(scores);
#' data(labels);
#' root <- root.node(g);
#' S.tpr <- tpr.dag(S, g, root, positive="children", bottomup="threshold.free",
#' topdown="gpav", t=0, w=0, W=NULL, parallel=FALSE, ncores=1);
tpr.dag <- function(S, g, root="00", positive="children", bottomup="threshold.free", topdown="gpav", t=0, w=0, W=NULL, parallel=FALSE, ncores=1){
    ## parameters check
    if(positive!="children" && positive!="descendants" || bottomup!="threshold" && bottomup!="threshold.free" &&
        bottomup!="weighted.threshold" && bottomup!="weighted.threshold.free" && bottomup!="tau" || topdown!="htd" && topdown!="gpav")
        stop("positive or bottomup or topdown value misspelled");
    if(positive=="children" && bottomup=="tau")
        stop("tau is a descens variant. Please set positive to descendants");
    if(bottomup=="threshold" || bottomup=="tau")
        w <- 0;
    if(bottomup=="threshold.free"){
        t <- 0;
        w <- 0;
    }
    if(bottomup=="weighted.threshold.free")
        t <-0;
    if(t==1 || w==1)
        warning("when t or w is equal to 1, tpr-dag is reduced to htd-dag");
    if(topdown=="gpav" && parallel==TRUE && ncores<2)
        warning("increase number of cores to exploit the gpav parallel version");
    if(topdown=="gpav" && parallel==FALSE && ncores>=2)
        warning("set parallel to TRUE to exploit the gpav parallel version");
    if(topdown=="htd" && (parallel==TRUE || ncores>=2))
        warning("does not exist a parallel version of htd");
    ## add root node if it does not exist
    if(!(root %in% colnames(S))){
        max.score <- max(S);
        z <- rep(max.score,nrow(S));
        S <- cbind(z,S);
        colnames(S)[1] <- root;
    }
    ## check consistency between nodes of g and classes of S
    class.check <- ncol(S)!=numNodes(g);
    if(class.check)
        stop("mismatch between the number of nodes of the graph g and the number of classes of the scores matrix S");
    ## computing graph levels
    levels <- graph.levels(g,root);
    ## bottom-up visit: positive children selection
    if(positive=="children"){
        chd.bup <- build.children.bottom.up(g,levels);
        for(i in 1:length(chd.bup)){
            if(length(chd.bup[[i]])!=0){
                parent <- S[,names(chd.bup[i])];
                children <- as.matrix(S[,chd.bup[[i]]]);
                # colnames(children) <- chd.bup[[i]]
                for(j in 1:length(parent)){
                    if(bottomup=="threshold"){
                        child.set <- children[j,] > t;    # positive children selection
                        child.pos <- children[j,][child.set];
                        parent[j] <- (parent[j] + sum(child.pos))/(1+length(child.pos));  # flat scores correction
                    }else if(bottomup=="threshold.free"){
                        child.set <- children[j,] > parent[j]; # positive children selection
                        child.pos <- children[j,][child.set];
                        parent[j] <- (parent[j] + sum(child.pos))/(1+length(child.pos));  # flat score correction
                    }else if(bottomup=="weighted.threshold.free"){
                        child.set <- children[j,] > parent[j];    # positive children selection
                        child.pos <- children[j,][child.set];
                        if(length(child.pos)!=0){
                            parent[j] <- w*parent[j] + (1-w)*sum(child.pos)/length(child.pos);  # flat score correction
                        }
                    }else if(bottomup=="weighted.threshold"){
                        child.set <- children[j,] > t;    # positive children selection
                        child.pos <- children[j,][child.set];
                        if(length(child.pos)!=0){
                            parent[j] <- w*parent[j] + (1-w)*sum(child.pos)/length(child.pos);  # flat score prediction
                        }
                    }
                }
                S[,names(chd.bup[i])] <- parent;
            }
        }
    ## bottom-up visit: positive descendants selection
    }else if(positive=="descendants"){
        if(bottomup=="tau")
            chd.bup <- build.children.bottom.up(g,levels);
        desc.bup <- build.descendants.bottom.up(g,levels);
        nodes <- names(desc.bup);
        for(i in 1:length(desc.bup)){
            if(length(desc.bup[[i]])!=1){
                node.curr <- nodes[i];
                parent <- S[,names(desc.bup[i])];
                tmp <- setdiff(desc.bup[[i]],node.curr);
                if(bottomup=="tau"){
                    delta <- setdiff(tmp, chd.bup[[i]]);  # descendants without children
                    children <- as.matrix(S[,chd.bup[[i]]]);    # genes considering children node
                    desc <-  as.matrix(S[,delta]);      # genes considering descendants nodes without children
                }else{
                    desc <- as.matrix(S[,tmp]);
                }
                for(j in 1:length(parent)){
                    if(bottomup=="threshold"){
                        desc.set <- desc[j,] > t;    # positive descendants selection
                        desc.pos <- desc[j,][desc.set];
                        parent[j] <- (parent[j] + sum(desc.pos))/(1+length(desc.pos));   # flat scores correction
                    }else if(bottomup=="threshold.free"){
                        desc.set <- desc[j,] > parent[j];   # positive descendants selection
                        desc.pos <- desc[j,][desc.set];
                        parent[j] <- (parent[j] + sum(desc.pos))/(1+length(desc.pos));   # flat scores correction
                    }else if(bottomup=="weighted.threshold.free"){
                        desc.set <- desc[j,] > parent[j];
                        desc.pos <- desc[j,][desc.set];
                        if(length(desc.pos)!=0){
                            parent[j] <- w*parent[j] + (1-w)*sum(desc.pos)/length(desc.pos);  # flat scores correction
                        }
                    }else if(bottomup=="weighted.threshold"){
                        desc.set <- desc[j,] > t;
                        desc.pos <- desc[j,][desc.set];
                        if(length(desc.pos)!=0){
                            parent[j] <- w*parent[j] + (1-w)*sum(desc.pos)/length(desc.pos);  # flat scores correction
                        }
                    }else if(bottomup=="tau"){
                        desc.set <- desc[j,] > parent[j];           # positive descendants (without children) selection
                        desc.pos <- desc[j,][desc.set];
                        child.set <- children[j,] > parent[j];      # positive children selection
                        child.pos <- children[j,][child.set];
                        parent[j] <- t * ((parent[j] + sum(child.pos))/(1+length(child.pos))) + (1-t) * ((parent[j] + sum(desc.pos))/(1+length(desc.pos)));
                    }
                    S[,names(desc.bup[i])] <- parent;
                }
            }
        }
    }
    # top-down visit
    if(topdown=="htd"){
        par.tod <- build.parents.top.down(g,levels,root);
        for(i in 1:length(par.tod)){
            child <- S[,names(par.tod[i])];
            parents <- as.matrix(S[,par.tod[[i]]]);
            # colnames(parents) <- par.tod[[i]]
            # Note: the version with an apply and an ifelse statement is slower ...
            for(j in 1:length(child)){
                x <- min(parents[j,]);
                if(x < child[j]){
                    child[j] <- x;    # hierarchical correction
                }
            }
            S[,names(par.tod[i])] <- child;
        }
    }else if(topdown=="gpav"){
        if(parallel){
            S <- gpav.parallel(S, g, W=W, ncores=ncores);
        }else{
            S <- gpav.over.examples(S, W=W, g);
        }
    }
    return(S);
}

#' @title TPR-DAG cross-validation experiments
#' @description Correct the computed scores in a hierarchy according to the a \code{TPR-DAG} ensemble variant.
#' @details The parametric hierarchical ensemble variants are cross-validated maximizing the parameter on the metric selected in \code{metric}.
#' @param S a named flat scores matrix with examples on rows and classes on columns.
#' @param g a graph of class \code{graphNEL}. It represents the hierarchy of the classes.
#' @param ann an annotation matrix: rows correspond to examples and columns to classes. \eqn{ann[i,j]=1} if example \eqn{i} belongs to
#' class \eqn{j}, \eqn{ann[i,j]=0} otherwise. \code{ann} matrix is necessary to maximize the hyper-parameter(s) of the chosen parametric
#' \code{TPR-DAG} ensemble variant respect to the metric selected in \code{metric}. For the parametric-free ensemble variant set \code{ann=NULL}.
#' @param norm a boolean value. Should the flat score matrix be normalized? By default \code{norm=FALSE}. If \code{norm=TRUE} the matrix \code{S}
#' is normalized according to the normalization type selected in \code{norm.type}.
#' @param norm.type a string character. It can be one of the following values:
#' \enumerate{
#'  \item \code{NULL} (def.): none normalization is applied (\code{norm=FALSE})
#'  \item \code{maxnorm}: each score is divided for the maximum value of each class (\code{\link{scores.normalization}});
#'  \item \code{qnorm}: quantile normalization. \pkg{preprocessCore} package is used (\code{\link{scores.normalization}});
#' }
#' @param positive choice of the \emph{positive} nodes to be considered in the bottom-up strategy. Can be one of the following values:
#' \itemize{
#'  \item \code{children} (\code{def.}): positive children are are considered for each node;
#'  \item \code{descendants}: positive descendants are are considered for each node;
#' }
#' @param bottomup strategy to enhance the flat predictions by propagating the positive predictions from leaves to root. It can be one of the following values:
#' \itemize{
#'  \item \code{threshold.free}: positive nodes are selected on the basis of the \code{threshold.free} strategy;
#'  \item \code{threshold} (\code{def.}): positive nodes are selected on the basis of the \code{threshold} strategy;
#'  \item \code{weighted.threshold.free}: positive nodes are selected on the basis of the \code{weighted.threshold.free} strategy;
#'  \item \code{weighted.threshold}: positive nodes are selected on the basis of the \code{weighted.threshold} strategy;
#'  \item \code{tau}: positive nodes are selected on the basis of the \code{tau} strategy.
#'  NOTE: \code{tau} is only a \code{DESCENS} variant. If you select \code{tau} strategy you must set \code{positive=descendants};
#' }
#' @param topdown strategy to make the scores hierarchy-consistent. It can be one of the following values:
#' \itemize{
#'  \item \code{htd}: \code{HTD-DAG} strategy is applied (\code{\link{htd}});
#'  \item \code{gpav} (\code{def.}): \code{GPAV} strategy is applied (\code{\link{gpav}});
#' }
#' @param W vector of weight relative to a single example. If \code{W=NULL} (def.) it is assumed that \code{W} is a unitary vector of the same length
#' of the columns' number of the matrix \code{S} (root node included). Set \code{W} only if \code{topdown=gpav}.
#' @param parallel a boolean value:
#' \itemize{
#'  \item \code{TRUE}: execute the parallel implementation of GPAV (\code{\link{gpav.parallel}});
#'  \item \code{FALSE} (def.): execute the sequential implementation of GPAV (\code{\link{gpav.over.examples}});
#' }
#' Use \code{parallel} only if \code{topdown=gpav}; otherwise set \code{parallel=FALSE}.
#' @param ncores number of cores to use for parallel execution. Set \code{ncores=1} if \code{parallel=FALSE}, otherwise set \code{ncores} to the
#' desired number of cores. Set \code{ncores} if \code{topdown=gpav}, otherwise set \code{ncores=1}.
#' @param threshold range of threshold values to be tested in order to find the best threshold (\code{def:} \code{from:0.1}, \code{to:0.9}, \code{by:0.1}).
#' The denser the range is, the higher the probability to find the best threshold is, but the execution time will be higher.
#' For the \emph{threshold-free} variants, set \code{threshold=0}.
#' @param weight range of weight values to be tested in order to find the best weight (\code{def:} \code{from:0.1}, \code{to:0.9}, \code{by:0.1}).
#' The denser the range is, the higher the probability to find the best threshold is, but the execution time will be higher.
#' For the \emph{weight-free} variants, set \code{weight=0}.
#' @param kk number of folds of the cross validation (\code{def: kk=5}) on which tuning the parameters \code{threshold}, \code{weight} and \code{tau} of
#' the parametric ensemble variants. For the parametric-free variants (i.e. if \code{bottomup = threshold.free}), set \code{kk=NULL}.
#' @param seed initialization seed for the random generator to create folds (\code{def. 23}). If \code{seed=NULL} folds are generated without seed
#' initialization. If \code{bottomup=threshold.free}, set \code{seed=NULL}.
#' @param metric a string character specifying the performance metric on which maximizing the parametric ensemble variant. It can be one of the following values:
#' \enumerate{
#'  \item \code{auprc} (def.): the parametric ensemble variant is maximized on the basis of AUPRC (\code{\link{auprc}});
#'  \item \code{fmax}: the parametric ensemble variant is maximized on the basis of Fmax (\code{\link{multilabel.F.measure}};
#'  \item \code{NULL}: \code{threshold.free} variant is parameter-free, so none optimization is needed.
#' }
#' @param n.round number of rounding digits (def. \code{3}) to be applied to the hierarchical scores matrix for choosing the best threshold on the basis of
#' the best Fmax. If \code{bottomup==threshold.free} or \code{metric="auprc"}, set \code{n.round=NULL}.
#' @return A named matrix with the scores of the functional terms corrected according to the chosen \code{TPR-DAG} ensemble algorithm.
#' @export
#' @examples
#' data(graph);
#' data(scores);
#' data(labels);
#' S.tpr <- tpr.dag.cv(S, g, ann=NULL, norm=FALSE, norm.type=NULL, positive="children",
#' bottomup="threshold.free", topdown="gpav", W=NULL, parallel=FALSE, ncores=1,
#' threshold=0, weight=0, kk=NULL, seed=NULL, metric=NULL, n.round=NULL);
tpr.dag.cv <- function(S, g, ann, norm=FALSE, norm.type=NULL, positive="children", bottomup="threshold", topdown="gpav", W=NULL,
    parallel=FALSE, ncores=1, threshold=seq(from=0.1, to=0.9, by=0.1), weight=0, kk=5, seed=23, metric="auprc", n.round=NULL){
    ## parameters check
    if(positive!="children" && positive!="descendants" || bottomup!="threshold" && bottomup!="threshold.free" && bottomup!="weighted.threshold"
        && bottomup!="weighted.threshold.free" && bottomup!="tau" || topdown!="htd" && topdown!="gpav")
        stop("positive or bottomup or topdown value misspelled");
    if(positive=="children" && bottomup=="tau")
        stop("tau is a descens variant. Please set positive to descendants");
    if(bottomup=="threshold" || bottomup=="tau")
        weight <- 0;
    if(bottomup=="threshold.free"){
        threshold <- weight <- 0;
        kk <- seed <- ann <- metric <- n.round <- NULL;
    }
    if(bottomup=="weighted.threshold.free")
        threshold <- 0;
    if(norm==TRUE && is.null(norm.type))
        stop("choose a normalization methods among those available");
    if(norm==FALSE && !is.null(norm.type))
        stop("do you wanna or not normalize the matrix S? norm and norm.type inconsistent");
    if((is.null(kk) || kk<=1) && bottomup!="threshold.free")
        stop("smallest number of folds to define test and training set is 2. Set kk larger or equal to 2");
    if(metric!="fmax" && metric!="auprc" && !is.null(metric))
        stop("value of parameter metric misspelled");
    if(is.null(metric) && bottomup!="threshold.free")
        stop(paste0("the bottom-up approach ", bottomup, " is parametric"),". Select the metric on which maximize according to those available");
    if(is.null(seed) && bottomup!="threshold.free")
        stop("set seed to create folds");
    if(is.null(ann) && bottomup!="threshold.free")
        stop("the annotation matrix must be provided to maximize the hyper-parameter(s) of the chosen tpr-dag ensemble variant");
    if(is.null(n.round) && (metric=="fmax" && !is.null(metric)))
        stop("set n.round properly");
    if(metric=="auprc" && !is.null(n.round))
        n.round <- NULL;
    ## add root node if it does not exist
    root <- root.node(g);
    if(!(root %in% colnames(S))){
        max.score <- max(S);
        z <- rep(max.score,nrow(S));
        S <- cbind(z,S);
        colnames(S)[1] <- root;
    }
    ## normalization
    if(norm){
        S <- scores.normalization(norm.type=norm.type, S);
        cat(norm.type, "normalization: done\n");
    }
    ## tpr-dag hierarchical correction
    if(bottomup=="threshold.free"){
        S.hier <- tpr.dag(S, g, root=root, positive=positive, bottomup=bottomup, topdown=topdown, t=0, w=0, W=W, parallel=parallel, ncores=ncores);
        cat("tpr-dag correction done\n");
        rm(S); gc();
    }else{
        ## remove root node from ann matrix (root node must not be included in computing the performance)
        if(root %in% colnames(ann))
            ann <- ann[,-which(colnames(ann)==root)];
        ## let's start k-fold crossing validation for choosing best threshold and weight maximizing on the selected metric
        testIndex <- unstratified.cv.data(S, kk=kk, seed=seed);
        S.hier <- c(); # variable to host the k-assembled sub-matrix
        # train.top <- vector(mode="list", length=kk); ## for check
        for(k in 1:kk){
            ## train and test set
            train <- S[-testIndex[[k]],];
            ann.train <- ann[-testIndex[[k]],];
            test <- S[testIndex[[k]],];
            ## degenerate case when train or test set has just one row/example
            if(!is.matrix(train)){
                train.sample <- rownames(S)[-testIndex[[k]]];
                train <- matrix(train, ncol=length(train), dimnames=list(train.sample, names(train)));
                ann.train <- matrix(ann.train, ncol=length(ann.train), dimnames=list(train.sample, names(ann.train)));
            }
            if(!is.matrix(test)){
                test.sample<- rownames(S)[testIndex[[k]]];
                test <- matrix(test, ncol=length(test), dimnames=list(test.sample, names(test)));
            }
            ## parameters initialization
            top.metric <- 0;
            bestT <- 0;
            bestW <- 0;
            for(t in threshold){
                for(w in weight){
                    pred.train <- tpr.dag(train, g, root=root, positive=positive, bottomup=bottomup, topdown=topdown, w=w, t=t, W=W, parallel=parallel, ncores=ncores);
                    if(metric=="fmax"){
                        if(root %in% colnames(pred.train)){
                            pred.train <- pred.train[,-which(colnames(pred.train)==root)];
                            if(!is.matrix(pred.train)){
                                train.sample <- rownames(train);
                                pred.train <- matrix(pred.train, ncol=length(pred.train), dimnames=list(train.sample, names(pred.train)));
                            }
                        }
                        train.metric <- find.best.f(ann.train, pred.train, n.round=n.round, verbose=FALSE, b.per.example=FALSE)[["F"]];
                    }else{
                        if(root %in% colnames(pred.train)){
                            pred.train <- pred.train[,-which(colnames(pred.train)==root)];
                            if(!is.matrix(pred.train)){
                                train.sample <- rownames(train);
                                pred.train <- matrix(pred.train, ncol=length(pred.train), dimnames=list(train.sample, names(pred.train)));
                            }
                        }
                        train.metric <- auprc.single.over.classes(ann.train, pred.train, folds=NULL, seed=NULL)$average;
                    }
                    if(train.metric > top.metric){
                        top.metric <- train.metric;
                        bestT <- t;
                        bestW <- w;
                        # train.top[[k]] <- c(metric=top.metric, best.thres=bestT, best.weight=bestW);
                        if(bottomup=="threshold" || bottomup=="tau"){
                            cat("training fold:", k, paste0("top ", metric, " avg found:"), top.metric, "best threshold:", bestT, sep="\t", "\n");
                        }else if(bottomup=="weighted.threshold.free"){
                            cat("training fold:", k, paste0("top ", metric, " avg found:"), top.metric, "best weight:", bestW, sep="\t", "\n");
                        }else{
                            cat("training fold:", k, paste0("top ", metric, " avg found:"), top.metric, "best threshold:",bestT, "best weight:", bestW, sep="\t", "\n");
                        }
                    }
                }
            }
            ## test set
            pred.test <- tpr.dag(test, g, root=root, positive=positive, bottomup=bottomup, topdown=topdown, t=bestT, w=bestW, W=W, parallel=parallel, ncores=ncores);
            ## assembling the hierarchical scores of each k sub-matrix
            S.hier <- rbind(S.hier, pred.test);
        }
        ## put the rows (i.e. genes) of assembled k sub-matrix in the same order of the beginning matrix
        S.hier <- S.hier[rownames(S),];
        cat("tpr-dag correction done\n");
        rm(S, ann, testIndex, pred.test, test, train, ann.train); gc();
    }
    return(S.hier);
}

#' @title TPR-DAG holdout experiments
#' @description Correct the computed scores in a hierarchy according to the selected \code{TPR-DAG} ensemble variant by applying a classical holdout procedure.
#' @details The parametric hierarchical ensemble variants are cross-validated maximizing the parameter on the metric selected in \code{metric},
#' @param S a named flat scores matrix with examples on rows and classes on columns.
#' @param g a graph of class \code{graphNEL}. It represents the hierarchy of the classes.
#' @param ann an annotation matrix: rows correspond to examples and columns to classes. \eqn{ann[i,j]=1} if example \eqn{i} belongs to
#' class \eqn{j}, \eqn{ann[i,j]=0} otherwise. \code{ann} matrix is necessary to maximize the hyper-parameter(s) of the chosen parametric \code{TPR-DAG} ensemble
#' variant respect to the metric selected in \code{metric}. For the parametric-free ensemble variant set \code{ann=NULL}.
#' @param testIndex a vector of integer numbers corresponding to the indexes of the elements (rows) of the scores matrix \code{S} to be used in the test set.
#' @param norm a boolean value. Should the flat score matrix be normalized? By default \code{norm=FALSE}.
#' If \code{norm=TRUE} the matrix \code{S} is normalized according to the normalization type selected in \code{norm.type}.
#' @param norm.type a string character. It can be one of the following values:
#' \enumerate{
#'  \item \code{NULL} (def.): none normalization is applied (\code{norm=FALSE})
#'  \item \code{maxnorm}: each score is divided for the maximum value of each class;
#'  \item \code{qnorm}: quantile normalization. \pkg{preprocessCore} package is used;
#' }
#' @param positive choice of the \emph{positive} nodes to be considered in the bottom-up strategy. Can be one of the following values:
#' \itemize{
#'  \item \code{children} (\code{def.}): positive children are are considered for each node;
#'  \item \code{descendants}: positive descendants are are considered for each node;
#' }
#' @param bottomup strategy to enhance the flat predictions by propagating the positive predictions from leaves to root. It can be one of the following values:
#' \itemize{
#'  \item \code{threshold.free}: positive nodes are selected on the basis of the \code{threshold.free} strategy (\code{def.});
#'  \item \code{threshold} (\code{def.}): positive nodes are selected on the basis of the \code{threshold} strategy;
#'  \item \code{weighted.threshold.free}: positive nodes are selected on the basis of the \code{weighted.threshold.free} strategy;
#'  \item \code{weighted.threshold}: positive nodes are selected on the basis of the \code{weighted.threshold} strategy;
#'  \item \code{tau}: positive nodes are selected on the basis of the \code{tau} strategy.
#'   NOTE: \code{tau} is only a \code{DESCENS} variant. If you select \code{tau} strategy you must set \code{positive=descendants};
#' }
#' @param topdown strategy to make the scores hierarchy-consistent. It can be one of the following values:
#' \itemize{
#'  \item \code{htd}: \code{HTD-DAG} strategy is applied (\code{\link{htd}});
#'  \item \code{gpav} (\code{def.}): \code{GPAV} strategy is applied (\code{\link{gpav}});
#' }
#' @param W vector of weight relative to a single example. If \code{W=NULL} (def.) it is assumed that \code{W} is a unitary vector of the same
#' length of the columns' number of the matrix \code{S} (root node included). Set \code{W} only if \code{topdown=gpav}.
#' @param parallel a boolean value:
#' \itemize{
#'  \item \code{TRUE}: execute the parallel implementation of GPAV (\code{\link{gpav.parallel}});
#'  \item \code{FALSE} (def.): execute the sequential implementation of GPAV (\code{\link{gpav.over.examples}});
#' }
#' Use \code{parallel} only if \code{topdown=gpav}; otherwise set \code{parallel=FALSE}.
#' @param ncores number of cores to use for parallel execution. Set \code{ncores=1} if \code{parallel=FALSE}, otherwise set \code{ncores}
#' to the desired number of cores. Set \code{ncores} if and only if \code{topdown=gpav}; otherwise set \code{ncores=1}.
#' @param threshold range of threshold values to be tested in order to find the best threshold (\code{def:} \code{from:0.1}, \code{to:0.9}, \code{by:0.1}).
#' The denser the range is, the higher the probability to find the best threshold is, but the execution time will be higher.
#' For the \emph{threshold-free} variants, set \code{threshold=0}.
#' @param weight range of weight values to be tested in order to find the best weight (\code{def:} \code{from:0.1}, \code{to:0.9}, \code{by:0.1}).
#' The denser the range is, the higher the probability to find the best threshold is, but the execution time will be higher.
#' For the \emph{weight-free} variants, set \code{weight=0}.
#' @param kk number of folds of the cross validation (\code{def: kk=5}) on which tuning the parameters \code{threshold}, \code{weight} and \code{tau}
#' of the parametric ensemble variants. For the parametric-free variants (i.e. if \code{bottomup = threshold.free}), set \code{kk=NULL}.
#' @param seed initialization seed for the random generator to create folds (\code{def. 23}). If \code{seed=NULL} folds are generated
#' without seed initialization. If \code{bottomup=threshold.free}, set \code{seed=NULL}.
#' @param metric a string character specifying the performance metric on which maximizing the parametric ensemble variant. It can be one of the following values:
#' \enumerate{
#'  \item \code{auprc} (def.): the parametric ensemble variant is maximized on the basis of AUPRC (\code{\link{auprc}});
#'  \item \code{fmax}: the parametric ensemble variant is maximized on the basis of Fmax (\code{\link{multilabel.F.measure}};
#'  \item \code{NULL}: \code{threshold.free} variant is parameter-free, so none optimization is needed.
#' }
#' @param n.round number of rounding digits (def. \code{3}) to be applied to the hierarchical scores matrix for choosing the best threshold
#' on the basis of the best Fmax. If \code{bottomup==threshold.free} or \code{metric="auprc"}, set \code{n.round=NULL}.
#' @return A named matrix with the scores of the classes corrected according to the chosen \code{TPR-DAG} ensemble algorithm.
#' Rows of the matrix are shrunk to \code{testIndex}.
#' @export
#' @examples
#' data(graph);
#' data(scores);
#' data(labels);
#' data(test.index);
#' S.tpr <- tpr.dag.holdout(S, g, ann=NULL, testIndex=test.index, norm=FALSE, norm.type=NULL,
#' positive="children", bottomup="threshold.free", topdown="gpav", W=NULL, parallel=FALSE,
#' ncores=1, threshold=0, weight=0, kk=NULL, seed=NULL, metric=NULL, n.round=NULL);
tpr.dag.holdout <- function(S, g, ann, testIndex, norm=FALSE, norm.type=NULL, W=NULL, parallel=FALSE, ncores=1, positive="children", bottomup="threshold",
    topdown="htd",  threshold=seq(from=0.1, to=0.9, by=0.1), weight=seq(from=0.1, to=0.9, by=0.1), kk=5, seed=23, metric="auprc", n.round=NULL){
    ## parameters check
    if(positive!="children" && positive!="descendants" || bottomup!="threshold" && bottomup!="threshold.free" && bottomup!="weighted.threshold"
        && bottomup!="weighted.threshold.free" && bottomup!="tau" || topdown!="htd" && topdown!="gpav")
        stop("positive or bottomup or topdown value misspelled");
    if(positive=="children" && bottomup=="tau")
        stop("tau is a descens variant. Please set positive to descendants");
    if(bottomup=="threshold" || bottomup=="tau")
        weight <- 0;
    if(bottomup=="threshold.free"){
        threshold <- weight <- 0;
        kk <- seed <- ann <- metric <- n.round <- NULL;
    }
    if(bottomup=="weighted.threshold.free")
        threshold <- 0;
    if(norm==TRUE && is.null(norm.type))
        stop("choose a normalization methods among those available");
    if(norm==FALSE && !is.null(norm.type))
        stop("do you wanna or not normalize the matrix S? norm and norm.type inconsistent");
    if((is.null(kk) || kk<=1) && bottomup!="threshold.free")
        stop("smallest number of folds to define test and training set is 2. Set kk larger or equal to 2");
    if(metric!="fmax" && metric!="auprc" && !is.null(metric))
        stop("value of parameter metric misspelled");
    if(is.null(metric) && bottomup!="threshold.free")
        stop(paste0("the bottom-up approach ", bottomup, " is parametric"),". Select the metric on which maximize according to those available");
    if(is.null(seed) && bottomup!="threshold.free")
        stop("set seed to create folds");
    if(is.null(ann) && bottomup!="threshold.free")
        stop("the annotation matrix must be provided to maximize the hyper-parameter(s) of the chosen tpr-dag ensemble variant");
    if(is.null(n.round) && (metric=="fmax" && !is.null(metric)))
        stop("set n.round properly");
    if(metric=="auprc" && !is.null(n.round))
        n.round <- NULL;
    ## add root node if it does not exist
    root <- root.node(g);
    if(!(root %in% colnames(S))){
        max.score <- max(S);
        z <- rep(max.score,nrow(S));
        S <- cbind(z,S);
        colnames(S)[1] <- root;
    }
    ## normalization
    if(norm){
        S <- scores.normalization(norm.type=norm.type, S);
        cat(norm.type, "normalization done\n");
    }
    ## shrink S to test indexes
    S.test <- S[testIndex,];
    ## degenerate case when test set has just one row/example
    if(!is.matrix(S.test)){
        test.sample <- rownames(S)[testIndex];
        S.test <- matrix(S.test, ncol=length(S.test), dimnames=list(test.sample, names(S.test)));
    }
    ## tpr-dag correction
    if(bottomup=="threshold.free"){
        S.hier <- tpr.dag(S.test, g, root=root, positive=positive, bottomup=bottomup, topdown=topdown, t=0, w=0, W=W, parallel=parallel, ncores=ncores);
        cat("tpr-dag holdout correction done\n");
        rm(S);
    }else{
        ## remove root node from ann matrix (root node must not be included in computing the performance)
        if(root %in% colnames(ann))
            ann <- ann[,-which(colnames(ann)==root)];
        ## let's start k-fold crossing validation for choosing best threshold and weight maximizing on the selected metric
        S.train <- S[-testIndex,];
        ann.train <- ann[-testIndex,];
        ## if train set has just one row/example stop since we cannot create fold for tuning hyper-parameters (kk>=2)
        if(!is.matrix(S.train)){
            stop("training matrix too small (only one example/row) for hyper-parameters tuning. Please use threshold.free strategy instead");
        }
        foldIndex <- unstratified.cv.data(S.train, kk=kk, seed=seed);
        ## parameters initialization
        top.metric <- 0;
        bestT <- 0;
        bestW <- 0;
        for(k in 1:kk){
            ## train and test set
            train <- S.train[foldIndex[[k]],];
            target.train <- ann.train[foldIndex[[k]],];
            if(!is.matrix(train)){
                train.sample <- rownames(S.train)[foldIndex[[k]]];
                train <- matrix(train, ncol=length(train), dimnames=list(train.sample, names(train)));
                target.train <- matrix(target.train, ncol=length(target.train), dimnames=list(train.sample, names(target.train)));
            }
            ## metric initialization            
            for(t in threshold){
                for(w in weight){
                    pred.train <- tpr.dag(train, g, root=root, positive=positive, bottomup=bottomup, topdown=topdown, w=w, t=t, W=W, parallel=parallel, ncores=ncores);
                    if(metric=="fmax"){
                        if(root %in% colnames(pred.train)){
                            pred.train <- pred.train[,-which(colnames(pred.train)==root)];
                            if(!is.matrix(pred.train)){
                                train.sample <- rownames(target.train);
                                pred.train <- matrix(pred.train, ncol=length(pred.train), dimnames=list(train.sample, names(pred.train)));
                            }
                        }
                        train.metric <- find.best.f(target.train, pred.train, n.round=n.round, verbose=FALSE, b.per.example=FALSE)[["F"]];
                    }else{
                        if(root %in% colnames(pred.train)){
                            pred.train <- pred.train[,-which(colnames(pred.train)==root)];
                            if(!is.matrix(pred.train)){
                                train.sample <- rownames(target.train);
                                pred.train <- matrix(pred.train, ncol=length(pred.train), dimnames=list(train.sample, names(pred.train)));
                            }
                        }
                        train.metric <- auprc.single.over.classes(target.train, pred.train, folds=NULL, seed=NULL)$average;
                    }
                    if(train.metric > top.metric){
                        top.metric <- train.metric;
                        bestT <- t;
                        bestW <- w;
                        # train.top[[k]] <- c(metric=top.metric, best.thres=bestT, best.weight=bestW);
                        if(bottomup=="threshold" || bottomup=="tau"){
                            cat("training fold:", k, paste0("top ", metric, " avg found:"), top.metric, "best threshold:", bestT, sep="\t", "\n");
                        }else if(bottomup=="weighted.threshold.free"){
                            cat("training fold:", k, paste0("top ", metric, " avg found:"), top.metric, "best weight:", bestW, sep="\t", "\n");
                        }else{
                            cat("training fold:", k, paste0("top ", metric, " avg found:"), top.metric, "best threshold:", bestT, "best weight:", bestW, sep="\t", "\n");
                        }
                    }
                }
            }
        }
        ## final best threshold
        if(bottomup=="threshold" || bottomup=="tau"){
            cat("across", paste0(k, " training folds"), paste0("best ", metric, " avg found:"), top.metric, "best threshold:", bestT, sep="\t", "\n");
        }else if(bottomup=="weighted.threshold.free"){
            cat("across", paste0(k, " training folds"), paste0("best ", metric, " avg found:"), top.metric, "best weight:", bestW, sep="\t", "\n");
        }else{
            cat("across", paste0(k, " training folds"), paste0("best ", metric, " avg found:"), top.metric, "best threshold:", bestT, "best weight:", bestW, sep="\t", "\n");
        }
        S.hier <- tpr.dag(S.test, g, root=root, positive=positive, bottomup=bottomup, topdown=topdown, t=bestT, w=bestW, W=W, parallel=parallel, ncores=ncores);
        cat("tpr-dag holdout correction done\n");
        rm(S, S.test, S.train, train); gc();
    }
    return(S.hier);
}

#' @title Unstratified cross validation
#' @description This function splits a dataset in k-fold in an unstratified way, i.e. a fold does not contain an equal amount of positive
#' and negative examples. This function is used to perform k-fold cross-validation experiments in a hierarchical correction contest where
#' splitting dataset in a stratified way is not needed.
#' @param S matrix of the flat scores. It must be a named matrix, where rows are example (e.g. genes) and columns are classes/terms (e.g. GO terms).
#' @param kk number of folds in which to split the dataset (\code{def. k=5}).
#' @param seed seed for random generator. If \code{NULL} (def.) no initialization is performed.
#' @return A list with \eqn{k=kk} components (folds). Each component of the list is a character vector contains the index of the examples,
#' i.e. the index of the rows of the matrix S.
#' @export
#' @examples
#' data(scores);
#' foldIndex <- unstratified.cv.data(S, kk=5, seed=23);
unstratified.cv.data <- function(S, kk=5, seed=NULL){
    set.seed(seed);
    examples <- 1:nrow(S);
    n <- nrow(S);
    size <- c();
    folds <- vector(mode="list", length=kk)
    names(folds) <- paste0(rep("fold",kk), 1:kk)
    for(k in 1:kk){
        first <- ((k - 1) * n) %/% kk
        last <- (k * n) %/% kk
        size <- last-first;
        x    <- sample(examples,size);
        folds[[k]] <- x;
        examples <- setdiff(examples,x);
    }
    fold.check <- unlist(lapply(folds,length));
    if(any(fold.check==0))
        stop("number of folds selected too high: some folds have no examples. Please reduce the number of folds");
    return(folds);
}