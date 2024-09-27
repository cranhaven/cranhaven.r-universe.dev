#'@title doEnrich
#'@description given a list of genes, this function combines topological properties of the disease ontology structure for enrichment analysis.
#'@author Haixiu Yang
#'@param interestGenes a vector of gene IDs.
#'@param test One of 'fisherTest','hypergeomTest','binomTest','chisqTest' and 'logoddTest' statistical model. Default is hypergeomTest.
#'@param method One of 'holm', 'hochberg', 'hommel', 'bonferroni', 'BH', 'BY','fdr' and 'none',for P value correction.
#'@param m Set the maximum number of ancestor layers for ontology enrichment. Default is layer 1.
#'@param maxGsize indicates that doterms with more annotation genes than maxGsize are ignored, and the P value of these doterms is set to 1.
#'@param minGsize indicates that doterms with less annotation genes than minGsize are ignored, and the P value of these doterms is set to 1.
#'@param traditional a logical variable, TRUE for traditional enrichment analysis, FALSE for enrichment analysis with weights. Default is FALSE.
#'@param delta Set the threshold of nodes, if the p value of doterm is greater than delta, the nodes are not significant, and these nodes are not weighted.
#'@param penalize Logical value, whether to add a penalty to the node.Adding a penalty will look for nodes with more branches.
#'@param allDOTerms Logical value, whether to store all doterms in EnrichResult, defaults is FALSE (only significant nodes are retained).
#'@return A \code{EnrichResult} instance.
#'@importFrom magrittr `%>%`
#'@importFrom purrr map pwalk map_dbl walk2 map2 map_int
#'@importFrom dplyr mutate filter select arrange
#'@importFrom stringr str_c
#'@importFrom BiocGenerics intersect
#'@import hash
#'@export
#'@examples
#'#The enrichment results were obtained by using demo.data
#'demo.data <- c(1636,351,102,2932,3077,348,4137,54209)
#'demo_result <- doEnrich(interestGenes=demo.data,maxGsize = 100, minGsize=10)
# main function
doEnrich <- function(interestGenes, test = c("hypergeomTest", "fisherTest", "binomTest", "chisqTest", "logoddTest"), method = c("BH",
    "holm", "hochberg", "hommel", "bonferroni", "BY", "fdr", "none"), m = 1, maxGsize = 5000, minGsize = 5, traditional = FALSE,
    delta = 0.01, penalize = TRUE, allDOTerms = FALSE) {
    if (m > 13) {
        stop("The disease ontology hierarchy is limited to layers 1 to 13")
    }

    test <- match.arg(test, several.ok = FALSE)
    method <- match.arg(method, several.ok = FALSE)

    init(traditional)


    interestGenes <- intersect(interestGenes, dotermgenes)
    enrich <- env$enrich
    enrich <- enrich %>%
        mutate(cg.arr = map(gene.arr, intersect, interestGenes)) %>%
        mutate(cg.len = map_int(cg.arr, length)) %>%
        mutate(ig.len = length(interestGenes))

    # filter DOID
    currentEnrich <- filter(enrich, cg.len != 0, gene.len >= minGsize, gene.len <= maxGsize)
    doidCount <- currentEnrich$DOID
    if (length(doidCount) == 0) {
        warning("No DOTerm met the condition")
    }
    env$doidCount <- doidCount

    enrichPvalue <- env$enrichPvalue

    if (traditional == TRUE) {
        pwalk(currentEnrich, function(DOID, p, gene.arr, gene.w, ...) {
            p <- Test(test, interestGenes, gene.arr, gene.w)
            assign(DOID, p, envir = enrichPvalue)
        })
    } else {

        enrich$child.arr <- sapply(enrich$child.arr, function(x) {
            intersect(x, doidCount)
        })
        enrich$child.len <- sapply(enrich$child.arr, function(x) {
            length(x)
        })

        # Step by step from the leaf node to the parent node
        for (i in max(enrich$level):m) {
            # current level
            currentLevelTerms <- enrich %>%
                filter(level == i, DOID %in% doidCount)
            # Record the number of nodes and annotated genes in each layer
            levelDOID <- length(currentLevelTerms$DOID)
            levelGene <- length(unique(unlist(currentLevelTerms$gene.arr)))
            message(str_c("LEVEL: ", i, "\t", levelDOID, " nodes\t", levelGene, " genes to be scored"))

            pwalk(currentLevelTerms, function(DOID, parent.arr, child.arr, ...) {
                computeTermSig(interestGenes, i, DOID, parent.arr, child.arr, test, traditional, delta, penalize)
            })

        }
    }

    enrichPvalue <- env$enrichPvalue
    enrich$p <- as.numeric(map(enrich$DOID, function(d) {
        as.numeric(enrichPvalue[[d]])
    }))

    enrichWeight <- env$enrichWeight
    enrich$gene.w <- map(enrich$DOID, function(d) {
        enrichWeight[[d]]
    })

    enrich <- enrich %>%
        arrange(p)
    enrich <- mutate(enrich, p.adjust = p.adjust(p, method = method))
    if (allDOTerms == FALSE) {
        enrich <- filter(enrich, p < delta)
    }
    result <- new("EnrichResult", enrich = enrich, interestGenes = interestGenes, test = test, method = method, m = m, maxGsize = maxGsize,
        minGsize = minGsize, delta = delta, traditional = traditional, penalize = penalize)

    return(result)
}
