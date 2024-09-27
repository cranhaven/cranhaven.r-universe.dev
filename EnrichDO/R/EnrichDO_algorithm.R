#'@title Enrich_internal
#'@description Internal calculation of enrichment analysis
#'@author Haixiu Yang
#'@param resultDO Receives the file output by the wrireResult function, which is used to visually display the enrichment results (without running the enrichment operation again).
#'@importFrom dplyr mutate filter select
#'@importFrom stringr str_c
#'@importFrom tidyr separate
#'@import purrr
#'@import hash
#'@importFrom utils str
#'@importFrom S4Vectors DataFrame
#'@importFrom magrittr `%>%`
#'@importFrom BiocGenerics intersect
#'@importFrom stats p.adjust fisher.test phyper pbinom chisq.test prop.test
#'@return  A \code{EnrichResult} instance.
# doterm structure output
TermStruct <- function(resultDO) {

    enrichDOterms <- filter(doterms, doterms$DOID %in% resultDO$DOID) %>%
        select(DOID, parent.arr, gene.len)
    enrichResult <- separate(resultDO, col = geneRatio, sep = "/", into = c("cg.len", "ig.len"))
    enrich <- merge(enrichResult, enrichDOterms, by = "DOID")
    enrich <- arrange(enrich, p)
    enrich <- DataFrame(enrich)
    env$enrich <- enrich

    message("The enrichment results you provide are stored in enrich", "\n")
}

# init
init <- function(traditional) {
    tryCatch(utils::data(list = "doterms", package = "EnrichDO"))
    doterms <- get("doterms")
    env$doterms <- doterms

    tryCatch(utils::data(list = "dotermgenes", package = "EnrichDO"))
    dotermgenes <- get("dotermgenes")
    env$dotermgenes <- dotermgenes

    env$enrich <- NULL
    env$doidCount <- NULL

    # init gene weight
    if (traditional == TRUE) {
        enrich <- doterms %>%
            mutate(gene.w = map2(gene.len, gene.arr, function(n, arr) {
                w <- rep(1, times = n)
                names(w) <- arr
                return(w)
            }))
        message("\t\t -- Traditional test-- ", "\n")

    } else {
        enrich <- doterms %>%
            mutate(gene.w = map2(gene.arr, weight.arr, function(g, w) {
                names(w) <- g
                return(w)
            }))
        message("\t\t -- Descending rights test-- ", "\n")
    }

    env$enrich <- enrich

    enrichWeight <- hash(enrich$DOID, enrich$gene.w)
    enrichPvalue <- hash(enrich$DOID, 1)
    enrichgeneArr <- hash(enrich$DOID, enrich$gene.arr)
    env$enrichWeight <- enrichWeight
    env$enrichPvalue <- enrichPvalue
    env$enrichgeneArr <- enrichgeneArr
}

# Recursive similarity calculation function
computeTermSig <- function(interestGenes, level, DOID, parents, childrenToTest, test, traditional, delta, penalize) {

    genes <- env$enrichgeneArr[[DOID]]
    weights <- env$enrichWeight[[DOID]]
    p <- Test(test, interestGenes, genes, weights)
    if (p == 0) {
        p <- .Machine$double.xmin
    }

    assign(DOID, p, envir = env$enrichPvalue)  #update P value

    if (length(childrenToTest) == 0)
        return(0)

    # Only significant nodes are calculated
    if (p >= delta)
        return(0)

    sigRatios <- map_dbl(childrenToTest, function(c.DOID) {
        return(log(env$enrichPvalue[[c.DOID]])/log(p))
    })
    names(sigRatios) <- childrenToTest

    if (penalize == TRUE) {
        penal <- map_dbl(childrenToTest, function(c.DOID) {
            penall <- max(1/10 * log10(.Machine$double.xmin)/(log10(env$enrichPvalue[[c.DOID]]) + log10(p)), 1)
            return(penall)
        })
    } else {
        penal <- map_dbl(childrenToTest, function(c.DOID) {
            return(1)
        })
    }

    names(penal) <- childrenToTest
    isigRatios <- sigRatios[sigRatios > 1]  #child significant
    nsigRatios <- sigRatios[sigRatios <= 1]  #parent significant

    Penal_sigR <- data.frame(ch.DOID = childrenToTest, ch.penal = penal, ch.sigRatio = sigRatios)

    if (length(isigRatios) == 0) {
        ## CASE1.None of the children are as significant as the parent

        pwalk(Penal_sigR, function(ch.DOID, ch.penal, ch.sigRatio) {

            ch.weight <- env$enrichWeight[[ch.DOID]]
            ch.genes <- env$enrichgeneArr[[ch.DOID]]

            ch.weight <- ch.weight * ch.sigRatio/ch.penal

            ch.p <- Test(test, interestGenes, ch.genes, ch.weight)

            assign(ch.DOID, ch.p, envir = env$enrichPvalue)
            assign(ch.DOID, ch.weight, envir = env$enrichWeight)

        })


    } else {
        # CASE2.children node are significant than parent.

        Penal_sigR <- filter(Penal_sigR, sigRatios > 1)

        pwalk(Penal_sigR, function(ch.DOID, ch.penal, ch.sigRatio) {

            ch.weight <- env$enrichWeight[[ch.DOID]]
            ch.genes <- env$enrichgeneArr[[ch.DOID]]

            same.genes <- intersect(ch.genes, genes)
            weights[same.genes] <- weights[same.genes]/(ch.sigRatio * ch.penal)

            assign(DOID, weights, envir = env$enrichWeight)

            ancestors <- getAncestors(DOID)

            doidCount <- env$doidCount
            ancestors <- intersect(ancestors, doidCount)

            walk(ancestors, function(anc.DOID) {

                anc.genes <- env$enrichgeneArr[[anc.DOID]]
                anc.weight <- env$enrichWeight[[anc.DOID]]

                same.genes <- intersect(ch.genes, anc.genes)
                anc.weight[same.genes] <- anc.weight[same.genes]/(ch.sigRatio * ch.penal)

                assign(anc.DOID, anc.weight, envir = env$enrichWeight)
            })
        })

        computeTermSig(interestGenes, level, DOID, parents, names(nsigRatios), test, traditional, delta, penalize)
    }
    return(0)
}

# Statistical model
Test <- function(test, interestGenes, genes, weights) {

    a <- intersect(interestGenes, genes)
    b <- intersect(setdiff(dotermgenes, interestGenes), genes)
    c <- intersect(interestGenes, setdiff(dotermgenes, genes))
    d <- intersect(setdiff(dotermgenes, interestGenes), setdiff(dotermgenes, genes))

    wa <- floor(sum(weights[a]))
    wb <- floor(sum(weights[b]))
    wc <- length(c)
    wd <- length(d)

    switch(test, fisherTest = fisherTest(wa, wb, wc, wd), hypergeomTest = hypergeomTest(wa, wb, wc, wd), binomTest = binomTest(wa,
        wb, wc), chisqTest = chisqTest(wa, wb, wc, wd), logoddTest = logoddTest(wa, wb, wc, wd))
}

### fisher
fisherTest <- function(wa, wb, wc, wd) {

    tableR <- matrix(c(wa, wb, wc, wd), nrow = 2, ncol = 2, byrow = TRUE)
    p <- fisher.test(tableR, alternative = "greater")$p.value
    return(p)
}
### hypergeom
hypergeomTest <- function(wa, wb, wc, wd) {

    tableR <- matrix(c(wa, wb, wc, wd), nrow = 2, ncol = 2, byrow = TRUE)
    p <- phyper(wa - 1, wa + wb, wc + wd, wa + wc, lower.tail = FALSE)
    return(p)
}
### binom
binomTest <- function(wa, wb, wc) {

    q <- wa - 1
    size <- wa + wc
    prob <- (wa + wb)/length(dotermgenes)
    p <- pbinom(q, size, prob, lower.tail = FALSE, log.p = FALSE)
    return(p)
}
### chisq
chisqTest <- function(wa, wb, wc, wd) {

    tableR <- matrix(c(wa, wb, wc, wd), nrow = 2, ncol = 2, byrow = TRUE)
    p <- chisq.test(tableR)
    p <- p[["p.value"]]
    if (is.nan(p)) {
        p <- 1
    }
    return(p)
}
### logodd
logoddTest <- function(wa, wb, wc, wd) {
    pval <- 1
    tryCatch({
        p <- prop.test(c(wa, wc), c(wa + wb, wc + wd))
        pval <<- p[["p.value"]]
    }, error = function(e) {
        pval <<- 1
    })
    return(pval)
}

# get ancestors
getAncestors <- function(DOID, trace = FALSE) {

    ancestors <- c()
    if (DOID == "DOID:4")
        return(ancestors)  #root

    parents <- doterms[doterms$DOID == DOID, ]$parent.arr[[1]]
    if (length(parents) == 0)
        return(ancestors)
    ancestors <- append(ancestors, parents)
    walk(parents, function(p) {
        p.ancestors <- getAncestors(p)
        ancestors <<- append(ancestors, p.ancestors)
    })
    ancestors <- unique(ancestors)
    # debug
    if (trace)
        str(ancestors)
    return(ancestors)
}






