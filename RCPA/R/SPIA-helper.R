#' @title Get KEGG pathway network for SPIA method
#' @description Get KEGG pathway network for SPIA method
#' @param org The organism abbreviation. E.g, hsa, mmu, dme, etc.
#' To see the full list of supported organisms, visit https://www.genome.jp/kegg/catalog/org_list.html.
#' @param updateCache A parameter to disable/enable cache update.
#' @return A named list with three elements: network, names and sizes.
#' @examples
#' \donttest{
#' spiaNetwork <- getSPIAKEGGNetwork("hsa")
#' }
#' @export
#' @importFrom graph nodes edges
#' @importFrom dplyr %>% filter select group_by group_split
#' @importFrom tidyr spread
#' @importFrom stringr str_split str_length str_replace
getSPIAKEGGNetwork <- function(org = "hsa", updateCache = FALSE) {

    if (!.requirePackage("ROntoTools")){
        return(NULL)
    }

    keggPathway <- ROntoTools::keggPathwayGraphs(org, relPercThresh = 0, updateCache = updateCache)
    
    for (i in seq_along(keggPathway)) {
      nodeNames <- graph::nodes(keggPathway[[i]])
      graph::nodes(keggPathway[[i]]) <- stringr::str_replace(nodeNames, paste0(org,":"), "")
    }
    
    keggPathwayNames <- ROntoTools::keggPathwayNames(org)
    
    pathways_size <- keggPathway %>% lapply(function (path) length(graph::nodes(path))) %>% unlist() %>% as.vector()
    names(pathways_size) <- names(keggPathway)
    
    # return(keggPathway)
    
    list(
        network = keggPathway,
        names = .getKEGGPathwayNames(org)[names(keggPathway)],
        sizes = pathways_size
    )


    # relationships <- c("activation", "compound", "binding/association",
    #                    "expression", "inhibition", "activation_phosphorylation",
    #                    "phosphorylation", "inhibition_phosphorylation",
    #                    "inhibition_dephosphorylation", "dissociation", "dephosphorylation",
    #                    "activation_dephosphorylation", "state change", "activation_indirect effect",
    #                    "inhibition_ubiquination", "ubiquination", "expression_indirect effect",
    #                    "inhibition_indirect effect", "repression", "dissociation_phosphorylation",
    #                    "indirect effect_phosphorylation", "activation_binding/association",
    #                    "indirect effect", "activation_compound", "activation_ubiquination")
    # 
    # replacements <- list(
    #     c('ubiquitination', 'ubiquination'),
    #     c(',missing interaction', ''),
    #     c('missing interaction', ''),
    #     c('compound,activation', 'activation,compound')
    # )
    # 
    # keggRels <- keggPathway %>%
    #     lapply(function(e) e@edgeData@data %>% lapply(function(e) {
    #         s <- e$subtype
    #         for (r in replacements) {
    #             s <- sub(r[1], r[2], s)
    #         }
    #         s
    #     })) %>%
    #     unlist() %>%
    #     unique() %>%
    #     sub(pattern = ",", replacement = "_") %>%
    #     strsplit(',') %>%
    #     unlist() %>%
    #     unique()
    # 
    # keggPathwayNames <- ROntoTools::keggPathwayNames(org)
    # 
    # pathInfo <- lapply(keggPathway, function(pathway) {
    #     nodes <- pathway@nodes
    #     edgeData <- pathway@edgeData@data
    # 
    #     rels <- lapply(keggRels, function(relationship) {
    #         dat <- matrix(0, nrow = length(nodes), ncol = length(nodes), dimnames = list(nodes, nodes))
    # 
    #         reactions <- edgeData %>%
    #             lapply(function(e) {
    #                 s <- e$subtype
    #                 for (r in replacements) {
    #                     s <- sub(r[1], r[2], s)
    #                 }
    #                 s <- sub(',', '_', s)
    #                 relationship %in% (strsplit(s, ",") %>% unlist())
    #             }) %>%
    #             unlist() %>%
    #             which() %>%
    #             names() %>%
    #             strsplit('\\|')
    # 
    #         for (r in reactions) {
    #             dat[r[2], r[1]] <- 1
    #         }
    # 
    #         return(dat)
    #     })
    #     names(rels) <- keggRels
    #     rels <- rels[relationships]
    #     rels$dissociation_phosphorylation <- matrix(0, nrow = length(nodes), ncol = length(nodes), dimnames = list(nodes, nodes))
    # 
    #     for (i in seq_along(rels)) {
    #         if (is.null(rels[[i]])) next()
    #         colnames(rels[[i]]) <- gsub("^.*:", "", colnames(rels[[i]]))
    #         rownames(rels[[i]]) <- gsub("^.*:", "", rownames(rels[[i]]))
    #     }
    # 
    #     rels$nodes <- gsub("^.*:", "", nodes)
    #     rels$NumberOfReactions <- 0
    # 
    #     return(rels)
    # })
    # 
    # for (pathwayId in names(pathInfo)) {
    #     pathInfo[[pathwayId]]$title <- keggPathwayNames[pathwayId] %>% as.character()
    # }
    # 
    # pathways_size <- pathInfo %>% lapply(function (path) length(path[["nodes"]])) %>% unlist() %>% as.vector()
    # names(pathways_size) <- names(pathInfo)
    # 
    # list(
    #     network = pathInfo,
    #     names = .getKEGGPathwayNames(org)[names(pathInfo)],
    #     sizes = pathways_size
    # )
}

#' @title SPIA combfunc method.
#' @description This function is combfunc from the original SPIA method.
#' @param p1 See SPIA function
#' @param p2 See SPIA function
#' @param combine See SPIA function
#' @return See SPIA function
#' @importFrom stats pnorm qnorm na.omit
#' @noRd
combfunc <- function (p1 = NULL, p2 = NULL, combine)
{
    tm = na.omit(c(p1, p2))
    if (!all(tm >= 0 & tm <= 1)) {
        stop("values of p1 and p2 have to be >=0 and <=1 or NAs")
    }
    if (combine == "fisher") {
        k = p1 * p2
        comb = k - k * log(k)
        comb[is.na(p1)] <- p2[is.na(p1)]
        comb[is.na(p2)] <- p1[is.na(p2)]
        return(comb)
    }
    if (combine == "norminv") {
        comb = pnorm((qnorm(p1) + qnorm(p2))/sqrt(2))
        comb[is.na(p1)] <- p2[is.na(p1)]
        comb[is.na(p2)] <- p1[is.na(p2)]
        return(comb)
    }
}

#' @title SPIA method modified with inputs are KEGG pathway networks instead of a folder.
#' @description This function is modified from the original SPIA method to accept KEGG pathway networks as inputs.
#' @param de See SPIA function
#' @param all See SPIA function
#' @param pathInfo pathway information generated by getSPIAKEGGNetwork function
#' @param nB See SPIA function
#' @param verbose See SPIA function
#' @param beta See SPIA function
#' @param combine See SPIA function
#' @return See SPIA function
#' @importFrom stats median
#' @noRd
.SPIAMod <- function(de = NULL, all = NULL, pathInfo, nB = 2000, verbose = TRUE, beta = NULL, combine = "fisher") {
    if (is.null(de) | is.null(all)) {
        stop("de and all arguments can not be NULL!")
    }

    rel <- c("activation", "compound", "binding/association",
             "expression", "inhibition", "activation_phosphorylation",
             "phosphorylation", "inhibition_phosphorylation", "inhibition_dephosphorylation",
             "dissociation", "dephosphorylation", "activation_dephosphorylation",
             "state change", "activation_indirect effect", "inhibition_ubiquination",
             "ubiquination", "expression_indirect effect", "inhibition_indirect effect",
             "repression", "dissociation_phosphorylation", "indirect effect_phosphorylation",
             "activation_binding/association", "indirect effect",
             "activation_compound", "activation_ubiquination")

    if (is.null(beta)) {
        beta = c(1, 0, 0, 1, -1, 1, 0, -1, -1, 0, 0, 1, 0, 1,
                 -1, 0, 1, -1, -1, 0, 0, 1, 0, 1, 1)
        names(beta) <- rel
    }else {
        if (!all(names(beta) %in% rel) | length(names(beta)) !=
            length(rel)) {
            stop(paste("beta must be a numeric vector of length",
                       length(rel), "with the following names:", "\n",
                       paste(rel, collapse = ",")))
        }
    }

    datpT <- pathInfo

    datp <- list()
    path.names <- NULL
    hasR <- NULL
    for (jj in 1:length(datpT)) {
        sizem <- dim(datpT[[jj]]$activation)[1]
        s <- 0
        con <- 0
        for (bb in 1:length(rel)) {
            if(is.null(datpT[[jj]][[rel[bb]]])) next()
            con = con + datpT[[jj]][[rel[bb]]] * abs(sign(beta[rel[bb]]))
            s = s + datpT[[jj]][[rel[bb]]] * beta[rel[bb]]
        }
        z = matrix(rep(apply(con, 2, sum), dim(con)[1]), dim(con)[1],
                   dim(con)[1], byrow = TRUE)
        z[z == 0] <- 1
        datp[[jj]] <- s / z
        path.names <- c(path.names, datpT[[jj]]$title)
        hasR <- c(hasR, datpT[[jj]]$NumberOfReactions >= 1)
    }
    names(datp) <- names(datpT)
    names(path.names) <- names(datpT)
    tor <- lapply(datp, function(d) {
        sum(abs(d))
    }) == 0 |
        hasR |
        is.na(path.names)
    datp <- datp[!tor]
    path.names <- path.names[!tor]
    IDsNotP <- names(de)[!names(de) %in% all]
    if (length(IDsNotP) / length(de) > 0.01) {
        stop("More than 1% of your de genes have IDs are not present in the reference array!. Are you sure you use the right reference array?")
    }
    if (!length(IDsNotP) == 0) {
        message("The following IDs are missing from all vector...:")
        message(paste(IDsNotP, collapse = ","))
        message("They were added to your universe...")
        all <- c(all, IDsNotP)
    }
    if (length(intersect(names(de), all)) != length(de)) {
        stop("de must be a vector of log2 fold changes. The names of de should be included in the refference array!")
    }
    ph <- pb <- pcomb <- nGP <- pSize <- smPFS <- tA <- tAraw <- KEGGLINK <- NULL

    for (i in 1:length(names(datp))) {
        path <- names(datp)[i]
        M <- datp[[path]]
        diag(M) <- diag(M) - 1
        X <- de[rownames(M)]
        noMy <- sum(!is.na(X))
        nGP[i] <- noMy
        okg <- intersect(rownames(M), all)
        ok <- rownames(M) %in% all
        pSize[i] <- length(okg)
        if ((noMy) > 0 & (abs(det(M)) > 1e-07)) {
            gnns <- paste(names(X)[!is.na(X)], collapse = "+")
            X[is.na(X)] <- 0
            pfs <- solve(M, -X)
            smPFS[i] <- sum(pfs - X)
            tAraw[i] <- smPFS[i]

            ph[i] <- phyper(q = noMy - 1, m = pSize[i], n = length(all) -
                pSize[i], k = length(de), lower.tail = FALSE)
            pfstmp <- NULL
            for (k in 1:nB) {
                x <- rep(0, length(X))
                names(x) <- rownames(M)
                x[ok][sample(1:sum(ok), noMy)] <- as.vector(sample(de,
                                                                   noMy))
                tt <- solve(M, -x)
                pfstmp <- c(pfstmp, sum(tt - x))
            }
            mnn <- median(pfstmp)
            pfstmp <- pfstmp - mnn
            ob <- smPFS[i] - mnn
            tA[i] <- ob
            if (ob > 0) {
                pb[i] <- sum(pfstmp >= ob) / length(pfstmp) *
                    2
                if (pb[i] <= 0) {
                    pb[i] <- 1 / nB / 100
                }
                if (pb[i] > 1) {
                    pb[i] <- 1
                }
            }
            if (ob < 0) {
                pb[i] <- sum(pfstmp <= ob) / length(pfstmp) *
                    2
                if (pb[i] <= 0) {
                    pb[i] <- 1 / nB / 100
                }
                if (pb[i] > 1) {
                    pb[i] <- 1
                }
            }
            if (ob == 0) {
                if (all(pfstmp == 0)) {
                    pb[i] <- NA
                }
                else {
                    pb[i] <- 1
                }
            }

            pcomb[i] <- combfunc(pb[i], ph[i], combine)
        } else {
            pb[i] <- ph[i] <- smPFS[i] <- pcomb[i] <- tAraw[i] <- tA[i] <- NA
        }
        if (verbose) {
            message("\n")
            message(paste("Done pathway ", i, " : ", substr(path.names[names(datp)[i]], 1, 30), "..", sep = ""))
        }
    }


    pcombFDR = p.adjust(pcomb, "fdr")
    phFdr = p.adjust(ph, "fdr")
    pcombfwer = p.adjust(pcomb, "bonferroni")
    Name = path.names[names(datp)]
    Status = ifelse(tA > 0, "Activated", "Inhibited")
    res <- data.frame(Name, ID = names(datp), pSize, NDE = nGP,
                      pNDE = ph, tA, pPERT = pb, pG = pcomb, pGFdr = pcombFDR,
                      pGFWER = pcombfwer, Status, stringsAsFactors = FALSE)
    res <- res[!is.na(res$pNDE),]
    res <- res[order(res$pG),]
    rownames(res) <- NULL
    res
}
