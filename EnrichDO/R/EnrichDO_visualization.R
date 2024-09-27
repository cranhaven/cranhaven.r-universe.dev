#'@title drawBarGraph
#'@description The enrichment results are shown in a bar chart
#'@param EnrichResult the EnrichResult object
#'@param enrich a data frame of enrichment result
#'@param n number of bars
#'@param delta the threshold of P value
#'@author Haixiu Yang
#'@return bar graph
#'@importFrom magrittr `%>%`
#'@importFrom dplyr mutate filter select
#'@importFrom stats na.omit reorder
#'@import ggplot2
#'@export
#'@examples
#'demo.data <- c(1636,351,102,2932,3077,348,4137,54209)
#'sample1 <- doEnrich(interestGenes=demo.data,maxGsize = 100, minGsize=10)
#'drawBarGraph(EnrichResult=sample1, n=10, delta=0.05)

drawBarGraph <- function(EnrichResult = NULL, enrich = NULL, n = 10, delta = 1e-15) {

    if (!is.null(EnrichResult) & is.null(enrich)) {
        enrich <- EnrichResult@enrich
    } else if (is.null(EnrichResult) & !is.null(enrich)) {
        enrich <- as.data.frame(enrich)
    } else {
        warning("Do not assign EnrichResult and enrich simultaneously")
    }
    data <- enrich %>%
        filter(p <= delta) %>%
        select(DOID, DOTerm, p, cg.len, ig.len) %>%
        mutate(log10p = -log10(p), DO = str_c(DOID, DOTerm, sep = "  ")) %>%
        mutate(geneRatio = as.numeric(cg.len)/as.numeric(ig.len))
    data <- data[1:n, ]
    data <- na.omit(data)
    if (dim(data)[1] < n) {
        message(str_c("The threshold delta is too low, only ", dim(data)[1], " nodes are less than the threshold", "\n"))
    }

    ggplot(data, aes(x = reorder(DO, log10p), y = geneRatio, fill = log10p)) + geom_bar(stat = "identity") + coord_flip() +
        scale_x_discrete(position = "top") + scale_fill_gradient(low = "orange", high = "red")

}
#'@title drawPointGraph
#'@description The enrichment results are shown in a scatter plot
#'@param EnrichResult the EnrichResult object
#'@param enrich a data frame of enrichment result.
#'@param n number of points.
#'@param delta the threshold of P value.
#'@author Haixiu Yang
#'@return scatter graph
#'@importFrom dplyr mutate filter select
#'@importFrom stats na.omit reorder
#'@importFrom ggplot2 ggplot
#'@importFrom magrittr `%>%`
#'@export
#'@examples
#'demo.data <- c(1636,351,102,2932,3077,348,4137,54209)
#'sample2 <- doEnrich(interestGenes=demo.data,maxGsize = 100, minGsize=10)
#'drawPointGraph(EnrichResult=sample2, n=10, delta=0.05)
drawPointGraph <- function(EnrichResult = NULL, enrich = NULL, n = 10, delta = 1e-15) {

    if (!is.null(EnrichResult) & is.null(enrich)) {
        enrich <- EnrichResult@enrich
    } else if (is.null(EnrichResult) & !is.null(enrich)) {
        enrich <- as.data.frame(enrich)
    } else {
        warning("Do not assign EnrichResult and enrich simultaneously")
    }

    data <- enrich %>%
        filter(p <= delta) %>%
        select(DOID, DOTerm, p, p.adjust, cg.len, gene.len, ig.len) %>%
        mutate(log10p = -log10(p), DO = str_c(DOID, DOTerm, sep = "  ")) %>%
        mutate(geneRatio = as.numeric(cg.len)/as.numeric(ig.len))
    data <- data[1:n, ]
    data <- na.omit(data)
    if (dim(data)[1] < n) {
        message(str_c("The threshold delta is too low, only ", dim(data)[1], " nodes are less than the threshold", "\n"))
    }

    ggplot(data) + geom_point(mapping = aes(x = geneRatio, y = reorder(DO, log10p), size = as.numeric(cg.len), color = log10p)) +
        scale_y_discrete(position = "right") + scale_color_gradient(low = "blue", high = "red")
}
#'@title writeDoTerms
#'@description Output DOterms as text
#'@param doterms a data frame of do terms.
#'@param file the address and name of the output file.
#'@author Haixiu Yang
#'@return text
#'@importFrom dplyr mutate select
#'@importFrom readr write_delim
#'@importFrom magrittr `%>%`
#'@export
#'@examples
#'writeDoTerms(doterms, file=file.path(tempdir(), 'doterms.txt'))
writeDoTerms <- function(doterms = doterms, file) {
    doterms <- as.data.frame(doterms)
    data <- doterms %>%
        mutate(genes = map_chr(gene.arr, str_c, collapse = ",")) %>%
        mutate(parents = map_chr(parent.arr, str_c, collapse = ",")) %>%
        mutate(children = map_chr(child.arr, str_c, collapse = ",")) %>%
        select(DOID, DOTerm, level, genes, parents, children, gene.len, parent.len, child.len)
    write_delim(data, file = file, delim = "\t", eol = "\n", quote = "none")
}

#'@title showDoTerms
#'@description show DOterms
#'@param doterms a data frame of DOterms.
#'@author Haixiu Yang
#'@return text
#'@export
#'@examples
#'showDoTerms(doterms)
showDoTerms <- function(doterms = doterms) {
    doterms <- as.data.frame(doterms)
    message("\n-------------annotation information for DO terms---------------\n")
    message("There are ", dim(doterms)[1], " DOTerms with ", dim(doterms)[2], " col")
    message("  DOID: DOterm ID on enrichment")
    message("  level: the hierarchy of DOterm in the DAG graph")
    message("  gene.arr: all genes related to the DOterm")
    message("  weight.arr: gene weights in each node")
    message("  parent.arr: the parent node of the DOterm")
    message("  child.arr: child nodes of the DOterm")
    message("  DOTerm: the standard name of DOterm")
    message("  .len: represents the corresponding quantity")

}


#'@title writeResult
#'@description Output enrichment result as text
#'@param EnrichResult the EnrichResult object
#'@param file the address and name of the output file.
#'@param P Output only doterm information with p values less than or equal to P.
#'@param Q Output only doterm information with p.adjust values less than or equal to Q.
#'@author Haixiu Yang
#'@return text
#'@importFrom dplyr mutate  select arrange
#'@importFrom readr write_delim
#'@importFrom magrittr `%>%`
#'@export
#'@examples
#'demo.data <- c(1636,351,102,2932,3077,348,4137,54209)
#'sample4 <- doEnrich(interestGenes=demo.data,maxGsize = 100, minGsize=10)
#'writeResult(EnrichResult=sample4, file=file.path(tempdir(), 'result.txt'))
writeResult <- function(EnrichResult = NULL, file, Q = 1, P = 1) {

    enrich <- as.data.frame(EnrichResult@enrich)
    data <- enrich %>%
        mutate(cg = map_chr(cg.arr, str_c, collapse = ",")) %>%
        mutate(geneRatio = paste0(cg.len, "/", ig.len)) %>%
        mutate(bgRatio = paste0(gene.len, "/", length(dotermgenes))) %>%
        select(DOID, DOTerm, p, p.adjust, geneRatio, bgRatio, cg) %>%
        arrange(p)
    data <- dplyr::filter(data, p <= P, p.adjust <= Q)
    write_delim(data, file = file, delim = "\t", eol = "\n", quote = "none")
}
#'@title drawGraphViz
#'@description the enrichment results are shown in a tree diagram
#'@param EnrichResult the EnrichResult object
#'@param enrich a data frame of the enrichment result
#'@param n the number of most significant nodes
#'@param labelfontsize the font size of nodes
#'@param numview Displays the number of intersections between the interest set and each doterm.
#'@param pview Displays the P value for each dotrem.
#'@author Haixiu Yang
#'@return tree diagram
#'@import purrr
#'@importFrom dplyr mutate filter select arrange
#'@import Rgraphviz
#'@import graph
#'@importFrom BiocGenerics unique
#'@importFrom magrittr `%>%`
#'@importFrom methods new
#'@importFrom tidyr unnest
#'@importFrom grDevices heat.colors
#'@importFrom RColorBrewer brewer.pal
#'@importFrom dplyr filter
#'@importFrom graphics text
#'@export
#'@examples
#'demo.data <- c(1636,351,102,2932,3077,348,4137,54209)
#'sample5 <- doEnrich(interestGenes=demo.data,maxGsize = 100, minGsize=10)
#'drawGraphViz(EnrichResult =sample5)
#'
#'#The p-value and the number of intersections are not visible
#'drawGraphViz(EnrichResult=sample5, numview = FALSE, pview = FALSE)
drawGraphViz <- function(EnrichResult = NULL, enrich = NULL, n = 10, labelfontsize = 14, numview = TRUE, pview = TRUE) {

    if (!is.null(EnrichResult) & is.null(enrich)) {
        enrich0 <- EnrichResult@enrich
    } else if (is.null(EnrichResult) & !is.null(enrich)) {
        enrich0 <- as.data.frame(enrich)
    } else {
        warning("Do not assign EnrichResult and enrich simultaneously")
    }
    enrich0 <- enrich0 %>%
        arrange(p)



    data <- enrich0[1:n, ]
    nodes <- c()
    walk(data$DOID, function(DOID) {
        nodes <<- append(nodes, DOID)
        ancestors <- getAncestors(DOID, trace = TRUE)
        nodes <<- append(nodes, ancestors)
    })
    nodes <- unique(nodes)
    rEG <- new("graphNEL", nodes = nodes, edgemode = "directed")

    # Add edge
    data.extends <- enrich0 %>%
        filter(DOID %in% nodes) %>%
        select(DOID, DOTerm, p, c(parent.arr), cg.len)
    if (dim(data.extends)[1] <= length(nodes)) {
        diff_nodes <- setdiff(nodes, data.extends$DOID)
        diff_doterm <- filter(doterms, DOID %in% diff_nodes)
        diff_enrich <- diff_doterm %>%
            select("DOID", "DOTerm", "parent.arr") %>%
            mutate(p = 1) %>%
            mutate(cg.len = 0) %>%
            select("DOID", "DOTerm", "p", "parent.arr", "cg.len")
        data.extends <- rbind(data.extends, diff_enrich)

    }

    edges <- data.extends %>%
        unnest(cols = parent.arr)
    pwalk(edges, function(DOID, parent.arr, ...) {
        rEG <<- addEdge(from = parent.arr, to = DOID, graph = rEG, weights = 1)
    })

    nAttrs <- list()
    labels <- data.extends$DOTerm
    names(labels) <- data.extends$DOID
    nAttrs$label <- labels

    pvalue <- data.extends$p
    names(pvalue) <- data.extends$DOID
    nAttrs$pvalue <- pvalue

    cglen <- data.extends$cg.len
    names(cglen) <- data.extends$DOID
    nAttrs$cglen <- cglen

    labelfontsize <- rep(labelfontsize, length(pvalue))
    names(labelfontsize) <- data.extends$DOID
    nAttrs$fontsize <- labelfontsize

    colors <- heat.colors(9)
    colors <- colors[9:1]

    data.extends <- enrich0 %>%
        filter(DOID %in% nodes(rEG)) %>%
        arrange(p) %>%
        mutate(log10p = -log10(p)) %>%
        mutate(fillcolor = as.character(cut(log10p, breaks = 9, label = colors)))

    fills <- data.extends$fillcolor
    names(fills) <- data.extends$DOID
    nAttrs$fillcolor <- fills

    # Fix Size
    fixedSize <- rep(FALSE, length(nodes(rEG)))
    names(fixedSize) <- nodes(rEG)
    nAttrs$fixedSize <- fixedSize

    shapes <- map(nodes(rEG), function(DOID) {
        if (DOID %in% data$DOID)
            return("rectangle") else return("ellipse")
    })
    names(shapes) <- nodes(rEG)
    nAttrs$shape <- shapes

    g1layout <- agopen(rEG, name = "foo", nodeAttrs = nAttrs, attrs = list(graph = list(rankdir = "TB"), node = list(fixedsize = FALSE)),
        )
    Rgraphviz::plot(g1layout)
    if (pview == TRUE) {
        for (i in 1:length(g1layout@AgNode)) {
            pval <- nAttrs[["pvalue"]][[g1layout@AgNode[[i]]@name]]
            if (pval == 1) {
                pval <- ""
            }
            if (pval != 1) {
                pval <- format(pval, digit = 5, scientific = TRUE)
            }
            text(getX(getNodeCenter(g1layout@AgNode[[i]])), getY(getNodeCenter(g1layout@AgNode[[i]])), labels = pval, pos = 1,
                col = "black", cex = 0.5)
        }
    }
    if (numview == TRUE) {
        for (i in 1:length(g1layout@AgNode)) {
            num <- nAttrs[["cglen"]][[g1layout@AgNode[[i]]@name]]
            if (num == 0) {
                num <- ""
            }
            text(getX(getNodeCenter(g1layout@AgNode[[i]])), getY(getNodeCenter(g1layout@AgNode[[i]])), labels = num, pos = 3,
                col = "dark blue", cex = 0.5)
        }
    }

}

#'@title drawHeatmap
#'@description The top DOID_n nodes in the enrichment results showed the top gene_n genes with the highest weight sum.
#'@param EnrichResult the EnrichResult object
#'@param interestGenes A collection of interest genes in vector form
#'@param DOID_n There are DOID_n nodes with the highest significance in the enrichment results.
#'@param gene_n Among the selected DOID_n nodes, the top gene_n genes with the highest weight sum are selected to show.
#'@param fontsize_row Set the font size of the gene tag.
#'@param readable Logical value that controls whether the gene tag is in symbol format
#'@param ... Other parameters in the pheatmap function also apply.
#'@author Haixiu Yang
#'@return heat map
#'@import pheatmap
#'@importFrom BiocGenerics unique
#'@importFrom purrr map2
#'@importFrom BiocGenerics intersect setdiff
#'@importFrom grDevices colorRampPalette
#'@importFrom clusterProfiler bitr
#'@export
#'@examples
#'demo.data <- c(1636,351,102,2932,3077,348,4137,54209)
#'sample6 <- doEnrich(interestGenes=demo.data,maxGsize = 100, minGsize=10)
#'drawHeatmap(interestGenes=demo.data, EnrichResult = sample6, gene_n = 10)

drawHeatmap <- function(interestGenes, EnrichResult = NULL, DOID_n = 10, gene_n = 50, fontsize_row = 10, readable = TRUE,
    ...) {

    enrich <- as.data.frame(EnrichResult@enrich)
    n <- DOID_n
    m <- gene_n
    if (m <= 1 | n <= 1) {
        stop("DOID_n and gene_n can not less than 2")
    }

    # data prepare
    data <- enrich[1:n, c("DOID", "gene.w")]

    allnodeGene <- unique(names(unlist(data$gene.w)))
    interestgenes <- intersect(interestGenes, allnodeGene)
    diffGene <- setdiff(interestGenes, interestgenes)
    m <- min(gene_n, length(interestgenes))

    if (length(diffGene) != 0) {
        note <- paste0("\033[31m", "The following genes you input do not exist in the top DOID_n nodes:", "\n", paste0(diffGene,
            collapse = " "), "\n", "\033[39m")
        message(note)
    }

    weightMatrix <- matrix(0, nrow = n + 1, ncol = length(interestgenes), dimnames = list(c(data$DOID, "colSum"), interestgenes))
    env$weightMatrix <- weightMatrix


    # DOID-gene weight matrix
    map2(data$gene.w, data$DOID, function(w, id) {
        weightMatrix <- env$weightMatrix
        gene <- w[intersect(interestgenes, names(w))]
        weightMatrix[id, names(gene)] <- as.numeric(gene)
        env$weightMatrix <- weightMatrix
    })
    weightMatrix <- env$weightMatrix
    weightMatrix[n + 1, ] <- colSums(weightMatrix)
    weightmatrix <- t(weightMatrix[-(n + 1), names(sort(weightMatrix[n + 1, ], decreasing = TRUE)[1:m])])

    if (readable == TRUE) {
        # gene entrez ID convert to symbol
        entrez <- row.names(weightmatrix)
        message("gene symbol conversion result: ")
        symbol <- clusterProfiler::bitr(entrez, fromType = "ENTREZID", toType = "SYMBOL", OrgDb = "org.Hs.eg.db")
        row.names(weightmatrix) <- symbol$SYMBOL

    }
    colors <- colorRampPalette(brewer.pal(11, "RdYlBu")[6:3])(10)
    pheatmap(weightmatrix, border_color = NA, cluster_cols = FALSE, color = colors, angle_col = 45, fontsize_row = fontsize_row,
        ...)

}


#'@title convDraw
#'@description using the result of writeResult for convenience drawing.
#'@param resultDO a data frame of enrichment result
#'@author Haixiu Yang
#'@return DataFrame
#'@export
#'@examples
#'#'#Draw from wrireResult output files
#'#Firstly, read the wrireResult output file,using the following two lines
#'data <- read.delim(file.path(system.file('examples', package = 'EnrichDO'), 'result.txt'))
#'enrich <- convDraw(resultDO = data)
#'#then, Use the drawing function you need
#'drawGraphViz(enrich=enrich)    #Tree diagram
#'drawPointGraph(enrich=enrich)  #Bubble diagram
#'drawBarGraph(enrich=enrich)    #Bar plot

convDraw <- function(resultDO) {
    TermStruct(resultDO = resultDO)
    message("Now you can use the drawing function")
    enrich <- env$enrich
    return(enrich)
}
