# sample session to exemplify the use of EnrichDO

library(EnrichDO)

EnrichDO_example <- function(path = NULL) {
    if (is.null(path)) {
        dir(system.file("extdata", package = "EnrichDO"))
    } else {
        system.file("extdata", path, package = "EnrichDO", mustWork = TRUE)
    }
}
# View all the enrichment input data involved in the literature under the inst/extdata folder
EnrichDO_example()

# Take the curated genes for Alzheimer's as input
Alzheimer <- read.csv(EnrichDO_example("Alzheimer_curated.csv"), header = F)
Alzheimer_gene <- Alzheimer$V1

# Enrichment Analysis
Alzheimer_EnrichDO <- doEnrich(interestGenes = Alzheimer_gene, test = "hypergeomTest", method = "BH", m = 1, maxGsize = 5000,
    minGsize = 5, traditional = FALSE, delta = 0.01, penalize = T)

# Enrichment result visualization
writeDoTerms(doterms, file = file.path(system.file("examples", package = "EnrichDO"), "doterms.txt"))
writeResult(EnrichResult = Alzheimer_EnrichDO, file = file.path(system.file("examples", package = "EnrichDO"), "result.txt"),
    Q = 1, P = 1)

pdf(file.path(system.file("examples", package = "EnrichDO"), "BarGraph.pdf"))
drawBarGraph(EnrichResult = Alzheimer_EnrichDO, n = 10, delta = 0.05)
dev.off()

pdf(file.path(system.file("examples", package = "EnrichDO"), "PointGraph.pdf"))
drawPointGraph(EnrichResult = Alzheimer_EnrichDO, n = 10, delta = 0.05)
dev.off()

pdf(file.path(system.file("examples", package = "EnrichDO"), "TreeGraph.pdf"))
drawGraphViz(EnrichResult = Alzheimer_EnrichDO, n = 10, numview = FALSE, pview = FALSE, labelfontsize = 17)
dev.off()

pdf(file.path(system.file("examples", package = "EnrichDO"), "HeatmapGraph.pdf"))
drawHeatmap(interestGenes = Alzheimer_gene, EnrichResult = Alzheimer_EnrichDO, gene_n = 10, fontsize_row = 8, readable = T)
dev.off()
