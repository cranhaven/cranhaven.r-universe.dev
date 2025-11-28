PlotBest.hclust <- function(dist, clust=c(
 "ward.D",
 "ward.D2",
 "single",
 "complete",
 "average",
 "mcquitty",
 "median",
 "centroid"
), plot=TRUE){
res <-  structure(numeric(length(clust)), names=clust)
.co.test <- function(hclust, dist, method="spearman") cor.test(cophenetic(hclust), dist, method=method)
for(i in seq_along(clust)) res[i] <- suppressWarnings(.co.test(hclust(dist, method=clust[i]), dist)$estimate)
if (plot) Dotchart(sort(res))
invisible(res)
}

# ===

PlotBest.dist <- function(data, distances=c(
 "euclidean",
 "maximum",
 "manhattan",
 "canberra",
 "binary",
 "minkowski"
)) {
if (any(data < 0)) distances <- setdiff(distances, "canberra") # because canberra wants positive values
if (!any(data == 0)) distances <- setdiff(distances, "binary") # because binary wants zero and non-zero
res <-  structure(numeric(length(distances)), names=distances)
for (i in seq_along(distances)) {
 ddist <- dist(data, method=distances[i])
 ddist[is.na(ddist)] <- 0 # cmdscale() does not work with NA
 ddc <- cor(cmdscale(ddist, k=2, add=TRUE)$points, prcomp(data)$x[, 1:2]) # cmdscale() converts to Euclidean with 'add=TRUE'
 res[i] <- mean(apply(abs(ddc), 2, max)) # chooses the best correlation because axes are frequently swapped
}
Dotchart(sort(res))
}

## ===

PlotBest.mdist <- function(data, distances=c(
 "manhattan",
 "euclidean",
 "canberra",
 "clark",
 "bray",
 "kulczynski",
 "jaccard",
 "gower",
 "altGower",
 "morisita",
 "horn",
 "binomial",
 "chao",
 "cao",
 "mahalanobis",
 "cor.pearson",
 "cor.spearman",
 "cor.kendall",
 "gower_dist",
 "simple_match_dist",
 "daisy.gower",
 "smirnov"
), binary.only=FALSE) {
notveg <- c( "gower_dist", "simple_match_dist", "cor.pearson", "cor.spearman", "cor.kendall", "daisy.gower", "smirnov")
if (binary.only) distances <- setdiff(distances, c("morisita", "cor.pearson"))
if (any(data < 0, na.rm=TRUE)) distances <- setdiff(distances, "canberra") # canberra wants positive values only
if (any(!is.integer(data), na.rm=TRUE)) distances <- setdiff(distances, c("cao", "chao", "morisita")) # they want integer mode data
if (any(!data %in% c(0, 1))) distances <- setdiff(distances, c("smirnov")) # wants 0/1 occurrence only
if (ncol(data) > nrow(data)) distances <- setdiff(distances, c("mahalanobis")) # mahalanobis transformation fails if there are too many cols
res <-  structure(numeric(length(distances)), names=distances)
for (i in seq_along(distances)) {
 mdist <- distances[i]
 cat(mdist, "\n")
 if (!binary.only && !mdist %in% notveg) ddist <- vegan::vegdist(data, method=mdist)
 if (binary.only && !mdist %in% notveg) ddist <- vegan::vegdist(data, method=mdist, binary=TRUE)
 if (grepl("^cor\\.", mdist)) ddist <- as.dist(1 - abs(cor(t(data), method=sub("^cor\\.", "", mdist), use="pairwise.complete.obs")))
 if (mdist == "gower_dist") ddist <- Gower.dist(data)
 if (mdist == "simple_match_dist") ddist <- SM.dist(data)
 if (mdist == "daisy.gower") ddist <- cluster::daisy(data, metric="gower")
 if (mdist == "smirnov") ddist <- as.dist(1 - smirnov::smirnov(data))
 ddist[is.na(ddist)] <- 0 # cmdscale() does not work with NA
 ddc <- cor(cmdscale(ddist, k=2, add=TRUE)$points, prcomp(data)$x[, 1:2])
 res[i] <- mean(apply(abs(ddc), 2, max))
}
Dotchart(sort(res))
}
