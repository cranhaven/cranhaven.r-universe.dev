## ----echo=FALSE---------------------------------------------------------------
knitr::opts_chunk$set(fig.width=7, fig.height=5) 

## ----eval=FALSE---------------------------------------------------------------
#  install.packages("spathial")

## ----eval=FALSE---------------------------------------------------------------
#  install.packages("devtools")
#  library(devtools)
#  devtools::install_github("erikagardini/spathial", build_vignettes=TRUE)

## -----------------------------------------------------------------------------
library("spathial")

## -----------------------------------------------------------------------------
# Load the dataset with 900 samples
myfile<-system.file("extdata", "2D_constellation.csv", package = "spathial")
data<-read.csv(myfile,as.is=TRUE,header=TRUE)
head(data)

## -----------------------------------------------------------------------------
# Vector X_labels
X_labels <- data$label
X_labels_num <- data$numLabels
# Data Matrix X
X <- data[,2:(ncol(data)-2)]
rownames(X)<-paste0("sam",rownames(X))

## -----------------------------------------------------------------------------
head(X)

## -----------------------------------------------------------------------------
head(X_labels)

## ----fig.cap="__Quick Start example dataset 2D.__ Each point is a sample of the dataset, colored according to its labels."----
# Plot the results
colors <- rainbow(length(table(as.numeric(as.factor(X_labels)))))
colors_labels <- sapply(as.numeric(as.factor(X_labels)), function(x){colors[x]})
oldpar <- par(no.readonly = TRUE)
par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
plot(X[,1],X[,2],col=colors_labels,pch=as.character(as.numeric(as.factor(X_labels))),
      xlab=colnames(X)[1],ylab=colnames(X)[2], main="Data points")
legend_names = unique(X_labels)
legend_color = unique(colors_labels)
legend_pch = unique(as.character(as.numeric(as.factor(X_labels))))
legend("topright", inset=c(-0.25,0), legend=legend_names, col=legend_color, pch=legend_pch)
par(oldpar)

## ----eval=FALSE---------------------------------------------------------------
#  # mode=1 (User-selected)
#  boundary_init <- spathial::spathialBoundaryIds(X, X_labels, mode=1)

## -----------------------------------------------------------------------------
# mode=2 (From named label centroid to another label centroid)
boundary_init <- spathial::spathialBoundaryIds(X, X_labels, mode=2, from="c3", to="c6")

## ----eval=FALSE---------------------------------------------------------------
#  # mode=3 (From named sample to another named sample)
#  boundary_init <- spathial::spathialBoundaryIds(X, X_labels, mode=3,
#                                                 from="sample123", to="sample456")

## -----------------------------------------------------------------------------
# Take the output from the variable boundary_init
boundary_ids<-boundary_init$boundary_ids
X<-boundary_init$X
X_labels<-boundary_init$X_labels

## ----fig.cap="__Quick Start example - boundaries.__ Each point is a sample of the 2D dataset, colored according to its labels. Boundaries are respectively the centroid of the samples labelled as *3* (start point) and the centroid of the samples labelled as *6* (end point)."----
#Plot the results
boundaries <- X[which(rownames(X) == boundary_ids[1] | rownames(X) == boundary_ids[2]),]
oldpar <- par(no.readonly = TRUE)
par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
plot(X[,1],X[,2], col=colors_labels, pch=as.character(as.numeric(as.factor(X_labels))), xlab=colnames(X)[1], ylab=colnames(X)[2], main="Boundary points")
points(boundaries[,1],boundaries[,2], pch="x",col="black",cex=4)
legend_names = c(unique(X_labels), "boundaries")
legend_color = c(unique(colors_labels), "black")
legend_pch = c(unique(as.character(as.numeric(as.factor(X_labels)))), "X")
legend("topright", inset=c(-0.3,0), legend=legend_names, col=legend_color, pch=legend_pch)
par(oldpar)

## -----------------------------------------------------------------------------
# Prefilter data
filter_res <- spathial::spathialPrefiltering(X, boundary_ids)
mask <- filter_res$mask
boundary_ids <- filter_res$boundary_ids

# Plot the results
boundaries <- X[which(rownames(X) == boundary_ids[1] | rownames(X) == boundary_ids[2]),]
X_garbage <- X[!mask,]

## ----fig.height=10, fig.cap="__Quick Start example - prefiltering.__ Each point is a sample of the 2D dataset, colored according to its labels. Boundaries are respectively the centroid of the samples labelled as *3* (start point) and the centroid of the samples labelled as *6* (end point). Prefiltered samples are marked in gray."----
oldpar <- par(no.readonly = TRUE)
par(mfrow=c(2,1))
par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
plot(X[,1],X[,2], col=colors_labels, pch=as.character(as.numeric(as.factor(X_labels))), xlab=colnames(X)[1], ylab=colnames(X)[2], main="Before Filtering")
points(boundaries[,1],boundaries[,2], pch="x",col="black",cex=4)
legend_names = c(unique(X_labels), "boundaries")
legend_color = c(unique(colors_labels), "black")
legend_pch = c(unique(as.character(as.numeric(as.factor(X_labels)))), "X")
legend("topright", inset=c(-0.3,0), legend=legend_names, col=legend_color, pch=legend_pch)

par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
plot(X[,1],X[,2], col=colors_labels, pch=as.character(as.numeric(as.factor(X_labels))), main="After Filtering", xlab=colnames(X)[1],ylab=colnames(X)[2])
points(boundaries[,1],boundaries[,2], pch="x",col="black",cex=4)
points(X_garbage[,1],X_garbage[,2], col="gray", pch=4)
legend_names = c(unique(X_labels), "boundaries", "filtered")
legend_color = c(unique(colors_labels), "black", "gray")
legend_pch = c(unique(as.character(as.numeric(as.factor(X_labels)))), "X", "x")
legend("topright", inset=c(-0.3,0), legend=legend_names, col=legend_color, pch=legend_pch)
par(oldpar)

## ----results='hide'-----------------------------------------------------------
# Compute principal path without prefiltering
NC <- 50
spathial_res_without_filtering <- spathial::spathialWay(X, boundary_ids, NC)

## ----results='hide'-----------------------------------------------------------
# Compute principal path after prefiltering
X_filtered <- X[mask,]
X_labels_filtered <- X_labels[mask]
NC <- 50
spathial_res_with_filtering <- spathial::spathialWay(X_filtered, boundary_ids, NC)

## ----fig.cap='__Output of the spathialPlot() function.__ Each point is a sample of the 2D dataset, colored according to its labels. Boundaries are respectively the centroid of the samples labelled as *3* (start point) and the centroid of the samples labelled as *6* (end point). The Principal Path is marked in blue. It moves from the start point to the end point passing through each waypoint (marked with "*"). Filtered samples are marked in grey.'----
# Plot principal path with prefiltering - provide a mask
spathial::spathialPlot(X, X_labels, boundary_ids, spathial_res_with_filtering,
                       perplexity_value=30, mask=mask,
                       title="Principal path with prefiltering"
)
# Plot principal path without prefiltering - mask NULL
spathial::spathialPlot(X, X_labels, boundary_ids, spathial_res_without_filtering,
                       perplexity_value=30,                       
                       title="Principal path without prefiltering"
)

## ----fig.height=10, fig.cap='__Quick Start example - path labels across path steps.__ For each waypoint of the Principal Path (computed from the centroid of the samples labelled as *3* (start point) to the centroid of the samples labelled as *6* (end point), the assigned label is the label of the nearest sample.'----
oldpar <- par(no.readonly = TRUE)
par(mfrow=c(2,1))
# Matrix X not filtered
ppath_labels <- spathial::spathialLabels(X, X_labels, spathial_res_with_filtering)
# Matrix X filtered
ppath_labels_filtered <- spathial::spathialLabels(X, X_labels, spathial_res_without_filtering)

# Plot the results
ppath_labels_numerical = as.numeric(as.factor(ppath_labels))
colors_labels_ppath <- sapply(ppath_labels_numerical, function(y){colors[as.integer(y)]})
plot(c(1:length(ppath_labels_numerical)), c(ppath_labels_numerical), col=colors_labels_ppath,
     pch=as.character(ppath_labels_numerical), xlab="Path Step", ylab="Sample Label",
     main="Path progression with prefiltering"
)

# Plot the results
ppath_labels_filtered_numerical = as.numeric(as.factor(ppath_labels_filtered))
colors_labels_ppath <- sapply(ppath_labels_filtered_numerical, function(y){colors[as.integer(y)]})
plot(c(1:length(ppath_labels_filtered_numerical)), c(ppath_labels_filtered_numerical), col=colors_labels_ppath,
     pch=as.character(ppath_labels_filtered_numerical), xlab="Path Step", ylab="Sample Label",
     main="Path progression without prefiltering"
)
par(oldpar)

## ----message="hide"-----------------------------------------------------------
# Calculate Association Statistics for each feature in the path
statistics <- spathial::spathialStatistics(spathial_res_with_filtering)

# Extract Pearson correlation coefficients between features and path
statistics$correlations

# Calculate Association Statistics for each feature in the path
statistics <- spathial::spathialStatistics(spathial_res_without_filtering)

# Extract Pearson correlation coefficients between features and path
statistics$correlations

## -----------------------------------------------------------------------------
# Load data
myfile<-system.file("extdata", "liver_tcga_example1.csv", package = "spathial")
data<-read.csv(myfile,as.is=TRUE,header=TRUE,row.names=1)
data[1:4,1:5]

## -----------------------------------------------------------------------------
X <- data[,1:(ncol(data)-1)]
X_labels <- data[,"Category"]
X[1:4,1:5]

## ----results="hide"-----------------------------------------------------------
# Choose the starting and ending points
boundary_init <- spathial::spathialBoundaryIds(X, X_labels, mode=2, from=2, to=1)
# Alternative, mode 3: 
# from="TCGA-DD-A39W-11A-11R-A213-07", to="TCGA-G3-AAV2-01A-11R-A37K-07"
boundary_ids <- boundary_init$boundary_ids
X <- boundary_init$X
X_labels <- boundary_init$X_labels

# run spathial
NC <- 50
spathial_res <- spathial::spathialWay(X, boundary_ids, NC)

## ----fig.cap="__Principal Path across the TCGA Liver Cancer dataset.__ 2D visualization of the Principal Path together with the data points. The x and y coordinates are the output of the dimensionality reduction performed with tSNE [2]. The start point and the end point of the Principal Path are the centroid of the normal samples and the centroid of the tumor samples, respectively. The Principal Path comprises 50 intermediate points (waypoints) plus the boundaries."----
# Plot the path in 2D using Rtsne
spathial::spathialPlot(X, X_labels, boundary_ids, spathial_res, perplexity_value=30)

## ----fig.cap='__Liver Cancer example - path labels across path steps.__ For each waypoint of the Principal Path (computed from the centroid of the normal samples to the centroid of the tumor samples), the assigned label is the label of the nearest sample.'----
# Labels for each waypoint with knn
ppath_labels <- spathial::spathialLabels(X, X_labels, spathial_res)
# Plot the results
colors <- rainbow(length(table(as.numeric(as.factor(X_labels)))))
ppath_labels <- as.vector(ppath_labels)
colors_labels_ppath <- sapply(ppath_labels, function(y){colors[as.integer(y)]})
plot(c(1:length(ppath_labels)), c(ppath_labels), col=colors_labels_ppath,
     pch=as.character(ppath_labels), xlab="Path Step", ylab="Sample Label",
     main="Path progression"
)

## -----------------------------------------------------------------------------
# Correlation along the path
statistics<-spathialStatistics(spathial_res)

## -----------------------------------------------------------------------------
singlepath_correlations<-statistics$correlations
top_positive_correlations<-sort(singlepath_correlations,decreasing=TRUE)[1:10]
top_positive_correlations
top_negative_correlations<-sort(singlepath_correlations,decreasing=FALSE)[1:10]
top_negative_correlations

## -----------------------------------------------------------------------------
load(url("https://github.com/erikagardini/spathial_dataframes/raw/master/luad_tcga.rda"))

## -----------------------------------------------------------------------------
#Compute centroids
normal_centroid <- colMeans(X[which(X_labels == "normal"),], na.rm = TRUE)
tumor_centroid <- colMeans(X[which(X_labels == "tumor"),], na.rm = TRUE)

#Subdivide normal samples from tumor samples
normal <- X[which(X_labels == "normal"),]
tumor <- X[which(X_labels == "tumor"),]

#Get start and end point names
getMaxDistancePoint <- function(centroid, samples){
  library(pracma)

  centroid <- t(centroid)
  dst<-pracma::distmat(
    as.matrix(centroid),
    as.matrix(samples)
  )
  ord <- order(-dst)
  return(rownames(samples)[ord[1]])
}

startPoint <- getMaxDistancePoint(tumor_centroid, normal)
endPoint <- getMaxDistancePoint(normal_centroid, tumor)

## -----------------------------------------------------------------------------
boundaries <- spathial::spathialBoundaryIds(X, X_labels, mode=3, from=startPoint, to=endPoint)
boundary_ids<-boundaries$boundary_ids
X <- boundaries$X
X_labels <- boundaries$X_labels

## ----fig.cap="__Principal Path across the TCGA Luad Cancer dataset.__ 2D visualization of the Principal Path together with the data points. The x and y coordinates are the output of the dimensionality reduction performed with tSNE [2]. The start and end points of the Principal Path are the most distant points from the centroid of the tumor samples and the centroid of the normal samples, respectively. The Principal Path comprises 50 intermediate points (waypoints) plus the boundaries."----
load(url("https://github.com/erikagardini/spathial_dataframes/raw/master/spathial_res.rda"))
spathial::spathialPlot(X, X_labels, boundary_ids, spathial_res, perplexity_value = 30, mask = NULL)

## -----------------------------------------------------------------------------
spathial_statistics <- spathial::spathialStatistics(spathial_res)

## -----------------------------------------------------------------------------
load(url("https://github.com/erikagardini/spathial_dataframes/raw/master/karlsson-rawcounts.rda"))

#NORMALIZATION AND PREFILTERING OF THE DATA (GENES IN AT LEAST 3 CELLS)
#library(Seurat)
#seuset<-CreateSeuratObject(counts=rawcounts,min.cells=3,min.features=200)
#normalized_seuset<-NormalizeData(seuset,normalization.method="LogNormalize",scale.factor=10000)
#normalized_expmat <- as.matrix(normalized_seuset[["RNA"]]@data)
#save(normalized_expmat,annotation,file="karlsson-normalized_expmat.rda")

load(url("https://github.com/erikagardini/spathial_dataframes/raw/master/karlsson-normalized_expmat.rda"))
X <- t(normalized_expmat)
X_labels <- annotation

## -----------------------------------------------------------------------------
boundaries <- spathial::spathialBoundaryIds(X, X_labels, mode=2, from="G1", to="G2/M")
boundary_ids<-boundaries$boundary_ids
X <- boundaries$X
X_labels <- boundaries$X_labels

## ----results='hide'-----------------------------------------------------------
spathial_res <- spathial::spathialWay(X, boundary_ids, NC = 50)

## ----fig.cap="__Principal Path across the Karlsson Single-cell RNA-Seq dataset__ 2D visualization of the Principal Path together with the data points. The x and the y coordinates are the output of the dimensionality reduction performed with tSNE [2]. The start end points of the Principal Path are the centroid of the samples labelled as *G1* and the centroid of the samples labelled as *G2/M*, respectively. The Principal Path comprises 50 intermediate points (waypoints) plus the boundaries."----
spathial::spathialPlot(X, X_labels, boundary_ids, spathial_res, perplexity_value = 30, mask = NULL)

## -----------------------------------------------------------------------------
spathial_statistics <- spathial::spathialStatistics(spathial_res)

