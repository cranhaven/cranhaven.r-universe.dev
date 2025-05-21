# RclusTool: clustering of items in datasets
#
# Copyright 2013 Guillaume Wacquet, Pierre-Alexandre Hebert, Emilie Poisson-Caillault
#                
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.

# GLOBAL Parameters and Variables : with Majuscule
#' function to initialize the global parameters and variables
#' @title Parameters initialization
#' @description Initialize the global parameters and variables and generate a new environment used in the \code{\link{RclusToolGUI}}.
#' @return RclusTool.env new environment in which all global parameters, raw data and results are stored.
#' @importFrom grDevices colors
#' @param RclusTool.env environment in which data and intermediate results are stored.
#' @examples 
#' RclusTool.env <- initParameters()
#' 
#' @keywords internal
#'
#' 


initParameters <- function(RclusTool.env=new.env()) {
    RclusTool.env$gui$protos.dir <- ""
    RclusTool.env$data.sample <- NULL # data sample
    RclusTool.env$param$classif$unsup$K.max <- 20
    RclusTool.env$param$preprocess$pca$nb.dims.max <- 10
    RclusTool.env$param$preprocess$pca$variance.cum.min <- 0.95 #minimal explained variance in PCA
    RclusTool.env$param$classif$unsup$kmeans.variance.min <- 0.95 #minimal explained variance in K-means
    RclusTool.env$param$preprocess$sampling.size.max <- 1000
    RclusTool.env$param$classif$unsup$nb.neighbours <- ceiling(6.99/100*RclusTool.env$param$preprocess$sampling.size.max/RclusTool.env$param$classif$unsup$K.max)
    RclusTool.env$gui$debug.mode <- FALSE

    # OS detection
    if (length(grep("linux", version$platform)))
        RclusTool.env$gui$operating.system <- "linux"
    if (length(grep("w64", version$platform)))
        RclusTool.env$gui$operating.system <- "windows"
    if (length(grep("w32", version$platform)))
        RclusTool.env$gui$operating.system <- "windows"
    if (length(grep("apple", version$platform)))
        RclusTool.env$gui$operating.system <- "apple"
    RclusTool.env$param$visu$scale.graphics <- 1 #figures scale
    # Default username and usertype
    RclusTool.env$gui$user.name <- "username"
    RclusTool.env$gui$user.type <- "standard"
    RclusTool.env$param$analysis$summary.functions <- c("Min"="min", "Max"="max", "Sum"="sum", "Average"="mean", "SD"="sd")
    RclusTool.env$param$preprocess$noise.cluster <- "Noise"
    RclusTool.env$param$preprocess$zero.threshold <- 2
    RclusTool.env$param$preprocess$featSpaceNames <- c("initial"="Initial Features", 
                                                       "preprocessed"="Preprocessed Features",
                                                       "pca"="Principal Component Analysis",
                                                       "pca_full"="Principal Component Analysis (full)",
                                                       "spectral"="Spectral Embedding",
                                                       "scaled"="Scaled",
                                                       "sampled"="Sampled")

    # classification functions parameters
    RclusTool.env$param$classif$sup <- list()
    #MLP
    RclusTool.env$param$classif$sup[["nnet"]] <- list(size=10, rang=1e-3, decay=5e-4, maxit=300, Hess=TRUE)
    #SVM
    RclusTool.env$param$classif$sup[["svm"]] <- list(type="C", kernel="linear")
    #RF
    RclusTool.env$param$classif$sup[["rf"]] <- list(ntree=500)
    #EM
    RclusTool.env$param$classif$sup[["Mclust"]] <- list(modelNames="VVV")
    #PCA
    RclusTool.env$param$classif$sup[["prcomp"]] <- list(center=TRUE, scale=TRUE)
    #HC
    RclusTool.env$param$classif$sup[["hclust"]] <- list(method = "complete", members = NULL)

    # Parameter pch and size and colors
    pch <- 20 
    RclusTool.env$param$visu$cex <- 0.5
    RclusTool.env$param$visu$titlefont <- "Consolas"
    RclusTool.env$param$visu$font <- "Arial"
    RclusTool.env$param$visu$titlesize <- 10
    RclusTool.env$param$visu$size <- 9
    RclusTool.env$param$visu$sizecm <- 32
    RclusTool.env$param$visu$style$size <- 9
    RclusTool.env$param$visu$hscale <- 1.2
    RclusTool.env$param$visu$console <- 70
    RclusTool.env$param$visu$palette.colors <- c("grey","black","red","blue","green","cyan", "yellow","orange",
                                     "rosybrown","palevioletred","darkblue","deeppink","blueviolet", 'darkgoldenrod1', 'chartreuse',
                                     "darkorchid1", "deeppink", "coral", "darkolivegreen1","#66C2A5","#9DAE8C","#D49A73","#F08F6D",
                                     "#C79693","#9E9DBA","#9F9BC9","#C193C6","#E28BC3","#D2A29F","#BABF77","#AAD852","#CBD844",
									 "#ECD836","#FAD53E","#F1CD64","#E7C689","#D7BF9C","#C5B9A7","#B3B3B3","#D53E4F","#E04F4A",
									 "#EB6046","#F47346","#F88B51","#FBA35C","#FDB869","#FDCA79","#FDDD88","#F6E68F","#EDEE93",
									 "#E2F398","#CDEA9D","#B7E2A1","#A0D8A4","#86CEA4","#6DC4A4","#58B2AB","#459DB4","#3288BD")
    point.symbol <- c(20, 0:18)
    RclusTool.env$param$visu$point.style <- expand.grid(col=RclusTool.env$param$visu$palette.colors,
                                             pch=pch)
    RclusTool.env$param$visu$point.style$col <- as.character(RclusTool.env$param$visu$point.style$col)

    # Tabs environments
    RclusTool.env$gui$tabs.env <- list()
	
    RclusTool.env
}
