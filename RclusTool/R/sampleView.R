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

#' plotProfile plots the profile and the image (if available) of a particle
#' @title Profile and image plotting 
#' @description Plot the profile and the image (if available) of a particle.
#' @param profiles matrix of profile data (signals in columns).
#' @param profiles.colors vector of colors corresponding to each signal in profile.
#' @param image character vector specifying the name (without path) + extension of the associated image (if available).
#' @param curve.names vector of character specifying the names of each signal in profile.
#' @param title character vector specifying the title of the plot.
#' @param sub character vector specifying the subtitle of the plot.
#' @param sub.color character vector specifying the color of the subtitle.
#' @param image.dir images directory. 
#' @param charsize character size
#' @return None
#' @importFrom graphics layout plot rasterImage matplot legend
#' @importFrom jpeg readJPEG
#' @importFrom png readPNG
#' @importFrom rlang .data
#' @import reshape ggplot2
#' @seealso \code{\link{plotSampleFeatures}}, \code{\link{visualizeSampleClustering}}
#'
#' @examples 
#' dat <- rbind(matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 4, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 6, sd = 0.3), ncol = 2))
#' colnames(dat) <- c("x","y")
#' tf1 <- tempfile()
#' write.table(dat, tf1, sep=",", dec=".")
#' 
#' sig <- data.frame(ID=rep(1:150, each=30), SIGNAL=rep(dnorm(seq(-2,2,length=30)),150))
#' tf2 <- tempfile()
#' write.table(sig, tf2, sep=",", dec=".")
#' 
#' x <- importSample(file.features=tf1, file.profiles=tf2)
#' 
#' plotProfile(x$profiles[[1]])
#'
#'
#' @keywords internal 
#' 

plotProfile <- function(profiles, profiles.colors=NULL, image=NULL, curve.names=NULL, title="Some observation", sub="", sub.color=NULL, image.dir=NULL, charsize=11){
 
  p <- ggplot() + ggplot2::ggtitle(paste(title,sub)) 
  p <- p + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) 
  p <- p + ggplot2::theme(axis.text.x = element_text(size = charsize))
  p <- p + ggplot2::theme(axis.text.y = element_text(size = charsize))
  p <- p + ggplot2::theme(plot.title = element_text(size = charsize))
   
  nc <- ncol(profiles)
  if (!is.null(nc)) {
    if (is.null(curve.names))
       curve.names <- colnames(profiles)
 
    profiles.df<-as.data.frame(profiles)
    profiles.df$line<-1:nrow(profiles.df)
    profiles.df <- reshape::melt(profiles.df, id.vars="line")
    # Everything on the same plot
    p <- ggplot2::ggplot(profiles.df, ggplot2::aes(x=.data$line,y=.data$value,col=.data$variable)) + ggplot2::labs(col="Profiles")
    p <- p + ggplot2::ggtitle(paste(title,sub)) 
    p <- p + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) 
    p <- p + ggplot2::xlab("") + ggplot2::ylab("Value")
  }
  if (is.null(image))
        image <- NA
  if (is.null(image.dir))
        image.dir <- ""
  if (!is.na(image)) {
  	imgFile <- file.path(image.dir, image)
  	readImg <- jpeg::readJPEG
        if (!grepl(".jpg", imgFile))
            readImg <- png::readPNG
        if (grepl(".jpg", imgFile) || grepl(".png", imgFile)) {
            img <- readImg(imgFile, native = TRUE)
            
        }
    p <- p + ggplot2::annotation_custom(grid::rasterGrob(img, 
                      width = unit(1,"npc"),
                      height = unit(1,"npc")), 
                   	   -Inf, Inf, -Inf, Inf) 
  }
  if (!is.null(nc)) {
  	if (is.null(profiles.colors)){
  		profiles.colors <- rep("black", length(curve.names))
  	}
  	p <- p + ggplot2::geom_line(size=0.9) 
  	p <- p + ggplot2::scale_color_manual(values= profiles.colors[1:nc])
  }
  print(p)
}

#' plotProfileExtract plots the profile and the image (if available) of a particle
#' @title Profile and image plotting 
#' @description Plot the profile and the image (if available) of a particle.
#' @param profiles matrix of profile data (signals in columns).
#' @param profiles.colors vector of colors corresponding to each signal in profile.
#' @param image character vector specifying the name (without path) + extension of the associated image (if available).
#' @param curve.names vector of character specifying the names of each signal in profile.
#' @param title character vector specifying the title of the plot.
#' @param sub character vector specifying the subtitle of the plot.
#' @param sub.color character vector specifying the color of the subtitle.
#' @param image.dir images directory. 
#' @return None
#' @importFrom graphics layout plot rasterImage matplot legend
#' @importFrom jpeg readJPEG
#' @importFrom png readPNG
#' @seealso \code{\link{plotSampleFeatures}}, \code{\link{visualizeSampleClustering}}
#'
#' @examples 
#' dat <- rbind(matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 4, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 6, sd = 0.3), ncol = 2))
#' colnames(dat) <- c("x","y")
#' tf1 <- tempfile()
#' write.table(dat, tf1, sep=",", dec=".")
#' 
#' sig <- data.frame(ID=rep(1:150, each=30), SIGNAL=rep(dnorm(seq(-2,2,length=30)),150))
#' tf2 <- tempfile()
#' write.table(sig, tf2, sep=",", dec=".")
#' 
#' x <- importSample(file.features=tf1, file.profiles=tf2)
#' 
#' plotProfileExtract(x$profiles[[1]])
#'
#'
#' @keywords internal
#' 
plotProfileExtract <- function(profiles, profiles.colors=NULL, image=NULL, curve.names=NULL, title="Some observation", sub="", sub.color=NULL, image.dir=NULL){
    opar <- graphics::par(no.readonly=TRUE)
    on.exit(graphics::par(opar))
    nc <- ncol(profiles)
    if (is.null(image))
        image <- NA
    if (is.null(image.dir))
        image.dir <- ""

    dimLayout <- as.numeric(!is.null(nc)) + as.numeric(!is.na(image))
    graphics::layout(matrix(c(1,dimLayout), 1, 2, byrow = TRUE), widths=rep(1, dimLayout))
    par(mar=c(6,3,2,.2)) #bottom large because of sub parameter
    if (!is.null(nc)) {
        if (is.null(curve.names))
            curve.names <- colnames(profiles)

        #correspondances of colors
        if (!is.null(profiles.colors)) {
            curve.colors <- sapply(curve.names, function (x) {col<-profiles.colors[x];
                                   if (is.na(col)) 'black' else col})
        } else {
            curve.colors <- rep("black", length(curve.names))
        }

        if (is.null(sub.color))
            sub.color <- "black"

        #matplot(profiles, pch=1:nc, type="o", col=curve.colors, main=title, sub=sub, col.sub=sub.color, ylab="level", cex = 0.8)
        graphics::matplot(profiles, pch=1:nc, type="o", col=curve.colors, main=title, sub=sub, ylab="level", cex = 0.8)

        if (!is.null(curve.names))
            graphics::legend("topright", legend=curve.names, col=curve.colors, pch=1:nc, cex=0.8)
    }

    par(mar=c(5,.2,5,.2))
    if (!is.na(image)) {
        imgFile <- file.path(image.dir, image)
        readImg <- jpeg::readJPEG
        if (!grepl(".jpg", imgFile))
            readImg <- png::readPNG
        if (grepl(".jpg", imgFile) || grepl(".png", imgFile)) {
            img <- readImg(imgFile, native = TRUE)
            dimg <- dim(img)
            h <- dimg[1]
            w <- dimg[2]
            graphics::plot(c(0, w), c(0, h), type = "n", xlab = "", ylab = "", axes = FALSE)
            graphics::rasterImage(img, 0, 0, w, h)
        }
    }
}

#' plotSampleFeatures plots 2D-features scatter-plot of all particles
#' @title 2D-features scatter-plot
#' @description Plot 2D-features scatter-plot of all particles. Grey color (label=0) is for data to cleaned or to remove in classification process.
#' @param data matrix or data.frame of raw data (points by line).
#' @param label vector of labels.
#' @param parH character vector specifying the name of the feature to use as x-axis.
#' @param parV character vector specifying the name of the feature to use as y-axis.
#' @param figure.title character vector specifying the title of the scatter-plot.
#' @param logscale character vector containing "x" if the x-axis is to be logarithmic, "y" if the y-axis is to be logarithmic and "xy" or "yx" if both axes are to be logarithmic.
#' @param cex numeric value specifying the size of the graphical labels. 
#' @param point.param data.frame specifying the colors and the symbols to use for clusters display.
#' @param env.plot environment where to store graphical parameters used by function PlotScatter to select points.
#' @return None
#' @importFrom grDevices colors
#' @importFrom graphics title plot abline legend par
#' @seealso \code{\link{plotProfile}}, \code{\link{visualizeSampleClustering}}
#' 
#' @examples 
#' dat <- rbind(matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 4, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 6, sd = 0.3), ncol = 2))
#' colnames(dat) <- c("x","y")
#' tf1 <- tempfile()
#' write.table(dat, tf1, sep=",", dec=".")
#' 
#' sig <- data.frame(ID=rep(1:150, each=30), SIGNAL=rep(dnorm(seq(-2,2,length=30)),150))
#' tf2 <- tempfile()
#' write.table(sig, tf2, sep=",", dec=".")
#' 
#' x <- importSample(file.features=tf1, file.profiles=tf2)
#' 
#' res <- KmeansQuick(x$features$initial$x, K=3)
#' new.labels <- formatLabelSample(res$cluster, x)
#' 
#' plotSampleFeatures(x$features$initial$x, label = new.labels, parH="x", parV="y")
#'
#'
#' @keywords internal
#' 
plotSampleFeatures<- function(data, label, parH=NULL, parV=NULL, figure.title="Scatter plot",
                              logscale="", cex=.8, point.param=expand.grid(
                                  col=c("grey","black","red","blue","green","cyan", "yellow","orange",
                                     "rosybrown","palevioletred","darkblue","deeppink","blueviolet", 'darkgoldenrod1', 'chartreuse',
                                     "darkorchid1", "deeppink", "coral", "darkolivegreen1","#66C2A5","#9DAE8C","#D49A73","#F08F6D",
                                     "#C79693","#9E9DBA","#9F9BC9","#C193C6","#E28BC3","#D2A29F","#BABF77","#AAD852","#CBD844",
									 "#ECD836","#FAD53E","#F1CD64","#E7C689","#D7BF9C","#C5B9A7","#B3B3B3","#D53E4F","#E04F4A",
									 "#EB6046","#F47346","#F88B51","#FBA35C","#FDB869","#FDCA79","#FDDD88","#F6E68F","#EDEE93",
									 "#E2F398","#CDEA9D","#B7E2A1","#A0D8A4","#86CEA4","#6DC4A4","#58B2AB","#459DB4","#3288BD"),
                                  pch=c(20, 0:18), stringsAsFactors = FALSE), env.plot = NULL){
    opar <- graphics::par(no.readonly=TRUE)
    on.exit(graphics::par(opar))
    if (is.null(parH)) {
        parH <- colnames(data)[1]
    }

    if (is.null(parV)) {
        parH <- colnames(data)[2]
    }

    #numeros des clusters dans l'ordre
    vlabel <- levels(label)

    #construction des donnees
    x <- data[, c(parH, parV), drop=FALSE]
    if (cex==0.8){
	graphics::par(mar=c(3.9, 3.9, 1.5, 10.5), xpd=TRUE)
	} else if (cex==0.7) {
	graphics::par(mar=c(3.9, 3.9, 1.5, 8.1), xpd=TRUE)
	} else if (cex==0.6) {
	graphics::par(mar=c(3.9, 3.9, 1.5, 7.1), xpd=TRUE)
	}
    #plot
    graphics::plot(x, type="p", col=point.param$col[unclass(label)], pch=point.param$pch[unclass(label)],
         cex=cex, cex.lab=cex, cex.sub=cex, cex.axis=cex, log=logscale, ann=FALSE) #pch=1, cex=.8, yaxt = "n",xaxt = "n");

    graphics::abline( h = 0, lty = 3, col = grDevices::colors()[ 440 ] )
    graphics::abline( v = 0, lty = 3, col = grDevices::colors()[ 440 ] )

    #titles+legendes
    graphics::title(figure.title, xlab=parH, ylab=parV, cex.main=cex)
    coord <- graphics::par("usr")
    graphics::legend("topright", legend=vlabel, col=point.param$col[1:length(vlabel)],
           pch=point.param$pch[1:length(vlabel)], cex=cex, pt.cex=cex, inset=c(-0.25,0), box.lty=0)
    if (!is.null(env.plot))
    {
        env.plot$parPlotSampleFeatures <- par(no.readonly=TRUE)
    }
}

#' plotDensity2D plots density of a variable by cluster
#' @title plot Variables Density
#' @description Plots density of a variable by cluster.
#' @param data density data.frame with x,y and Cluster indication.
#' @param parH character vector specifying the name of the feature to use as x-axis.
#' @param clustering.name character vector specifying the name of the clustering.
#' @param charsize character size
#' @param col vector of colors
#' @return None
#' @import ggplot2
#' 
#' @examples 
#'
#' dat <- rbind(matrix(rnorm(100, mean = 0, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 4, sd = 0.3), ncol = 2))
#' tf <- tempfile()
#' write.table(dat, tf, sep=",", dec=".")
#' 
#' x <- importSample(file.features=tf)
#' x <- computeUnSupervised(x, K=3, method.name="K-means")
#' 
#' label<-x[["clustering"]][["K-means_preprocessed"]][["label"]]
#' 
#' cluster.density <- clusterDensity(x, label, "preprocessed",features.to.keep='V1')
#' plotDensity2D(cluster.density, clustering.name='Test Kmeans', parH='V1')
#'
#'
#' @keywords internal
#' 

plotDensity2D <- function(data, parH=NULL, clustering.name, charsize = 11,
                                  col=c("grey","black","red","blue","green","cyan", "yellow","orange",
                                     "rosybrown","palevioletred","darkblue","deeppink","blueviolet", 'darkgoldenrod1', 'chartreuse',
                                     "darkorchid1", "deeppink", "coral", "darkolivegreen1","#66C2A5","#9DAE8C","#D49A73","#F08F6D",
                                     "#C79693","#9E9DBA","#9F9BC9","#C193C6","#E28BC3","#D2A29F","#BABF77","#AAD852","#CBD844",
									 "#ECD836","#FAD53E","#F1CD64","#E7C689","#D7BF9C","#C5B9A7","#B3B3B3","#D53E4F","#E04F4A",
									 "#EB6046","#F47346","#F88B51","#FBA35C","#FDB869","#FDCA79","#FDDD88","#F6E68F","#EDEE93",
									 "#E2F398","#CDEA9D","#B7E2A1","#A0D8A4","#86CEA4","#6DC4A4","#58B2AB","#459DB4","#3288BD")){
    tryCatch(
        expr = {
			p <- ggplot2::ggplot(data=data, ggplot2::aes(x=data$x, y=data$y, colour=data$Cluster, shape=data$Cluster)) + ggplot2::labs(col="Cluster")
			p <- p + ggplot2::geom_line(size=1) + ggplot2::scale_color_manual(values=(col))
			p <- p + ggplot2::xlab(parH) + ggplot2::ylab("Density")
			p <- p + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) 
			p <- p + ggplot2::ggtitle(paste('Variables density plot by', clustering.name, sep=" "))
			p <- p + ggplot2::theme(axis.text.x = element_text(size = charsize))
			p <- p + ggplot2::theme(axis.text.y = element_text(size = charsize))
			p <- p + ggplot2::theme(plot.title = element_text(size=charsize))
			print(p)
        },
        error = function(e){
        	message(e)
            if (grepl('-monotype-arial',as.character(e))==TRUE){
            	message("Error :Please, Try to install gsfonts-x11")
           	}
        }
    )    
}                          


#' visualizeSampleClustering opens an interactive figure with 2D scatter-plot of all particles with axis choice
#' @title Interactive figure with 2D scatter-plot
#' @description Open an interactive figure with 2D scatter-plot of all particles with axis choice. Grey color (label=0) is for data to cleaned or to remove in classification process.
#' @param data.sample list containing features, profiles and clustering results.
#' @param label vector of labels.
#' @param clustering.name character vector specifying the clustering method used to get labels.
#' @param cluster.summary data.frame containing the clusters summaries (as returned by 'clusterSummary').
#' @param RclusTool.env environment in which all global parameters, raw data and results are stored.
#' @param prototypes list containing vectors of prototypes indices.
#' @param profile.mode character vector specifying the plot mode of profiles. Must be 'none' (default), 'whole sample', 'cluster i' or 'constrained pairs'.
#' @param selection.mode character vector specifying the selection mode of profiles. Must be 'none' (default), 'prototypes' or 'pairs'.
#' @param compare.mode character vector specifying the mode of comparison between two clusterings results. Must be 'off' (default) or 'on'.
#' @param pairs list of constrained pairs (must-link and cannot-link).
#' @param features.mode character vector specifying the plot mode of features (projection in a specific space). Must be 'initial' (default), 'preprocessed', 'pca', 'pca_full' or 'spectral', or prefixed versions ('sampled', 'scaled') of those space names.
#' @param wait.close boolean: if FALSE (default), the following steps of the analysis calculations are computed even if the window is not closed.
#' @param fontsize size of font (default is 9)
#' @return prototypes in \code{selection.mode} = "prototypes" mode, pairs in \code{selection.mode} = "pairs" mode.
#' @seealso \code{\link{plotProfile}}, \code{\link{plotSampleFeatures}}
#' @importFrom graphics barplot boxplot axis points filled.contour par
#' @importFrom SearchTrees createTree knnLookup
#' @importFrom grDevices gray topo.colors
#' @importFrom MASS kde2d
#' @import tcltk tcltk2 tkrplot ggplot2
#' @examples 
#' dat <- rbind(matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 4, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 6, sd = 0.3), ncol = 2))
#' colnames(dat) <- c("x","y")
#' tf1 <- tempfile()
#' write.table(dat, tf1, sep=",", dec=".")
#' 
#' sig <- data.frame(ID=rep(1:150, each=30), SIGNAL=rep(dnorm(seq(-2,2,length=30)),150))
#' tf2 <- tempfile()
#' write.table(sig, tf2, sep=",", dec=".")
#' 
#' x <- importSample(file.features=tf1, file.profiles=tf2)
#' 
#' res <- KmeansQuick(x$features$initial$x, K=3)
#' new.labels <- formatLabelSample(res$cluster, x)
#' 
#' visualizeSampleClustering(x, label = new.labels, clustering.name="K-means", 
#'			     profile.mode="whole sample")
#'  
#' @export 
#' 
visualizeSampleClustering<- function(data.sample, label=NULL, clustering.name="proposed clustering",
                                     cluster.summary=NULL, RclusTool.env=initParameters(),  
                                     prototypes=NULL, profile.mode="none",
                                     selection.mode="none", compare.mode="off",
                                     pairs=NULL, features.mode="initial",
                                     wait.close=FALSE, fontsize=9){


    #----------------------------------------------------------------------------------------
    # Variables of the function
    #----------------------------------------------------------------------------------------
    visu.env <- new.env()
    visu.env$plot.mode <- "scatter-plot"
    visu.env$plotSampleFunction <- function(compare){} #function called by plotSample
    visu.env$parH <- NULL
    visu.env$parV <- NULL
    visu.env$parPlotSampleFeatures <- NULL
    visu.env$previous.clustering.name <- NULL
    visu.env$label <- label
    visu.env$cluster.summary <- cluster.summary
    visu.env$clustering.name <- clustering.name
    visu.env$data.sample <- data.sample
    visu.env$prototypes <- NULL
    visu.env$profile.mode <- profile.mode
    visu.env$slider.values <- NULL #ids of subset of particles dealt with (may be a cluster, or prototypes)
    visu.env$pts.search.tree <- NULL #search tree to locate mouse-selected points in scatter plot
    visu.env$profile.id <- c(
                    sel=NA, #profile selected (in the top) id relative to described particles (by features$x)
                    mem=NA, #profile memorized (in the bottom)
                    fig=NA) #profile plotted (not always the one selected)
    visu.env$mem.mode <- FALSE
    visu.env$pairs.values <- NULL
    visu.env$pairs.constraints <- NULL
    visu.env$plot.height <- c() #graphic parameter : height of plot part (left)
    visu.env$features.mode <- features.mode
    visu.env$features <- NULL
    visu.env$tcl.proto.label <- tclVar("")
    visu.env$box.pixel <- NULL #plot bounding box in unit pixel
    visu.env$compare.mode <- compare.mode
    visu.env$summary.feat.select <- NULL
    visu.env$summary.fun.select <- NULL 
    visu.env$summary.feature <- NULL # = couple feat + fun
    visu.env$onScatterPlotMove <- function(x,y){}
    visu.env$ProfileModeNumber <- 1
    visu.env$tk.font <- tkfont.create(size = fontsize, family = "Arial", weight = "bold")
    if (fontsize>=11){
    	visu.env$cex=0.8
    } else if (fontsize>=9) {
    	visu.env$cex=0.7
    } else {
    	visu.env$cex=0.6
    }

    if (!is.null(visu.env$label))
    {
        # case : new label to add
        visu.env$label <- formatLabelSample(visu.env$label[visu.env$data.sample$id.clean], visu.env$data.sample, new.labels=FALSE, noise.cluster=RclusTool.env$param$preprocess$noise.cluster)
        if (is.null(visu.env$cluster.summary))
            visu.env$cluster.summary <- clusterSummary(visu.env$data.sample, visu.env$label, summary.functions=RclusTool.env$param$analysis$summary.functions)

        visu.env$data.sample$clustering <- addClustering(clustering=visu.env$data.sample$clustering, new.clustering.name=visu.env$clustering.name, 
                      new.cluster.summary=visu.env$cluster.summary, new.label=visu.env$label)
    }

    if (!is.null(visu.env$data.sample$clustering[[visu.env$clustering.name]])) 
    {
        # case: data.sample contains the clustering named 
        visu.env$label <- visu.env$data.sample$clustering[[visu.env$clustering.name]]$label
        visu.env$cluster.summary <- visu.env$data.sample$clustering[[visu.env$clustering.name]]$summary
    } else {
        if (is.null(visu.env$label)) {
            # case: clustering named doesn't exist and no clustering proposed
            visu.env$clustering.name <- "no-clustering"
            visu.env$label <- visu.env$data.sample$clustering[[visu.env$clustering.name]]$label
            visu.env$cluster.summary <- visu.env$data.sample$clustering[[visu.env$clustering.name]]$summary
        }
    }

    visu.env$prototypes <- sapply(visu.env$data.sample$clustering, function(x) x$prototypes)
    if (is.null(visu.env$prototypes[[visu.env$clustering.name]]))
    {
        if (!is.null(visu.env$data.sample$clustering[[visu.env$clustering.name]]$prototypes)) {
            visu.env$prototypes[[visu.env$clustering.name]] <- visu.env$data.sample$clustering[[visu.env$clustering.name]]$prototypes
        } else {
            visu.env$prototypes[[visu.env$clustering.name]] <- c()
            }
        visu.env$prototypes[[visu.env$clustering.name]] <- as.character(visu.env$label[names(visu.env$prototypes[[visu.env$clustering.name]])])
        }
    ###MODIF
    #  prototypes$id <- visu.env$data.sample$id.abs.inv[prototypes$id.abs] #id: in sample not cleaned
    #  prototypes$label <- as.character(visu.env$label[prototypes$id])
    #  prototypes$id.abs.inv <- rep(NA, max(visu.env$data.sample$id.abs))
    #  prototypes$id.abs.inv[prototypes$id.abs] <- 1:length(prototypes$label) #id: in prototypes
    #

    if (length(grep("prototype", visu.env$profile.mode, ignore.case=TRUE))&&(!length(visu.env$prototypes)))
        visu.env$profile.mode <- "none"

    if (is.null(pairs)) {
        visu.env$pairs.values <- list() #list of pairs (vector of 2 particle ids)
        visu.env$pairs.constraints <- c() #how pair are constrained (+1 for ML, -1 for CNL)
    } else {
        visu.env$pairs.values <- c(pairs$ML, pairs$CNL)
        visu.env$pairs.constraints <- c(rep(1,length(pairs$ML)), rep(-1, length(pairs$CNL)))
    }

    color.select <- c(grDevices::gray(0.25),grDevices::gray(0.90)) #color to plot selected particles points

    if (!(visu.env$features.mode %in% names(visu.env$data.sample$features)))
        visu.env$features.mode <- names(visu.env$data.sample$features)[1]
    visu.env$features <- visu.env$data.sample$features[[visu.env$features.mode]]
    visu.env$features$x <- visu.env$features$x[sapply(visu.env$features$x, is.numeric)]
    signals <- visu.env$data.sample$profiles
    images <- visu.env$data.sample$images

    tcl.plot.mode <- tclVar(visu.env$plot.mode)
    tcl.clustering.mode <- tclVar(visu.env$clustering.name)
    tcl.features.mode <- tclVar(visu.env$features.mode)
    tcl.profile.mode <- tclVar(visu.env$profile.mode) #"none", "whole sample", ou "cluster 'i'"
    tcl.compare.mode <- tclVar(visu.env$compare.mode)
    tcl.slider.big <- tclVar("0")
    tcl.slider.tune <- tclVar("1")
    slider.max <- 300 #maximal incrementations of slider.tune

    #if (is.null(visu.env$cluster.summary))
        #visu.env$cluster.summary <- clusterSummary(visu.env$data.sample, visu.env$label, summary.functions=RclusTool.env$param$analysis$summary.functions)

    initClusterSummaryFeature <- function()
    {
        old.feature <- visu.env$summary.feature

        summary.feats <- extractFeaturesFromSummary(visu.env$data.sample$clustering[[visu.env$clustering.name]]$summary)

        if (!length(summary.feats$simple))
            stop("sampleView, visualizeSampleClustering: no simple features in the summary")

        cluster.summary.simple.features <- paste("Result", summary.feats$simple.feats)

        if (is.null(old.feature)|| !(old.feature %in% cluster.summary.simple.features))
            visu.env$summary.feature <- cluster.summary.simple.features[1]
    }

    initClusterSummaryFeature()

    cluster.summary.init <- visu.env$cluster.summary

    #----------------------------------------------------------------------------------------
    # functions to deal with ids of selected particles
    #----------------------------------------------------------------------------------------

    # not for profile.mode=="constrained pairs"
    setProfileId <- function(names, value){
        isna <- is.na(value)||!is.element(value, visu.env$slider.values)#is.na(visu.env$features$ids.inv[value])
        if (isna)
            value <- visu.env$slider.values[1]
        for (name in names) {
            visu.env$profile.id[name] <- value
        }
        if (is.element("sel", names))
            setSlidersId(value, value=TRUE)
        if ((is.element("mem", names))&&isna)
            setMemMode(FALSE)
        updateProfilesPlot(names)
    }

    # pair : a pair from slider.value
    setProfileIdsPair <- function(pair){
        names <- c("fig", "mem", "sel")
        pair <- unlist(pair)
        if (any(is.na(visu.env$features$x[pair,1]))) #(any(is.na(features$id.abs.inv[pair]))) ????
            pair <- unlist(visu.env$slider.values[1])

        for (name in names) {
            val <- switch(name, fig=pair[1], sel=pair[1], mem=pair[2])
            visu.env$profile.id[name] <- val
        }
        #    if (is.element("sel", names))
        #      setSlidersId(value, value=TRUE)
        updateProfilesPlot(names)
    }

    updateProfileId <- function(names=c("fig","mem","sel")){
        for (name in names) {
            setProfileId(name, visu.env$profile.id[name])#test
        }
    }  

    #----------------------------------------------------------------------------------------
    # plot functions of the sample (left area)
    #----------------------------------------------------------------------------------------

    plotSample <- function() {
        opar <- graphics::par(no.readonly=TRUE)
		on.exit(graphics::par(opar))
        graphics::par(bg="white")
        visu.env$plotSampleFunction(compare=FALSE)
    }

    plotSampleCompare <- function() {
    	opar <- graphics::par(no.readonly=TRUE)
		on.exit(graphics::par(opar))
        graphics::par(bg="white")
        visu.env$plotSampleFunction(compare=TRUE)
    }

    plotScatter <- function(compare=FALSE){
        label <- visu.env$features$label
        clustering.name <- visu.env$clustering.name
        if (compare) {
            clustering.name <- visu.env$previous.clustering.name
            label <- visu.env$data.sample$clustering[[clustering.name]]$label[rownames(visu.env$features$x)]
        }
        x <- visu.env$features$x.2D.logscale
        logscale <- visu.env$features$logscale.2D
        title <- paste(visu.env$plot.mode, "clustered by", clustering.name)
        plotSampleFeatures(x, label, parH=visu.env$parH, parV=visu.env$parV, figure.title=title,
                           logscale=logscale, cex=visu.env$cex, point.param=RclusTool.env$param$visu$point.style, env.plot=visu.env)        

        if (is.null(visu.env$parPlotSampleFeatures))
            return()
        opar <- graphics::par(no.readonly=TRUE)
        on.exit(graphics::par(opar))
        par(visu.env$parPlotSampleFeatures)

        #ids <- visu.env$features$id.abs.inv[c(visu.env$profile.id$sel, visu.env$profile.id$mem)]
        #names(ids) <- c("sel", "mem")
        ids <- visu.env$profile.id[c("sel", "mem")]

        # add links
        if (visu.env$profile.mode=="constrained pairs") {
            selected.pair.id <- getSlidersValue(value=FALSE)
            for (i in 1:length(visu.env$slider.values)) {
                if (i==selected.pair.id)
                    next
                pair <- visu.env$pairs.values[[i]]
                w <- pair #features$id.abs.inv[pair]
                #if (any(is.na(w)))
                if (any(is.na(x[w,])))
                    next
                if (visu.env$pairs.constraints[i]==1) {
                    graphics::points(x[w,], type="l", col="darkgreen", lwd=2)
                }
                if (visu.env$pairs.constraints[i]==-1) {
                    graphics::points(x[w,], type="l", col="darkred", lwd=2)
                }
            }
            if (!any(is.na(ids))) {###A verifier
                if (visu.env$pairs.constraints[selected.pair.id]==1) {
                    graphics::points(x[ids,], type="l", col="darkgreen", lwd=4)
                }
                if (visu.env$pairs.constraints[selected.pair.id]==-1) {
                    graphics::points(x[ids,], type="l", col="darkred", lwd=4)
                }
            }
        }

        # add point whose profile is plotted
        #colored cross + circle + point on selected point(s)
        if ((visu.env$profile.mode!="none") && (!(is.na(ids["sel"]))))
            graphics::points(x[rep(ids["sel"],3),,drop=FALSE], 
                             pch=c("+", "O", RclusTool.env$param$visu$point.style$pch[unclass(label[ids["sel"]])]),
                             cex=c(2, 2, RclusTool.env$param$visu$cex),
                             col=c(color.select[1:2], RclusTool.env$param$visu$point.style$col[unclass(label[ids["sel"]])]))
        if ((visu.env$mem.mode) && (!(is.na(ids["mem"]))))
            graphics::points(x[rep(ids["mem"],3),,drop=FALSE], 
                             pch=c("+", "O",  RclusTool.env$param$visu$point.style$pch[unclass(label[ids["mem"]])]),
                             cex=c(2, 2, RclusTool.env$param$visu$cex),
                             col=c(color.select[1:2], RclusTool.env$param$visu$point.style$col[unclass(label[ids["mem"]])]))

        parPlotSize <- par("plt")
        width  <- as.numeric(tclvalue(tkwinfo("reqwidth", tk.plot.fig)))
        height <- as.numeric(tclvalue(tkwinfo("reqheight", tk.plot.fig)))

        usrCoords <- par("usr")
        width  <- as.numeric(tclvalue(tkwinfo("reqwidth", tk.plot.fig)))
        xMin <- parPlotSize[1] * width
        xMax <- parPlotSize[2] * width
        yMin <- parPlotSize[3] * height
        yMax <- parPlotSize[4] * height
        rangeX <- usrCoords[2] - usrCoords[1]
        rangeY <- usrCoords[4] - usrCoords[3]
        visu.env$plot.height <- height      

        # A VERIFIER
        visu.env$features$x.2D.pixel <- cbind((visu.env$features$x.2D[,1]-usrCoords[1])*(xMax-xMin)/rangeX + xMin,
                                              (visu.env$features$x.2D[,2]-usrCoords[3])*(yMax-yMin)/rangeY + yMin )
        rownames(visu.env$features$x.2D.pixel) <- rownames(visu.env$features$x.2D)
        visu.env$box.pixel <- c(xMin, xMax, yMin, yMax)
        visu.env$usrCoords <- usrCoords

        # if new plot, then search tree is rebuilt
        if (is.null(visu.env$pts.search.tree)) 
            buildPtsSearchTree()

    }
    
    plotDensity2Dcall <- function(compare=FALSE){
        if (!compare) {
        	   label <- visu.env$label
        	   clustering.name <- visu.env$clustering.name
        } else {
          	 clustering.name <- visu.env$previous.clustering.name
          	 label <- visu.env$data.sample$clustering[[clustering.name]]$label
        }
        if (is.null(visu.env$parH)) {
        	visu.env$parH <- colnames(data.sample$features[[visu.env$features.mode]]$x)[1]
    	}
    	cluster.density <- clusterDensity(data.sample, label, visu.env$features.mode, features.to.keep=visu.env$parH)
    	plotDensity2D(cluster.density,parH=visu.env$parH, clustering.name=clustering.name, col= RclusTool.env$param$visu$palette.colors)
	}

    plotDensity1D <- function(compare=FALSE){
        compare <- NULL
        x <- visu.env$features$x.2D
        xtick <- pretty(x[,1])
        ytick <- pretty(x[,2])
        logscale <- visu.env$features$logscale.2D
        labx <- xtick
        laby <- ytick
        if (length(grep("x",logscale)))
            labx <- 10^xtick
        if (length(grep("y",logscale)))
            laby <- 10^ytick

        densite <- MASS::kde2d(x[,1], x[,2], n=100, h=.1)
        densite$z <- log10( removeZeros(densite$z, threshold=10^-6, positive=TRUE) ) #echelle log...

        graphics::filled.contour(densite, color = grDevices::topo.colors,
                       plot.axes = { graphics::axis(1, at=xtick, labels=labx);
                       graphics::axis(2, at=ytick, labels=laby) })
        #      points(x)
        graphics::title(xlab=colnames(visu.env$features$x.2D)[1]) #paste(visu.env$parH[1], visu.env$parH[2], sep=" - "))
        graphics::title(ylab=colnames(visu.env$features$x.2D)[2]) #paste(visu.env$parV[1], visu.env$parV[2], sep=" - "))
        graphics::title(paste(visu.env$plot.mode, "(log)"), cex.main=visu.env$cex) 
    }

    plotSummary <- function(compare=FALSE){
        if (!compare) {
            label <- visu.env$label
            clustering.name <- visu.env$clustering.name
        } else {
            clustering.name <- visu.env$previous.clustering.name
            label <- visu.env$data.sample$clustering[[clustering.name]]$label
        }
        cluster.summary <- visu.env$data.sample$clustering[[clustering.name]]$summary

        title <- paste(visu.env$plot.mode, "clustered by", clustering.name)

        feature.name.summary <- visu.env$summary.feature
        m <- match(rownames(cluster.summary), levels(label))
        col <- RclusTool.env$param$visu$point.style$col[m]
        if (grepl("Result", feature.name.summary)) {
            feature.name.summary <- sub("Result ", "", feature.name.summary)
            cluster.factor <- factor(rownames(cluster.summary), levels = rownames(cluster.summary))
            df=data.frame(values=cluster.summary[, feature.name.summary],cluster=cluster.factor)
            p <- ggplot2::ggplot(data=df,aes(y=values,x=df$cluster,fill=df$cluster)) + geom_bar(stat="identity")
			p <- p + ggplot2::xlab("") + ggplot2::ylab(feature.name.summary)
			p <- p + ggplot2::ggtitle(title)
			p <- p + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) 
			p <- p + ggplot2::theme(axis.text.x = element_text(angle = 90, size=11))
			p <- p + ggplot2::scale_fill_manual(values=col, drop=FALSE)
			p <- p + ggplot2::theme(legend.position='none')
			p <- p + ggplot2::theme(axis.text.x = element_text(size = RclusTool.env$param$visu$size))
	        p <- p + ggplot2::theme(axis.text.y = element_text(size = RclusTool.env$param$visu$size))
	        p <- p + ggplot2::theme(plot.title = element_text(size = RclusTool.env$param$visu$size))
            print(p)
        } else {
            graphics::boxplot(visu.env$data.sample$features[[visu.env$features.mode]]$x[visu.env$data.sample$id.clean,feature.name.summary]~
                    label[visu.env$data.sample$id.clean],
                names=levels(label),
                las=3, col=RclusTool.env$param$visu$point.style$col, border=RclusTool.env$param$visu$point.style$col, outline = FALSE,
                main=title,
                ylab=feature.name.summary)
        }
    }


    # function linked to tk.plot.frame
    OnPlotSample <- function() {
        tkrreplot.RclusTool(tk.plot.fig)
    if (visu.env$compare.mode=="on"){
    	tkrreplot.RclusTool(tk.plot.compare.fig)
    }
    }


    #----------------------------------------------------------------------------------------
    # functions to deal with interactivity in Plot part (left)
    #----------------------------------------------------------------------------------------
    OnLeavePlot <- function() {
        if (is.element(visu.env$profile.mode, c("none", "constrained pairs")))
            return()
        if (visu.env$profile.id["sel"] != visu.env$profile.id["fig"])
            setProfileId("fig", visu.env$profile.id["sel"])
    }

    OnMouseMove <- function(x, y) {
        if (is.element(visu.env$profile.mode, c("none", "constrained pairs")))
            return()

        if (is.null(visu.env$pts.search.tree))
            return()

        xClick <- as.numeric(x)+0.5
        yClick <- as.numeric(y)-0.5
        yClick <- visu.env$plot.height - yClick

        if ( (xClick<visu.env$box.pixel[1])||(xClick>visu.env$box.pixel[2])||
            (yClick<visu.env$box.pixel[3])||(yClick>visu.env$box.pixel[4]) ) {
            OnLeavePlot()
            return()
        }

        #   coords <- 10^c(usrCoords[1]+(xClick-xMin)*rangeX/(xMax-xMin),
        #             usrCoords[3]+(yClick-yMin)*rangeY/(yMax-yMin))




        if (!is.null(visu.env$pts.search.tree))  {
           	w <- as.character(visu.env$slider.values[SearchTrees::knnLookup(visu.env$pts.search.tree, xClick, yClick, k=1)[1]])
           	#w <- as.character(class::knn(visu.env$features$x.2D.pixel[visu.env$slider.values,,drop=FALSE],
            	#                      c(xClick, yClick), visu.env$slider.values)) #A VERIFIER !!

        	setProfileId("fig", w)
        }
    }

    OnMovePlotFun <- function(x,y)
    {
        visu.env$onScatterPlotMove(x,y)
    }

    OnMouseClick <- function(x, y) {
        if (is.element(visu.env$profile.mode, c("none", "constrained pairs")))
            return()
        OnMouseMove(x, y)
        setProfileId("sel", visu.env$profile.id["fig"])
        OnPlotSample()
    }

    #----------------------------------------------------------------------------------------
    # plot functions of the profile(s) (right area)
    #----------------------------------------------------------------------------------------
    OnPlotProfile <- function() {
    	opar <- graphics::par(no.readonly=TRUE)
		on.exit(graphics::par(opar))
        graphics::par(bg="white")
        if (is.na(visu.env$profile.id["fig"]))
            return()
        id <- visu.env$profile.id["fig"]
        cluster <- NA
        if ( (selection.mode=="prototypes") && length(visu.env$prototypes[[visu.env$clustering.name]])){
            cluster <- visu.env$prototypes[[visu.env$clustering.name]][id]
        }
        if (is.na(cluster))
            cluster <- visu.env$features$label[id]
        if (is.null(visu.env$data.sample$config$signalColor)){
        	signalColor <- RclusTool.env$param$visu$palette.colors
        } else {
        	signalColor <- visu.env$data.sample$config$signalColor 
        }
        if (is.null(visu.env$data.sample$profiles)){
        	visu.env$ProfileModeNumber <- 2
        }       
        title.color <- RclusTool.env$param$visu$palette.colors[unclass(cluster)]
        if (visu.env$ProfileModeNumber == 1){
        	plotProfile(visu.env$data.sample$profiles[[id]], profiles.colors=signalColor,
                    	NULL, title=paste("Observation", id), 
                    	sub=paste("in", cluster), sub.color=title.color, image.dir=NULL ,charsize=fontsize)             
        } else if (visu.env$ProfileModeNumber == 2){
        	plotProfile(NULL, profiles.colors=signalColor,
                    	visu.env$data.sample$images[id], title=paste("Observation", id), 
                    	sub=paste("in", cluster), sub.color=title.color, image.dir=visu.env$data.sample$files$images, charsize=fontsize)             
		} else {
        	plotProfile(visu.env$data.sample$profiles[[id]], profiles.colors=signalColor,
                    	visu.env$data.sample$images[id], title=paste("Observation", id), 
                    	sub=paste("in", cluster), sub.color=title.color, image.dir=visu.env$data.sample$files$images, charsize=fontsize) 
        }
        tkpack(tk.profile.fig, fill="x")

    }

    OnPlotProfile2 <- function(){
    	opar <- graphics::par(no.readonly=TRUE)
		on.exit(graphics::par(opar))
        graphics::par(bg="white")
        if (is.na(visu.env$profile.id["mem"]))
            return()
        id <- visu.env$profile.id["mem"]
        cluster <- NA
        if ( (selection.mode=="prototypes") && length(visu.env$prototypes[[visu.env$clustering.name]])){
            cluster <- visu.env$prototypes[[visu.env$clustering.name]][id]
        }
        if (is.na(cluster))
            cluster <- visu.env$features$label[id]
        if (is.null(visu.env$data.sample$config$signalColor)){
        	signalColor <- RclusTool.env$param$visu$palette.colors
        } else {
        	signalColor <- visu.env$data.sample$config$signalColor 
        }
        if (is.null(visu.env$data.sample$profiles)){
        	visu.env$ProfileModeNumber <- 2
        } 
        if (visu.env$mem.mode)
        {
            title.color <- RclusTool.env$param$visu$palette.colors[unclass(cluster)] #[unclass(label[id])] ??
            if (visu.env$ProfileModeNumber == 1){
        		plotProfile(visu.env$data.sample$profiles[[id]], profiles.colors=signalColor,
                    		NULL, title=paste("Observation", id), 
                    		sub=paste("in", cluster), sub.color=title.color, image.dir=NULL , charsize=fontsize)             
        	} else if (visu.env$ProfileModeNumber == 2){
        		plotProfile(NULL, profiles.colors=signalColor,
                    		visu.env$data.sample$images[id], title=paste("Observation", id), 
                    		sub=paste("in", cluster), sub.color=title.color, image.dir=visu.env$data.sample$files$images ,charsize=fontsize)             
			} else {
        		plotProfile(visu.env$data.sample$profiles[[id]], profiles.colors=signalColor,
                    		visu.env$data.sample$images[id], title=paste("Observation", id), 
                    		sub=paste("in", cluster), sub.color=title.color, image.dir=visu.env$data.sample$files$images, charsize=fontsize) 
        }
        tkpack(tk.profile.mem, fill="x")

        } else
            tkpack.forget(tk.profile.mem)
    }

    #----------------------------------------------------------------------------------------
    # Menu functions 
    #----------------------------------------------------------------------------------------  
    OnModifPlotMode <- function() {
        plot.mode.old <- visu.env$plot.mode
        visu.env$plot.mode <- tclvalue(tcl.plot.mode)

        visu.env$plotSampleFunction <- switch(visu.env$plot.mode,
                                      "scatter-plot"=plotScatter,
                                      "density-plot"=plotDensity1D,
                                      "variables density by cluster"=plotDensity2Dcall,
                                      "clusters summary"={
                                          if (!is.null(visu.env$cluster.summary)){
                                              plotSummary
                                          } else {
                                              visu.env$plot.mode <- plot.mode.old
                                              tclvalue(tcl.plot.mode) <- visu.env$plot.mode
                                              visu.env$plotSampleFunction
                                          }
                                      }
                                      )

        visu.env$onScatterPlotMove <- function(x,y){}
        if (visu.env$plot.mode == "scatter-plot")
            visu.env$onScatterPlotMove <- OnMouseMove

        visibilityProfile() #hides/shows Profiles frame according to "cluster summary" mode or not
        
        OnPlotSample()
    }


    OnModifFeaturesMode <- function() {
        visu.env$features.mode <- tclvalue(tcl.features.mode)
        tkdelete(tk.axis.menu, 0, "end")

        visu.env$features <- visu.env$data.sample$features[[visu.env$features.mode]]
        visu.env$features$x <- as.data.frame(visu.env$features$x)
        if (visu.env$features.mode=="initial")
            visu.env$features$x <- visu.env$features$x[sapply(visu.env$features$x,is.numeric)]

        visu.env$features$label <- visu.env$label[rownames(visu.env$features$x)]

        initAxis()

        buildMenuInitialFeatures()
        OnModifProfileMode()
    }
    
    # Transform parameters button
    OnModifAxisH <- function() {OnModifAxisHV(Horizontal=TRUE)}
    OnModifAxisV <- function() {OnModifAxisHV(Horizontal=FALSE)}

    OnModifAxisHV <- function(Horizontal) {
        feat.names <- colnames(visu.env$features$x)
        if (!length(feat.names))
            return()

        if (Horizontal) {
            axis.name <- "Horizontal"
            feat.select <- visu.env$parH
        } else {
            axis.name <- "Vertical"
            feat.select <- visu.env$parV 
        }

        # Create new window for the parameter transformation
        onmodiftt <- tktoplevel()
        tktitle(onmodiftt) <- paste("Select", axis.name, "axis")
        #Search the selected parameter
        tkgrid(tk2label(onmodiftt, text = "     "), row = 1, sticky = "w")

        # First listbox with all parameters
        feat.list <- tk2listbox(onmodiftt, selectmode = "single", activestyle = "dotbox", 
                                height = 10, width = 27, autoscroll = "none", background = "white")
        tkgrid(feat.list, row = 2, column = 0, rowspan = 4, sticky = "w")
        # Close selection window operation
        onClose <- function() {
            tkdestroy(onmodiftt)
        }
        butClose <- tk2button(onmodiftt, text = "Close", width = -6, command = onClose)
        tkgrid(butClose, row = 5, column=1, columnspan = 2)

        for (feat in feat.names) {
            tkinsert(feat.list, "end", feat)
        }

        OnFeatSelectH <- function()
        {
            visu.env$parH <- feat.names[as.numeric(tkcurselection(feat.list)) + 1]
            modifAxisUpdate()
            OnPlotSample()
            buildMenuInitialFeatures() 
        }

        OnFeatSelectV <- function()
        {
            visu.env$parV <- feat.names[as.numeric(tkcurselection(feat.list)) + 1]
            modifAxisUpdate()
            OnPlotSample()
            buildMenuInitialFeatures() 
        }

        # Initialize selection in the two listboxes
        tkselection.set(feat.list, which(feat.names==feat.select)-1)
        if (Horizontal) {
            tkbind(feat.list, "<ButtonRelease-1>", OnFeatSelectH)
        } else {
            tkbind(feat.list, "<ButtonRelease-1>", OnFeatSelectV)
        }
    }
    
    OnSimpleFeatSummary <- function(feat.name) {
        tclvalue(tcl.plot.mode) <- "clusters summary"
        visu.env$summary.feature <- paste("Result", feat.name)
        OnModifPlotMode()
    }

    OnFeatFunSummary <- function() {
        tclvalue(tcl.plot.mode) <- "clusters summary"
        summary.names <- extractFeaturesFromSummary(visu.env$data.sample$clustering[[visu.env$clustering.name]]$summary)
        summary.names1 <- summary.names$feats
        summary.names2  <- summary.names$funs
        if (!length(summary.names1))
            return()
        if (!is.null(visu.env$summary.feat.select) && !is.null(visu.env$summary.fun.select))  {
            summary.select1 <- visu.env$summary.feat.select 
            summary.select2 <- visu.env$summary.fun.select
        } else {
            summary.select1 <- summary.names1[1]
            summary.select2 <- summary.names2[1]
        }
        # Create new window for the parameter transformation
        onmodifcl <- tktoplevel()
        tktitle(onmodifcl) <- paste("Select Cluster Summary")
        #Search the selected parameter
        tkgrid(tk2label(onmodifcl, text = "     "), row = 1, sticky = "w")
        # First listbox with all parameters
        tkgrid(tk2label(onmodifcl, text = "Select the feature", justify = "left"),
               row = 0, column = 0, sticky = "w")
        summary.list1 <- tk2listbox(onmodifcl, selectmode = "single", activestyle = "dotbox", 
                                    height = 10, width = 27, autoscroll = "none", background = "white")
        tkgrid(summary.list1, row = 1, column = 0, rowspan = 4, sticky = "w")
        # Second listbox with all parameters
        tkgrid(tk2label(onmodifcl, text = "Select the parameter", justify = "left"),
               row = 0, column = 1, sticky = "w")
        summary.list2 <- tk2listbox(onmodifcl, selectmode = "single", activestyle = "dotbox", 
                                    height = 10, width = 27, autoscroll = "none", background = "white")
        tkgrid(summary.list2, row = 1, column = 1, rowspan = 4, sticky = "w")
        # Close selection window operation
        onClose <- function() {
            tkdestroy(onmodifcl)
        }
        butClose <- tk2button(onmodifcl, text = "Close", width = -6, command = onClose)
        tkgrid(butClose, row = 8, column=1, columnspan = 2)

        for (summary1 in summary.names1) {
            tkinsert(summary.list1, "end", summary1)
        }
        for (summary2 in summary.names2) {
            tkinsert(summary.list2, "end", summary2)
        }

        onSummarySelect <- function()
        {
            visu.env$summary.feat.select <- summary.names1[as.numeric(tkcurselection(summary.list1)) + 1]
            visu.env$summary.fun.select <- summary.names2[as.numeric(tkcurselection(summary.list2)) + 1]
            value1 <- paste("Result", visu.env$summary.feat.select)
            visu.env$summary.feature <- paste(value1, visu.env$summary.fun.select, sep="...")
            OnModifPlotMode()
        }

        # Initialize selection in the two listboxes
        tkselection.set(summary.list1, which(summary.names1==summary.select1)-1)
        tkselection.set(summary.list2, which(summary.names2==summary.select2)-1)
        tkbind(summary.list1, "<ButtonRelease-1>", onSummarySelect)
        tkbind(summary.list2, "<ButtonRelease-1>", onSummarySelect)  
    }


    modifAxisUpdate <- function() {
        visu.env$features$x.2D <- visu.env$features$x[, c(visu.env$parH, visu.env$parV), drop=FALSE]
        visu.env$features$x.2D.logscale <- visu.env$features$x.2D #representation for plot functions with log scale

        logscale <- ""
        if (visu.env$features$logscale[visu.env$parH]) {
            logscale <- "x"
            visu.env$features$x.2D.logscale[,1] <- 10^visu.env$features$x.2D[,1]
        }
        if (visu.env$features$logscale[visu.env$parV]) {
            logscale <- paste(logscale, "y", sep="")
            visu.env$features$x.2D.logscale[,2] <- 10^visu.env$features$x.2D[,2]
        }
        visu.env$features$logscale.2D <- logscale

	# force rebuilding of the search tree
        visu.env$pts.search.tree <- NULL
    }

    #without plotSample
    initAxis <- function() {
        pref.features <- NULL
        #if (visu.env$features.mode == "initial")

        # pref.features must be in colnames(features$x)!
        # (if preprocessed data, with "log" suffix...)
        # pref.features <- colnames(features$x)[grep(paste(visu.env$data.sample$config$defaultFeat, collapse = "|"), colnames(features$x))] #regular expression
        pref.features <- intersect(visu.env$data.sample$config$defaultFeat, colnames(visu.env$features$x))
        if (!is.null(pref.features) & (length(pref.features)>=2))
            names(pref.features)[1:2] <- c("parH","parV")
        #pref.features <- visu.env$data.sample$config$defaultFeat
        if ( is.null(pref.features) || (length(pref.features)<2)
            || !any(colnames(visu.env$features$x)==pref.features["parH"])
            || !any(colnames(visu.env$features$x)==pref.features["parV"]) )
            pref.features <- colnames(visu.env$features$x)[1:2]

        visu.env$parH <- pref.features[1]
        visu.env$parV <- pref.features[2]

        modifAxisUpdate()
    }

    visibilityProfile <- function()
    {
        # hide profiles
        if (visu.env$plot.mode != "scatter-plot")
        {
            tkpack.forget(tk.profile.frame)
            return()
        } 

        if (visu.env$profile.mode=="none") {
            tkpack.forget(tk.profile.frame)
            return()
        }
        tkpack(tk.profile.frame, side="left")
 }

    OnModifProfileMode <- function() {
        new.mode <- tclvalue(tcl.profile.mode)
        tkpack.forget(tk.profile.frame)

        # if new mode not possible
        if (!length(visu.env$prototypes) && ((new.mode=="prototypes"))){
            tclvalue(tcl.profile.mode) <- visu.env$profile.mode
            return()
        }
        if (!length(visu.env$pairs.values) && ((new.mode=="constrained pairs"))){
            tclvalue(tcl.profile.mode) <- visu.env$profile.mode
            return()
        }

        visu.env$profile.mode <- new.mode

        # select particles able to be selected -> visu.env$slider.values
        switch(visu.env$profile.mode,
               "none"=c(),
               "whole sample"=sortCharAsNum(rownames(visu.env$features$x)), 
               "items with signal(s)"=sortCharAsNum(rownames(signals)),
               "items with image(s)"=sortCharAsNum(names(images)),
               "prototypes"= names(visu.env$prototypes[[visu.env$clustering.name]][order(visu.env$prototypes[[visu.env$clustering.name]])]),
               "constrained pairs"=visu.env$pairs.values,
               { #default
                   cluster.num <- tclvalue(tcl.profile.mode)
                   sortCharAsNum(rownames(visu.env$features$x)[visu.env$features$label==cluster.num])
               }
               ) -> values

        # if no particles can be selected : go back
        if (!length(values)){
            visu.env$profile.mode <- "none"
            tclvalue(tcl.profile.mode) <- visu.env$profile.mode
            OnPlotSample()
            return()
        }

        configureSliders(values) #settings BEFORE positioning !
        buildPtsSearchTree()

        if (visu.env$profile.mode=="constrained pairs") {
            setMemMode(TRUE)
            OnSliderMove()
        } else{
            updateProfileId()
        }

        if (visu.env$profile.mode=="none")
            setMemMode(FALSE)

        OnPlotSample()

        tkpack(tk.profile.frame, side="left")
    }


    #----------------------------------------------------------------------------------------
    # Slider functions 
    #----------------------------------------------------------------------------------------  

    setSlidersId <- function(id, value=FALSE) {
        if (value)
            id <- match(id, visu.env$slider.values)
        slider.tune.max <- min(slider.max, length(visu.env$slider.values))
        val.big <- floor((id-1)/slider.tune.max)*slider.tune.max
        val.tune <-  id - val.big
        tclvalue(tcl.slider.big) <-  as.integer(val.big)
        tclvalue(tcl.slider.tune) <- as.integer(val.tune)
    }

    getSlidersValue <- function(value=TRUE) {
        if (tclvalue(tcl.slider.tune)=="0")
            tclvalue(tcl.slider.tune) <- 1
        id <- as.integer(tclvalue(tcl.slider.big)) + as.integer(tclvalue(tcl.slider.tune))
        if (id > length(visu.env$slider.values)) {
            tclvalue(tcl.slider.tune) <-  length(visu.env$slider.values) - as.integer(tclvalue(tcl.slider.big))
            id <- length(visu.env$slider.values)
        }
        switch(value+1, id, visu.env$slider.values[id])
    }

    buildPtsSearchTree <- function()
    {

        #search tree not useful => not computed
        if (is.element(visu.env$profile.mode, c("none", "constrained pairs")))
            return()

        #data required
        if (is.null(visu.env$features$x.2D.pixel) || !length(visu.env$slider.values))
            return()

        visu.env$pts.search.tree <- SearchTrees::createTree(visu.env$features$x.2D.pixel[visu.env$slider.values,,drop=FALSE])
    }

    configureSliders <- function(values, reset=TRUE){
        visu.env$slider.values <- values
        slider.tune.max <- min(slider.max, length(visu.env$slider.values))
        slider.big.max <- (ceiling((length(visu.env$slider.values))/slider.tune.max)-1)*slider.tune.max
        tkconfigure(tk.slider.big, from=0, to=slider.big.max, res=slider.tune.max)
        tkconfigure(tk.slider.big.label, text=paste("big tuning\n(step = ", slider.tune.max, ")", sep=""))
        tkconfigure(tk.slider.tune, from=1, to=slider.tune.max, res=1)
        tkconfigure(tk.slider.tune.label, text=paste("fine tuning\n(from 1 to ", slider.tune.max,")", sep=""))

        if (!reset) {
            setSlidersId( getSlidersValue(value=FALSE), value=FALSE)
        } else {
            tclvalue(tcl.slider.big) <- 0
            tclvalue(tcl.slider.tune) <- 1
        }
    }

    #binded to tk.slider... Warning : button release dealt by OnPlotSample
    #updates profile.*.id and plot profiles
    OnSliderMove <- function(...) {
        if (visu.env$profile.mode=="constrained pairs") {
            setProfileIdsPair(getSlidersValue())
            OnPlotSample()
        } else {
            setProfileId("sel", getSlidersValue())
            setProfileId("fig", getSlidersValue())
        }
    }

    updateProfilesPlot <- function(names=c("fig","mem")) {
        if (is.element("fig", names))
            tkrreplot(tk.profile.fig)
        if (is.element("mem", names))
            tkrreplot(tk.profile.mem)

        if (visu.env$mem.mode) {
            pair.status <- ""
            pair <- c(visu.env$profile.id["sel"], visu.env$profile.id["mem"])
            w <- which.pair(pair, visu.env$pairs.values)
            if (!is.null(w))
                pair.status <- switch(2+visu.env$pairs.constraints[w], "CNL","", "ML")
            tkconfigure(tk.pairs.label, text=paste(pair.status, "Pair", paste(pair,
                                                                              collapse=",")))
        }

        if (selection.mode=="prototypes"){
            w <- visu.env$profile.id["sel"]
            if (!is.na(w))
            {
                cluster <- NA
                if (length(visu.env$prototypes[[visu.env$clustering.name]]))
                	cluster <- visu.env$prototypes[[visu.env$clustering.name]][w]
                if (is.na(cluster))
                    cluster <- as.character(visu.env$features$label[w])
                tclvalue(visu.env$tcl.proto.label) <- cluster
            }
        }
    }

    #----------------------------------------------------------------------------------------
    # Pairs functions 
    #----------------------------------------------------------------------------------------  

    setMemMode <-function(bool) {
        visu.env$mem.mode <- bool
        if (!visu.env$mem.mode) {
            tkpack.forget(tk.profile.mem)
        } else
            tkpack(tk.profile.mem, fill="x")
    }

    OnMemHide <- function() {
        setMemMode(FALSE)
        OnPlotSample()
    }
    
    ChangeProfileMode <- function() {
        if (visu.env$ProfileModeNumber < 3){
            visu.env$ProfileModeNumber <- visu.env$ProfileModeNumber + 1
        } else {
            visu.env$ProfileModeNumber <- 1
        }
        OnPlotProfile()
        OnPlotProfile2()
    }

    OnMem <- function() {
        setMemMode(TRUE)
        setProfileId("mem", visu.env$profile.id["sel"]) 
        OnPlotSample()
    }

    OnML <- function() {
        if (!visu.env$mem.mode)
            return() # message
        pair <- c(visu.env$profile.id["sel"], visu.env$profile.id["mem"])
        w <- which.pair(pair, visu.env$pairs.values)
        if (is.null(w)){
            visu.env$pairs.values <- c(visu.env$pairs.values, list(pair))
            visu.env$pairs.constraints <- c(visu.env$pairs.constraints, +1)
        } else
            visu.env$pairs.constraints[w] <- +1
        tkconfigure(tk.pairs.label, text=paste("ML", "Pair", paste(pair, collapse=",")))
        OnPlotSample()
    }

    OnCNL <- function() {
        if (!visu.env$mem.mode)
            return() # message
        pair <- c(visu.env$profile.id["sel"], visu.env$profile.id["mem"])
        w <- which.pair(pair, visu.env$pairs.values)
        if (is.null(w)){
            visu.env$pairs.values <- c(visu.env$pairs.values, list(pair))
            visu.env$pairs.constraints <- c(visu.env$pairs.constraints, -1)
        } else
            visu.env$pairs.constraints[w] <- -1
        tkconfigure(tk.pairs.label, text=paste("CNL", "Pair", paste(pair, collapse=",")))
        OnPlotSample()
    }

    OnCancelPair <- function() {
        if (!visu.env$mem.mode)
            return() # message
        pair <- c(visu.env$profile.id["sel"], visu.env$profile.id["mem"])
        w <- which.pair(pair, visu.env$pairs.values)
        if (!is.null(w)){
            visu.env$pairs.values <- visu.env$pairs.values[-w]
            visu.env$pairs.constraints <- visu.env$pairs.constraints[-w]
        }
        tkconfigure(tk.pairs.label, text=paste("Pair", paste(pair, collapse=",")))
        if (visu.env$profile.mode=="constrained pairs"){
            if (!length(visu.env$pairs.values)) {
                tclvalue(tcl.profile.mode) <- "none"
                OnModifProfileMode()
            } else {
                configureSliders(visu.env$pairs.values, reset=FALSE) #settings BEFORE positioning !
                OnSliderMove()
                OnPlotSample()      
            }
        }
    }


    OnValidPairs <- function() {
        tkdestroy(tt)
    }

    which.pair <- function(pair, list.of.pairs) {
        is.equal <- sapply(list.of.pairs, function(x) length(intersect(x, pair))==2)
        if (!length(is.equal))
            return()
        res <- which(is.equal)
        if (!length(res))
            return()
        res
    }


    #----------------------------------------------------------------------------------------
    # Prototypes functions 
    #----------------------------------------------------------------------------------------  

    OnRenameCluster <- function() {
        id <- visu.env$profile.id["sel"]
        if (!nchar(tclvalue(visu.env$tcl.proto.label))) {
            tclvalue(visu.env$tcl.proto.label) <- as.character(visu.env$label[id])
        } 

        if (!is.na(id)){ ###A verifier
            levels(visu.env$label)[which(levels(visu.env$label)==visu.env$label[id])] <- tclvalue(visu.env$tcl.proto.label)
            visu.env$data.sample$clustering[[visu.env$clustering.name]]$label <- visu.env$label
        }
        buildMenuClustering()
        buildMenuProfileMode()
        OnModifProfileMode()
        OnModifFeaturesMode()
        OnPlotSample()
        updateProfilesPlot()
    }

    OnSelectProto <- function() {
        id <- visu.env$profile.id["sel"]
        if (!nchar(tclvalue(visu.env$tcl.proto.label))) {
            tclvalue(visu.env$tcl.proto.label) <- as.character(visu.env$label[id])
        } 

        if (is.na(id)){ ###A verifier
            visu.env$prototypes[[visu.env$clustering.name]] <- c(visu.env$prototypes[[visu.env$clustering.name]], tclvalue(visu.env$tcl.proto.label))
            names(visu.env$prototypes[[visu.env$clustering.name]])[length(visu.env$prototypes[[visu.env$clustering.name]])] <- id
        } else {
            visu.env$prototypes[[visu.env$clustering.name]][id] <- tclvalue(visu.env$tcl.proto.label)
            levels(visu.env$label)[which(levels(visu.env$label)==visu.env$label[id])] <- tclvalue(visu.env$tcl.proto.label)
            visu.env$data.sample$clustering[[visu.env$clustering.name]]$label <- visu.env$label
        }
        buildMenuClustering()
        buildMenuProfileMode()
        OnModifProfileMode()
        OnModifFeaturesMode()
        OnPlotSample()
        updateProfilesPlot()
    }


    OnCancelProto <- function() {
        id <- visu.env$profile.id["sel"]
        if (!is.na(id)){
            visu.env$prototypes[[visu.env$clustering.name]] <- visu.env$prototypes[[visu.env$clustering.name]][names(visu.env$prototypes[[visu.env$clustering.name]]) != id]
            if (visu.env$profile.mode=="prototypes"){
                if (!length(visu.env$prototypes)) {
                    tclvalue(tcl.profile.mode) <- "none"
                    OnModifProfileMode()
                } else {
                    configureSliders(names(visu.env$prototypes[[visu.env$clustering.name]][order(visu.env$prototypes[[visu.env$clustering.name]]$label)]), reset=FALSE) #settings BEFORE positioning !
                    OnSliderMove()
                } 
                OnPlotSample()      
            }
            updateProfilesPlot()
        }
    }

    OnLeaveProto <- function() {
        id <- visu.env$profile.id["sel"]
        if (!is.na(id) && nchar(tclvalue(visu.env$tcl.proto.label))&&(!is.na(visu.env$prototypes[[visu.env$clustering.name]][id]))){
            #prototypes[w] <- tclvalue(visu.env$tcl.proto.label)
            visu.env$prototypes[[visu.env$clustering.name]][id] <- tclvalue(visu.env$tcl.proto.label)
            updateProfilesPlot()
        }  
    }


    #----------------------------------------------------------------------------------------
    # File clustering functions 
    #----------------------------------------------------------------------------------------  

    OnModifClusteringMode <- function() {
        visu.env$previous.clustering.name <- visu.env$clustering.name
        visu.env$clustering.name <- tclvalue(tcl.clustering.mode)

        if (visu.env$clustering.name=="import clustering"){
            filename.csv <- tclvalue(tkgetOpenFile(initialfile = "",
                                                   filetypes="{{csv Files} {.csv .CSV}}"))
            res <- buildClusteringSample(filename.csv, visu.env$data.sample, RclusTool.env$param$preprocess$noise.cluster)
            if ( !is.null(res) ) {
                visu.env$clustering.name <- basename(filename.csv)
                visu.env$data.sample$clustering[[visu.env$clustering.name]] <- res
                visu.env$data.sample$clustering <-
                    c(visu.env$data.sample$clustering[-which(names(visu.env$data.sample$clustering)==visu.env$clustering.name)],
                      visu.env$data.sample$clustering[visu.env$clustering.name])
            } else {
                visu.env$clustering.name <- names(visu.env$data.sample$clustering[1])
            }
        }
        tclvalue(tcl.clustering.mode) <- visu.env$clustering.name

        visu.env$label <- visu.env$data.sample$clustering[[visu.env$clustering.name]]$label
        visu.env$cluster.summary <- visu.env$data.sample$clustering[[visu.env$clustering.name]]$summary

        if (visu.env$previous.clustering.name!=visu.env$clustering.name)
            visu.env$previous.clustering.name <- visu.env$previous.clustering.name

        buildMenuClustering()
        buildMenuProfileMode()
        OnModifProfileMode()
        OnModifFeaturesMode()
        OnModifPlotMode()
    }

    OnModifCompareMode <- function() {
        visu.env$compare.mode <- tclvalue(tcl.compare.mode)

        if (is.null(visu.env$previous.clustering.name) || (visu.env$previous.clustering.name == visu.env$clustering.name)) {
            visu.env$compare.mode <- "off"
            tclvalue(tcl.compare.mode) <- visu.env$compare.mode
            return()
        }

        if (visu.env$compare.mode=="off") {
            tkpack.forget(tk.plot.compare.frame)
        } else {
            if (visu.env$compare.mode=="comparison measure") {
                warning("Function not implemented yet.")
                #compareClusterings(visu.env$data.sample, c(visu.env$clustering.name, visu.env$previous.clustering.name))
                visu.env$compare.mode <- "on" # env assignment should be better.
                tclvalue(tcl.compare.mode) <- visu.env$compare.mode
            }

            tkpack(tk.plot.compare.frame, side="left", anchor="n")
            OnPlotSample()
        }
    }

    OnExportClustering <- function() {
        filename.init <- paste("clustering ", RclusTool.env$gui$user.name, " ",
                               visu.env$clustering.name, ".csv", sep="")

        rep.init <- visu.env$data.sample$files$results$clustering

        params <- list(initialfile = filename.init, initialdir = rep.init,
                                               filetypes="{{csv Files} {.csv .CSV}}")
        if (!length(rep.init))
            params$initialdir <- NULL

        filename.csv <- tclvalue(do.call(tkgetSaveFile, params))
                                               
		rep.final <- substr(filename.csv, 1, (nchar(filename.csv)-nchar(basename(filename.csv)))-1)
		
        # saveClustering(filename.csv, label=visu.env$label, id.abs=rownames(visu.env$data.sample$features[[1]]$x))
        saveClustering(filename.init, label=visu.env$label, dir=rep.final)
        visu.env$data.sample$clustering[[visu.env$clustering.name]] <- buildClusteringSample(filename.csv, visu.env$data.sample, RclusTool.env$param$preprocess$noise.cluster)
    }

    OnExportSummary <- function() {
        if (is.null(visu.env$cluster.summary)) {
            tkmessageBox(message="First, select a clustering.")
            return()
        }

        filename.init <- paste(visu.env$data.sample$name, " summary ", RclusTool.env$gui$user.name, " ",
                               visu.env$clustering.name, ".csv", sep="")

        rep.init <- visu.env$data.sample$files$results$clustering

        params <- list(initialfile = filename.init, initialdir = rep.init,
                                               filetypes="{{csv Files} {.csv .CSV}}")
        if (!length(rep.init))
            params$initialdir <- NULL

        filename.csv <- tclvalue(do.call(tkgetSaveFile, params))
                                      
        rep.final <- substr(filename.csv, 1, (nchar(filename.csv)-nchar(basename(filename.csv)))-1)

        #     saveSummary(filename.csv=filename.csv, cluster.summary=visu.env$cluster.summary,
        #                 data.sample=visu.env$data.sample, operator.name=RclusTool.env$gui$user.name, method.name=visu.env$clustering.name)
        saveSummary(filename.csv=filename.init, cluster.summary=visu.env$cluster.summary, dir=rep.final)
    }

    # OnCountItems <- function() {
    #   if (is.character(visu.env$data.sample$files$images)) {
    #     #dev.off()
    #     countItemsSampleGUI(visu.env$data.sample, RclusTool.env=RclusTool.env)
    #   } else 
    #     tkmessageBox(message = "No images to process!", icon = "warning", type = "ok")
    # }
    # 
    # OnApplyItems <- function() {
    #   computeItemsSampleGUI(visu.env$data.sample, method.select = visu.env$clustering.name, RclusTool.env=RclusTool.env)
    # }


    #----------------------------------------------------------------------------------------
    # GUI 
    #----------------------------------------------------------------------------------------  
    tt <- tktoplevel(bg="white");
    tkwm.title(tt,"Sample visualization")

    #  tkwm.geometry(tt,"670x680+0+0")

    #--------------------
    #definition of menus
    #--------------------

    buildMenuInitialFeatures <- function() {
        tkdelete(tk.axis.menu, 0, "end")
        tkadd(tk.axis.menu, "command", label=paste("H-axis:",visu.env$parH), command=cat)
        tkadd(tk.axis.menu, "command", label=paste("V-axis:",visu.env$parV), command=cat)
        tkadd(tk.axis.menu, "command", label="Modify H-axis", command=OnModifAxisH)
        tkadd(tk.axis.menu, "command", label="Modify V-axis", command=OnModifAxisV)
    }

    buildMenuProfileMode <- function() {
        tkdelete(tk.profile.mode.menu, 0, "end")

        tkadd(tk.profile.mode.menu, "radio", label="none", variable=tcl.profile.mode,
              command=OnModifProfileMode)
        tkadd(tk.profile.mode.menu, "radio", label="whole sample", variable=tcl.profile.mode,
              command=OnModifProfileMode)
        tkadd(tk.profile.mode.menu, "radio", label="items with signal(s)", variable=tcl.profile.mode,
              command=OnModifProfileMode)
        tkadd(tk.profile.mode.menu, "radio", label="items with image(s)", variable=tcl.profile.mode,
              command=OnModifProfileMode)
        #  if (!is.null(prototype.ids))
        tkadd(tk.profile.mode.menu, "radio", label="prototypes", variable=tcl.profile.mode,
              command=OnModifProfileMode)
        tkadd(tk.profile.mode.menu, "radio", label="constrained pairs", variable=tcl.profile.mode,
              command=OnModifProfileMode)
        for (lev in levels(visu.env$label))
            tkadd(tk.profile.mode.menu, "radio", label=lev, variable=tcl.profile.mode,
                  command=OnModifProfileMode)
    }

    buildMenuSummary <- function(){
        tkdelete(tk.summary.mode.menu, 0, "end")
        summary.feats <- extractFeaturesFromSummary(visu.env$data.sample$clustering[[visu.env$clustering.name]]$summary)

        if (!is.null(summary)){
            for (s in summary.feats$simple.feats)
                tkadd(tk.summary.mode.menu, "command", label=s, command=function(){ OnSimpleFeatSummary(s)})
            tkadd(tk.summary.mode.menu, "command", label="Select Your Parameters", command=OnFeatFunSummary)
        }
    }
	
    buildMenuClustering <- function() {
        tkdelete(tk.clustering.menu, 0, "end")

        tkadd(tk.clustering.menu, "radio", label="import clustering", variable=tcl.clustering.mode,
              command=OnModifClusteringMode)
        tkadd(tk.clustering.menu, "command", label="export clustering", command=OnExportClustering)
        tkadd(tk.clustering.menu, "command", label="export summary", command=OnExportSummary)
        # tkadd(tk.clustering.menu, "command", label="count items from images", command=OnCountItems)
        # tkadd(tk.clustering.menu, "command", label="apply items counts models", command=OnApplyItems)

        for (name in names(visu.env$data.sample$clustering)) 
            tkadd(tk.clustering.menu, "radio", label=name, variable=tcl.clustering.mode,
                  command=OnModifClusteringMode)

    }
    
    tk.topmenu <- tkmenu(tt, tearoff=FALSE)
    tkconfigure(tt, menu=tk.topmenu)
    tkconfigure(tk.topmenu, font=visu.env$tk.font) 
    tk.plot.mode.menu <- tkmenu(tk.topmenu, tearoff=FALSE)
    tkconfigure(tk.plot.mode.menu, font=visu.env$tk.font) 
    tkadd(tk.topmenu, "cascade", label="Plot type", menu=tk.plot.mode.menu)
    tkadd(tk.plot.mode.menu, "radio", label="scatter-plot", variable=tcl.plot.mode,
          value="scatter-plot", command=OnModifPlotMode)
    tkadd(tk.plot.mode.menu, "radio", label="density-plot", variable=tcl.plot.mode,
          value="density-plot", command=OnModifPlotMode)
    tkadd(tk.plot.mode.menu, "radio", label="clusters summary", variable=tcl.plot.mode,
          value="clusters summary", command=OnModifPlotMode)
    tkadd(tk.plot.mode.menu, "radio", label="variables density by cluster", variable=tcl.plot.mode,
          value="variables density by cluster", command=OnModifPlotMode)
    tk.features.mode.menu <- tkmenu(tk.topmenu, tearoff=FALSE)
    tkconfigure(tk.features.mode.menu, font=visu.env$tk.font) 
    tkadd(tk.topmenu, "cascade", label="Features space", menu=tk.features.mode.menu)
    for (name in sortCharAsNum(names(visu.env$data.sample$features)))
        tkadd(tk.features.mode.menu, "radio", label=featSpaceNameConvert(name, short2long=TRUE, RclusTool.env), variable=tcl.features.mode,
              value=name, command=OnModifFeaturesMode)
    tk.axis.menu <- tkmenu(tk.topmenu, tearoff=FALSE)
    tkconfigure(tk.axis.menu, font=visu.env$tk.font) 
    tkadd(tk.topmenu, "cascade", label="Scatter Plot-axis", menu=tk.axis.menu)

    if (!is.null(visu.env$cluster.summary)) {
        tk.summary.mode.menu <- tkmenu(tk.topmenu, tearoff=FALSE)
        tkconfigure(tk.summary.mode.menu, font=visu.env$tk.font) 
        tkadd(tk.topmenu, "cascade", label="Clusters summaries", menu=tk.summary.mode.menu)
        buildMenuSummary()
    }

    tk.profile.mode.menu <- tkmenu(tk.topmenu, tearoff=FALSE)
    tkconfigure(tk.profile.mode.menu, font=visu.env$tk.font) 
    tkadd(tk.topmenu, "cascade", label="Signals/Images view", menu=tk.profile.mode.menu)

    tk.clustering.menu <- tkmenu(tk.topmenu, tearoff=FALSE)
    tkconfigure(tk.clustering.menu, font=visu.env$tk.font) 
    tkadd(tk.topmenu, "cascade", label="Clustering", menu=tk.clustering.menu)

    tk.compare.menu <- tkmenu(tk.topmenu, tearoff=FALSE)
    tkconfigure(tk.compare.menu, font=visu.env$tk.font)
    tkadd(tk.topmenu, "cascade", label="Comparison", menu=tk.compare.menu)
    tkadd(tk.compare.menu, "radio", label="off", variable=tcl.compare.mode,
          value="off", command=OnModifCompareMode)
    tkadd(tk.compare.menu, "radio", label="on", variable=tcl.compare.mode,
          value="on", command=OnModifCompareMode)
    #tkadd(tk.compare.menu, "radio", label="comparison measure", variable=tcl.compare.mode,
    #      value="comparison measure", command=OnModifCompareMode)


    buildMenuProfileMode()
    buildMenuClustering()


    #------------------
    #frames and figures
    #------------------
    tk.plot.frame <- tkframe(tt, bg="white")
    tk.profile.frame <- tkframe(tt, bg="white")
    visu.env$plotSampleFunction <- function(...){plot(1)}
    tk.plot.fig <- tkrplot.RclusTool(tk.plot.frame, fun=plotSample, hscale=RclusTool.env$param$visu$hscale+0.4, vscale=RclusTool.env$param$visu$hscale+0.2)
    tkbind(tk.plot.fig, "<Motion>", OnMovePlotFun)
    tkbind(tk.plot.fig, "<Button-1>", OnMouseClick)
    tkbind(tk.plot.fig, "<Leave>", OnLeavePlot)

    tk.plot.compare.frame <- tkframe(tt, bg="white")
    tk.plot.compare.fig <- tkrplot.RclusTool(tk.plot.compare.frame, fun=plotSampleCompare, hscale=RclusTool.env$param$visu$hscale+0.4, vscale=RclusTool.env$param$visu$hscale+0.2)

    tk.profile.fig <- tkrplot(tk.profile.frame, fun=OnPlotProfile, hscale=(RclusTool.env$param$visu$hscale)*RclusTool.env$param$visu$scale.graphics, vscale=0.6*RclusTool.env$param$visu$hscale*RclusTool.env$param$visu$scale.graphics)
    tk.profile.mem <- tkrplot(tk.profile.frame, fun=OnPlotProfile2, hscale=(RclusTool.env$param$visu$hscale)*RclusTool.env$param$visu$scale.graphics, vscale=0.6*RclusTool.env$param$visu$hscale*RclusTool.env$param$visu$scale.graphics)
    tk.slider.frame <- tkframe(tk.profile.frame, bg="white")
    tk.slider.left.frame <- tkframe(tk.slider.frame, bg="white")
    tk.slider.center.frame <- tkframe(tk.slider.frame, bg="white")
    tk.slider.right.frame <- tkframe(tk.slider.frame, bg="white")
    tk.slider.big <- tkscale(tk.slider.center.frame, length=450, bg="white",
                             command=OnSliderMove,
                             showvalue=TRUE, variable=tcl.slider.big, orient="horizontal")
    tk.slider.tune <- tkscale(tk.slider.center.frame, length=450, bg="white",
                              command=OnSliderMove,
                              showvalue=TRUE, variable=tcl.slider.tune, orient="horizontal")
    tk.slider.tune.label <- tklabel(tk.slider.right.frame, text="fine tuning", fg="black", bg="white", font=visu.env$tk.font)
    tk.slider.big.label <- tklabel(tk.slider.right.frame, text="big tuning", fg="black", bg="white", font=visu.env$tk.font)
    tkbind(tk.slider.big, "<ButtonRelease-1>", OnPlotSample) #Sample plotted only when mouse released
    tkbind(tk.slider.big, "<ButtonRelease-2>", OnPlotSample) #Sample plotted only when mouse released
    tkbind(tk.slider.tune, "<ButtonRelease-1>", OnPlotSample)
    tkbind(tk.slider.tune, "<ButtonRelease-2>", OnPlotSample)
    tk.mem.but <- tkbutton(tk.slider.left.frame, text="MR", command=OnMem, font=visu.env$tk.font)
    tk.mem.hide.but <- tkbutton(tk.slider.left.frame, text="MC", command=OnMemHide, font=visu.env$tk.font)
    tk.change.profile.mode <- tkbutton(tk.slider.left.frame, text="CPM", command=ChangeProfileMode, font=visu.env$tk.font)

    tk.pairs.frame <- tkframe(tk.plot.frame, bg="white")
    tk.pairs.valid.but <- tkbutton(tk.pairs.frame, text="Valid set of pairs", command=OnValidPairs, font=visu.env$tk.font)
    tk.pairs.buts.frame <- tkframe(tk.pairs.frame, bg="white")
    tk.pairs.label <- tklabel(tk.pairs.buts.frame, bg="white", fg="black", width=20, font=visu.env$tk.font)
    tk.pairs.ML.but <- tkbutton(tk.pairs.buts.frame, text="ML Pair", command=OnML, font=visu.env$tk.font)
    tk.pairs.CNL.but <- tkbutton(tk.pairs.buts.frame, text="CNL Pair", command=OnCNL, font=visu.env$tk.font)
    tk.pairs.cancel.but <- tkbutton(tk.pairs.buts.frame, text="Cancel pair", command=OnCancelPair, font=visu.env$tk.font)

    tk.protos.frame <- tkframe(tk.plot.frame, bg="white")
    #tk.protos.valid.but <- tkbutton(tk.protos.frame, text="Valid set\nof protos", command=OnValidPairs)
    tk.protos.buts.frame <- tkframe(tk.protos.frame, bg="white")
    tk.protos.entry <- tkentry(tk.protos.frame, textvariable=visu.env$tcl.proto.label, width=50,
                               justify="left", fg="black", bg="white", font=visu.env$tk.font)
    tk.cluster.rename.but <- tkbutton(tk.protos.buts.frame, text="Rename\ncluster", command=OnRenameCluster, font=visu.env$tk.font)
    tk.protos.select.but <- tkbutton(tk.protos.buts.frame, text="Select\nproto", command=OnSelectProto, font=visu.env$tk.font)
    tk.protos.cancel.but <- tkbutton(tk.protos.buts.frame, text="Cancel\nproto", command=OnCancelProto, font=visu.env$tk.font)
    tkbind(tk.protos.entry, "<Leave>", OnLeaveProto)


    #------------------
    #positioning
    #------------------
    tkpack(tklabel(tk.plot.frame, text=visu.env$data.sample$name, fg="black",
                   bg="white", wraplength=600, font=visu.env$tk.font), fill="x")
    tkpack(tk.plot.fig, fill="x")
    tkpack(tk.plot.frame, side="left")

    tkpack(tklabel(tk.plot.compare.frame, text=visu.env$data.sample$name, fg="black",
                   bg="white", wraplength=600, font=visu.env$tk.font), fill="x")
    tkpack(tk.plot.compare.fig, fill="x")
    if (visu.env$compare.mode!="off")
        tkpack(tk.plot.compare.frame, side="left", anchor="n")

    tkpack(tk.profile.fig, fill="x")
    tkpack(tk.slider.big.label)
    tkpack(tk.slider.big, fill="x")
    tkpack(tk.slider.tune.label)
    tkpack(tk.slider.tune, fill="x")
    tkpack(tk.mem.but, fill="x")
    tkpack(tk.mem.hide.but, fill="x")
    tkpack(tk.change.profile.mode, fill="x")
    tkpack(tk.slider.left.frame, side="left")
    tkpack(tk.slider.center.frame, side="left")
    tkpack(tk.slider.right.frame, side="left")
    tkpack(tk.slider.frame, fill="x")
    #  tkpack(tk.profile.mem, sticky="n") #hidden

    tkpack(tk.pairs.buts.frame, side="left")
    tkpack(tk.pairs.valid.but, side="right")
    tkpack(tk.pairs.label, side="left")
    tkpack(tk.pairs.ML.but, side="left")
    tkpack(tk.pairs.cancel.but, side="left")
    tkpack(tk.pairs.CNL.but, side="left")
    if (selection.mode=="pairs")
        tkpack(tk.pairs.frame, fill="x")

    tkpack(tk.protos.entry, side="left", padx=4)
    tkpack(tk.protos.buts.frame, side="right")
    #tkpack(tk.protos.valid.but, side="right")
    tkpack(tk.cluster.rename.but, side="left")
    tkpack(tk.protos.select.but, side="left")
    tkpack(tk.protos.cancel.but, side="left")
    if (selection.mode=="prototypes")
        tkpack(tk.protos.frame, fill="x")

    #    tkpack(tk.profile.frame, side="right", padx=10, ipadx=10)


    #initialisations
    OnModifFeaturesMode()
    OnModifPlotMode()

    #horizontal axis - modification of feature
    #  ttt <- tkframe(tt)
    #  tkgrid(ttt, row=0, column=0, sticky="nw", padx=10)
    #  tkgrid(tklabel(ttt, text="Feature - First axis"), sticky="n")
    #  for (i in 1:length(feat)) {
    #    but <- tkradiobutton(ttt)
    #    tkconfigure(but, variable=featH.var, text=feat[i], value=feat[i], command=OnModif)
    #    tkgrid(but, sticky="w")
    #  }

    #  OnOK <- function(){
    #  	tkdestroy(tt)
    #  }
    #  OK.but <- tkbutton(tt,text="End",command=OnOK)
    #  tkgrid(OK.but,row=14,column=1)

    #default vizualisation
    if ((selection.mode!="none")||wait.close)
        tkwait.window(tt) ## A COMMENTER POUR LES TESTS

    switch(selection.mode,
           "prototypes"=list(prototypes=visu.env$prototypes,label=visu.env$data.sample$clustering), 
           "pairs"=list(ML=visu.env$pairs.values[visu.env$pairs.constraints==1],
                        CNL=visu.env$pairs.values[visu.env$pairs.constraints==-1]),
           NULL
           )
}

#' analyzePlot creates specific plot for data exploration/analysis.
#' @title Plot for data exploration/analysis
#' @description Create some specific plots for data exploration/analysis.
#' @param nb notebook in which the analyze plot will be added.
#' @param data.sample list containing features, profiles and clustering results.
#' @param selectedVar character vector containing the selected variables names to analyze.
#' @param type character vector specifying the analysis type. Must be 'boxplot', 'gapSE', 'histo', 'pcaCorr' or 'pcaVar'.
#' @param hscale numeric value corresponding to the horizontal scale of graphic.
#' @param K.max maximal number of clusters (K.Max=20 by default).
#' @param fontsize size of font (fontsize=11 by default).
#' @return None
#' @importFrom graphics barplot par layout boxplot title hist plot arrows text axis abline mtext box
#' @importFrom grDevices colorRampPalette
#' @importFrom corrplot corrplot
#' @importFrom stats cor
#' @import factoextra
#' @examples 
#' dat <- rbind(matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 4, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 6, sd = 0.3), ncol = 2))
#' colnames(dat) <- c("x","y")
#' tf <- tempfile()
#' write.table(dat, tf, sep=",", dec=".")
#' 
#' x <- importSample(file.features=tf)
#' 
#' mainWindow <- tktoplevel()
#' tktitle(mainWindow) <- "Barplot clustering"  
#' mainWindow$env$nb <- tk2notebook(mainWindow, tabs = c())
#' tkpack(mainWindow$env$nb, fill="both", expand= TRUE)
#' 
#' analyzePlot(mainWindow$env$nb, x, selectedVar="x", type="boxplot")
#'
#'
#' @keywords internal
#' 
analyzePlot <- function(nb, data.sample, selectedVar, type = "boxplot", hscale = 1.2, K.max=20, fontsize=11) {
    datTemp <- data.sample$features[["preprocessed"]]$x[data.sample$id.clean,
                                                        which(colnames(data.sample$features[["preprocessed"]]$x) %in% selectedVar)]
    # Check if all values are numeric
    if (all(sapply(datTemp, is.numeric))) {
        # Boxplot for selected parameter
        if (type == "boxplot") {
        tk2add.notetab(nb, "    Statistics    ", "Statistics")
        tk2draw.notetab(nb, "Statistics", hscale = hscale,
        function() { 
        						opar <- graphics::par(no.readonly=TRUE)
								on.exit(graphics::par(opar))
                                graphics::par(bg = "white", oma = c(0,2,2,0))
                                graphics::layout(matrix(c(1,2), 1, 2, byrow = TRUE), widths=c(1.5,2.5))
                                bp <- graphics::boxplot(data.sample$features[["preprocessed"]]$x[data.sample$id.clean, 
                                              which(colnames(data.sample$features[["preprocessed"]]$x) %in% selectedVar)], 
                                main = NULL, ylab = "Values", outline = FALSE, col="black")
                                graphics::title(sub = paste("Max: ", toString(signif(bp$stats[5], digits = 5)), 
                                                  "\n3rd quartile: ", toString(signif(bp$stats[4], digits = 5)),
                                                  "\nMedian: ", toString(signif(bp$stats[3], digits = 5)), 
                                                  "\n1st quartile: ", toString(signif(bp$stats[2], digits = 5)), 
                                                  "\nMin: ", toString(signif(bp$stats[1], digits = 5)), "\n", sep = ""), 
                                      adj = 1, font.sub = 1, cex.sub = 0.8)

                                graphics::hist(data.sample$features[["preprocessed"]]$x[data.sample$id.clean,
                                     which(colnames(data.sample$features[["preprocessed"]]$x) %in% selectedVar)], 
                                main = NULL, xlab = "Values", col="black")
                                graphics::title(selectedVar, outer=TRUE)
        
        	       }
        	       )
		}

        # Correlation circle from PCA for selected parameters
        if (type == "pcaCorr") {
            res.pca <- data.sample$features[[selectedVar]]$save
            tk2add.notetab(nb, "    PCA Correlation    ", "pcaCorr")
            tk2draw.notetab(nb, "pcaCorr", hscale = hscale,
                             function() { 
										p <- factoextra::fviz_pca_var(res.pca,
             							col.var = "contrib", 
             							gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             							repel = TRUE)
             							p <- p + ggplot2::theme(text = element_text(size = fontsize),
        									 axis.title = element_text(size = fontsize),
        									 axis.text = element_text(size = fontsize))
             							plot(p)
                             })
        }

        # Variances from PCA for selected parameters
        if (type == "pcaVar") {
            eig <- (data.sample$features[[selectedVar]]$sdev)^2
            variance <- eig*100/sum(eig)
            df=data.frame(Variance=variance ,CumVariance=cumsum(variance))
            tk2add.notetab(nb, "    PCA Variance    ", "pcaVar")
            tk2draw.notetab(nb, "pcaVar", hscale = hscale,
                            function() {
                            	opar <- graphics::par(no.readonly=TRUE)
								on.exit(graphics::par(opar))
                                xcoord <- seq(0.7, length(variance) * 1.2, by = 1.2)
                                graphics::par(bg = "white", mar = c(4,4,1,4)+.1)
                                bp <- graphics::barplot(variance, ylab = "Percentage of variance (%)", 
                                              xlim = c(0.2, xcoord[length(variance)]), col = "black")
                                graphics::axis(side = 1, at = bp, labels = paste("PC", 1:length(variance)))
                                graphics::par(new = TRUE)
                                graphics::plot(bp, cumsum(variance), xlim = c(0.2, xcoord[length(variance)]), 
                                     type = "o", col = "red", ylim = c(0, 100), lwd = 3, ann = FALSE, axes = FALSE)
                                graphics::axis(side = 4, col = "red", col.axis = "red")
                                graphics::mtext("Cumulative variance (%)", side = 4, line = 3, col = "red")
                            })
        }

        # Gap and K estimated from Spectral Embedding
        if (type == "gapSE") {
            length.eig <- length(data.sample$features[[selectedVar]]$eig)
            eig <- data.sample$features[[selectedVar]]$eig[1:min(length.eig)]
            gap <- data.sample$features[[selectedVar]]$gap
            tk2add.notetab(nb, "    Gap Spectral    ", "gapSE")
            tk2draw.notetab(nb, "gapSE", hscale = hscale,
                            function() {
                                opar <- graphics::par(no.readonly=TRUE)
								on.exit(graphics::par(opar)) 
                                graphics::par(bg = "white")
                                graphics::plot(eig, type = "o", col = "blue", lwd = 3, ann = FALSE, axes = FALSE)
                                graphics::abline(v = which.max(gap), lwd = 2, lty = 2, col = "darkred")
                                graphics::abline(h = eig[which.max(gap)], lwd = 2, lty = 2, col = "darkred")
                                graphics::axis(side = 1, at = c(seq(0,K.max, by=5), which.max(gap)), 
                                     labels = c(seq(0,K.max, by=5), which.max(gap)), cex.axis = .7)
                                graphics::axis(side = 2, at = c(min(eig),max(eig)), 
                                     labels = c(signif(min(eig),2), max(eig)), las = 2, cex.axis = .7)
                                graphics::text(x = which.max(gap), y = max(eig)-0.01, "K", srt = 0, pos = 4, cex = .7, col = "darkred")
                                graphics::mtext("Index", side = 1, line = 3, col = "black")
                                graphics::mtext("Eigenvalue", side = 2, line = 3, col = "black")
                                graphics::box()
                            })
        }
        # Correlation circle
        if (type == "Corr") {
        	data <- data.sample$features[["preprocessed"]]$x[data.sample$id.clean,selectedVar]
            M <- stats::cor(data)
            p.mat <- NULL
            title.mat <- "Correlation matrix"
			tryCatch(
        	expr = {
				p.mat <- cor.mtest(M)
				title.mat <- "Correlation matrix (p-value 5%)"
       		},
        	error = function(e){
        		if (grepl('not enough finite observations',as.character(e))==TRUE){
       				message("Not compatible with p-value test : not enough finite observations")
        		}
        	}
    		) 
            tk2add.notetab(nb, "    Correlations    ", "Corr")
            tk2draw.notetab(nb, "Corr", hscale = hscale,
                             function() { 
                                 col <- grDevices::colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
                                 if (length(selectedVar) > 12){
                                 	corrplot::corrplot(M, type="upper", order="hclust", col=col(200),  
         									 p.mat = p.mat, sig.level = 0.05, insig = "blank", tl.col="black",
         									 diag=FALSE,title= title.mat, mar=c(0,0,1,0), tl.cex= fontsize/11, cl.cex = fontsize/11, number.cex=fontsize/11)
                                 } else {
								 	corrplot::corrplot(M, method="color", col=col(200),  
         						 	type="upper", order="hclust", 
         						 	addCoef.col = "black",
         						 	tl.col="black", tl.srt=45, 
        						 	p.mat = p.mat, sig.level = 0.05, insig = "blank", 
         						 	diag=FALSE, title= title.mat, mar=c(0,0,1,0), tl.cex= fontsize/11, cl.cex = fontsize/11, number.cex=fontsize/11)
         						 }
                             	 })
        }
    } else {
        tkmessageBox(message = "Non numeric value(s) in the selected parameter(s)!", icon = "warning", type = "ok")
    }
}

#' cor.mtest Correlation test on correlation matrix.
#' @title Correlation test.
#' @description Display the abundances barplot of a clustering.
#' @param mat correlation matrix
#' @return matrix with p-values.
#' @importFrom stats cor.test
#' @references \url{https://www.sthda.com/french/wiki/visualiser-une-matrice-de-correlation-par-un-correlogramme}
#' @keywords internal

cor.mtest <- function(mat) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat<- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
        for (j in (i + 1):n) {
            tmp <- stats::cor.test(mat[, i], mat[, j])
            p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
        }
    }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

#' abdPlotTabs displays the abundances barplot of a set of clusterings in Tk tabs of a notebook.
#' @title Abundances barplots inside Tk tabs.
#' @description Display the abundances barplot of a clustering.
#' @param clusterings clustering list.
#' @param nb a notebook.
#' @param RclusTool.env environment in which all global parameters, raw data and results are stored.
#' @param hscale numeric value corresponding to the horizontal scale of graphic.
#' @return None
#'
#' @examples 
#' dat <- rbind(matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 4, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 6, sd = 0.3), ncol = 2))
#' colnames(dat) <- c("x","y")
#' tf <- tempfile()
#' write.table(dat, tf, sep=",", dec=".")
#' 
#' mainWindow <- tktoplevel()
#' tktitle(mainWindow) <- "Barplot clustering"  
#' mainWindow$env$nb <- tk2notebook(mainWindow, tabs = c())
#' tkpack(mainWindow$env$nb, fill="both", expand= TRUE)
#' 
#' x <- importSample(file.features=tf)
#' method <- "K-means"
#' 
#' x <- computeUnSupervised(x, K=3, method.name=method)
#' 
#' abdPlotTabs(x$clustering, mainWindow$env$nb)
#'
#'
#' @keywords internal
#' 
abdPlotTabs <- function(clusterings, nb, RclusTool.env=initParameters(), hscale=NULL)
{
    if (is.null(hscale))
        hscale <- 1.2

    num.cluster <- 0
    for (title in names(clusterings))
    {
        if (title == "no-clustering")
            next
        num.cluster <- num.cluster + 1
        label <- clusterings[[title]]$label
        text <- paste("", as.character(num.cluster),"")
        tk2add.notetab(nb, text, as.character(num.cluster))

        tk2draw.notetab(nb, as.character(num.cluster), hscale= hscale,
                        function() abdPlot(label, title, point.param=RclusTool.env$param$visu$palette.colors))
    }
}

#' abdPlotTabsGUI calls abdPlotTabs to display the abundances barplot of the clusterings of the current RclusTool.env$data.sample, inside the GUI.
#' @title Abundances barplots inside Tk tabs.
#' @description Display the abundances barplot of a clustering.
#' @param RclusTool.env environment in which all global parameters, raw data and results are stored.
#' @return None
#'
#' @keywords internal
abdPlotTabsGUI <- function(RclusTool.env=initParameters())
{
    clusterings <- RclusTool.env$data.sample$clustering
    nb <- RclusTool.env$gui$win2$env$nb
    hscale <- RclusTool.env$param$visu$hscale

    if (!is.null(clusterings) && !is.null(nb))
        abdPlotTabs(clusterings, nb, RclusTool.env, hscale=hscale)
}
 
#' abdPlot displays the abundances barplot of a clustering.
#' @title Abundances barplot
#' @description Display the abundances barplot of a clustering.
#' @param label factor describing the clustering.
#' @param title naming the graph.
#' @param charsize character size
#' @param point.param specifying the colors and the symbols to use for clusters display.
#' @return None
#' @importFrom grDevices colors
#' @importFrom graphics par barplot
#' @import ggplot2
#'
#' @examples 
#' dat <- rbind(matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 4, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 6, sd = 0.3), ncol = 2))
#' colnames(dat) <- c("x","y")
#' tf <- tempfile()
#' write.table(dat, tf, sep=",", dec=".")
#' 
#' x <- importSample(file.features=tf)
#' 
#' x <- computeUnSupervised(x, K=3, method.name='K-means')
#' 
#' abdPlot(x[["clustering"]][["K-means_preprocessed"]][["label"]], 'K-means_preprocessed')
#'
#'
#' @keywords internal
#' 
abdPlot <- function(label, title, charsize=11, point.param=c("grey","black","red","blue","green","cyan", "yellow","orange",
                                     "rosybrown","palevioletred","darkblue","deeppink","blueviolet", 'darkgoldenrod1', 'chartreuse',
                                     "darkorchid1", "deeppink", "coral", "darkolivegreen1","#66C2A5","#9DAE8C","#D49A73","#F08F6D",
                                     "#C79693","#9E9DBA","#9F9BC9","#C193C6","#E28BC3","#D2A29F","#BABF77","#AAD852","#CBD844",
									 "#ECD836","#FAD53E","#F1CD64","#E7C689","#D7BF9C","#C5B9A7","#B3B3B3","#D53E4F","#E04F4A",
									 "#EB6046","#F47346","#F88B51","#FBA35C","#FDB869","#FDCA79","#FDDD88","#F6E68F","#EDEE93",
									 "#E2F398","#CDEA9D","#B7E2A1","#A0D8A4","#86CEA4","#6DC4A4","#58B2AB","#459DB4","#3288BD")){
	l=levels(label)
  	id=names(label)
	values=unname(label)
	df<-data.frame(id=id,label=values)
	p <- ggplot2::ggplot(df, aes(factor(label),fill=label)) + ggplot2::geom_bar() + ggplot2::scale_x_discrete(drop=FALSE) 
	p <- p + ggplot2::xlab("") + ggplot2::ylab("Abundances")
	p <- p + ggplot2::ggtitle(title)
	p <- p + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) 
	p <- p + ggplot2::theme(axis.text.x = element_text(angle = 90, size=11))
	p <- p + ggplot2::scale_fill_manual(values=point.param[1:length(l)], drop=FALSE)
	p <- p + ggplot2::theme(legend.position='none')
	p <- p + ggplot2::theme(axis.text.x = element_text(size = charsize))
	p <- p + ggplot2::theme(axis.text.y = element_text(size = charsize))
	p <- p + ggplot2::theme(plot.title = element_text(size = charsize))
	print(p)
}

#' ElbowPlot displays the Elbow plot after a clustering step.
#' @title Elbow Plot.
#' @description Display the Elbow Plot after a clustering step.
#' @param nb tk-notebook in which the Elbow plot will be made.
#' @param method.space.name complete name of space.
#' @param RclusTool.env environment in which data and intermediate results are stored.
#' @param hscale numeric value corresponding to the horizontal scale of graphic
#' @param charsize character size
#' @return None
#' @import ggplot2
#' @keywords internal


ElbowPlot <- function(nb, method.space.name, RclusTool.env, hscale= 1.2, charsize=11) {

	Within=RclusTool.env$data.sample$clustering[[method.space.name]]$Within
	K=ElbowFinder(1:length(Within),Within)
	df=data.frame(K= 1:length(Within) , Within=Within)
    tk2add.notetab(nb, "    Elbow    ", "Elbow")
    tk2draw.notetab(nb, "Elbow", hscale= hscale,
        						function() { 
        							    tryCatch(
        									expr = {
												p <- ggplot2::ggplot(data=df, ggplot2::aes(x=K, y=Within))
												p <- p + ggplot2::geom_line(size=1) + geom_point()
												p <- p + ggplot2::scale_color_manual(values=(col))
												p <- p + ggplot2::xlab("K") + ggplot2::ylab("Withins tot")
												p <- p + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
												p <- p + ggplot2::ggtitle(paste('Elbow', method.space.name, sep=" "))
												p <- p + ggplot2::geom_vline(xintercept = K, linetype="dashed", color = "red", size=1.5)
												p <- p + ggplot2::scale_x_continuous(breaks=1:length(Within))
												p <- p + ggplot2::theme(axis.text.x = element_text(size = charsize))
												p <- p + ggplot2::theme(axis.text.y = element_text(size = charsize))
												p <- p + ggplot2::theme(plot.title = element_text(size = charsize))
												print(p)
       			 									},
        									error = function(e){
            									if (grepl('-monotype-arial',as.character(e))==TRUE){
            										message("Error :Please, Try to install gsfonts-x11")
           										}
        									}
    									)     
        	                			   }
        	       )  
    	}

#' tk2delete.notetab delete a notetab
#' @title Delete notetab inside a tk-notebook
#' @description Delete notetab inside a tk-notebook.
#' @param nb a notebook.
#' @param tab.names either NULL to delete all the notetabs, or a vector of notetab names.
#' @return None
#' @keywords internal

tk2delete.notetab <- function(nb, tab.names=NULL)
{
    ids <- names(nb$env) # noms des objets
    nt.ok <- sapply(ids, function(x) inherits(nb$env[[x]],"ttk2notetab"))
    ids <- ids[nt.ok] #noms des onglets de classe tk2notetab
    if (is.null(tab.names))
        tab.names <- ids

    for (id in tab.names)
    {
        tkdestroy(nb$env[[id]])
        rm(list=id, envir=nb$env)
    }
}


#' tk2add.notetab add a notetab.
#' @title Add notetab.
#' @description Add a notetab.
#' @param nb a notebook.
#' @param tab.label string the label of a tab on the interface.
#' @param tab.name string the name of the tab in a notebook.
#' @return None
#' @keywords internal

tk2add.notetab <- function(nb, tab.label, tab.name=NULL)
{
    if (is.null(tab.name)) 
        tab.name <- tab.label

    tframe <- tk2frame(nb)
    tkadd(nb, tframe, text=tab.label, sticky="nsew")
    tk2notetab.RclusTool(nb, tab.label, tab.name)
}

#' tk2draw.notetab draw in a notetab.
#' @title Draw in a Notetab.
#' @description Draw in a Notetab with a function.
#' @param nb a notebook.
#' @param tab.name name of the tk2notetab variable.
#' @param hscale numeric value corresponding to the horizontal scale of graphic
#' @param fun a function.
#' @return None
#' @keywords internal

tk2draw.notetab <- function(nb, tab.name, fun, hscale= 1.2){
    Plot <- tkrplot.RclusTool(nb$env[[tab.name]], hscale = hscale, fun=fun)        
    tkgrid(Plot, row = 0, column = 1, sticky = "w")
}

#' tk2notetab.RclusTool RclusTool adaptation of tk2notetab.
#' @title RclusTool tk2notetab.
#' @description RclusTool adaptation of tk2notetab; builds and returns a R tk2notetab matching a notetab already declared in tk notebook.
#' @param nb a tk2notebook or ttk2notebook widget.
#' @param tab.label the label of a tab on the interface.
#' @param tab.name the name of the tab in a notebook.
#' @return None
#' @keywords internal

tk2notetab.RclusTool <- function (nb, tab.label, tab.name=NULL) 
{
    if (inherits(nb, "tk2notebook")) {
        if (is.null(tab.name)) 
            tab.name <- tab.label

        ntab <- as.numeric(tcl(nb, "index", "end"))
        if (ntab < 1) 
            return(NULL)
        tabid <- ""
        for (i in 0:(ntab - 1)) if (tclvalue(tcl(nb, "tab", i, 
            "-text")) == tab.label) {
            tabid <- as.character(tcl(nb,"tabs"))[i+1]
            break
        }
        if (tabid !="") {
            w <- list()
            w$ID <- tabid
            w$env <- new.env()
            w$env$num.subwin <- 0
            w$env$parent <- nb
            class(w) <- c("ttk2notetab", "tk2container", "tkwin")
            nb$env[[tab.name]] <- w #sauvegarde dans l'environnement du notebook
            return(w)
        }
        else return(NULL)
    }
    else stop("'nb' must be a 'tk2notebook' object")
}

#' tkrplot.RclusTool RclusTool adaptation of tkrplot.
#' @title RclusTool tkrplot.
#' @description RclusTool adaptation of tkrplot.
#' @param graphicFrame frame of the RclusTool interface in which graphics should be displayed.
#' @param fun a function
#' @param hscale numeric value corresponding to the horizontal scale of graphic
#' @param vscale vertical scale 
#' @return None
#' @importFrom grDevices png
#' @keywords internal

tkrplot.RclusTool <- function(graphicFrame, fun, hscale=1.0, vscale=1.0)
{
    file <- tempfile()
    grDevices::png(file, width=hscale*480, height=vscale*480)
    try(fun())
    dev.off()

    image <- tclVar()
    tkimage.create("photo", image, file = file)
    lab <- tklabel(graphicFrame, image=image)
    tkbind(lab, "<Destroy>", function() .Tcl(paste("image delete", image)))
    lab$file <- file 
    lab$image <- image
    lab$fun <- fun
    lab$graphicFrame <- graphicFrame
    lab$hscale <- hscale
    lab$vscale <- vscale
    lab
}

#' tkrreplot.RclusTool RclusTool adaptation of tkrreplot.
#' @title RclusTool tkrreplot.
#' @description RclusTool adaptation of tkrreplot.
#' @param env.graphic graphic environment generated by tkrplot.RclusTool 
#' @seealso \code{\link{tkrreplot.RclusTool}}
#' @return None
#' @importFrom grDevices png
#' @keywords internal

tkrreplot.RclusTool <- function(env.graphic)
{
    grDevices::png(env.graphic$file, width=env.graphic$hscale*480, height=env.graphic$vscale*480)
    try(env.graphic$fun())
    dev.off()
    tkimage.create("photo", env.graphic$image, file=env.graphic$file)
}

#' makeTitle Makes a title from a string.
#' @title RclusTool makeTitle.
#' @description Makes a character title from string by completion with a specified character.
#' @param string string to be completed
#' @param length final title length in characters
#' @param prefix.length space length in front of the title
#' @param char character used to complete the title
#' @return title
#' @keywords internal

makeTitle <- function(string, length=120, prefix.length=5, char='_'){
    n.reps <- length - nchar(string) - prefix.length
    if (n.reps<1)
        return(string)
    string_return <- paste(strrep(char, 5), string, strrep(char, n.reps), sep='')
    return(string_return)  
}

#' tkEmptyLine Inserts an empty line in a graphical tk objects dealt with grid.
#' @title RclusTool tkEmptyLine.
#' @description Makes a character title from string by completion with a specified character.
#' @param tk.object tk object in which to insert the new line
#' @param row row number of the insertion
#' @return None
#' @keywords internal

tkEmptyLine <- function(tk.object, row=NULL)
{
    label <- tklabel(tk.object, text="  ")
    if (!is.null(row)) {
        tkgrid(label, row = row)
    } else {
        tkgrid(label)
    }
}

#' consoleMessage Adds a message in the RclusTool GUI.
#' @title RclusTool consoleMessage.
#' @description Adds a message in the RclusTool GUI.
#' @param message string to print in the console.
#' @param RclusTool.env environment in which all global parameters, raw data and results are stored.
#' @return None
#' @keywords internal

messageConsole <- function(message, RclusTool.env=initParameters())
{
    console <- RclusTool.env$gui$console
    if (!is.null(console))
    {
        tkinsert(console, "0.0", message)
    }
}
