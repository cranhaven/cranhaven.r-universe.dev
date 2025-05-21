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

#' buildClusteringSample builds a clustering result from a csv file to be later embedded into a data.sample object
#' @title Clustering loading
#' @description Load a clustering result from a csv file into a data.sample object.
#' @param filename.csv character vector specifying the path and the name of the csv file containing the clustering result.
#' @param data.sample matrix of raw data (point by line).
#' @param noise.cluster character name of the cluster "Noise".
#' @importFrom utils read.csv
#' @return The function returns a list 'clustering' containing:
#' \item{label}{vector of labels.}
#' \item{summary}{data.frame containing clusters summaries (min, max, sum, average, sd).}
#' \item{K}{number of clusters.}
#' @seealso \code{\link{saveClustering}}
#' 
#' @examples 
#' dat <- rbind(matrix(rnorm(100, mean = 0, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 4, sd = 0.3), ncol = 2))
#' tf1 <- tempfile()
#' write.table(dat, tf1, sep=",", dec=".")
#' x <- importSample(file.features=tf1)
#' 
#' lab <- data.frame(ID=1:nrow(dat), label=c(rep("Cluster 1",50), rep("Cluster 2",50), 
#'                                           rep("Cluster 3",50)))
#' tf2 <- tempfile()
#' write.table(lab, tf2, sep=",")
#' 
#' clustering <- buildClusteringSample(tf2, x)
#' 
#' @keywords internal 
#' 
buildClusteringSample <- function(filename.csv, data.sample, noise.cluster="Noise") {
    clustering <- NULL
    message(filename.csv)
    if (file.exists(filename.csv)) {
        encoding <- guessFileEncoding(filename.csv)
        clustering.df <- utils::read.csv(filename.csv, row.names=1, fileEncoding=encoding)
        if (!( is.data.frame(clustering.df) && (ncol(clustering.df)==2) ))
            clustering.df <- NULL

        if ( !is.null(clustering.df) ) {
            clustering <- clustering.df[[2]]
            names(clustering) <- clustering.df[[1]] 
            new.label <- importLabelSample(clustering, data.sample, noise.cluster)
            #summary=clusterSummary(data.sample, new.label)[,-1,drop=FALSE]
            clustering <- list(label=new.label,
                               summary=clusterSummary(data.sample, new.label)[,,drop=FALSE], K=sum(table(new.label)>0))
        }
    }

    clustering
}

#' saveClustering saves a clustering result in a csv file
#' @title Clustering saving
#' @description Save a clustering result in a csv file.
#' @param filename.csv character vector specifying the path and the name of the csv file.
#' @param label vector of labels.
#' @param dir character vector specifying the directory where to save the csv file.
#' @return csv file containing clustering result.
#' @importFrom utils alarm
#' @seealso \code{\link{buildClusteringSample}}
#' 
#' @examples 
#' dat <- rbind(matrix(rnorm(100, mean = 0, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 4, sd = 0.3), ncol = 2))
#' tf1 <- tempfile()
#' write.table(dat, tf1, sep=",", dec=".")
#' 
#' x <- importSample(file.features=tf1, dir.save=tempdir())
#' res <- KmeansQuick(x$features$initial$x, K=3)
#' 
#' tf2 <- tempfile()
#' saveClustering(basename(tf2), res$cluster, tempdir())
#' 
#' 
#' @export 
#' 
saveClustering <- function(filename.csv, label, dir) {
	filename.csv = file.path(dir,filename.csv)
    if (!nchar(filename.csv)) {
        utils::alarm()
        tkmessageBox(message="Filename not valid.", title="Save clustering")
        return()
    }

    write.csv(file=filename.csv, x=data.frame(ID=names(label), label=label))#, row.names = FALSE)
    # write.table(file=file.path(dirname(filename.csv), "cytoclus import.csv"), 
    #             x=data.frame(ID=as.numeric(names(label))-1, label=label), row.names = FALSE, col.names = FALSE, sep=",")
}

#' saveCounts saves a count result in a csv file
#' @title Count saving
#' @description Save a count result in a csv file.
#' @param filename.csv character vector specifying the path and the name of the csv file.
#' @param counts vector of counts.
#' @param dir character vector specifying the directory where to save the csv file.
#' @return csv file containing count result.
#' @importFrom utils alarm
#' 
#' @examples 
#' dat <- rbind(matrix(rnorm(100, mean = 0, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 4, sd = 0.3), ncol = 2))
#' tf1 <- tempfile()
#' write.table(dat, tf1, sep=",", dec=".")
#' 
#' x <- importSample(file.features=tf1)
#' res <- KmeansQuick(x$features$initial$x, K=3)
#' 
#' tf2 <- tempfile()
#' saveCounts(basename(tf2), table(res$cluster), dirname(tf2))
#'  
#' 
#' @export
#' 
saveCounts <- function(filename.csv, counts, dir) {
	filename.csv = file.path(dir, filename.csv)
    if (!nchar(filename.csv)) {
        utils::alarm()
        tkmessageBox(message="Filename not valid.", title="Save counts")
        return()
    }

    write.csv(file=filename.csv, x=data.frame(ID=names(counts), counts=counts))
}

#' saveCalcul saves object created after calculation in a csv file
#' @title Object saving
#' @description Save object created after calculation in a csv file.
#' @param filename.rdata character vector specifying the path and the name of the rdata file.
#' @param dat object to save.
#' @param dir character vector specifying the directory where to save the rdata file.
#' @return RDS file containing calculation.
#' @importFrom utils alarm
#' 
#' @examples 
#' dat <- rbind(matrix(rnorm(100, mean = 0, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 4, sd = 0.3), ncol = 2))
#' tf1 <- tempfile()
#' write.table(dat, tf1, sep=",", dec=".")
#' 
#' x <- importSample(file.features=tf1)
#' res.pca <- computePcaSample(x)
#' 
#' tf2 <- tempfile()
#' saveCalcul(basename(tf2), res.pca$pca, dirname(tf2))
#' 
#' 
#' @export 
#' 
saveCalcul <- function(filename.rdata, dat, dir) {
	filename.rdata = file.path(dir, filename.rdata)
    if (!nchar(filename.rdata)) {
        utils::alarm()
        tkmessageBox(message="Filename not valid.", title="Save calculations")
        return()
    }

    saveRDS(dat, file = filename.rdata)
}

#' loadSummary loads the clusters summaries results (min, max, sum, average, sd) from a csv file
#' @title Summaries loading
#' @description Load the clusters summaries results (min, max, sum, average, sd) from a csv file.
#' @param filename.csv character vector specifying the name and directory of the csv file.
#' @importFrom utils read.csv
#' @return res data.frame containing the clusters summaries.
#' @seealso \code{\link{saveSummary}}
#' 
#' @examples 
#' dat <- rbind(matrix(rnorm(100, mean = 0, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 4, sd = 0.3), ncol = 2))
#' colnames(dat) <- c("x","y")
#' tf1 <- tempfile()
#' write.table(dat, tf1, sep=",", dec=".")
#' 
#' x <- importSample(file.features=tf1)
#' res <- KmeansQuick(x$features$initial$x, K=3)
#' labels <- formatLabelSample(res$cluster, x)
#' cluster.summary <- clusterSummary(x, labels)
#' 
#' tf2 <- tempfile()
#' saveSummary(basename(tf2), cluster.summary, dirname(tf2))
#' 
#' loadSummary(tf2)
#'  
#' 
#' @keywords internal 
#' 
loadSummary <- function(filename.csv) {
    res <- NULL
    if (file.exists(filename.csv)){
        encoding <- guessFileEncoding(filename.csv)
        res <- utils::read.csv(file=filename.csv, row.names=1, fileEncoding=encoding)
        ind <- !sapply(res, is.numeric)
        res[,ind] <- sapply(res[,ind], as.character)
        #res[, c("cluster", "date", "filename", "machine", "operator", "method")] <- NULL
    }
    res
}

# ici, il faudrait prevoir un ensemble d'attributs, dans une liste, a  associer a  chaque donnee (ligne = cluster)
# info : strings vector about sample or clustering
#' saveSummary saves clusters summaries results in a csv file
#' @title Clusters summaries saving
#' @description Save clusters summaries results in a csv file.
#' @param filename.csv character vector specifying the path and the name of the csv file.
#' @param cluster.summary data.frame containing the clusters summaries results.
#' @param dir character vector specifying the directory where to save the csv file.
#' @param info character vector about sample or clustering.
#' @return csv file containing clusters summaries results.
#' @importFrom utils read.csv alarm
#' @seealso \code{\link{loadSummary}}
#'
#' @examples 
#' dat <- rbind(matrix(rnorm(100, mean = 0, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 4, sd = 0.3), ncol = 2))
#' colnames(dat) <- c("x","y")
#' tf1 <- tempfile()
#' write.table(dat, tf1, sep=",", dec=".")
#' 
#' x <- importSample(file.features=tf1)
#' res <- KmeansQuick(x$features$initial$x, K=3)
#' labels <- formatLabelSample(res$cluster, x)
#' cluster.summary <- clusterSummary(x, labels)
#' 
#' tf2 <- tempfile()
#' saveSummary(basename(tf2), cluster.summary, dirname(tf2))
#' 
#' 
#' @export 
#' 
saveSummary <- function(filename.csv, cluster.summary, dir, info=NULL) {
	filename.csv=file.path(dir, filename.csv) 
    if (!nchar(filename.csv)) {
        utils::alarm()
        tkmessageBox(title="Save results", message="Filename not valid.")
        return()
    }

    #if (!is.null(data.sample$date))
    #date <- format(as.POSIXct(data.sample$date, format="%d/%m/%Y %H:%M:%S"),
    #"%Y-%m-%d %H:%M:%S")

    #filename <- data.sample$name #basename(data.sample$filename$CYZ)
    #machine <- data.sample$machine.name
    #volume <- data.sample$volume

    if (!is.null(info)) 
        cluster.summary <- cbind(data.frame(t(as.character(info))), cluster.summary)

    if (file.exists(filename.csv)) {
        answer <- tkmessageBox(title="Save results",
                               message="Replace the existing file ?\nIf 'No', the existing file will be keeped.",
                               type="yesno", default="yes")
        if (tclvalue(answer)=="no")
            return()
    }
    write.csv(file=filename.csv, cluster.summary, na="", quote=TRUE, row.names = TRUE)
    return()
}

#' clusterSummary computes the clusters summaries (min, max, sum, average, sd) from a clustering result.
#' @title Clusters summaries computation
#' @description Save clusters summaries results in a csv file.
#' @param data.sample list containing features, profiles and clustering results.
#' @param label vector of labels.
#' @param features.to.keep vector of features names on which the summaries are computed.
#' @importFrom stats aggregate
#' @importFrom stats sd
#' @param summary.functions vector of functions names for the summaries computation. Could be 'Min', 'Max', 'Sum', 'Average', 'sd'. 
#' @return out data.frame containing the clusters summaries.
#'
#' @examples 
#' dat <- rbind(matrix(rnorm(100, mean = 0, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 4, sd = 0.3), ncol = 2))
#' tf1 <- tempfile()
#' write.table(dat, tf1, sep=",", dec=".")
#' 
#' x <- importSample(file.features=tf1)
#' res <- KmeansQuick(x$features$initial$x, K=3)
#' labels <- formatLabelSample(res$cluster, x)
#' cluster.summary <- clusterSummary(x, labels)
#' 
#' 
#' @export
#' 
clusterSummary <- function(data.sample, label, features.to.keep=colnames(data.sample$features[["preprocessed"]]$x),
                           summary.functions=c("Min"="min", "Max"="max", "Sum"="sum", "Average"="mean", "SD"="sd")) {

    # function to ap
    if (is.null(names(summary.functions)))
        names(summary.functions) <- summary.functions
    functions <- sapply(summary.functions, get)

    my.function <- function(x) {
        sapply(functions, function(y) y(x))
    }

    #compute mean on cleaned data #not log !!
    #features.to.keep <- intersect(gsub(" \\(log\\)", "", features.to.keep), 
    #                              colnames(data.sample$features$initial$x))

    features.to.keep <- colnames(data.sample$features$preprocessed$x)
    values <- stats::aggregate(data.sample$features[["preprocessed"]]$x[data.sample$id.clean, features.to.keep,
                               drop=FALSE], list(label[data.sample$id.clean]), my.function)
    #     values <- aggregate(data.sample$features[["preprocessed"]]$x[data.sample$id.clean, features.to.keep,
    #         drop=FALSE], list(label[data.sample$id.clean]), my.function)
    col.id <- values[,1]

    cluster.summary <- as.data.frame(as.matrix(round(values[-1], 2)))
    rownames(cluster.summary) <- values[,1]
    colnames(cluster.summary) <- apply(as.matrix(expand.grid(names(summary.functions), features.to.keep)), MARGIN=1, FUN=function(x){ paste(x[2], x[1], sep="...")})

    #add count by cluster but no more ID column
    cluster.count <- table(label)

    #as.numeric required (but don't know why ???)
    cluster.summary <- cbind(as.numeric(cluster.count[rownames(cluster.summary)]), cluster.summary)
    #colnames(cluster.summary)[1] <- c("Class")
    #colnames(cluster.summary)[2] <- c("Count")
    colnames(cluster.summary)[1] <- c("Count")
    out <- cluster.summary
}


#' clusterDensity computes the clusters density from a clustering result.
#' @title Clusters density computation
#' @description Save density summaries results.
#' @param data.sample list containing features, profiles and clustering results.
#' @param label vector of labels.
#' @param space space in which is the feature to deal with.
#' @param features.to.keep vector of features names on which the summaries are computed.
#' @return out data.frame containing the density summaries.
#' @importFrom stats density
#' @examples 
#' 
#' dat <- rbind(matrix(rnorm(100, mean = 0, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 4, sd = 0.3), ncol = 2))
#' tf1 <- tempfile()
#' write.table(dat, tf1, sep=",", dec=".")
#'
#' x <- importSample(file.features=tf1)
#' x <- computeUnSupervised(x, K=3, method.name="K-means")
#' 
#' label<-x[["clustering"]][["K-means_preprocessed"]][["label"]]
#' 
#' cluster.density <- clusterDensity(x, label, "preprocessed", features.to.keep='V1')
#' 
#' 
#' @keywords internal 
#' 

clusterDensity <- function(data.sample, label, space, features.to.keep=colnames(data.sample$features[[space]]$x)){
  
values <- data.sample$features[[space]]$x[, features.to.keep, drop=FALSE]
label_levels=levels(label)
df <- data.frame("x" = double(), "y" = double(), "Cluster" = factor(), "Variable"= character())

for (i in colnames(values)){
	for (j in label_levels){
		id<-names(label[label==as.character(j)])
		if (length(id)>2){
		  valeurs=values[id,i]
		  valeurs=stats::density(valeurs)
		  x <- data.frame("x" = valeurs$x, "y" = valeurs$y, "Cluster" = j, "Variable"= i)  
		  df =rbind(df,x)
		} else {
		  x <- data.frame("x" = NA, "y" = NA, "Cluster" = j, "Variable"= i)  
		  df =rbind(df,x)
		}
	}
}
return(df)
}

#' extractFeaturesFromSummary decomposes the summary column names into simple features (not combined to functions), statistic functions and features combined to the functions
#' @title Extraction of features from a summary object.
#' @description Extract features from a summary object.
#' @param summary a summary object.
#' @param split separator string  
#' @return list of simple features, features combined to functions, functions combined to features
#'
#' @examples 
#'
#' summary <- data.frame("x...min"=1,"x...max"=2,"count"=3,"length"=4)
#' 
#' extractFeaturesFromSummary(summary)
#'
#' @keywords internal 
extractFeaturesFromSummary <- function(summary, split="...")
{
    all.feats <- names(summary)
    is.fun.feat <- grepl(split, all.feats, fixed=TRUE)
    simple.feats <- all.feats[!is.fun.feat]
    fun.feats <- unlist(strsplit(all.feats[is.fun.feat], split=split, fixed=TRUE))

    feats <- unique(fun.feats[rep(c(TRUE,FALSE),length.out=length(fun.feats))])
    funs <- unique(fun.feats[rep(c(FALSE,TRUE),length.out=length(fun.feats))])

    list(simple.feats=simple.feats, feats=feats, funs=funs)
}


#' imgClassif sorts images (if available) in different directories according to a clustering result
#' @title Images clustering
#' @description Sort images (if available) in different directories according to a clustering result.
#' @param data.sample list containing features, profiles and clustering results.
#' @param imgdir character vector specifying the path of the images directory.
#' @param method character vector specifying the clustering method (already performed) to use.
#' @param user.name character vector specifying the user name.
#' @return images files in the different directories, csv file containing the detail.
#' @seealso \code{\link{sigClassif}}
#'
#' @examples 
#' dat <- rbind(matrix(rnorm(100, mean = 0, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 4, sd = 0.3), ncol = 2))
#' tf1 <- tempfile()
#' write.table(dat, tf1, sep=",", dec=".")
#' 
#' rep <- system.file("extdata", package="RclusTool")
#' imgdir <- file.path(rep, "img_example")
#'
#' dir.results <- tempdir()
#' x <- importSample(file.features=tf1, dir.images=imgdir, dir.save=dir.results)
#' x <- computeUnSupervised(x, K=3, method.name="K-means")
#' 
#' imgClassif(x, imgdir, method = "K-means_preprocessed")
#' 
#' 
#' @export 
#' 
imgClassif <- function(data.sample, imgdir, method, user.name="") {
    dat2 <- NULL
    if (is.character(imgdir) && dir.exists(imgdir)) {
        #Determine extension of images
        if ((length(list.files(imgdir, pattern = ".jpg", ignore.case=TRUE)) > 0)) {
            extension <- ".jpg"
        } else if ((length(list.files(imgdir, pattern = ".png", ignore.case=TRUE)) > 0)) {
            extension <- ".png"
        } else if ((length(list.files(imgdir, pattern = ".jpeg", ignore.case=TRUE)) > 0)) {
            extension <- ".jpeg"
        }
        message(paste(extension, "format file"))

        img <- list.files(imgdir, pattern = extension)
        ## Numbers (ID) of imaged particles
        imgNum <- sortCharAsNum(gsub(extension, "", img))
        label <- data.sample$clustering[[method]]$label
        grp <- unique(label[imgNum])
        dirClassifImg <- file.path(data.sample$files$results$clustering, paste("img_clustering", user.name, method, sep = "_"))
        if (dir.exists(dirClassifImg))
            unlink(dirClassifImg, recursive = TRUE)
        dir.create(dirClassifImg)
        #Creation of the Cluster folders
        for (j in grp)
        {
            if (!dir.exists(file.path(dirClassifImg, j)))
                dir.create(file.path(dirClassifImg, j))
        }

        for (i in imgNum) {
            # Verification that all columns of features are not NA
            #if (!any(is.na(data.sample$features$initial$x[i,])))
            if (!any(is.na(data.sample$features$preprocessed$x[i,])))
            {
                imgLabel <- label[i]
                file.copy(from = file.path(imgdir, paste(i, extension, sep = "")),
                          to = file.path(dirClassifImg, imgLabel))
                Id <- paste(i, "img", data.sample$name, method, sep = "_")
                #dat <- cbind(data.sample$features$initial$x[i,], imgLabel, Id)
                dat <- cbind(data.sample$features$preprocessed$x[i,], imgLabel, Id)
                dat2 <- rbind(dat2, dat)
            }
        }
        colnames(dat2)[which(colnames(dat2)=="imgLabel")] <- "Class"
        write.csv(dat2, file.path(dirClassifImg, paste("img_", data.sample$name, "_", method, ".csv", sep = "")), row.names = FALSE)
    } else {
        warning("No Image To Classify")
    }
}

#' sigClassif sorts signals (if available) in different directories according to a clustering result
#' @title Signals clustering
#' @description Sort signals (if available) in different directories according to a clustering result.
#' @param data.sample list containing features, profiles and clustering results.
#' @param method character vector specifying the clustering method (already performed) to use.
#' @param user.name character vector specifying the user name.
#' @return signals plots images in the different directories.
#' @importFrom grDevices jpeg dev.off
#' @seealso \code{\link{imgClassif}}
#' @examples 
#' dat <- rbind(matrix(rnorm(100, mean = 0, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 4, sd = 0.3), ncol = 2))
#' tf1 <- tempfile()
#' write.table(dat, tf1, sep=",", dec=".")
#' 
#' sig <- data.frame(ID=rep(1:150, each=30), SIGNAL=rep(dnorm(seq(-2,2,length=30)),150))
#' tf2 <- tempfile()
#' write.table(sig, tf2, sep=",", dec=".")
#'
#' dir.results <- tempdir()
#' x <- importSample(file.features=tf1,file.profiles = tf2, dir.save=dir.results)
#' x <- computeUnSupervised(x, K=3, method.name="K-means")
#' 
#' sigClassif(x, method = "K-means_preprocessed")
#'  
#' 
#' @export 
#' 
sigClassif <- function(data.sample, method, user.name="") {
    if (is.character(data.sample$files$profiles) && file.exists(data.sample$files$profiles)) {
        label <- data.sample$clustering[[method]]$label
        grp <- unique(label)
        dirClassifSig <- file.path(data.sample$files$results$clustering, paste("sig_clustering", user.name, method, sep = "_"))
        if (dir.exists(dirClassifSig))
            unlink(dirClassifSig, recursive = TRUE)
        dir.create(dirClassifSig)
        for (i in grp)
        {
            if (!dir.exists(file.path(dirClassifSig, i)))
                dir.create(file.path(dirClassifSig, i))
        }

        for (j in names(label)) {
            if(is.numeric(data.sample$profiles[[j]])) {
                grDevices::jpeg(file.path(dirClassifSig, file.path(label[j], paste(j, "_sig_", data.sample$name, "_", method, ".jpg", sep = ""))),
                                quality=70,  width = 600, height = 600)
                plotProfileExtract(data.sample$profiles[[j]], 
                            profiles.colors=data.sample$config$signalColor, 
                            title=paste("Observation", j))
                grDevices::dev.off()
            }
        }
    } else {
        warning("No Signal To Classify")
    }
}

#' extractProtos extracts prototypes automatically according to a clustering result, and save them in different directories
#' @title Prototypes extraction
#' @description Extract prototypes of each cluster automatically, according to a clustering result, and save them in different directories. 
#' In order to catch the whole variability, each cluster is divided into several sub-clusters, and medoids of each sub-cluster are considered as prototypes.
#' @param data.sample list containing features, profiles and clustering results.
#' @param method character vector specifying the clustering method (already performed) to use.
#' @param K.max maximal number of clusters (K.max=20 by default).
#' @param kmeans.variance.min elbow method cumulative explained variance > criteria to stop K-search.
#' @return csv file containing the prototypes 
#' @param user.name character vector specifying the user name.
#' @importFrom grDevices jpeg dev.off
#' @importFrom cluster pam
#' @examples 
#' dat <- rbind(matrix(rnorm(100, mean = 0, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 4, sd = 0.3), ncol = 2))
#' tf1 <- tempfile()
#' write.table(dat, tf1, sep=",", dec=".")
#' 
#' dir.results <- tempdir()
#' x <- importSample(file.features=tf1, dir.save=dir.results)
#' x <- computeUnSupervised(x, K=3, method.name="K-means")
#' 
#' extractProtos(x, method = "K-means_preprocessed")
#' 
#' 
#' @export 
#' 
extractProtos <- function(data.sample, method, K.max=20, kmeans.variance.min=0.95, user.name="") {
    label <- data.sample$clustering[[method]]$label
    grp <- levels(label)[-1]
    protos <- NULL

    dirProto <- file.path(data.sample$files$results$prototypes, paste("protos", user.name, method, sep = "_"))
    if (dir.exists(dirProto))
        unlink(dirProto, recursive = TRUE)
    dir.create(dirProto)

    for (i in grp) {
        Id <- NULL
        clusterIdx <- names(label)[label==i]
        if (length(clusterIdx)==0)
            next
        x <- data.sample$features[["preprocessed"]]$x[clusterIdx, ]

        if (length(clusterIdx) > 3000) {
            data.sample$sampling <- computeSampling(x=x, K=K.max,
                                                    sampling.size.max=3000, kmeans.variance.min=kmeans.variance.min)
            x <- x[data.sample$sampling$selection.ids, , drop=FALSE]
        }

        # subclustering on each cluster and extraction of 10 (max) prototypes (medoids)
        res.kmeans <- computeKmeans(x, K.max=K.max,
                                    kmeans.variance.min=kmeans.variance.min)
        K <- max(res.kmeans$cluster)
        #x <- scale(x, center=TRUE, scale=TRUE) #???

        res.pam <- list()
        if (nrow(x)>K) {
            res.pam <- cluster::pam(x, K, diss = FALSE)
            clusterLabel <- res.pam$clustering
        } else
        {
            clusterLabel <- 1:nrow(x)
            names(clusterLabel) <- rownames(x)
            res.pam$id.med <- 1:nrow(x)
        }

        #if ((is.character(data.sample$files$profiles) && file.exists(data.sample$files$profiles))
        #|| (is.character(data.sample$files$images) && file.exists(data.sample$files$images))) {
        dirProtoCluster <- file.path(dirProto, i)

        for (j in res.pam$id.med) {
            idx <- clusterIdx[j]
            Id <- c(Id, paste(idx, "protos", data.sample$name, method, sep = "_"))
            #jpeg(file.path(dirProtoCluster, paste(idx, ".jpg", sep = "")))
            if (!is.null(data.sample$profiles[[idx]])) {
                if (!dir.exists(dirProtoCluster))
                    dir.create(dirProtoCluster)
                grDevices::jpeg(file.path(dirProtoCluster, paste(idx, "_protos_", data.sample$name, "_", method, ".jpg", sep = "")),
                                quality=70,  width = 800, height = 800)
                plotProfileExtract(data.sample$profiles[[idx]], 
                            image = data.sample$images[idx],
                            profiles.colors=data.sample$config$signalColor, title = "", image.dir=data.sample$files$images)
                grDevices::dev.off()
            }
        }
        #}
        Class <- rep(i,K)

        dat <- cbind(data.sample$features$initial$x[clusterIdx[res.pam$id.med],], Class, Id)
        #dat <- data.sample$features$initial$x[clusterIdx[res.pam$id.med],]
        protos <- rbind(protos, dat)
    }
    colnames(protos)[colnames(protos)=="ID"] <- "i..ID"
    #colnames(protos)[which(colnames(protos)=="rep.i..K.")] <- "Class"
    write.csv(protos, file.path(dirProto, paste("protos_", data.sample$name, "_", method, ".csv", sep ="")), row.names = FALSE)
}

#' readTrainSet reads a training set built from prototypes, to train a classifier for supervised classification
#' @title Training set reading
#' @description Read a training set built from prototypes, to train a classifier for supervised classification.
#' @param traindir character vector specifying the path of the training set.
#' @param keep_ boolean: if FALSE (default), the '_' directory is not considered in the training set.
#' @param operations list of data.frames describing all preprocessing operations. 
#' @param RclusTool.env environment in which all global parameters, raw data and results are stored.
#' @return prototypes data.frame containing the features of each prototype associated to a class.
#' @importFrom utils read.csv
#' @seealso \code{\link{dropTrainSetVars}}
#' 
#' @examples 
#'
#' rep <- system.file("extdata", package="RclusTool")
#' traindir <- file.path(rep, "train_example")
#' train <- readTrainSet(traindir)
#' 
#' @export 
#' 
readTrainSet <- function(traindir, keep_ = FALSE, operations=NULL, RclusTool.env=initParameters()) {
    Id <- NULL
    ## 'traindir' must be the base directory of the prototypes classification

    ## Make sure we have .csv files in this traindir (otherwise it is, perhaps not a training set root dir!)
    Dats <- list.files(traindir, pattern = ".csv", full.names = TRUE)
    if (!length(Dats)) {
        warning("'traindir' does not appear to be a training set root dir!")
        return(invisible(FALSE))
    }

    ## List the .jpg or .png files (recursively) in the dir
    res <- list.files(traindir, recursive=TRUE, pattern = ".jpg")
    if (!length(res))
        res <- list.files(traindir, recursive=TRUE, pattern = ".png")

    ## Check the result...
    if (!length(res)) {
        warning("no PNG or JPEG vignettes found in this tree")
    } else {
        res <- gsub("[\\]", "/", res)
        # Eliminate the '_' directory
        if (!is.na(keep_) && !isTRUE(as.logical(keep_)))
            res <- grep("^[^_]", res, value = TRUE)
        # Extract 'Id' and Class of the images
        Id <- sub("\\.[^.]+$", "", basename(res))
        Path <- dirname(res)
        Class <- basename(Path)
        if (is.na(keep_)) Class[grepl("^[_]", res)] <- NA
    }

    # Merge all data.frames
    proto.sample <- importSample(file.features = Dats[1], sepFeat = ",", decFeat = ".", RclusTool.env=RclusTool.env) 
    proto.sample <- applyPreprocessing(proto.sample, operations, RclusTool.env, reset=TRUE, preprocessed.only=TRUE)
    if (is.null(proto.sample)) {
        tkmessageBox(message = "Prototypes reading fails.", icon = "warning", type = "ok")
        return()
    }
    prototypes <- proto.sample$features$preprocessed$x[proto.sample$id.clean,, drop=FALSE]
    proto.class <- as.character(proto.sample$features$initial$x[proto.sample$id.clean,"Class"])

    #prototypes <- prototypes[, -which(names(prototypes) %in% paramDrop)]
    if (length(Dats) > 1) {
        for (i in 2:length(Dats)) {
            proto.sample <- importSample(file.features = Dats[i], sepFeat = ",", decFeat = ".", RclusTool.env=RclusTool.env) 
            proto.sample <- applyPreprocessing(proto.sample, operations, RclusTool.env, reset=TRUE, preprocessed.only=TRUE)
            if (is.null(proto.sample)) {
                tkmessageBox(message = "Prototypes reading fails.", icon = "warning", type = "ok")
                return()
            }
            feats <- intersect(colnames(prototypes), colnames(proto.sample$features$preprocessed$x))
            if (!length(feats)){
                tkmessageBox(message = "Prototypes merging fails: no shared features.", icon = "warning", type = "ok")
            }
            prototypes <- rbind(prototypes[, feats, drop=FALSE], proto.sample$features$preprocessed$x[proto.sample$id.clean, feats, drop=FALSE])
            proto.class <- c(proto.class, as.character(proto.sample$features$initial$x[proto.sample$id.clean,"Class"]))
        }
    }

    prototypes$Class <- proto.class

    # Check if some images moved
    if (!is.null(Id)){
    for (l in 1:length(Id))
        prototypes$Class[which(prototypes$Id == Id[l])] <- Class[l]
    }
    rownames(prototypes) <- 1:nrow(prototypes)
    # Remove variables with NA
    if(any(is.na(prototypes$Class)))
        prototypes <- prototypes[-which(is.na(prototypes$Class)), ]

    #prototypes <- prototypes[which(prototypes$Id %in% gsub(".jpg", "", basename(res))),]
    #prototypes <- dropTrainSetVars(prototypes)
    # Print informations about this training set
    message("Training set data collected")
    message("\nClassification stats:\n")
    print(table(prototypes$Class))
    message("\nProportions per class:\n")
    print(table(prototypes$Class) / length(prototypes$Class) * 100)
    prototypes
}

#' dropTrainSetVars drops some parameters (columns) in the training set
#' @title Parameters dropping
#' @description Drop some parameters (columns) in the training set.
#' @param dat data.frame containing the features of each prototype associated to a class.
#' @param VarToDrop character vector specifying variables to drop from the training set.
#' @return dat data.frame containing the kept features of each prototype associated to a class.
#' @seealso \code{\link{readTrainSet}}
#' 
#' @examples 
#'
#' rep <- system.file("extdata", package="RclusTool")
#' traindir <- file.path(rep, "train_example")
#'
#' train <- readTrainSet(traindir)
#' train <- dropTrainSetVars(train, c("Id", "i..ID", "ArrivalTime", "X"))
#' 
#' @keywords internal 
#' 
dropTrainSetVars <- function (dat, VarToDrop) {
    # Remove default variables
    dat <- dat[, -which(colnames(dat) %in% VarToDrop)]
    dat
}

#' nameClusters assigns a class name to each cluster obtained by unsupervised or semi-supervised classification, thanks to the use of a training set and the majority rule method
#' @title Clusters renaming
#' @description Assign a class name to each cluster obtained by unsupervised or semi-supervised classification, thanks to the use of a training set and the majority rule method. 
#' @param data.sample list containing features, profiles and clustering results.
#' @param method character vector specifying the clustering method (already performed) to use.
#' @param RclusTool.env environment in which all global parameters, raw data and results are stored.
#' @importFrom class knn
#' @return data.sample list containing features, profiles and clustering results with updated labels names.
#' 
#' @examples 
#' \dontrun{
#' dat <- rbind(matrix(rnorm(100, mean = 0, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 4, sd = 0.3), ncol = 2))
#' tf1 <- tempfile()
#' write.table(dat, tf1, sep=",", dec=".")
#' 
#' x <- importSample(file.features=tf1)
#' x <- computeUnSupervised(x, K=3, method.name="K-means")
#' 
#' nameClusters(x, method = "K-means_preprocessed")
#' 
#' 
#' }
#' @keywords internal 
#' 
nameClusters <- function(data.sample, method, RclusTool.env=initParameters()) {
    protos.directory.default <- RclusTool.env$gui$protos.dir
    if (is.null(protos.directory.default) || !dir.exists(protos.directory.default))
        protos.directory.default <- getwd()
    protos.directory <- tk_choose.dir(default = protos.directory.default, caption = "Select folder for prototypes.")
    if (is.na(protos.directory))
        return(NULL)
    RclusTool.env$gui$protos.dir <- protos.directory
    #if (nchar(RclusTool.env$gui$protos.dir)) {
    prototypes <- readTrainSet(traindir = protos.directory, operations=data.sample$config$operations, RclusTool.env=RclusTool.env)
    prototypes.label <- prototypes$Class
    # For flow cytometry data...!
    prototypes <- prototypes[, -which(names(prototypes) %in% c("Id", "Class", "X", "i..ID"))]
    #}

    selected.var <- RclusTool.env$data.sample$config$selectFeat

    clusters <- levels(data.sample$clustering[[method]]$label)
    for (i in clusters) {
        test <- data.sample$features$preprocessed$x[which(data.sample$clustering[[method]]$label == i), ]
        if (length(selected.var)==0)
            stop("Training aborted: there isn't any common features between training set and sample set.")

        if (dim(test)[1] > 0) {
            res.knn <- class::knn(prototypes[,selected.var, drop=FALSE], 
                                  test[, selected.var, drop=FALSE], cl=prototypes.label, 
                                  k=min(RclusTool.env$param$classif$unsup$nb.neighbours, min(table(prototypes.label))))
            levels(data.sample$clustering[[method]]$label)[which(levels(data.sample$clustering[[method]]$label) == i)] <- names(which.max(table(res.knn)))[1]
        }
    }
    # update labels and summaries (with new clusters names)
    label <- data.sample$clustering[[method]]$label
    summary <- clusterSummary(data.sample, label)
    data.sample$clustering[[method]] <- list(label=label, summary=summary)
    data.sample
}

#' saveLogFile saves a log txt file at the end of the session, describing the different steps of the analyses
#' @title Log file saving
#' @description Save a log txt file at the end of the session, describing the different steps of the analyses. 
#' @param filename.txt character vector specifying the path and the name of the txt file.
#' @param txt character vector describing the different steps of the analyses.
#' @param dir character vector specifying the directory where to save the txt file.
#' @return log txt file.
#' @importFrom utils alarm
#' 
#' @examples 
#' logfile <- tempfile()
#' saveLogFile(basename(logfile), txt=rbind("Analysis date: ...", "Analysis duration: ..."), 
#'      dirname(logfile))
#' 
#'
#' @keywords internal 
#' 
saveLogFile <- function(filename.txt, txt, dir) {
	filename.txt=file.path(dir,filename.txt)
    if (!is.null(filename.txt)) {
        if (!nchar(filename.txt)) {
            utils::alarm()
            tkmessageBox(message="Filename not valid.", title="Save Logfile")
            return()
        }
        sink(filename.txt)
        cat(txt)
        sink()
    }
}

#' updateClustersNames updates the clusters names according to the names assigning to each prototype
#' @title Clusters names updating
#' @description Update the clusters names according to the names assigning to each prototype. 
#' @param data.sample list containing features, profiles and clustering results.
#' @param protos list of selected prototypes (with index and name).
#' @return data.sample list containing features, profiles and clustering results with updated labels names.
#' 
#' @examples 
#' \donttest{
#' dat <- rbind(matrix(rnorm(100, mean = 0, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 4, sd = 0.3), ncol = 2))
#' tf1 <- tempfile()
#' write.table(dat, tf1, sep=",", dec=".")
#' 
#' x <- importSample(file.features=tf1)
#' 
#' new.protos <- visualizeSampleClustering(x, selection.mode = "prototypes", 
#'				 profile.mode="none", wait.close=FALSE)
#' x <- updateClustersNames(x, new.protos$prototypes)
#' 
#' }
#' @keywords internal 
#' 
updateClustersNames <- function(data.sample, protos) {
    for (meth in names(protos)) {
        if (length(protos[[meth]]) > 0) {
            for (item in 1:length(protos[[meth]]))
                levels(data.sample$clustering[[meth]]$label)[
                                                             which(levels(data.sample$clustering[[meth]]$label)==
                                                                   data.sample$clustering[[meth]]$label[names(protos[[meth]])[item]])] <- protos[[meth]][item]
        }
    }
    data.sample
}

#' addClustering adds a new clustering to an existing set of clusterings (replaces if exists)
#' @title Clustering addition
#' @description adds a new clustering to an existing set of clusterings (replaces if exists).
#' @param clustering list containing a set of clustering results.
#' @param new.clustering.name string naming the clustering to add.
#' @param new.cluster.summary summary object containing statistics about the new clustering.
#' @param new.label vector of labels 
#' @return updated list of clusterings.
#' 
#' @examples 
#' dat <- rbind(matrix(rnorm(100, mean = 0, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 4, sd = 0.3), ncol = 2))
#' 
#' tf <- tempfile()
#' write.table(dat, tf, sep=",", dec=".")
#' 
#' x <- importSample(file.features=tf)
#' 
#' res_new <- KmeansQuick(x$features$initial$x, K=3)
#' labels_new <- formatLabelSample(res_new$cluster, x)
#' cluster.summary.new <- clusterSummary(x, labels_new)
#' 
#' x$clustering <- addClustering(clustering=x$clustering, new.clustering.name='clusterin_test', 
#'                               new.cluster.summary=cluster.summary.new, new.label=labels_new)
#'  
#' 
#' @keywords internal 
#' 
addClustering <- function(clustering, new.clustering.name, new.cluster.summary, new.label)
{
    clustering[[new.clustering.name]] <- list(label=new.label, summary=new.cluster.summary)
    clustering <- c(clustering["no-clustering"], clustering[new.clustering.name],
                    clustering[-which(names(clustering) %in% c("no-clustering", new.clustering.name))])
    clustering
}

#' saveManualProtos saves the profiles and images of prototypes selected manually by user in a scatterplot
#' @title Manual prototypes saving
#' @description Save the profiles and images of prototypes selected manually by user in a scatterplot. 
#' @param data.sample list containing features, profiles and clustering results.
#' @param protos list of selected prototypes (with index and name).
#' @return profiles and images of prototypes selected, csv file with detail.
#' @importFrom grDevices jpeg dev.off
#' @examples 
#' \dontrun{
#' dat <- rbind(matrix(rnorm(100, mean = 0, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 4, sd = 0.3), ncol = 2))
#' tf <- tempfile()
#' write.table(dat, tf, sep=",", dec=".")
#' 
#' x <- importSample(file.features=tf1, dir.save=dirname(tf))
#' 
#' new.protos <- visualizeSampleClustering(x, selection.mode = "prototypes", 
#'				 profile.mode="whole sample", wait.close=FALSE)
#' saveManualProtos(x, new.protos)
#' 
#' }
#' @export 
#' 
saveManualProtos <- function(data.sample, protos) {
    if (!length(data.sample$files$results$prototypes)) {
        warning("directory for prototypes results not specified.")
        return()
    }
    dirsaveManualProtos= file.path(data.sample$files$results$prototypes, paste("protos","manual", format(Sys.time(),'_%Y%m%d_%Hh%Mm%Ss'), sep = "_"))
    unlink(dirsaveManualProtos, recursive = TRUE)
    dir.create(dirsaveManualProtos)
    if (length(unlist(protos)) >= 1) {
        protos.all <- NULL
        for (i in 1:length(protos))
            protos.all <- c(protos.all, protos[[i]])
        protos.dat <- data.sample$features[["initial"]]$x[names(protos.all),]
        protos.dat <- cbind(protos.dat, protos.all)
        Id <- paste(names(protos.all), "protos", data.sample$name, "manual", sep = "_")
        protos.dat <- cbind(protos.dat, Id)
        colnames(protos.dat)[which(names(protos.dat) == "protos.all")] <- "Class"
        write.csv(protos.dat, file.path(dirsaveManualProtos,
                                        paste("protos", data.sample$name, "manual.csv", sep="_")), 
                  append = TRUE, row.names = FALSE)
        # Save selected prototypes (signals + images)
        for (i in unique(protos.all)) {
            dirProtoCluster <- file.path(dirsaveManualProtos, i)
            if (!dir.exists(dirProtoCluster))
                dir.create(dirProtoCluster)
            idx <- names(protos.all)[which(protos.all==i)]
            for (j in idx) {
                grDevices::jpeg(file.path(dirProtoCluster, paste(j, "protos", data.sample$name, "manual.jpg", sep = "_")),
                                quality=70,  width = 600, height = 600)
                plotProfileExtract(data.sample$profiles[[j]], image = data.sample$images[j],
                            profiles.colors=data.sample$config$signalColor, title = "", image.dir=data.sample$files$images)
                grDevices::dev.off()
            }
        }
    }
}
