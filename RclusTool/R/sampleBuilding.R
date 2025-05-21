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

#' loadSample reads RDS sample file; sample is preprocessed by call to function preprocessSample()
#' @title Sample loading
#' @description Load and preprocess sample.
#' @param file.RDS character vector for the name of the .RDS file where built data.sample object is saved.
#' @param file.config character vector for the name of the configuration file.
#' @param RclusTool.env environment in which all global parameters, raw data and results are stored.
#' @return data.sample loaded data.sample. 
#' @seealso \code{\link{importSample}}
#' 
#' @examples 
#' dat <- rbind(matrix(rnorm(100, mean = 0, sd = 0.3), ncol = 2), 
#'            matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2), 
#'            matrix(rnorm(100, mean = 4, sd = 0.3), ncol = 2))
#' tf <- tempfile()
#' write.table(dat, tf, sep=",", dec=".")
#' 
#' x <- importSample(tf, dir.save=dirname(tf))
#' 
#' res <- loadSample(x$files$RDS)       
#' 
#'
#' @keywords internal
#' 
loadSample <- function(file.RDS, RclusTool.env=initParameters(), file.config="") {
    data.sample <- NULL

    if (file.exists(file.RDS)) {
        data.sample <- readRDS(file.RDS)

        if (file.exists(file.config)) {
            operations <- loadPreprocessFile(file.config)
        } else operations <- data.sample$config$operations

        new.data.sample <- applyPreprocessing(data.sample, operations, RclusTool.env)
        if (!is.null(new.data.sample)) {
            data.sample <- new.data.sample
        } 
    }
    data.sample
}

#' listDerivableFeatureSpaces build the list of feature spaces matching parameters.
#' @title Builds list of derivable feature spaces
#' @description Builds list of derivable feature spaces.
#' @param scaling boolean TRUE if scaling required.
#' @param pca boolean TRUE if Principal Components Analysis required.
#' @param spectral boolean TRUE if Spectral Embedding required.
#' @param RclusTool.env environment in which all global parameters, raw data and results are stored.
#' @return character vector naming feature spaces.
#' 
#' @examples 
#' 
#' listDerivableFeatureSpaces(scaling=TRUE, spectral=TRUE)
#'
#' @keywords internal
#' 
listDerivableFeatureSpaces <- function(scaling=FALSE, pca=FALSE, spectral=FALSE, RclusTool.env=initParameters())
{
    prefix.space.name <- c()
    space.names <- "preprocessed"
    if (scaling)
    {
        space.names <- "." #point, doubled, will then be removed
        prefix.space.name <- c(prefix.space.name, "scaled")
    }

    if (pca)
        space.names <- c(space.names, "pca")
    if (spectral)
        space.names <- c(space.names, "spectral")

    merged.prefix <- paste(prefix.space.name, collapse=".") 
    if (nchar(merged.prefix))
        space.names <- sapply(merged.prefix, paste, space.names, sep=".")

    # empty space name with prefix values contain ".." that must be set to ""
    space.names <- sapply(space.names, function(x) gsub(pattern="..", replacement="", x=x, fixed=TRUE))
    names(space.names) <- space.names

    # long names
    sapply(space.names, featSpaceNameConvert, short2long=TRUE, RclusTool.env)
}

#' featSpaceNameConvert converts feature space names: either long name to short name, or short name to long name.
#' @title Feature Space Name Conversion
#' @description Converts Feature Space Name.
#' @param name2convert character vector for name to convert.
#' @param short2long boolean setting the direction of conversion (higher priority).
#' @param RclusTool.env list of parameters.
#' @return character name.
#' 
#' @examples 
#' 
#' long.name <- "Scaled - Principal Component Analysis"
#' short.name <- "scaled.pca"
#' 
#' res1 <- featSpaceNameConvert(long.name, short2long=TRUE)
#' res2 <- featSpaceNameConvert(short.name, short2long=FALSE)
#' 
#' @keywords internal
featSpaceNameConvert <- function(name2convert, short2long=TRUE, RclusTool.env=initParameters())
{
    set <- RclusTool.env$param$preprocess$featSpaceNames
    result <- NULL
    if (!(short2long))
    {
        split.names <- unlist(strsplit(name2convert, split=" - ", fixed=TRUE))
        reduced.names <- sapply(split.names, function(x) {names(set)[set==x]})
        result <- paste(rev(reduced.names), collapse=".")
    } else {
        split.names <- unlist(strsplit(name2convert, split=".", fixed=TRUE))
        long.names <- set[split.names]
        result <- paste(rev(long.names), collapse=" - ")
    }

    result
}

#' function to import sample from CSV files; sample is preprocessed
#' @title Sample importation
#' @description Import the required and the optional files, and build a dataset.
#' @param file.features character vector specifying the csv file containing features data.
#' @param file.meta character vector specifying the txt file containing metadata.
#' @param file.profiles character vector specifying the csv file containing profiles data.
#' @param file.RDS character vector for a RDS file containing a data.sample object. This file is automatically saved when importing a (csv-)file-features. When both a csv-file-features and a RDS file are given, the last one is ignored.
#' @param file.config character vector for the name of the configuration file.
#' @param dir.images character vector containing the path of images directory.
#' @param dir.save character vector specifying path of the working directory to save results ; "" to not save any results
#' @param sepFeat character specifying the field separator for the csv file containing features data.
#' @param decFeat character specifying the decimal points for the csv file containing features data.
#' @param naFeat vector containing missing values for the csv file containing features data.
#' @param sepSig character specifying the field separator for the csv file containing profiles data.
#' @param decSig character specifying the decimal point for the csv file containing profiles data.
#' @param naSig vector containing missing values for the csv file containing profiles data.
#' @param headerCSV boolean if TRUE (default) the file contains the names of the variables as its first line. 
#' @param RclusTool.env environment in which data and intermediate results are stored.
#' @param ... parameters adressed to read.csv functions.
#' @return data.sample loaded data.sample.
#' @importFrom tools file_path_sans_ext file_ext file_path_as_absolute
#' @importFrom utils read.csv read.table
#' @seealso \code{\link{loadSample}}
#' @examples 
#' dat <- rbind(matrix(rnorm(100, mean = 0, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 4, sd = 0.3), ncol = 2))
#' tf1 <- tempfile()
#' write.table(dat, tf1, sep=",", dec=".")
#' 
#' metadat <- rbind("First metadata: ...", "Second metadata: ...")
#' tf2 <- tempfile()
#' writeLines(metadat, tf2)
#' 
#' x <- importSample(file.features=tf1, file.meta=tf2)
#' 
#'
#' @export 
#' 
importSample <- function(file.features="", file.meta="", file.profiles="", file.RDS="", file.config="", dir.images="", dir.save="", sepFeat = ",", decFeat = ".", naFeat=c("","NA"), sepSig = ",", decSig= ".", naSig=c("","NA"), headerCSV=TRUE, RclusTool.env=new.env(), ...) {
    data.sample <- NULL
    naFeat <- unique(c("","NA",naFeat))
    naSig <- unique(c("","NA",naSig))

    if ((file.features!="")&&(file.RDS!=""))
    {
        message("Function importSample can't accept both csv-features file and RDS file: in this case, the RDS file is ignored.")
        file.RDS <- ""
    }

    if ( !dir.exists(dir.save) )
    {
        message("Save directory does not exist: no RDS file saved.")
    }

    if (!length(RclusTool.env))
        initParameters(RclusTool.env)

    # get sample name
    if (file.exists(file.features)) {
        file.features.no.ext <- tools::file_path_sans_ext(file.features)
        sample.name <- basename(file.features.no.ext)
    } else if (file.exists(file.RDS)) {
        file.RDS.no.ext <- tools::file_path_sans_ext(file.RDS)
        sample.name <- basename(file.RDS.no.ext)
    } else return(NULL)


    data.sample <- list()
    feature.csv <- NULL

    # case : loading given RDS file
    if (file.exists(file.RDS)) {
        message("Import RDS...")
        data.sample <- loadSample(file.RDS, RclusTool.env, file.config)
    }

    RDS.loaded <- length(data.sample)
    RDS.to.be.saved <- FALSE

    # case : no RDS file loaded or wrong loaded file
    if (!RDS.loaded)
    {
        RDS.to.be.saved <- TRUE

    	# features extraction
        message("Import Features...")
        features.csv <- utils::read.csv(file.features, sep = sepFeat, dec = decFeat, na.strings=naFeat, header = headerCSV, ...)
        if (ncol(features.csv)==1) {
            data.sample <- NULL
            return(data.sample)
        }
        # ID extraction of features rows
        is.row.names <- is.element("row.names", names(match.call()))
        if (!is.row.names) {
            ind.id.feat <- which(toupper(names(features.csv))=="ID")[1]
            if (is.na(ind.id.feat)) {
            	ind.id.feat <- which(grepl('ID',names(features.csv)))
            	if (length(ind.id.feat)==0 || length(ind.id.feat)>1){
            		ind.id.feat<-NA
            	}
            }	
            if (!is.na(ind.id.feat)) {
                id.feat <- features.csv[[ind.id.feat]]
                if (!is.factor(id.feat))
                    features.csv[[ind.id.feat]] <- as.factor(id.feat)
                rownames(features.csv) <- id.feat
            }
        }

        #purge features from nan and na
        a.retirer <- sapply(features.csv, function (x) any(is.na(x)))
        if (any(a.retirer)) {
            message(paste("Drop variables containing NA values:", paste(colnames(features.csv)[a.retirer], collapse= " ")))
            features.csv[a.retirer] <- NULL
        }
        a.retirer <- sapply(features.csv, function (x) any(is.nan(x)))
        if (any(a.retirer)) {
            message(paste("Drop variables containing NaN values:", paste(colnames(features.csv)[a.retirer], collapse= " ")))
            features.csv[a.retirer] <- NULL
        }


        within(data.sample, {
               name <- sample.name
               size <- nrow(features.csv)

               files <- list(features=file.features, 
                             profiles=file.profiles, 
                             RDS=file.path(dir.save, paste(sample.name, ".RDS", sep="")),
                             meta=file.meta , 
                             images=dir.images,
                             dir=dirname(tools::file_path_as_absolute(file.features)))

               features <- list()
               features[["initial"]] <- list(x=features.csv, logscale=rep(FALSE,ncol(features.csv)), prefered=NULL)

               names(features[["initial"]]$logscale) <- colnames(features.csv)

               clustering <- list()

			   }) -> data.sample

	} else {
        data.sample$files$dir <- dirname(tools::file_path_as_absolute(file.RDS))
        data.sample$files$RDS <- file.RDS
    }
	

	if (file.exists(file.profiles))
    {
        RDS.to.be.saved <-TRUE

        message("Import Profiles...")
        profiles.csv <- NULL
        profiles.csv <- utils::read.csv(file.profiles, sep = sepSig, dec = decSig, na.strings=naSig, header = headerCSV, ...)
        ind.id.sig <- which(toupper(names(profiles.csv))=="ID")[1]
        if (is.na(ind.id.sig)) {
            ind.id.sig <- which(grepl('ID',names(profiles.csv)))
            if (length(ind.id.sig)==0 || length(ind.id.sig)>1){
                ind.id.sig<-NA
            }
        }	
        if (is.na(ind.id.sig))
            ind.id.sig <- 1 # ID 1st column ?

        if (!is.factor(profiles.csv[[ind.id.sig]]))
            profiles.csv[[ind.id.sig]] <- as.factor(profiles.csv[[ind.id.sig]]) # individuals IDs 

        profiles.ok <- !apply(profiles.csv,2,function(x) any(is.na(x)))
        profiles.csv <- profiles.csv[,profiles.ok,drop=FALSE]
        profiles <- NULL
        if (!is.null(profiles.csv)) {
            profiles <- by(profiles.csv, profiles.csv[[ind.id.sig]], function(x) as.matrix(x[,-ind.id.sig,drop=FALSE]))
        }
        data.sample$profiles <- profiles
        data.sample$files$profiles=file.profiles       
    }

    if (dir.exists(dir.images))
    {
        RDS.to.be.saved <- TRUE

    	message("Import Images...")
    	if (is.null(features.csv)){
    		features.csv <- data.sample$features[["initial"]]$x
    	}
    	images.dir <- ""
    	images <- rep(NA, nrow(features.csv))
    	names(images) <- rownames(features.csv)
        imgs <- list.files(dir.images)
        ind <- tools::file_path_sans_ext(imgs)
        names(imgs) <- ind
        ext <- tolower(tools::file_ext(imgs))
        ext.ok <- ext %in% c("jpg", "png")
        imgs <- imgs[ext.ok]
        imgs <- imgs[intersect(names(imgs), names(images))]
        images[names(imgs)] <- imgs
        data.sample$images <- images   
        data.sample$files$images=dir.images
    }
    
    if (file.exists(file.meta))
    {
        RDS.to.be.saved <- TRUE

    	message("Import Metadata...")
		meta.txt <- as.character(unlist(utils::read.table(file.meta, sep = "\n")))
		metadata <- list()
        metadata$x <- meta.txt 
        data.sample$metadata$x <- metadata$x
        data.sample$files$meta=file.meta     
    }


    # Create the results folder
    old.results.dir <- data.sample$files$results

    if (dir.save != "") {
        data.sample <- createResFolder(data.sample = data.sample, dir.path=dir.save)
    } else
        data.sample$files$RDS <- NULL

    if (!identical(old.results.dir, data.sample$files$results))
        RDS.to.be.saved <- TRUE

    # Saving RDS file
    if (RDS.to.be.saved && (dir.save != ""))
        saveRDS(data.sample, file=data.sample$files$RDS, compress="bzip2")

    operations <- NULL
    if (file.exists(file.config))
        operations <- loadPreprocessFile(file.config)
    new.data.sample <- applyPreprocessing(data.sample, operations, RclusTool.env)
    if (!is.null(new.data.sample)) {
        data.sample <- new.data.sample
    } 

    # Load previous results and computations (pca and/or spectral embedding)
    data.sample <- loadPreviousRes(data.sample = data.sample, noise.cluster=RclusTool.env$param$preprocess$noise.cluster)

    message(paste("Sample size = ", data.sample$size))

    data.sample
}



#' Function to purgeSample from its temporary computing results
#' @title Sample purging
#' @description Purge sample from its temporary computing results.
#' @param data.sample sample object
#' @param purge.preprocessing boolean: if TRUE (default), the configuration is reset.
#' @param purge.clustering boolean: if TRUE (default), the clusterings are reset.
#' @param user.expert boolean : if FALSE (default), initial classification feature space is PCA.
#' @return data.sample purged data.sample.
#' 
#' @examples 
#' dat <- rbind(matrix(rnorm(100, mean = 0, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 4, sd = 0.3), ncol = 2))
#' tf <- tempfile()
#' write.table(dat, tf, sep=",", dec=".")
#' 
#' x <- importSample(file.features=tf)
#' x <- computeUnSupervised(x, K=3, method.name="K-means")
#' x <- purgeSample(x, purge.clustering=TRUE)
#' 
#'
#' @export 
#' 
purgeSample <- function(data.sample, purge.preprocessing=TRUE, purge.clustering=TRUE, user.expert=FALSE) {
    if (!purge.preprocessing && !is.null(data.sample$config)) {
        data.sample$features <- data.sample$features[1:2]
    } else {
        data.sample$id.clean <- 1:data.sample$size
        data.sample$features <- data.sample$features[1]
        data.sample$features[["preprocessed"]] <- list()
        data.sample$sampling <- NULL

        numeric.ok <- sapply(data.sample$features[[1]]$x, is.numeric)
        data.sample$features[["preprocessed"]]$x <- data.sample$features[[1]]$x[numeric.ok]
        space <- "initial"
        if (!user.expert)
            space <- "pca"
        data.sample$config <- list(selectFeat=c(), logFeat=c(), defaultFeat=c(), signalColor=c(), operations=NULL, space=space, pca.nb.dims=0)
    }
    if (purge.clustering)
        data.sample$clustering <- list()
    data.sample
}


#' loadPreprocessFile reads a csv file configuration with instruction to remove bad particles and builds object config that describes all preprocessings done
#' @title Preprocessing loading
#' @description Load a csv file configuration with instruction to remove bad observations and builds object config that describes all preprocessings to apply.
#' @param file.config character vector specifying the name of a csv file with preprocessing instructions.
##' @param ... parameters adressed to read.csv functions.
#' @importFrom utils read.csv
#' @return operations character matrix describing all preprocessing operations.
#' @seealso \code{\link{applyPreprocessing}}
#' 
#' @examples
#' instr <- rbind(c("select","x","log",""), c("select","y","log",""))
#' tf <- tempfile()
#' write.table(instr, tf, sep=",", col.names = FALSE, row.names = FALSE)
#' 
#' operations <- loadPreprocessFile(tf)
#'
#' @export 
#' 
loadPreprocessFile <- function(file.config, ...)
{
    operations <-  utils::read.csv(file.config, header=FALSE, na.strings=c("NA",""), colClasses="character", ...)
    if (ncol(operations)!=4)
        stop("Preprocessing error: bad config file (number of fields per line should be 4)")
    as.matrix(operations)
}

#' applyPreprocessing applies a new preprocess to a data.sample object
#' @title Preprocessing application
#' @description Apply a new preprocess to a data.sample object.
#' @param data.sample sample object.
#' @param operations list of data.frames describing all preprocessing operations. 
#' @param RclusTool.env environment in which all global parameters, raw data and results are stored.
#' @param reset boolean : if TRUE (default) the configuration is reset.
#' @param preprocessed.only boolean : if TRUE (default) processing are restricted to the "preprocessed" features.
#' @importFrom utils tail
#' @return The \code{data.sample} sample object on which was applied the \code{operations} or NULL if preprocessing operations fail.
#' @seealso \code{\link{loadPreprocessFile}}
#' @examples
#' dat <- rbind(matrix(rnorm(150, mean = 2, sd = 0.3), ncol = 3), 
#'              matrix(rnorm(150, mean = 4, sd = 0.3), ncol = 3), 
#'              matrix(rnorm(150, mean = 6, sd = 0.3), ncol = 3))
#' colnames(dat) <- c("x","y","z")
#' tf1 <- tempfile()
#' write.table(dat, tf1, sep=";", dec=",")
#' x <- importSample(file.features=tf1, sepFeat=";", decFeat=",")
#' 
#' instr <- rbind(c("select","x","log",""), c("select","y","log",""))
#' tf2 <- tempfile()
#' write.table(instr, tf2, sep=",", col.names = FALSE, row.names = FALSE)
#' 
#' operations <- loadPreprocessFile(tf2)
#' x <- applyPreprocessing(x, operations)
#' 
#'
#' @export 


applyPreprocessing <- function(data.sample, operations=NULL, RclusTool.env=initParameters(), reset=TRUE, preprocessed.only=FALSE) {
    if (is.null(data.sample$config$operations))
        data.sample$config$operations <- matrix(ncol=4,nrow=0)

    if (!reset) {
        idem <- identical(data.sample$config$operations, operations)
        if (idem)
            return(data.sample)
    }

    if (reset)
        data.sample <- purgeSample(data.sample, purge.preprocessing=TRUE, purge.clustering=FALSE, user.expert=(RclusTool.env$gui$user.type=="expert"))
	
	#if (!nrow(data.sample$config$operations))
	#	return(data.sample)

    #format
    #if (!is.null(operations))
    #    operations <- tolower(operations)

    #develop short operations (combinations select+log for example)
    operations <- detailOperation(operations)

    if (preprocessed.only)
    {
        to.remove <- tolower(operations[,1]) %in% c("projection", "space", "sampling")
        operations <- operations[!to.remove,,drop=FALSE]
    }

    pca.compute <- FALSE
    pca.nb.dims <- NA
    spectral.compute <- FALSE
    sampling.compute <- FALSE
    scaling.compute <- FALSE
    sampling.size.max <- NA
    data.sample$config$scaling <- FALSE
    default.space.info <- NULL

    for (row in rownames(operations))
    {
        op <- operations[row,]
        type <- tolower(op[1])

        feature <- NULL

        # filter observations
        if (type=="outlier")
        {
            feature <- matchNames(op[2], colnames(data.sample$features$preprocessed$x))
            sign <- op[3]
            threshold <- as.numeric(op[4])
            if (length(feature)) {
                if (sign==">") {
                    data.sample$id.clean <- data.sample$id.clean[!(data.sample$features[["preprocessed"]]$x[data.sample$id.clean,feature] > threshold)]
                } else if (sign=="<") {
                    data.sample$id.clean <- data.sample$id.clean[!(data.sample$features[["preprocessed"]]$x[data.sample$id.clean,feature] < threshold)]
                } else if (sign=="=") {
                    data.sample$id.clean <- data.sample$id.clean[!(data.sample$features[["preprocessed"]]$x[data.sample$id.clean,feature] == threshold)]
                } 
            } else {
                message("Error: Corrupted preprocessing configuration, please retry with another one (filter operation).")
                warning("Corrupted preprocessing configuration, please retry with another one (filter operation).")
                return(NULL)
            }
        }
        #select features
        if (type=="select")
        {
            feature <- matchNames(op[2], colnames(data.sample$features$preprocessed$x))
            if (length(feature)){
                data.sample$config$selectFeat <- c(data.sample$config$selectFeat, feature)
            } else {
                message("Error: Corrupted preprocessing configuration, please retry with another one (select operation).")
                warning("Corrupted preprocessing configuration, please retry with another one (select operation).")
                return(NULL)
            }
        }

        if (type=="default")
        {
            feature <- matchNames(op[2], colnames(data.sample$features$preprocessed$x))
            if (length(feature))
            {
                id <- which(data.sample$config$logFeat==feature)
                #if (length(id))
                #feature <- names(data.sample$config$logFeat)[id] #pour la visu, on vire le log
                data.sample$config$defaultFeat <- c(data.sample$config$defaultFeat, feature)
            } else {
                message("Error: Corrupted preprocessing configuration, please retry with another one (default operation).")
                warning("Corrupted preprocessing configuration, please retry with another one (default operation).")
                return(NULL)
            }
        }

        if ((type=="signalcolor")&&(!is.null(data.sample$profiles)))
        {
            feature <- matchNames(op[2], colnames(data.sample$profiles[[1]]))
            if (length(feature)){
                data.sample$config$signalColor[feature] <- tolower(op[3])
            } else {
                message("Error: Corrupted preprocessing configuration, please retry with another one (signalColor operation). ")
                warning("Corrupted preprocessing configuration, please retry with another one (signalColor operation). ")
                return(NULL)
            }                
        }

        if ((type=="/")||(type=="*")||(type=="+")||(type=="-"))
        {
            operator <- type
            feature <- matchNames(op[2:3], colnames(data.sample$features$preprocessed$x)) 
            operation.name <- paste(feature[1], operator, feature[2])
            if (length(feature)==2)
            {
                if (operator=="/") {
                    y <- data.sample$features[["preprocessed"]]$x[, feature[1]] / removeZeros(data.sample$features[["preprocessed"]]$x[, feature[2]], threshold=RclusTool.env$param$preprocess$zero.threshold) #attention au seuil
                } else if (operator=="+") {
                    y <- data.sample$features[["preprocessed"]]$x[, feature[1]] + data.sample$features[["preprocessed"]]$x[, feature[2]]
                } else if (operator=="-") {
                    y <- data.sample$features[["preprocessed"]]$x[, feature[1]] - data.sample$features[["preprocessed"]]$x[, feature[2]]
                } else if (operator=="*") {
                    y <- data.sample$features[["preprocessed"]]$x[, feature[1]] * data.sample$features[["preprocessed"]]$x[, feature[2]]
                } 
                y <- matrix(y, ncol=1)
                data.sample$features[["preprocessed"]]$x[,operation.name] <- y
            } else {
                message("Error: Corrupted preprocessing configuration, please retry with another one (/*+- operation).")
                warning("Corrupted preprocessing configuration, please retry with another one (/*+- operation).")
                return(NULL)
            }  
            feature <- operation.name
        }

        if (type=="log")
        {
            feature <- matchNames(op[2], colnames(data.sample$features$preprocessed$x))
            id <- which(colnames(data.sample$features[["preprocessed"]]$x)==feature)
            z <- data.sample$features[["preprocessed"]]$x[, feature]
            ok <- all(z>=-RclusTool.env$param$preprocess$zero.threshold)
            if (ok && length(feature)) {
                newName <- paste(feature, "(log)")
                names(newName) <- feature
                data.sample$features[["preprocessed"]]$x[, newName] <- log10(removeZeros(z, threshold=RclusTool.env$param$preprocess$zero.threshold, positive=TRUE))
                data.sample$config$logFeat[feature] <- newName
            } else {
                message("Error: Corrupted preprocessing configuration, please retry with another one (log operation).")
                warning("Corrupted preprocessing configuration, please retry with another one (log operation).")
                return(NULL)
            } 
        }

        if (type=="projection")
        {
            if (tolower(op[2])=="pca")
            {
                pca.nb.dims <- as.numeric(op[3])
                pca.compute <- TRUE
            }

            if (tolower(op[2])=="spectral")
                spectral.compute <- TRUE
        }

        if (type=="scaling")
        {
            scaling.compute <- TRUE
            data.sample$config$scaling <- TRUE
        }

        if (type=="sampling")
        {
            sampling.size.max <- as.numeric(op[2])
            sampling.compute <- TRUE
        }

        if (type=="space")
        {
            default.space.info <- tolower(op[2])
        }
    }

    # filter
    if (length(data.sample$id.clean) == 0)
        message("Problem: all observations are cleaned = considered as noise !!")

    # log
    logscale <- is.element(colnames(data.sample$features[["preprocessed"]]$x), data.sample$config$logFeat)
    names(logscale) <- colnames(data.sample$features[["preprocessed"]]$x)
    data.sample$features[["preprocessed"]]$logscale <- logscale

    # clusterings
    label <- rep(RclusTool.env$param$preprocess$noise.cluster, length(data.sample$id.clean))
    label <- formatLabelSample(label, data.sample, noise.cluster=RclusTool.env$param$preprocess$noise.cluster)
    summary <- clusterSummary(data.sample, label, summary.functions=RclusTool.env$param$analysis$summary.functions)
    data.sample$clustering[["no-clustering"]] <- list(label=label,
                                                      summary=summary,
                                                      K=1)
    data.sample$config$operations <- rbind(data.sample$config$operations, operations)

    if (is.null(data.sample$config$selectFeat))
            data.sample$config$selectFeat <- colnames(data.sample$features[["preprocessed"]]$x)
    if (length(data.sample$config$selectFeat)<2)
    {
        warning("The number of selected features is lower than 2: then, all features are selected.")
        data.sample$config$selectFeat <- colnames(data.sample$features[["preprocessed"]]$x)
    }

    data.sample$config$defaultFeat <- utils::tail(data.sample$config$defaultFeat, 2) # last 2 features are kept
    if (length(data.sample$config$defaultFeat)>=1)
        names(data.sample$config$defaultFeat)[1] <- "parH"
    if (length(data.sample$config$defaultFeat)>=2)
        names(data.sample$config$defaultFeat)[2] <- "parV"

    prefix.space.name <- c()
    space.name <- "preprocessed"

    # scaling check
	if (scaling.compute)
	{
        prefix.space.name <- c(prefix.space.name, "scaled")
        space.name <- paste(prefix.space.name, collapse=".")

        if ((reset==TRUE)||is.null(data.sample$features[[space.name]]))
        {
            data.sample$features[[space.name]] <- data.sample$features[["preprocessed"]]
            data.sample$features[[space.name]]$x <- as.data.frame(scale(data.sample$features[["preprocessed"]]$x, center=TRUE, scale=TRUE))
        }
	}

    # sampling check
    if (sampling.compute)
    {
        if (is.na(sampling.size.max) || (sampling.size.max<0)){
            warning("Please enter a valid number for sampling > 0")
            return(NULL)
        }

        # Call 'computeSampling' function
        if ((reset==TRUE)||is.null(data.sample$sampling)|| (!is.null(data.sample$config$sampling.size.max)&&(sampling.size.max!=0)&&(data.sample$config$sampling.size.max!=sampling.size.max)))
        {
            data.sample$sampling <- computeSampling(data.sample$features[[space.name]]$x[data.sample$id.clean,data.sample$config$selectFeat,drop=FALSE], K.max=RclusTool.env$param$classif$unsup$K.max, 
                                                    sampling.size.max=sampling.size.max, kmeans.variance.min=RclusTool.env$param$classif$unsup$kmeans.variance.min)
            data.sample$config$sampling.size.max <- sampling.size.max
        }
    }

    # projections
    if (pca.compute) 
    {
        if ((is.na(pca.nb.dims) || (pca.nb.dims<0) || (pca.nb.dims==1) || (pca.nb.dims>length(data.sample$config$selectFeat) )
             || (pca.nb.dims>RclusTool.env$param$preprocess$pca$nb.dims.max))){
            warning(paste("Please enter a valid number of component principals <=",
                          min(ncol(data.sample$features[["preprocessed"]]$x), RclusTool.env$param$preprocess$pca$nb.dims.max)))
            return(NULL)
        } 

        #prefix.space.name <- c(prefix.space.name, "pca")
        space.name <- paste(c(prefix.space.name, "pca"), collapse=".")
        space.name.full <- paste(c(prefix.space.name, "pca_full"), collapse=".")
        
        if ((reset==TRUE)||is.null(data.sample$features[[space.name]]))
        {

            # Call 'computePcaSample' function
            data.sample$features[c(space.name.full,space.name)] <- computePcaSample(data.sample,
                                                                               pca.nb.dims = pca.nb.dims, selected.var=data.sample$config$selectFeat, 
                                                                               prcomp.options=list(center=RclusTool.env$param$classif$sup$prcomp$center,
                                                                                                   scale=scaling.compute), pca.variance.cum.min=RclusTool.env$param$preprocess$pca$variance.cum.min)
            data.sample$config$pca.nb.dims <- pca.nb.dims
        }
    }

    if (spectral.compute)
    {
        prefix.space.name <- c(prefix.space.name, "spectral")
        space.name <- paste(prefix.space.name, collapse=".")

        sampling.change <- FALSE
        if (!is.null(data.sample$features[[space.name]]))
        { 
            sampling.ok <- (nrow(data.sample$features[[space.name]]$x) == length(data.sample$sampling$selection.ids))
            if (sampling.compute && !sampling.ok)
                sampling.change <- TRUE

            no.sampling.ok <- (nrow(data.sample$features[[space.name]]$x) == nrow(data.sample$features$preprocessed$x))
            if (!sampling.compute && !no.sampling.ok)
                sampling.change <- TRUE
        }

        if (reset || is.null(data.sample$features[[space.name]]) || sampling.change)
        { 
            data.sample$features[[space.name]] <- computeSpectralEmbeddingSample(data.sample = data.sample, 
                                                                                 use.sampling = sampling.compute,,
                                                                                 sampling.size.max=sampling.size.max, 
                                                                                 scale=scaling.compute, 
                                                                                 selected.var = data.sample$config$selectFeat,
                                                                                 RclusTool.env=RclusTool.env)
     
        }
    }

    if (!is.null(default.space.info))
    {
        space <- "preprocessed"
        pattern <- tolower(default.space.info)
        if ((pattern=="pca")||(pattern=="principal components analysis"))
            space <- grep("pca", names(data.sample$features), fixed=TRUE, value=TRUE)[1]
        if ((pattern=="spectral")||(pattern=="spectral embedding"))
            space <- grep("spectral", names(data.sample$features), fixed=TRUE, value=TRUE)[1]
        data.sample$config$default.classif.feature.space <- space
    }

    data.sample
}

#' addOperation create configuration object for the datasample
#' @title Add operation
#' @param parameterList, list of Preprocessing instructions for an operation. 
#' @param featureOperations, matrix where to list Operations on features.
#' @return The configuration object created by the list of preprocessing instructions \code{parameterList} in \code{featureOperations}.
#' @examples
#' featOp <- matrix(ncol=4,nrow=0)
#' #Adding two differents variables
#' featOp <- addOperation(list("+","x","y"), featOp)
#' #Select a variable
#' featOp <- addOperation(list("select","x"), featOp)
#' #Change a profile color
#' featOp <- addOperation(list("signalColor","x","grey"), featOp)
#' #Make a PCA projection (with the number of dimensions)
#' featOp <- addOperation(list("projection","pca","0"), featOp)
#' #Make a spectral projection
#' featOp <- addOperation(list("projection","spectral"), featOp)
#' #Scale the data
#' featOp <- addOperation(list("scaling","on"), featOp)
#' #Sample the data (with a sampling size)
#' featOp <- addOperation(list("sampling","150"), featOp)
#' #Make a log transformation of a variable
#' featOp <- addOperation(list("log","x"), featOp)
#' 
#' @export
#'


addOperation <- function(parameterList, featureOperations){
    mat <- formatParameterList(parameterList)
    name <- row.names(mat)
    matchName <- matchNames(row.names(mat), row.names(featureOperations))
    	if (!is.null(matchName)) {
    		featureOperations[name,] <- mat
    	} else
    		featureOperations <- rbind(featureOperations, mat)
    featureOperations
}
    
#' makeFeatureSpaceOperations create config object for datasample
#' @title Make operation config object to build feature spaces
#' @param pca boolean: if TRUE, Principal Components Analysis is applied to reduce the data space.
#' @param pca.nb.dims number of principal components kept. If pca.nb.dims=0, this number is computed automatically.
#' @param spectral boolean: if TRUE, spectral embedding is applied to reduce the data space.
#' @param sampling boolean: if TRUE, data sampling is used.
#' @param sampling.size.max : maximum size of the sampled dataframe.
#' @param scaling boolean: if TRUE, scaling is applied.
#' @return configuration object created from parameters.
#' @examples
#'
#' operations <- makeFeatureSpaceOperations(pca=TRUE)
#' 
#' @keywords internal

makeFeatureSpaceOperations <- function(pca=FALSE, pca.nb.dims=0, spectral=FALSE, sampling=FALSE, sampling.size.max=0, scaling=FALSE) {
	featureOperations <- matrix(ncol=4,nrow=0)
	txt.scaling <- NULL
	txt.space <- "preprocessed"
	if (sampling==TRUE){
		featureOperations <- addOperation(list("sampling",as.character(sampling.size.max)), featureOperations)
	}
	if (scaling==TRUE){
		featureOperations <- addOperation(list("scaling","on"), featureOperations)
		txt.space <- NULL
		txt.scaling="scaled"
	}
	if (pca==TRUE){
		featureOperations <- addOperation(list("projection","pca", as.character(pca.nb.dims)), featureOperations)
		txt.space<- "pca"
	}
	if (spectral==TRUE){
		featureOperations <- addOperation(list("projection","spectral"), featureOperations)
		txt.space<- "spectral"
	}
	char.vector <- c(txt.scaling, txt.space)
	space <- paste(char.vector, collapse=".")  
	operations <- list(instr=featureOperations, space=space)
	operations
}


#' formatLabelSample formats labels for unsupervised classification and adds cleaned observations as 'Noise'
#' @title Labels formatting
#' @description Format labels for unsupervised classification and add cleaned observations as 'Noise'.
#' @param label vector of labels.
#' @param data.sample sample object.
#' @param new.labels boolean: if TRUE (default), new names are given for each cluster (beginning by 'Cluster'). 
#' @param use.sampling boolean: if TRUE (not default), data.sample$sampling is used to generalize label from sampling set to the whole set.
#' @param noise.cluster character name of the cluster "noise".
#' @importFrom stats relevel
#' @return new.labels formatted labels.
#' 
#' @examples 
#' dat <- rbind(matrix(rnorm(100, mean = 0, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 4, sd = 0.3), ncol = 2))
#' tf <- tempfile()
#' write.table(dat, tf, sep=",", dec=".")
#' 
#' x <- importSample(file.features=tf)
#' res <- KmeansQuick(x$features$initial$x, K=3)
#' 
#' new.labels <- formatLabelSample(res$cluster, x)
#' 
#'
#' @export
#' 
formatLabelSample <- function(label, data.sample, new.labels=TRUE, use.sampling=FALSE, noise.cluster="Noise") {
    label <- sortLabel(label)

    if (new.labels)
        levels(label) <- paste("Cluster", 1:length(levels(label)))

    niv <- levels(label)
    if (!(noise.cluster %in% niv)) {
        niv <- c(noise.cluster, niv)
        label <- factor(label, levels=niv)
    } else {
        label <- stats::relevel(label, noise.cluster)
        niv <- levels(label)
    }

    if (use.sampling)
    {
        names(label) <- data.sample$sampling$selection.ids
        label <- label[data.sample$sampling$matching]
    }

    new.label <- factor(rep(noise.cluster, data.sample$size), levels=niv)
    new.label[data.sample$id.clean] <- label
    names(new.label) <- rownames(data.sample$features[[1]]$x)

    new.label
}

#' sortLabel sorts clusters labels by decreasing sizes, in unsupervised classification
#' @title Clusters labels sorting
#' @description Sort clusters labels by decreasing sizes for unsupervised classification.
#' @param label vector of labels (integers).
#' @importFrom stats reorder
#' @return sorted labels
#' 
#' @examples 
#' dat <- rbind(matrix(rnorm(100, mean = 0, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 4, sd = 0.3), ncol = 2))
#' tf <- tempfile()
#' write.table(dat, tf, sep=",", dec=".")
#' 
#' x <- importSample(file.features=tf)
#' res <- KmeansQuick(x$features$initial$x, K=3)
#' 
#' sortLabel(res$cluster)
#' 
#'
#' @keywords internal
#' 
sortLabel <- function(label) {
    stats::reorder(label, rep(1, length(label)), function(x) {-sum(x)})
}

#' importLabelSample imports labels
#' @title Labels importation
#' @description Import labels and add cleaned observations as 'noise'.
#' @param label vector of labels.
#' @param data.sample sample object.
#' @param noise.cluster character name of the cluster "Noise".
#' @importFrom stats relevel
#' @return new.labels imported labels.
#' 
#' @examples 
#' dat <- rbind(matrix(rnorm(100, mean = 0, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 4, sd = 0.3), ncol = 2))
#' tf <- tempfile()
#' write.table(dat, tf, sep=",", dec=".")
#' 
#' x <- importSample(file.features=tf)
#' res <- KmeansQuick(x$features$initial$x, K=3)
#' 
#' new.labels <- importLabelSample(res$cluster, x)
#' 
#'
#' @keywords internal 
#' 
importLabelSample <- function(label, data.sample, noise.cluster="Noise") {
    new.label <- rep(noise.cluster, data.sample$size)
    names(new.label) <- rownames(data.sample$features[[1]]$x)

    if (length(unique(names(label)))!=length(label))
        warning("Clustering data.frame contains non-unique item IDs.")

    #matching <- match(names(label), rownames(data.sample$features[[1]]$x)
    #ind.ok <- !is.na(matching)
    #new.label[matching[ind.ok]] <- as.character(label[ind.ok])
    new.label[names(label)] <- as.character(label)

    new.label <- sortLabel(new.label)

    niv <- levels(new.label)
    if (!(noise.cluster %in% niv)) {
        niv <- c(noise.cluster, niv)
        new.label <- factor(new.label, levels=niv)
    } else {
        new.label <- stats::relevel(new.label, noise.cluster)
        niv <- levels(label)
    }

    to.clean <- rep(TRUE, data.sample$size)
    to.clean[data.sample$id.clean] <- FALSE

    new.label[to.clean] <- noise.cluster

    new.label
}


#' createResFolder creates results directories for the sample.
#' @title Results directories creation
#' @description Create results directories for the sample.
#' @keywords internal
#' @param data.sample sample object.
#' @param dir.name directory name where to store results.
#' @param dir.path path where to create results directory.
#' @return data.sample updated with results directories.
#' @importFrom tools file_path_sans_ext
#' @examples 
#' dat <- rbind(matrix(rnorm(100, mean = 0, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 4, sd = 0.3), ncol = 2))
#' tf <- tempfile()
#' write.table(dat, tf, sep=",", dec=".")
#' 
#' x <- importSample(file.features=tf, dir.save=dirname(tf))
#'
#' #Already called in importSample 
#' createResFolder(x, dir.path=dirname(tf))
#' 
#'
#' @keywords internal
#' 
createResFolder <- function(data.sample, dir.name="", dir.path="") {
    if (!length(data.sample))
        return()

    if (!dir.exists(dir.path))
    {
        resDir <- tempdir()
    } else {
        # What name to give???
        if (dir.name == "")
            dir.name <- paste(tools::file_path_sans_ext(basename(data.sample$files$features )), 
                              "_RclusTool", sep = "")

        resDir <- file.path(dir.path, dir.name)
    }
    data.sample$files$results$dir <- resDir
    data.sample$files$results$preprocess <- file.path(resDir, "preprocess")
    data.sample$files$results$clustering <- file.path(resDir, "clustering")
    data.sample$files$results$rdata <- file.path(resDir, "rdata")
    data.sample$files$results$prototypes <- file.path(resDir, "prototypes")

    # Check if results directory already exists
    if (!dir.exists(resDir)) {
        dir.create(resDir)
        dir.create(file.path(resDir, "preprocess"))
        dir.create(file.path(resDir, "clustering"))
        dir.create(file.path(resDir, "rdata"))
        dir.create(file.path(resDir, "prototypes"))
    }

    data.sample
}

#' loadPreviousRes loads previous results obtained from previous analyses.
#' @title Previous clustering results loading
#' @description Load previous clustering results obtained from previous analyses.
#' @keywords internal
#' @param data.sample sample object.
#' @param noise.cluster character name of the cluster "Noise".
#' @importFrom utils read.csv
#' @return data.sample updated with previous clustering results.
#' 
#' @examples 
#' dat <- rbind(matrix(rnorm(100, mean = 0, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 4, sd = 0.3), ncol = 2))
#' tf <- tempfile()
#' write.table(dat, tf, sep=",", dec=".")
#' 
#' x <- importSample(file.features=tf, dir.save=dirname(tf))
#' loadPreviousRes(x)
#' 
#'
#' @keywords internal
#' 
loadPreviousRes <- function(data.sample, noise.cluster="Noise") {
    message("Search previous results...\n")

    # Load features spaces
    rep <- data.sample$files$results$rdata
    rdataFiles <- NULL
    if (is.character(rep) && dir.exists(rep)) {
        rdataFiles <- list.files(rep)
        for (i in rdataFiles) {
            message("Load", i, "...")
            data.sample$features[[i]] <- readRDS(file.path(data.sample$files$results$rdata, i))
            message("done!\n")
        }
    }

    # Load clusterings
    rep <- data.sample$files$results$clustering
    clusteringFiles <- NULL
    if (is.character(rep) && dir.exists(rep)) {
        resultsFiles <- list.files(rep, pattern = ".csv")
        clusteringFiles <- resultsFiles[grep("clustering", resultsFiles)]
        for (i in clusteringFiles) {
            clustering_name <- unlist(strsplit(i, split='.csv', fixed=TRUE))[1]
            clustering_name <- unlist(strsplit(clustering_name, split='clustering ', fixed=TRUE))[2]
            message("Load", clustering_name, "...")
            clustering_name <- paste(clustering_name, "(Loaded)", sep=" ")
            data.sample$clustering[[clustering_name]] <- buildClusteringSample(file.path(data.sample$files$results$clustering, i), 
                                                                              data.sample, noise.cluster=noise.cluster)
            countFiles <- resultsFiles[grep("counts", resultsFiles)]
            countFiles <- countFiles[grep(i, countFiles)]
            if (length(countFiles) > 0) {
                file <- file.path(data.sample$files$results$clustering, countFiles)
                counts.df <- utils::read.csv(file, row.names=1)
                nbItems <- counts.df[[2]]
                names(nbItems) <- counts.df[[1]]
                data.sample$clustering[[i]]$nbItems <- nbItems
            }
            message("done!\n")

        }
    }

    if ((length(rdataFiles) == 0) && (length(clusteringFiles) == 0))
        message("No previous results to load!\n")

    data.sample
}

#' savePreprocess exports all preprocessing operations in a csv file
#' @title Preprocessing exportation
#' @description Export all preprocessing operations in a csv file.
#' @param filename.csv character vector specifying the name of the csv file.
#' @param config 4-columns character matrix describing all preprocessing operations.
#' @param dir character vector specifying the directory of the csv file.
#' @return csv file containing preprocessing.
#' @importFrom utils alarm write.table
#'
#' @examples 
#' test.file <- tempfile()
#' config <- matrix(c("select","x",NA,NA,"select","y",NA,NA), byrow=TRUE, ncol=4)
#' savePreprocess(basename(test.file), config, dirname(test.file))
#' 
#'
#' @export
#' 
savePreprocess <- function(filename.csv, config, dir) {
	filename.csv <- file.path(dir, filename.csv) 
    if (!nchar(filename.csv)) {
        utils::alarm()
        tkmessageBox(message="Filename not valid.", title="Save preprocessing")
        return()
    }
    utils::write.table(config, file = filename.csv, fileEncoding="UTF-8", 
                row.names = FALSE, col.names = FALSE, sep = ",", dec = ".")
}


#' matchNames correct Maj/Min to match operations and Features/Profiles columns
#' @title Match Names
#' @description make a new character from two characters with maj/min adjustment 
#' @param requested.names a character vector from an operation.
#' @param names.set a character vector from Features/Profiles column.
#' @return character
#' @examples 
#' 
#' matchedName<-matchNames("Profiles","profiles")
#' 
#' matchedName
#' 
#' @keywords internal
#' 

matchNames <- function(requested.names, names.set)
{
    formated.names <- make.names(toupper(as.character(requested.names)))

    if (!is.null(names.set))
        names(names.set) <- make.names(toupper(names.set))
    val <- names.set[formated.names]
    if (length(val)&&!is.na(val))
    {
        val <- as.character(val)
    } else {
        val <- NULL
    }
    val
}

#' formatParameterList format parameters required by following functions, dealing with operations on features
#' @title Format Parameter List
#' @param parameterList list describing a single preprocessing instruction.
#' @param lowercase boolean to build lowercase operation name.
#' @return format Parameter character matrix
#'
#' @examples
#' 
#' instr <- list("select","x","log")
#' formatParameterList <- formatParameterList(instr)
#' 
#' @keywords internal
#' 

formatParameterList <- function(parameterList, lowercase=FALSE)
{
    vec <- as.character(parameterList)
    n <- length(vec)
    if (n<4) {
        vec <- c(vec, rep(NA,4-n))
    }

    mat <- matrix(vec, ncol=4)

    colnames(mat) <- paste("arg", 1:4, sep="") 
    rownames(mat) <- buildNameOperation(mat[1,], lowercase)

    mat
}

#' buildNameOperation build name of a single operation from its parameters
#' @title Build Name Operation
#' @param formatedRow vector describing preprocessing operations
#' @param lowercase boolean to build lowercase operation name.
#' @return name of a single operation
#'
#' @examples 
#' 
#' formatedRow<-c(arg1="log", arg2="x", arg3="", arg4="")
#' buildNameOperation(formatedRow)
#' 
#' @keywords internal
#' 

buildNameOperation <- function(formatedRow, lowercase=FALSE)
{
    name <- NULL
    if (lowercase)
        formatedRow <- tolower(formatedRow)
    type <- tolower(formatedRow["arg1"])
    # attention : en minuscules !!
    if ((type=="select")||(type=="default")||(type=="signalcolor")||(type=="log")||(type=="projection")||(type=="space")||(type=="sampling")||(type=="scaling"))
    {
        extension <- paste("(",type,")", sep="")
        name <- paste(formatedRow["arg2"], extension)
    } else
        if ((type=="/")||(type=="*")||(type=="+")||(type=="-"))
        {
            name <- paste(formatedRow["arg2"], type, formatedRow["arg3"], sep = " ")
        } else
            if (type=="outlier")
            {
                extension <- paste("(",formatedRow["arg3"],")", sep="")
                name <- paste(formatedRow["arg2"], extension)
            }
    name
}

#' detailOperation develop short operations (combinations select+log for example)
#' @description detail operation which are in config list
#' @title detail Operation
#' @param config list of data.frames describing all preprocessing operations.
#' @return newConfig
#'
#' instr <- rbind(c("select","x","log",""), c("select","y","log",""))
#' tf2 <- tempfile()
#' write.table(instr, tf2, sep=",", col.names = FALSE, row.names = FALSE)
#' operations <- loadPreprocessFile(tf2)
#'
#' detailOperations<-detailOperation(operations)
#' 
#' @keywords internal
#' 

detailOperation <- function(config=NULL)
{
    if (is.null(config))
        config <- matrix(ncol=4,nrow=0)

    newConfig <- matrix(ncol=4,nrow=0)
    if (!nrow(config))
        return(newConfig)
    for (i in 1:nrow(config))
    {
        op <- formatParameterList(config[i,,drop=FALSE])
        if ((op[1]=="select") && (!is.na(op[3]))) # case: (op[3]=="log")
        {
            newfeat <- formatParameterList(list(op[3], op[2]))
            newConfig <- rbind(newConfig, newfeat)
            newConfig <- rbind(newConfig, formatParameterList(list(op[1],row.names(newfeat))))
        } else
            if (((op[1]=="/")||(op[1]=="*")||(op[1]=="+")||(op[1]=="-")) && (!is.na(op[4]))) # case: (op[4]=="log")
            {
                newfeat <- formatParameterList(op[-4])
                newConfig <- rbind(newConfig, newfeat)
                newConfig <- rbind(newConfig, formatParameterList(list(op[4],row.names(newfeat))))
            } else
            {
                newConfig <- rbind(newConfig, op)
            }
    }
    newConfig
}
