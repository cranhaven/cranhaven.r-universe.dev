

# Project class definition ------------------------------------------------



#' Portable Encapsulated Project object
#'
#' Provides an in-memory representation and functions to access project
#' configuration and sample annotation values for a PEP.
#'
#' Can be created with the constructor: \code{"\linkS4class{Project}"}
#'
#' @slot file character vector path to config file on disk.
#' @slot samples a data table object holding the sample metadata
#' @slot config a list object holding contents of the config file
#' @slot sampleNameAttr a string indicating the sample attribute that is used
#'  to index the sample table
#' @slot subSampleNameAttr a string indicating the sample attribute that is used
#'  to index the sample table
#'
#' @export
setClass(
    "Project",
    slots = c(
        file = "character",
        samples = "data.frame",
        config = "list",
        sampleNameAttr = "character",
        subSampleNameAttr = "character"
    )
)


# Project class methods ---------------------------------------------------


setMethod("initialize", "Project", function(.Object, ...) {
    .Object = methods::callNextMethod(.Object)  # calls generic initialize
    ellipsis <- list(...)
    stIndex = ellipsis$sampleTableIndex
    sstIndex = ellipsis$subSampleTableIndex
    if (!is.null(ellipsis$file)) {
        # check if 'file' path provided
        if (.isCfg(ellipsis$file)) {
            # provided 'file' seems to be a config
            .Object@file = .makeAbsPath(ellipsis$file, parent = path.expand(getwd()))
            # instantiate config object and stick it in the config slot
            .Object@config = Config(.Object@file, ellipsis$amendments)
            # determine the effective (sub)sample table indexes
            .Object = .getTableIndexes(.Object, stIndex, sstIndex)
            sampleTablePath = .getSampleTablePathFromConfig(config = config(.Object))
            .Object@samples = .loadSampleAnnotation(sampleTablePath = sampleTablePath)
            .Object = .modifySamples(.Object)
        } else {
            # provided 'file' seems to be a sample table
            # determine the effective (sub)sample table indexes
            .Object = .getTableIndexes(.Object, stIndex, sstIndex)
            .Object@samples = .loadSampleAnnotation(sampleTablePath = ellipsis$file)
            .Object = .autoMergeDuplicatedNames(.Object)
        }
    }
    # no 'file' provided, creating an empty object
    # determine the effective (sub)sample table indexes
    .Object = .getTableIndexes(.Object, stIndex, sstIndex)
    return(.Object)
})


#' The constructor of a class representing a Portable Encapsulated Project
#'
#' This is a helper that creates the project with empty samples and config slots
#'
#' @param file a string specifying a path to a project configuration YAML file
#' @param amendments a string with the amendments names to be activated
#' @param sampleTableIndex a string indicating the sample attribute that is used
#'  to index the sample table
#' @param subSampleTableIndex a string indicating the sample attribute that is used
#'  to index the sample table
#' @return an object of \code{"\linkS4class{Project}"}
#' @examples
#' projectConfig = system.file("extdata", "example_peps-master",
#' "example_amendments1", "project_config.yaml", package="pepr")
#' p=Project(projectConfig)
#' @export
Project = function(file = NULL,
                   amendments = NULL,
                   sampleTableIndex = NULL,
                   subSampleTableIndex = NULL) {
    methods::new(
        "Project",
        file = file,
        amendments = amendments,
        sampleTableIndex = sampleTableIndex,
        subSampleTableIndex = subSampleTableIndex
    )
}


setMethod(
    "show",
    signature = "Project",
    definition = function(object) {
        cat("PEP project object. Class: ", class(object), fill = T)
        cat("  file: ", object@file, fill = T)
        cat("  samples: ", NROW(object@samples), fill = T)
        if (length(object@config) != 0) {
            .listAmendments(object@config, style = "cat")
        }
        invisible(NULL)
    }
)


#' Extract \code{"\linkS4class{Project}"}
#'
#' This method can be used to view the config slot of
#' the \code{"\linkS4class{Project}"} class
#'
#' @param object an object of \code{"\linkS4class{Project}"}
#'
#' @return project config
#'
#' @examples
#' projectConfig = system.file("extdata", "example_peps-master",
#' "example_amendments1", "project_config.yaml", package="pepr")
#' p=Project(projectConfig)
#' config(p)
#'
#' @export
setGeneric("config", function(object)
    standardGeneric("config"))

#' @describeIn config Extract \code{"\linkS4class{Project}"} of the object of \code{"\linkS4class{Project}"}
setMethod(
    "config",
    signature = "Project",
    definition = function(object) {
        object@config
    }
)


#' Perform all the sample attribute modifications
#'
#' @param object an object of \code{"\linkS4class{Project}"}
#'
#' @return modified Project object
#' @keywords internal
setGeneric(".modifySamples", function(object)
    standardGeneric(".modifySamples"))

setMethod(
    ".modifySamples",
    signature = "Project",
    definition = function(object) {
        object = .removeAttrs(object)
        object = .appendAttrs(object)
        object = .duplicateAttrs(object)
        object = .implyAttrs(object)
        object = .autoMergeDuplicatedNames(object)
        object = .mergeAttrs(object,
                             .getSubSampleTablePathFromConfig(config(object)))
        object = .deriveAttrs(object)
        return(object)
    }
)


#' Extract samples
#'
#' This method extracts the samples
#'
#' @param .Object An object of Project class
#' @param sampleName character the name of the sample
#'
#' @return data.table one row data table with the sample associated metadata
#' @examples
#' projectConfig = system.file(
#' "extdata",
#' "example_peps-master",
#' "example_basic",
#' "project_config.yaml",
#' package = "pepr"
#' )
#' p = Project(projectConfig)
#' sampleName = "frog_1"
#' getSample(p, sampleName)
#' @export
setGeneric(name = "getSample", function(.Object, sampleName)
    standardGeneric("getSample"))

#' @describeIn getSample extracts the sample from the \code{"\linkS4class{Project}"} object
setMethod(
    f = "getSample",
    signature(.Object = "Project",
              sampleName = "character"),
    definition = function(.Object, sampleName) {
        sampleNames = unlist(.Object@samples[[.Object@sampleNameAttr]])
        rowNumber = which(sampleNames == sampleName)
        if (length(rowNumber) == 0)
            stop("Such sample name does not exist.")
        result = sampleTable(.Object)[rowNumber,]
        return(result)
    }
)


#' Extract subsamples
#'
#' This method extracts the subsamples
#'
#' @param .Object An object of Project class
#'
#' @param sampleName character the name of the sample
#' @param subsampleName character the name of the subsample
#'
#' @return data.table one row data table with the subsample associated metadata
#' @examples
#' projectConfig = system.file(
#' "extdata",
#' "example_peps-master",
#' "example_subtable1",
#' "project_config.yaml",
#' package = "pepr"
#' )
#' p = Project(projectConfig)
#' sampleName = "frog_1"
#' subsampleName = "sub_a"
#' getSubsample(p, sampleName, subsampleName)
#' @export
setGeneric(name = "getSubsample", function(.Object, sampleName, subsampleName)
    standardGeneric("getSubsample"))

#' @describeIn getSubsample extracts the subsamples from the \code{"\linkS4class{Project}"} object
setMethod(
    f = "getSubsample",
    signature(
        .Object = "Project",
        sampleName = "character",
        subsampleName = "character"
    ),
    definition = function(.Object, sampleName, subsampleName) {
        if (is.null(.Object@samples[[.Object@subSampleNameAttr]]))
            stop(
                "There is no subsample_name attribute in the subsample table, ",
                " therefore this method cannot be called."
            )
        sampleNames = unlist(.Object@samples[[.Object@sampleNameAttr]])
        rowNumber = which(sampleNames == sampleName)
        if (length(rowNumber) == 0)
            stop("Such sample name does not exist.")
        subsampleNames = .Object@samples[[.Object@subSampleNameAttr]][[rowNumber]]
        sampleNumber = which(subsampleNames == subsampleName)
        if (length(sampleNumber) == 0)
            stop("Such sample and sub sample name combination does not exist.")
        result = .Object@samples[1, ]
        for (iColumn in names(result)) {
            if (length(.Object@samples[[iColumn]][[rowNumber]]) > 1) {
                result[[iColumn]] =
                    .Object@samples[[iColumn]][[rowNumber]][[sampleNumber]]
            } else{
                result[[iColumn]] = .Object@samples[[iColumn]][[rowNumber]][[1]]
            }
        }
        return(result)
    }
)


#' List amendments
#'
#' Lists available amendments within a \code{"\linkS4class{Project}"} object.
#'
#' The amendments can be activated by passing their names to the  \code{\link{activateAmendments}} method
#'
#' @param .Object an object of \code{"\linkS4class{Project}"}
#' @return names of the available amendments
#' @examples
#' projectConfig = system.file("extdata",
#' "example_peps-master",
#' "example_amendments1",
#' "project_config.yaml",
#' package = "pepr")
#' p = Project(file = projectConfig)
#' availAmendemtns = listAmendments(p)
#' @export
setGeneric("listAmendments", function(.Object)
    standardGeneric("listAmendments"))

#' @describeIn listAmendments list amendments in a \code{"\linkS4class{Project}"} object
setMethod(
    f = "listAmendments",
    signature = signature(.Object = "Project"),
    definition = function(.Object) {
        config = config(.Object)
        .listAmendments(cfg = config, style = "message")
    }
)


#' Activate amendments in objects of \code{"\linkS4class{Project}"}
#'
#' This method switches between the amendments
#' within the \code{"\linkS4class{Project}"} object
#'
#' To check what are the amendments names
#' call \code{listAmendments(p)}, where \code{p} is the object
#' of \code{"\linkS4class{Project}"} class
#'
#' @param .Object an object of class \code{"\linkS4class{Project}"}
#' @param amendments character with the amendment name
#' @return an object of class \code{"\linkS4class{Project}"} with activated amendments
#' @examples
#' projectConfig = system.file("extdata",
#' "example_peps-master",
#' "example_amendments1",
#' "project_config.yaml",
#' package = "pepr")
#' p = Project(file = projectConfig)
#' availAmendments = listAmendments(p)
#' activateAmendments(p, availAmendments[1])
#' @export
setGeneric("activateAmendments", function(.Object, amendments)
    standardGeneric("activateAmendments"))

#' @describeIn activateAmendments activate amendments in a \code{"\linkS4class{Project}"} object
setMethod(
    f = "activateAmendments",
    signature = signature(.Object = "Project", amendments = "character"),
    definition = function(.Object, amendments) {
        .Object@config = .applyAmendments(.Object@config, amendments)
        .Object@config = makeSectionsAbsolute(
            .Object@config,
            c(CFG_SAMPLE_TABLE_KEY, CFG_SUBSAMPLE_TABLE_KEY),
            .Object@file
        )
        sampleTablePath = .getSampleTablePathFromConfig(config = config(.Object))
        .Object@samples = .loadSampleAnnotation(sampleTablePath = sampleTablePath)
        .Object = .modifySamples(.Object)
        return(.Object)
    }
)


#' View samples in the objects of \code{"\linkS4class{Project}"}
#'
#' This method can be used to view the samples slot
#' of the \code{"\linkS4class{Project}"} class
#'
#' @param object an object of \code{"\linkS4class{Project}"}
#'
#' @return a data.table with the with metadata about samples
#' @examples
#' projectConfig = system.file("extdata", "example_peps-master",
#' "example_amendments1", "project_config.yaml", package="pepr")
#' p=Project(projectConfig)
#' sampleTable(p)
#'
#' @export
setGeneric("sampleTable", function(object)
    standardGeneric("sampleTable"))

#' @describeIn sampleTable extract sample table from a \code{"\linkS4class{Project}"}
setMethod(
    "sampleTable",
    signature = "Project",
    definition = function(object) {
        object@samples
    }
)

# Sample modifiers --------------------------------------------------------

#' Remove attributes across all the samples
#'
#' @param .Object an object of \code{"\linkS4class{Project}"}
#'
#' @return an object of \code{"\linkS4class{Project}"}
#' @keywords internal
.removeAttrs <- function(.Object) {
    if (!CFG_S_MODIFIERS_KEY %in% names(config(.Object)))
        return(.Object)
    modifiers = config(.Object)[[CFG_S_MODIFIERS_KEY]]
    if (!CFG_REMOVE_KEY %in% names(modifiers))
        return(.Object)
    toRemove = modifiers[[CFG_REMOVE_KEY]]
    if (!is.null(toRemove)) {
        # get a copy of samples to get the dimensions
        for (rem in toRemove) {
            if (rem %in% colnames(sampleTable(.Object))) {
                .Object@samples[, rem] = NULL
            }
        }
    }
    return(.Object)
}


#' Append constant attributes across all the samples
#'
#' @param .Object an object of \code{\link{Project-class}}
#'
#' @return an object of \code{\link{Project-class}}
#' @keywords internal
.appendAttrs <- function(.Object) {
    if (!CFG_S_MODIFIERS_KEY %in% names(config(.Object)))
        return(.Object)
    modifiers = config(.Object)[[CFG_S_MODIFIERS_KEY]]
    if (!CFG_APPEND_KEY %in% names(modifiers))
        return(.Object)
    constants = modifiers[[CFG_APPEND_KEY]]
    if (is.list(constants)) {
        # get names
        constantsNames = names(constants)
        # get a copy of samples to get the dimensions
        colLen = dim(sampleTable(.Object))[1]
        for (iConst in seq_along(constants)) {
            # create a one column data.table and append it to the current one
            if (!constantsNames[iConst] %in% colnames(sampleTable(.Object))) {
                tempDT = data.table::data.table(matrix(matrix(
                    list(constants[[iConst]]),
                    ncol = 1,
                    nrow = colLen
                )))
                names(tempDT) = constantsNames[iConst]
                .Object@samples = cbind(.Object@samples, tempDT)
            }
        }
    }
    return(.Object)
}

#' Duplicate a selected attribute across all the samples
#'
#' @param .Object an object of \code{"\linkS4class{Project}"}
#'
#' @return an object of \code{"\linkS4class{Project}"}
#' @keywords internal
.duplicateAttrs <- function(.Object) {
    if (!CFG_S_MODIFIERS_KEY %in% names(config(.Object)))
        return(.Object)
    modifiers = config(.Object)[[CFG_S_MODIFIERS_KEY]]
    if (!CFG_DUPLICATE_KEY %in% names(modifiers))
        return(.Object)
    duplicated = modifiers[[CFG_DUPLICATE_KEY]]
    for (oriAttrName in names(duplicated)) {
        .Object@samples[, duplicated[[oriAttrName]]] = .Object@samples[, oriAttrName]
    }
    return(.Object)
}

#' Imply attributes
#'
#' @param .Object an object of \code{"\linkS4class{Project}"}
#'
#' @return an object of \code{"\linkS4class{Project}"}
#' @keywords internal
.implyAttrs = function(.Object) {
    if (!CFG_S_MODIFIERS_KEY %in% names(config(.Object)))
        return(.Object)
    modifiers = config(.Object)[[CFG_S_MODIFIERS_KEY]]
    if (!CFG_IMPLY_KEY %in% names(modifiers))
        return(.Object)
    implications = modifiers[[CFG_IMPLY_KEY]]
    for (implication in implications) {
        if (!(
            CFG_IMPLY_IF_KEY %in% names(implication) &&
            CFG_IMPLY_THEN_KEY %in% names(implication)
        ))
            stop(CFG_IMPLY_KEY, " section is not formatted properly")
        implierAttrs = names(implication[[CFG_IMPLY_IF_KEY]])
        implierVals = implication[[CFG_IMPLY_IF_KEY]]
        impliedAttrs = names(implication[[CFG_IMPLY_THEN_KEY]])
        impliedVals = as.character(implication[[CFG_IMPLY_THEN_KEY]])
        attrs = colnames(.Object@samples)
        if (!all(implierAttrs %in% attrs))
            next
        allHitIds = list()
        for (i in seq_along(implierAttrs)) {
            hitIds = list()
            implierStrings = as.character(unlist(implierVals[i]))
            for (j in seq_along(implierStrings)) {
                hitIds[[j]] = which(.Object@samples[, implierAttrs[i]] == implierStrings[j])
            }
            allHitIds[[i]] = Reduce(union, hitIds)
            if (length(allHitIds[[i]]) < 1)
                break
        }
        qualIds = Reduce(intersect, allHitIds)
        if (length(qualIds) < 1)
            next
        for (i in seq_along(impliedAttrs)) {
            if (!impliedAttrs[i] %in% attrs)
                .Object@samples[, impliedAttrs[i]] = ""
            .Object@samples[qualIds, impliedAttrs[i]] = impliedVals[i]
        }
    }
    return(.Object)
}


#' Derive attributes
#'
#' @param .Object an object of \code{"\linkS4class{Project}"}
#'
#' @return an object of \code{"\linkS4class{Project}"}
#' @keywords internal
.deriveAttrs = function(.Object) {
    if (!CFG_S_MODIFIERS_KEY %in% names(config(.Object)))
        return(.Object)
    parentDir = dirname(.Object@file)
    modifiers = config(.Object)[[CFG_S_MODIFIERS_KEY]]
    if (!CFG_DERIVE_KEY %in% names(modifiers))
        return(.Object)
    derivations = modifiers[[CFG_DERIVE_KEY]]
    if (!all(c(CFG_DERIVE_ATTRS_KEY, CFG_DERIVE_SOURCES_KEY)
             %in% names(derivations)))
        stop(CFG_DERIVE_KEY, " section is not formatted properly")
    for (derivedAttr in derivations[[CFG_DERIVE_ATTRS_KEY]]) {
        if (!derivedAttr %in% colnames(.Object@samples))
            stop(paste(
                "Failed to derive. Sample attribute not found:",
                derivedAttr
            ))
        derivedSamplesVals = .Object@samples[, derivedAttr]
        for (derivedSource in names(derivations[[CFG_DERIVE_SOURCES_KEY]])) {
            hitIds = which(derivedSamplesVals == derivedSource)
            if (length(hitIds) < 1)
                next
            for (hitId in hitIds) {
                rgx = derivations[[CFG_DERIVE_SOURCES_KEY]][[derivedSource]]
                res = .matchesAndRegexes(.strformat(rgx, as.list(sampleTable(
                    .Object
                )[hitId, ]), parentDir))
                .Object@samples[hitId, derivedAttr] = list(unique(unlist(res)))
            }
        }
    }
    return(.Object)
}


#' Read sample table from disk
#'
#' @param sampleTablePath a character string indicating a path to the sample table
#'
#' @return an data.frame with samples; one sample per row
#' @keywords internal
.loadSampleAnnotation = function(sampleTablePath) {
    if (.safeFileExists(sampleTablePath)) {
        samples = data.table::fread(sampleTablePath)
    } else{
        warning("The sample table does not exist: ", sampleTablePath)
        samples = data.frame()
    }
}


#' Load single subsample annotation
#'
#' @param .Object an object of \code{"\linkS4class{Project}"}
#' @param path string, a path to the subsample table to read and incorporate
#'
#' @return an object of \code{"\linkS4class{Project}"}
#' @keywords internal
.loadSubsampleAnnotation = function(.Object, path) {
    if (.safeFileExists(path)) {
        subsamplesTable = data.table::fread(path)
    } else{
        subsamplesTable = data.table::data.table()
    }
    subNames = unique(subsamplesTable[[.Object@sampleNameAttr]])
    samples = sampleTable(.Object)
    samples = .listifyDF(samples)
    rowNum = nrow(samples)
    # Creating a list to be populated in the loop and inserted
    # into the samples data.table as a column. This way the "cells"
    # in the samples table can consist of multiple elements
    for (iName in subNames) {
        whichNames = which(subsamplesTable[[.Object@sampleNameAttr]] == iName)
        subTable = subsamplesTable[whichNames, ]
        dropCol = which(names(subsamplesTable[whichNames, ]) == .Object@sampleNameAttr)
        subTable = subset(subTable, select = -dropCol)
        colList = vector("list", rowNum)
        for (iColumn in seq_len(ncol(subTable))) {
            colName = names(subset(subTable, select = iColumn))
            if (!any(names(samples) == colName)) {
                # The column doesn't exist, creating
                samples[, colName] = NULL
            } else{
                colList = samples[[colName]]
            }
            # The column exists
            whichColSamples = which(names(samples) == colName)
            whichRowSamples = which(samples[[.Object@sampleNameAttr]] == iName)
            if (length(whichRowSamples) < 1) {
                warning("No samples named '", iName, "'")
            } else {
                # Inserting element(s) into the list
                colList[[whichRowSamples]] = subTable[[colName]]
                # Inserting the list as a column in the data.table
                samples[[colName]] = colList
            }
        }
    }
    samples[is.na(samples)] = ""
    .Object@samples = samples
    return(.Object)
}



#' Merge samples defined in sample table with ones in subsample table(s)
#'
#' @param .Object an object of \code{"\linkS4class{Project}"}
#' @param subsampleAannotationPaths a vector of strings specifying the paths to sample
#'
#' @return an object of \code{"\linkS4class{Project}"}
#' @keywords internal
.mergeAttrs = function(.Object, subsampleAannotationPaths) {
    if (is.null(subsampleAannotationPaths))
        return(.Object)
    for (p in subsampleAannotationPaths) {
        .Object = .loadSubsampleAnnotation(.Object, p)
    }
    return(.Object)
}


#' Merge samples with identical names
#'
#' If sample table specifies samples with non-unique names, try to merge these samples
#'
#' @param .Object an object of \code{"\linkS4class{Project}"}
#'
#' @return an object of \code{"\linkS4class{Project}"}
#' @keywords internal
.autoMergeDuplicatedNames = function(.Object) {
    s = sampleTable(.Object)
    sampleNames = s[[.Object@sampleNameAttr]]
    dups = duplicated(sampleNames)
    if (!any(dups))
        return(.Object)
    duplicatedSampleNames = sampleNames[which(dups)]
    if (!is.null(.getSubSampleTablePathFromConfig(config(.Object))))
        stop(
            paste0(
                "Duplicated sample names were found (",
                duplicatedSampleNames,
                ") and subsample_table is specified in the config. ",
                "You may use either auto-merging or subsample_table-based merging."
            )
        )
    rowsWithDuplicates = which(s[[.Object@sampleNameAttr]] %in% duplicatedSampleNames)
    dupRows = s[rowsWithDuplicates,]
    combinedList = list()
    # create a list of list that combined rows with duplicated sample names
    for (duplicatedSampleName in duplicatedSampleNames) {
        toCombine = s[which(sampleNames == duplicatedSampleName),]
        combinedList[[duplicatedSampleName]] = apply(toCombine, 2, function(x) {
            return(list(unique(x)))
        })
    }
    # remove rows that include duplicates
    s = s[-rowsWithDuplicates,]
    # insert the combined rows
    for (combinedRow in combinedList) {
        s = rbind(s, combinedRow)
    }
    .Object@samples = s
    return(.Object)
}

#' Set table indexes
#'
#' Get sample and subsample table indexes and save as a slot on the Project object
#'
#' This is the (sub)sample table index selection priority order:
#' \enumerate{
#'   \item Project constructor specified
#'   \item Config specified
#'   \item Deafult value
#' }
#'
#' @param .Object an object of \code{"\linkS4class{Project}"}
#' @param stIndex character string indicating a constructor-specified sample table index
#' @param sstIndex character string indicating a constructor-specified subsample table index
#' @keywords internal
.getTableIndexes <- function(.Object, stIndex, sstIndex) {
    .getIndexVal <- function(spec, config, default, key) {
        if (!is.null(spec))
            return(spec)
        if (length(config))
            return(ifelse(is.null(config[[key]]), default, config[[key]]))
        return(default)
    }
    
    .Object@sampleNameAttr = .getIndexVal(stIndex,
                                          config(.Object),
                                          SAMPLE_NAME_ATTR,
                                          CFG_SAMPLE_TABLE_INDEX_KEY)
    .Object@subSampleNameAttr = .getIndexVal(sstIndex,
                                             config(.Object),
                                             SUBSAMPLE_NAME_ATTR,
                                             CFG_SUBSAMPLE_TABLE_INDEX_KEY)
    
    return(.Object)
}
