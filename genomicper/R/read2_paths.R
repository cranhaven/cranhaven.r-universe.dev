read2_paths <-
function (ordered_alldata = "", gs_locs = "", sets_from = "workspace", 
    sets_prefix = "RHSA", level = "snp",envir= "") 
{
    print("Arguments set:")
    print(paste("Ordered dataset: ", substitute(ordered_alldata), 
        sep = ""))
    print(paste("Gene Annotations: ", substitute(gs_locs)))
    print(paste("Gene-sets Location: ", sets_from))
    print(paste("Gene-sets prefix: ", sets_prefix))
    print(paste("Annotation level: ", level))
    if (missing(level) == TRUE) {
        stop("Argument \"level\" must be defined. Permutations performed at \"gene\" or \"snp\" level")
    }
    if (level != "snp"){
if(level != "gene") {
        stop("Argument \"level\" must be \"snp\" or \"gene\"")
}
    }
    if (sets_from != "workspace"){
if(sets_from != "directory") {
        stop("Argument \"from\" must be \"workspace\" or \"directory\"")
}
    }
    if (sets_from == "workspace") {
        ps <- ls(pattern = sets_prefix, envir = envir)
        if (length(ps) == 0) {
            stop("No pathways/gene-sets found in workspace, if data saved at working directory modify the argument \"sets_from\" to \"directory\"")
        }
    }
    if (sets_from == "directory") {
        ps <- list.files(pattern = sets_prefix)
        if (length(ps) == 0) {
            stop("No pathways/gene-sets found in directory, if data saved at workspace modify the argument \"sets_from\" to \"workspace\"")
        }
    }
    ttl_paths <- length(ps)
    paths_ids <- list()
    paths_names <- NULL
    per_ors <- list()
    indx_paths <- NULL
    per_orids_list <- NULL
    pathways <- matrix(data = 0, ncol = 4, nrow = ttl_paths)
    for (i in 1:ttl_paths) {
        if (sets_from == "workspace") {
            moe <- ps[i]
            genes <- get(ps[i],envir=envir)
        }
        if (sets_from == "directory") {
            moe <- strsplit(ps[i], split = "[/.]")[[1]][1]
            genes <- as.numeric(read.table(ps[i], header = F, 
                sep = " "))
        }
        paths_names <- c(paths_names, moe)
        per_orids_list <- c(per_orids_list, paste("per_orids_", 
            moe, sep = ""))
        paths_ids <- c(paths_ids, list(moe = genes))
        pathways[i, 1] <- moe
        ids <- NULL
        ids2 <- NULL
        pathways[i, 2] <- length(genes)
        for (j in 1:length(genes)) {
            x <- which(as.numeric(gs_locs[, 4]) == as.numeric(genes[j]))
            if (length(x) != 0) {
                ids <- c(ids, x)
            }
            y <- which(as.numeric(as.character(ordered_alldata[, 
                4])) == as.numeric(genes[j]))
            if (length(y) != 0) {
                ids2 <- c(ids2, y)
            }
            if (j == length(genes)) {
                pathways[i, 3] <- length(ids)
                pathways[i, 4] <- length(ids2)
            }
        }
        if (level == "snp") {
            per_ors <- c(per_ors, list(ids2))
        }
        if (level == "gene") {
            per_ors <- c(per_ors, list(ids))
        }
    }
    names(paths_ids) <- paths_names
    names(per_ors) <- per_orids_list
    colnames(pathways) <- c("ID", "GenesInPath", "GenesFound", 
        "SNPsInPath")
    return(list(pathways = pathways, per_ors = per_ors))
}
