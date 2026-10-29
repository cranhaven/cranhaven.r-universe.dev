
##############################
## Utils
##############################
load_single_file <- function(file_path) {
    tryCatch(
        {
            args <- readRDS(file_path)
            do.call(Epoch, args)
        }, 
        error = function(e) {
            file.remove(file_path)
            stop(paste("Error loading file:", file_path, "\n", e$message))
        }
    )
}


# get the file names from the data names
get_file_names <- function(x, i) {
    dataNames <- x@dataNames
    files <- x@files
    idx <- sapply(i, function(name) which(dataNames == name))
    files$name[idx]
}

# get the nonexist file path
find_nonexist_files <- function(x, i) {
    file_names <- get_file_names(x, i)
    tmp_folder <- x@tmp_folder

    ## list all the files in the tmp folder
    tmp_files <- list.files(tmp_folder, full.names = FALSE)
    non_exist_names <- file_names[!file_names %in% tmp_files]
    non_exist_names
}

download_to_tmp_folder <- function(x, file_names) {
    files <- x@files
    tmp_folder <- x@tmp_folder
    files[files$name %in% file_names, ] |>
        osf_download(
            path = tmp_folder,
            conflicts = "overwrite",
            progress = x@progress
        )
}

##############################
## class
##############################
#' EpochDownloader
#' 
#' @description
#' `EpochDownloader` is a class that allows
#' downloading and accessing files from a OSF project.
#' 
#' @slot id The ID of the OSF project.
#' @slot files The files in the OSF project.
#' @slot dataNames The names of the files in the OSF project.
#' @slot tmp_folder The temporary folder where the files are downloaded.
#' @slot progress Logical indicating whether to show progress during download.
#' 
#' @family Epoch downloader
.EpochDownloader <- setClass("EpochDownloader",
    slots = list(
        id = "character",
        files = "ANY",
        dataNames = "character",
        tmp_folder = "character",
        progress = "logical"
    )
)


##############################
## functions
##############################
#' EpochDownloader constructor
#' 
#' @param id Either the ID of an OSF project or the name of an iEEG projects (case insensitive). Check the available projects using `EpochRepos()`. The default points to the fragility data from the Fragility multi-center retrospective study.
#' @param path The path to the temporary folder where the files will be downloaded.
#' @param progress Logical indicating whether to show progress during download.
#' @param verbose Logical indicating whether to show messages
#' 
#' @return `EpochDownloader`: An `EpochDownloader` object.
#' @rdname EpochDownloader-method
#' @examples 
#' \donttest{
#'  # list all available projects
#'  EpochRepos()
#' 
#'  # downloader for the fragility data
#'  dl <- EpochDownloader(id = "fragility")
#' 
#'  # list all Epoch objects in the downloader
#'  names(dl)
#' 
#'  # download the first Epoch object
#'  dl[1]
#'  # equivalent to (index by name)
#'  dl[names(dl)[1]]
#' 
#'  # download the multiple Epoch objects
#' 
#'  dl[c(1, 2)]
#'  # equivalent to (index by name)
#'  dl[names(dl)[c(1, 2)]]
#' }
#' 
#' @export 
EpochDownloader <- function(id = NULL,
    progress = TRUE,
    verbose = FALSE,
    path = NULL) {

    # Handle id parameter
    if (length(id) > 1) {
        id <- id[1]
    }

    if (is.null(id)) {
        id <- EpochRepos(verbose=verbose)[[1]]
    } else{
        project_list <- EpochRepos(verbose=verbose)
        ## make it case insensitive
        names(project_list) <- tolower(names(project_list))
        id <- tolower(id)
        if (id %in% names(project_list)) {
            id <- project_list[[id]]
        }
    }
    
    files <- tryCatch({
        osf_retrieve_node(id) |>
          osf_ls_files(n_max = Inf)
      },
      error = function(e) {
        if (grepl("404", e$message)) {
          stop("The ID does not exist or you do not have permission to access it.", call. = FALSE)
        } else if (grepl("Detected invalid OSF identifiers", e$message)) {
          stop("Invalid OSF ID format.", call. = FALSE)
        } else {
          stop(e)  # re-throw other errors
        }
      })
    
    # remove the duplicated file names due to an OSF bug
    files <- files[!duplicated(files$name), , drop=FALSE]

    # Only select .rds files
    rds_files <- files[grep("\\.rds$", files$name, ignore.case = TRUE), , drop = FALSE]
    
    ## remove .rds extension from name column
    dataNames <- gsub("\\.rds$", "", rds_files$name, ignore.case = TRUE)

    ## get path
    if (is.null(path)) {
      path <- file.path(tempdir(), id)
    }
    ## create the tmp folder if it does not exist
    if (!dir.exists(path)) {
        dir.create(path)
    }

    .EpochDownloader(
        id = id,
        files = rds_files,
        dataNames = dataNames,
        tmp_folder = path,
        progress = progress
    )
}

#' EpochDownloader Methods
#' 
#' @param x An `EpochDownloader` object.
#' 
#' @return `names`: A character vector of file names.
#' @rdname EpochDownloader-method
#' @export
setMethod(
    "names", "EpochDownloader",
    function(x) {
        x@dataNames
    }
)


#' @param i Index or name of the files to be accessed.
#' 
#' @return `[`: A named list of `Epoch` objects. The names are the dataset names.
#' @rdname EpochDownloader-method
#' @export
setMethod(
    "[", "EpochDownloader",
    function(x, i) {
        dataNames <- x@dataNames
        if (is.numeric(i)) {
            if (!all(isWholeNumber(i))) {
                stop("Index must be a whole number")
            }
            if(max(i) > length(dataNames)) {
                stop(paste("Index out of bounds. The maximum index is", length(dataNames)))
            }
            i <- dataNames[i]
        } else if (is.character(i)) {
          # pass
        } else {
          stop("Index must be integer positions or character names.", call. = FALSE)
        }

        non_exist_names <- i[!i %in% dataNames]
        if (length(non_exist_names) > 0) {
            stop(paste("The following data do not exist:", paste(non_exist_names, collapse = ", ")))
        }

        # file_names <- get_file_names(x, i)
        non_exist_file_names <- find_nonexist_files(x, i)
        non_exist_file_names <- unique(non_exist_file_names)
        download_to_tmp_folder(x, non_exist_file_names)

        file_names <- get_file_names(x, i)
        file_paths <- file.path(x@tmp_folder, file_names)
        names(file_paths) <- i
        lapply(
            file_paths,
            load_single_file
        )
    }
)



#' @param name The name of the file to be accessed.
#' @return `$`: A single `Epoch` object.
#' @rdname EpochDownloader-method
#' @export
setMethod(
    "$", "EpochDownloader",
    function(x, name) {
        x[name][[1]]
    }
)


#' @return `[[`: A single `Epoch` object.
#' @rdname EpochDownloader-method
#' @export
setMethod(
    "[[", "EpochDownloader",
    function(x, i) {
        if (length(i) != 1L) stop("`[[` expects a single index or name.", call. = FALSE)
        x[i][[1]]
    }
)


#' @param object An `EpochDownloader` object.
#' @return `show`: Prints a summary of the `EpochDownloader` object.
#' @rdname EpochDownloader-method
#' @export
setMethod(
    "show", "EpochDownloader",
    function(object) {
        n_show <- min(5, length(object))
        cat("EpochDownloader object\n")
        cat("  - ID:", object@id, "\n")
        cat("  - Number of files:", nrow(object@files), "\n")
        cat(glue("  - Files: {paste(names(object)[1:n_show], collapse = ', ')}"))
        if (length(object) > n_show) {
            cat(glue("...({length(object) - n_show} more)"))
        }
        cat("\n")
        cat("Use $, [, or [[ to access the Epoch Data; Use wiki() to open the project wiki in your browser\n")
    }
)


#' @return `length`: Returns the number of files in the `EpochDownloader` object.
#' @rdname EpochDownloader-method
#' @export
setMethod(
    "length", "EpochDownloader",
    function(x) {
        length(x@dataNames)
    }
)

#' @rdname EpochDownloader-method
#' @export
setGeneric("wiki", function(x, ...) standardGeneric("wiki"))

#' @param x An `EpochDownloader` object.
#' @param ... Not used, for future extensibility
#' @return `wiki`: Opens the wiki page in the default browser
#' @rdname EpochDownloader-method
#' @export
setMethod(
    "wiki", "EpochDownloader",
    function(x) {
        wiki_name <- "home"
        # Construct the URL for the wiki page
        wiki_url <- paste0("https://osf.io/", x@id, "/wiki/", wiki_name, "/")
        
        # Open the URL in the default browser
        utils::browseURL(wiki_url)
        
        # Return the URL for reference
        invisible(wiki_url)
    }
)