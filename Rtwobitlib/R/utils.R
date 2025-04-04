isTRUEorFALSE <- function(x)
{
    is.logical(x) && length(x) == 1L && !is.na(x)
}

### Like file.path() but doesn't produce a "//" if dirname has a trailing "/".
.file_path <- function(dirname, basename)
{
    if (endsWith(dirname, .Platform$file.sep))
        return(paste0(dirname, basename))
    file.path(dirname, basename)
}

normarg_filepath <- function(filepath, for.writing=FALSE)
{
    if (!is.character(filepath) || length(filepath) != 1L || is.na(filepath))
        stop("'filepath' must be a single string")
    if (!nzchar(filepath))
        stop("'filepath' must be a non-empty string")
    if (!for.writing)
        return(file_path_as_absolute(filepath))
    dirpath <- dirname(filepath)
    if (!file.exists(dirpath))
        stop("directory '", dirpath, "' does not exist")
    dirpath <- file_path_as_absolute(dirpath)
    .file_path(dirpath, basename(filepath))
}

