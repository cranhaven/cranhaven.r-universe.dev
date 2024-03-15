gftp_list <- function(url, pattern = NULL, verbose = FALSE, handle = NULL) {
    if (is.null(handle)) {
        handle <- curl::new_handle()
    }
    curl::handle_setheaders(
        handle,
        .list = list(Authorization = "Basic anonymous:anonymous")
    )
    curl::handle_setopt(handle, dirlistonly = TRUE, verbose = verbose)
    if (!grepl("/$", url)) {
        url <- paste0(url, "/")
    }
    files <- curl::curl_fetch_memory(url, handle = handle)$content %>%
        rawToChar() %>%
        strsplit("\r*\n") %>%
        unlist() %>%
        .[. != ""] %>%
        .[. != "."] %>%
        .[. != ".."]

    if (!is.null(pattern)) {
        files <- files[grepl(pattern, basename(files))]
    }

    return(files)
}

gftp_download <- function(urls, path, verbose = FALSE, handle = NULL) {
    if (is.null(handle)) {
        handle <- curl::new_handle()
    }
    curl::handle_setheaders(
        handle,
        .list = list(Authorization = "Basic anonymous:anonymous")
    )
    curl::handle_setopt(handle, dirlistonly = FALSE, verbose = verbose)

    ok_files <- c()

    for (url in urls) {
        message(sprintf("Downloading %s", url))
        tryCatch(
            {
                curl::curl_fetch_disk(url, path = file.path(path, basename(url)), handle = handle)
                ok_files <- c(ok_files, file.path(path, basename(url)))
            },
            error = function(e) {
                warning(sprintf("Failed to download %s", url))
            }
        )
    }

    invisible(ok_files)
}

gftp_download_glob <- function(urls, path, verbose = FALSE, handle = NULL) {
    glob_urls <- grep("\\*", urls, value = TRUE)
    files <- setdiff(urls, glob_urls)
    if (length(glob_urls) > 0) {
        for (url in glob_urls) {
            new_files <- gftp_list(dirname(url), pattern = utils::glob2rx(basename(url)), verbose = verbose, handle = handle)
            new_files <- file.path(dirname(url), new_files)
            files <- c(files, new_files)
        }
    }
    gftp_download(unique(files), path, verbose = verbose, handle = handle)
}
