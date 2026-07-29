
cat("Starting test tiddlywiki server...\n")


# Find tiddlywiki in PATH (works cross-platform)
tiddlywiki_path <- Sys.which("tiddlywiki")
if (nzchar(tiddlywiki_path)) {
    tw_folder <- test_folder()
    system(paste0(tiddlywiki_path, " ", tw_folder, " --init server"))

    tiddlers <- list.files(testthat::test_path("tiddlers"), full.names = TRUE)
    tiddlers_dir <- file.path(tw_folder, "tiddlers")
    
    # Ensure tiddlers directory exists and copy files
    if (!dir.exists(tiddlers_dir)) {
        dir.create(tiddlers_dir, recursive = TRUE)
    }
    file.copy(tiddlers, tiddlers_dir, overwrite = TRUE)
    Sys.sleep(2)
    server_process <- processx::process$new(
        tiddlywiki_path,
        c(tw_folder, "--listen", "port=9090"),
        # stdout = "wiki-stdout.txt",
        # stderr = "wiki-stderr.txt",
        cleanup_tree = TRUE
    )
    Sys.sleep(4)

    Sys.setenv(TEST_TIDDLYWIKI_SERVER_PID = server_process$get_pid())
}
