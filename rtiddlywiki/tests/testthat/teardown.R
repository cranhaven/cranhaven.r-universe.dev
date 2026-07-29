


cat("Stopping test widdlywiki server...\n")

pid <- Sys.getenv("TEST_TIDDLYWIKI_SERVER_PID")
if (nzchar(pid)) {
    tools::pskill(as.numeric(pid))
}

cat("Cleanup tempory test folder")
tw_folder <- test_folder()
unlink(tw_folder, recursive = TRUE)

# Clean up any remaining temporary directories that might have been created by external tools
temp_base <- tempdir()
if (dir.exists(temp_base)) {
    temp_files <- list.files(temp_base, pattern = "^(calibre|pandoc)", full.names = TRUE)
    if (length(temp_files) > 0) {
        cat("Cleaning up external temporary directories...\n")
        unlink(temp_files, recursive = TRUE)
    }
}

