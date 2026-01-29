#" Data Product console logger
#"
#" A helper for DataProductFile
#" Keeps track of the messages printed in a single product run / download process
#" @keywords internal
#"
DPLogger <- setRefClass("DPLogger",
    fields = list(
        .lastMsg  = "character",
        .count    = "numeric",
        .newLine  = "logical",
        .showInfo = "logical"
    ),
    methods = list(
        #" Initializer
        #"
        #" @param showInfo (logical) same as in Onc object
        initialize = function(showInfo = FALSE) {
            .self$.lastMsg <- ""
            .self$.showInfo <- showInfo
            .self$.newLine <- FALSE # true when last msg printed includes a newline
            .self$.count <- 0
        },


        printLine = function(msg) {
            if (.self$.count == 0 || (.self$.lastMsg != msg)) {
                cat(sprintf("\n   %s", msg))
                .self$.lastMsg <- msg
            } else {
                cat(".")
            }
            .self$.count <- .self$.count + 1
        },

        printResponse = function(response) {
            # Adds a message to the messages list if it"s new
            # Prints message to console, or "." if it is a redundant message
            #
            # * response (object) Parsed httr response
            #
            # Detect if the response comes from a "run" or "download" method
            origin <- "download"

            if (is.list(response) && ("status" %in% names(response[[1]]))) {
                origin <- "run"
            }

            # Store and print message
            if (origin == "run") {
                msg <- response[[1]]$status
            }
            else {
                if ("message" %in% names(response)) {
                    msg <- response$message
                }
                else {
                    msg <- "Generating"
                }
            }

            if (.self$.count == 0) {
                cat(sprintf("\n   %s", msg))
                .self$.lastMsg <- msg
            }
            else {
                if (.self$.lastMsg != msg) {
                    cat(sprintf("\n   %s", msg))
                    .self$.lastMsg <- msg
                }
                else {
                    cat(".")
                }
            }
            .self$.count <- .self$.count + 1;
        }
    )
)
