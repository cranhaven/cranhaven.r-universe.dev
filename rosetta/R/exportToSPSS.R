#' @rdname basicSPSStranslation
#' @export
exportToSPSS <- function (dat,
                          savfile = NULL,
                          datafile = NULL,
                          codefile = NULL,
                          fileEncoding = "UTF-8",
                          newLinesInString = " |n| ") {

  if (is.null(savfile)) {
    if (is.null(datafile) || is.null(codefile)) {
      stop("If no savfile is specified, specify both a datafile and a codefile!");
    }
  }

  ### Convert newline characters to spaces
  if (any(charVectors <- sapply(dat, is.character))) {
    dat[, charVectors] <- data.frame(lapply(dat[, charVectors],
                                            function(x) {
                                              return(gsub('\n', newLinesInString,
                                                          x));
                                            }), stringsAsFactors=FALSE);
  }

  if (is.null(savfile)) {
    ### Export datafile
    utils::write.table(ufs::massConvertToNumeric(dat), file = datafile,
                       row.names = FALSE, col.names = TRUE,
                       sep = ",", quote = TRUE, na = "",
                       fileEncoding = fileEncoding);

    codeFileConnection <- file(codefile, open="w", encoding=fileEncoding);

    cat(paste0("GET DATA
               /TYPE = TXT
               /FILE = \"", datafile, "\"
               /DELIMITERS = \",\"
               /QUALIFIER = '\"'
               /FIRSTCASE = 2
               /VARIABLES =\n"), file=codeFileConnection);

    varlabels = names(dat);
    varnames = gsub("[^[:alnum:]_\\$@#]", "\\.", names(dat));

    cat(paste0("  ", varnames, " ",
               unlist(lapply(dat, function(x) {
                 if (is.character(x)) {
                   return(paste0('A', max(nchar(x))));
                 } else {
                   return("F8.2");
                 }
               })), collapse="\n"), file=codeFileConnection, append=TRUE);

    cat(".\n\nVARIABLE LABELS\n", file = codeFileConnection, append = TRUE);

    cat(paste(varnames,
              paste("\"", varlabels, "\"", sep = ""),
              "\n"), ".\n", file = codeFileConnection,
        append = TRUE);

    factors <- sapply(dat, is.factor);

    if (any(factors)) {
      cat("\nVALUE LABELS\n", file = codeFileConnection, append = TRUE);
      for (v in which(factors)) {
        cat("/\n", file = codeFileConnection, append = TRUE);
        cat(varnames[v], " \n", file = codeFileConnection, append = TRUE,
            sep = "");
        levs <- levels(dat[[v]]);
        cat(paste(seq_along(levs),
                  paste("\"", levs, "\"", sep = ""),
                  "\n", sep = " "),
            file = codeFileConnection, append = TRUE);
      }
      cat(".\n", file = codeFileConnection, append = TRUE);
    }

    cat("\nEXECUTE.\n", file = codeFileConnection, append = TRUE);

    close(codeFileConnection);

  } else {

    if (!requireNamespace("rio", quietly = TRUE)) {
      stop("Package \"rio\" needed to save in .sav format. Please install it using `install.packages('rio');`.",
           call. = FALSE)
    } else {
      rio::export(x=dat, file=savfile);
    }

  }

}
