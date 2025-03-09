writeTxtFile <- function(x,
                         output,
                         encoding = rock::opts$get("encoding"),
                         preventOverwriting = rock::opts$get("preventOverwriting"),
                         silent = rock::opts$get("silent")) {

  if (!dir.exists(dirname(output))) {

    msg("The directory specified where the output file '",
        basename(output), "' is supposed to be written ('",
        dirname(output),
        "') does not exist.",
        silent = silent);

    return(FALSE);

  }

  if (file.exists(output) && preventOverwriting) {

    msg("The specified output file '", output, "' exists, and ",
        "`preventOverwriting` is `TRUE`, so not overwriting it.\n",
        silent = silent);

    return(FALSE);

  } else {

    if (file.exists(output)) {
      msg("Writing output file to '", output,
          "', overwriting the existing file (because ",
          "`preventOverwriting` is set to `FALSE`).\n",
          silent = silent);
    } else {
      msg("Writing output file to '", output, "'.\n",
          silent = silent);
    }

    if (encoding == "UTF-8") {

      resToWrite <- paste0(x, collapse="\n");

      Encoding(resToWrite) <- "UTF-8";

      conToWriteTo <- file(output, "wb");

      writeBin(
        charToRaw(resToWrite),
        conToWriteTo,
        endian="little"
      );

    } else {

      conToWriteTo <-
        file(
          output,
          open = "w",
          encoding = encoding
        );

      writeLines(
        x,
        conToWriteTo
      );

    }

    close(conToWriteTo);

  }

  return(invisible(TRUE));

}
