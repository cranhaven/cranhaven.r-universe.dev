composeOutputFilename <- function(path,
                                  filename,
                                  outputPrefix = "",
                                  outputSuffix = "") {
  return(
    file.path(
      path,
      paste0(
        outputPrefix,
        tools::file_path_sans_ext(filename),
        outputSuffix,
        ".",
        tools::file_ext(filename)
      )
    )
  );
}
