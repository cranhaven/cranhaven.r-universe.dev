load_ROCKproject <- function(x,
                             output = tempdir()) {

  if (!file.exists(x)) {

    stop("The file to specified to load, `", x,
         "`, does no exist!");

  }

  fileext <- tools::file_ext(x);

  if (!grepl("rockproj|zip", fileext, ignore.case = TRUE)) {

    stop("The file to specified to load, `", x,
         "`, seems to have a different extension from 'ROCKproject'!");

  }

  projectFiles <-
    utils::unzip(
      zipfile = x,
      exdir = output
    );





}
