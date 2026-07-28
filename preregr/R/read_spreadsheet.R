#' Convenience function to read spreadsheet-like files
#'
#' Currently reads spreadsheets from Google Sheets or from `xlsx`, `csv`,
#' or `sav` files. Normally, you don't use this, but instead you
#' use [preregr::form_fromSpreadsheet()].
#'
#' @param x The URL or path to a file.
#' @param sheet Optionally, the name(s) of the worksheet(s) to select.
#' @param columnDictionary Optionally, a dictionary with column names to
#' check for presence. A named list of vectors.
#' @param localBackup If not `NULL`, a valid filename to write a local
#' backup to.
#' @param exportGoogleSheet If `x` is a URL to a Google Sheet, instead of using
#' thr `googlesheets4` package to download the data, by passing
#' `exportGoogleSheet=TRUE`, an export link will be produced and the data
#' will be downloaded as Excel spreadsheet.
#' @param flattenSingleDf Whether to return the result as a data frame if
#' only one data frame is returned as a result.
#' @param xlsxPkg Which package to use to work with Excel spreadsheets.
#' @param failQuietly Whether to give an error when `x` is not a valid URL
#' or existing file, or just return `NULL` invisibly.
#' @param silent Whether to be silent or chatty.
#'
#' @return A list of dataframes, or, if only one data frame was loaded and
#' `flattenSingleDf` is `TRUE`, a data frame.
#' @export
#'
#' @examples \donttest{
#' ### Note that this example requires an internet connection!
#' read_spreadsheet(
#'   paste0(
#'     "https://docs.google.com/",
#'     "spreadsheets/d/",
#'     "1bHDzpCu4CwEa5_3_q_9vH2691XPhCS3e4Aj_HLhw_U8"
#'   )
#' );
#' }
read_spreadsheet <- function(x,
                             sheet = NULL,
                             columnDictionary = NULL,
                             localBackup = NULL,
                             exportGoogleSheet = FALSE,
                             flattenSingleDf = FALSE,
                             xlsxPkg = c("rw_xl", "openxlsx", "XLConnect"),
                             failQuietly = FALSE,
                             silent = preregr::opts$get("silent")) {

  xlsxPkgLabels <-
    c(rw_xl = "readxl and writexl",
      openxlsx = "openxlsx",
      XLConnect = "XLConnect");

  gSheetId_extractionRegex <- preregr::opts$get("gSheetId_extractionRegex");
  gSheetId_to_exportLink <- preregr::opts$get("gSheetId_to_exportLink");

  if (!is.character(x)) {
    stop("As `x`, you must pass a character value (i.e. a single string). ",
         "Instead, you passed something with class ", vecTxtQ(class(x)), ".");
  }

  if (!(length(x) == 1)) {
    stop("As `x`, you must pass a character value (i.e. a single string). ",
         "Instead, you passed a character vector with ", length(x),
         " elements.");
  }

  x <- trimws(x);

  xlsxPkg <- xlsxPkg[1];

  ###---------------------------------------------------------------------------
  ### Download file or data, if necessary
  ###---------------------------------------------------------------------------

  if (grepl(gSheetId_extractionRegex, x)) {

    msg("The value you passed as `x` (", x,
        ") matched the regular expression for extracting a Google Sheet ",
        "identifier (", gSheetId_extractionRegex,
        "), so proceeding to extract that.\n",
        silent=silent);

    gSheetId <-
      gsub(
        gSheetId_extractionRegex,
        "\\1",
        x
      );

    if (exportGoogleSheet) {

      gSheet_exportLink <-
        sprintf(gSheetId_to_exportLink, gSheetId);

      fileToRead <- tempfile(fileext=".xlsx");

      msg("You set `exportGoogleSheet=TRUE`, so I will export the ",
          "spreadsheet as an xlsx file which I will store as a temporary ",
          "file and then import.\n",
          silent=silent);

      downloadResult <-
        utils::download.file(
          url = gSheet_exportLink,
          destfile = fileToRead,
          quiet = silent,
          mode = "wb"
        );

      if (!(downloadResult == 0)) {
        stop("Could not download the google sheet as xlsx file from ",
             gSheet_exportLink, " and save it to a local temporary file.");

      } else {

        downloaded <- TRUE;
        extension <- tools::file_ext(fileToRead);

        msg("Succesfully downloaded the file.\n",
            silent=silent);

      }

    } else {

      if (requireNamespace("googlesheets4", quietly = TRUE)) {

        msg("You set `exportGoogleSheet=FALSE`, so I will download the data ",
            "through the Google Sheets API.\n",
            silent=silent);

        ### Indicate we want to use the Google Sheets API without authenticating
        googlesheets4::gs4_deauth();

        sheetNames <- googlesheets4::sheet_names(x);

        res <-
          lapply(
            sheetNames,
            googlesheets4::read_sheet,
            ss = x
          );
        names(res) <- sheetNames;

        downloaded <- TRUE;
        fileToRead <- NULL;

        msg("Succesfully downloaded the data.\n",
            silent=silent);

      } else {

        stop("To read Google Sheets URLs, the {googlesheets4} package ",
             "has to be installed. You can install it with:\n\n",
             "  install.packages('googlesheets4');\n");

      }

    }

  } else if (grepl("^http", x)) {

    msg("The value you passed as `x` (", x,
        ") did not match the regular expression for extracting a ",
        "Google Sheet identifier (", gSheetId_extractionRegex,
        "), but did start with http(s), so attempting to download.\n",
        silent=silent);

    extension <- tools::file_ext(x);

    fileToRead <- tempfile(fileext = extension);

    downloadResult <- utils::download.file(x,
                                           fileToRead,
                                           quiet = silent,
                                           mode = "wb");

    if (!(downloadResult == 0)) {

      stop("Could not download the google sheet as xlsx file from ",
           x, " and save it to a local temporary file.");

    } else {

      downloaded <- TRUE;
      extension <- tools::file_ext(fileToRead);

      msg("Succesfully downloaded the file.\n",
          silent=silent);

    }

  } else if (file.exists(x)) {

    msg("The value you passed as `x` (", x,
        ") refers to a file that exists on your hard drive. ",
        "Proceeding to import the data from that file.\n",
        silent=silent);

    fileToRead <- x;
    extension <- tools::file_ext(x);
    downloaded <- FALSE;

  } else {

    if (failQuietly) {
      return(invisible(NULL));
    } else {
      stop("In `x`, I couldn't recognize a Google Sheets URL, an URL to a file ",
           "hosted on a website, or a path to an existing file.");
    }

  }

  ###---------------------------------------------------------------------------
  ### Read file from disk, if necessary
  ###---------------------------------------------------------------------------

  if (!is.null(fileToRead)) {

    extension <- trimws(tolower(extension));

    if (extension == "xlsx") {

      msg("The extension of the file is `xlsx`, and you specified {",
          xlsxPkgLabels[xlsxPkg],
          "} as the package to use to read Excel files, so ",
          "starting to read the data with those parameters.\n",
          silent=silent);

      if (xlsxPkg == "rw_xl") {

        if (requireNamespace("readxl", quietly = TRUE)) {

          sheetNames <- readxl::excel_sheets(fileToRead);

          res <-
            lapply(
              sheetNames,
              readxl::read_excel,
              path = fileToRead,
              col_types = "text"
            );
          names(res) <- sheetNames;

        } else {

          stop("To read Excel spreadsheets (`.xlsx`), the {readxl} package ",
               "has to be installed. You can install it with:\n\n",
               "  install.packages('readxl');\n");

        }

      } else if (xlsxPkg == "openxlsx") {

        if (requireNamespace("openxlsx", quietly = TRUE)) {

          wb <- openxlsx::loadWorkbook(fileToRead);

          sheetNames <- names(wb);

          res <-
            lapply(
              sheetNames,
              openxlsx::read.xlsx,
              xlsxFile = wb
            );
          names(res) <- sheetNames;

        } else {

          stop("To read Excel spreadsheets (`.xlsx`), the {openxlsx} package ",
               "has to be installed. You can install it with:\n\n",
               "  install.packages('openxlsx');\n");

        }

      } else if (xlsxPkg == "XLConnect") {

        if (requireNamespace("XLConnect", quietly = TRUE)) {

          wb <- XLConnect::loadWorkbook(fileToRead);

          sheetNames <- XLConnect::getSheets(wb);

          res <-
            lapply(
              sheetNames,
              openxlsx::readWorkbook,
              xlsxFile = wb
            );
          names(res) <- sheetNames;

        } else {

          stop("To read Excel spreadsheets (`.xlsx`), the {XLConnect} package ",
               "has to be installed. You can install it with:\n\n",
               "  install.packages('XLConnect');\n");

        }

      }

      msg("Succesfully imported spreadsheet.\n",
          silent=silent);

    } else if (extension == "sav") {

      if (requireNamespace("haven", quietly = TRUE)) {

        msg("Starting to import spreadsheet from SPSS file.\n",
            silent=silent);

        res <- haven::read_sav(fileToRead);

        msg("Succesfully imported spreadsheet.\n",
            silent=silent);

      } else {

        stop("To read SPSS files (`.sav`), the {haven} package ",
             "has to be installed. You can install it with:\n\n",
             "  install.packages('haven');\n");

      }

    } else if (extension == "csv") {

      msg("Starting to import spreadsheet from comma separated values file.\n",
          silent=silent);

      res <- utils::read.csv(x);

      msg("Succesfully imported spreadsheet.\n",
          silent=silent);

    } else {

      stop("Sorry, but I can't (yet) read files with the extension of the ",
           "file you provided me with (", extension, ").");

    }

  }

  ###---------------------------------------------------------------------------
  ### Select desired sheet
  ###---------------------------------------------------------------------------

  if (!is.null(sheet)) {

    msg("You specified a value for the `sheet` argument: selecting that ",
        "spreadsheet (or those spreadsheets).\n",
        silent=silent);

    if (all(sheet %in% names(res))) {
      res <- res[sheet];
      msg("Selected sheet(s) ", vecTxtQ(sheet), "!\n",
          silent = silent);
    } else {
      msg("Sheet(s) ", vecTxtQ(sheet),
          " not (all) found in the imported data, so not ",
          "selecting anything.\n",
          silent = silent);
    }
  } else {
    if (is.data.frame(res)) {
      res <- list(res);
    }
  }

  ### Convert everything to a data frame
  resNames <- names(res);
  res <- lapply(res, as.data.frame);
  names(res) <- resNames;

  ### Note, at this point we have the results as a list of dataframes

  ###---------------------------------------------------------------------------
  ### Optionally write backup
  ###---------------------------------------------------------------------------

  if (downloaded && (!is.null(localBackup))) {

    msg("You passed a value for `localBackup` (", localBackup,
        "), so attempting to save a local backup.\n",
        silent = silent);

    if (!is.null(downloadResult) && (downloadResult == 0)) {

      msg("A file was downloaded, so just copying that file as local backup.\n",
          silent = silent);

      file.copy(fileToRead, localBackup, overwrite = TRUE);

    } else {

      msg("The data were read directly, so storing the local backup ",
          "in an Excel spreadsheet.\n",
          silent = silent);

      if (xlsxPkg == "rw_xl") {

        if (requireNamespace("writexl", quietly = TRUE)) {

          writexl::write_xlsx(
            x = res,
            path = localBackup
          );

        } else {

          stop("To write Excel spreadsheets (`.xlsx`), the {writexl} package ",
               "has to be installed. You can install it with:\n\n",
               "  install.packages('writexl');\n");

        }

      } else if (xlsxPkg == "openxlsx") {

        if (requireNamespace("openxlsx", quietly = TRUE)) {

          openxlsx::write.xlsx(
            x = res,
            file = localBackup,
            overwrite = TRUE
          );

        } else {

          stop("To write Excel spreadsheets (`.xlsx`), the {openxlsx} package ",
               "has to be installed. You can install it with:\n\n",
               "  install.packages('openxlsx');\n");

        }

      } else if (xlsxPkg == "XLConnect") {

        if (requireNamespace("XLConnect", quietly = TRUE)) {

          XLConnect::writeWorksheetToFile(
            file = localBackup,
            data = res,
            sheet = names(res)
          );

        } else {

          stop("To write Excel spreadsheets (`.xlsx`), the {XLConnect} package ",
               "has to be installed. You can install it with:\n\n",
               "  install.packages('XLConnect');\n");

        }

      }

    }

    msg("Local backup stored successfully.\n",
        silent = silent);

  }

  if ((length(res) == 1) && flattenSingleDf) {
    res <- res[[1]];
  }

  ### Return result

  msg("Returning result.\n",
      silent = silent);

  return(res);

}
