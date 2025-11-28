#' Basic SPSS translation functions
#'
#' Basic functons to make working with R easier for SPSS users: getData and
#' getDat provide an easy way to load SPSS datafiles, and exportToSPSS to write
#' to a datafile and syntax file that SPSS can import; filterBy and useAll
#' allow easy temporary filtering of rows from the dataframe; mediaan and modus
#' compute the median and mode of ordinal or numeric data.
#'
#' @aliases basicSPSStranslation getData getDat
#' @param filename,file It is possible to specify a path and filename to load
#' here. If not specified, the default R file selection dialogue is shown.
#' \code{file} is still available for backward compatibility but will
#' eventually be phased out.
#' @param errorMessage The error message that is shown if the file does not
#' exist or does not have the right extension; "\[defaultErrorMessage\]" is
#' replaced with a default error message (and can be included in longer
#' messages).
#' @param applyRioLabels Whether to apply the labels supplied by Rio. This will
#' make variables that has value labels into factors.
#' @param use.value.labels Only useful when reading from SPSS files: whether to
#' read variables with value labels as factors (TRUE) or numeric vectors
#' (FALSE).
#' @param to.data.frame Only useful when reading from SPSS files: whether to
#' return a dataframe or not.
#' @param stringsAsFactors Whether to read strings as strings (FALSE) or
#' factors (TRUE).
#' @param silent Whether to suppress potentially useful information.
#' @param ...  Additional options, passed on to the function used to import the
#' data (which depends on the extension of the file).
#' @param dfName The name of the dataframe to create in the parent environment.
#' @param backup Whether to backup an object with name \code{dfName}, if one
#' already exists in the parent environment.
#' @param dat Dataframe to process: for filterBy, dataframe to filter rows
#' from; for useAll, dataframe to restore ('unfilter').
#' @param datafile The name of the data file, a comma separated values file
#' that can be read into SPSS by using the code file.
#' @param codefile The name of the code file, the SPSS syntax file that can be
#' used to import the data file.
#' @param savfile The name of the SPSS format .sav file (alternative for
#' writing a datafile and a codefile).
#' @param fileEncoding The encoding to use to write the files.
#' @param newLinesInString A string to replace newlines with (SPSS has problems
#' reading newlines).
#' @param expression Logical expression determining which rows to keep and
#' which to drop. Can be either a logical vector or a string which is then
#' evaluated. If it's a string, it's evaluated using 'with' to evaluate the
#' expression using the variable names.
#' @param replaceOriginalDataframe Whether to also replace the original
#' dataframe in the parent environment. Very messy, but for maximum
#' compatibility with the 'SPSS way of doing things', by default, this is true.
#' After all, people who care about the messiness/inappropriateness of this
#' function wouldn't be using it in the first place :-)
#' @param envir The environment where to create the 'backup' of the unfiltered
#' dataframe, for when useAll is called and the filter is deactivated again.
#' @param replaceFilteredDataframe Whether to replace the filtered dataframe
#' passed in the 'dat' argument (see replaceOriginalDataframe).
#' @param vector For mediaan and modus, the vector for which to find the median
#' or mode.
#' @return
#'
#' getData returns the imported dataframe, with the filename from which it was
#' read stored in the 'filename' attribute.
#'
#' getDat is a simple wrapper for \code{getData()} which creates a dataframe in
#' the parent environment, by default with the name 'dat'. Therefore, calling
#' \code{getDat()} in the console will allow the user to select a file, and the
#' data from the file will then be read and be available as 'dat'. If an object
#' with \code{dfName} (i.e. 'dat' by default) already exists, it will be backed
#' up with a warning. \code{getDat()} therefore returns nothing.
#'
#' mediaan returns the median, or, in the case of a factor where the median is
#' in between two categories, both categories.
#'
#' modus returns the mode.
#' @note getData() currently can't read from LibreOffice or OpenOffice files.
#' There doesn't seem to be a platform-independent package that allows this.
#' Non-CRAN package ROpenOffice from OmegaHat should be able to do the trick,
#' but fails to install (manual download and installation using
#' http://www.omegahat.org produces "ERROR: dependency 'Rcompression' is not
#' available for package 'ROpenOffice'" - and manual download and installation
#' of RCompression produces "Please define LIB_ZLIB; ERROR: configuration
#' failed for package 'Rcompression'"). If you have any suggestions, please let
#' me know!
#' @rdname basicSPSStranslation
#' @keywords utilities file univar
#' @examples
#'
#'
#' \dontrun{
#' ### Open a dialogue to read an SPSS file
#' getData();
#' }
#'
#' ### Get a median and a mode
#' mediaan(c(1,2,2,3,4,4,5,6,6,6,7));
#' modus(c(1,2,2,3,4,4,5,6,6,6,7));
#'
#' ### Create an example dataframe
#' (exampleDat <- data.frame(x=rep(8, 8), y=rep(c(0,1), each=4)));
#' ### Filter it, replacing the original dataframe
#' (filterBy(exampleDat, "y=0"));
#' ### Restore the old dataframe
#' (useAll(exampleDat));
#' @export
getData <- function(filename=NULL, file=NULL,
                    errorMessage = "[defaultErrorMessage]",
                    applyRioLabels= TRUE,
                    use.value.labels=FALSE,
                    to.data.frame=TRUE,
                    stringsAsFactors=FALSE,
                    silent=FALSE, ...) {

  dots <- list(...);
  fullArguments <- as.list(environment());
  matchedCall <- match.call();
  fullCall <- utils::capture.output(print(matchedCall));
  filenameArgument <- filename;

  ### 2016-08-02: Replacing imports with call to rio's 'import' function.
  encoding <- ifelse(is.null(dots$encoding), 'unknown', dots$encoding);

  #   ### File formats that have been implemented
  #   supportedFormats <- c("sav", "csv", "tsv", "rda", "ods", "xls", "xlsx", "rdata");

  if (is.null(filename)) {
    ### If no filename is specified, request one from the user
    if (!silent) {
      cat("You did not specify a file to open. Therefore, please select the",
          "file to open in the File Selection Dialog.",
          "Note that this dialog can sometimes appear behind the R window.",
          "If you do not see the file dialog now, use ALT-TAB or check the ",
          "start bar (in Windows), use COMMAND-TAB (in OSX), or check the ",
          "dock (in *nux based systems such as",
          "Ubuntu or OS X).");
    }
    filename <- file <- file.choose();
    slashesFilename <- gsub("\\", "/", filename, fixed=TRUE);

    if (length(matchedCall) == 1) {
      filenameArgument <- sub('getData(', paste0('getData(filename="',
                                                 trimws(slashesFilename), '"'),
                              fullCall, fixed=TRUE);
    } else {
      filenameArgument <- sub('getData(', paste0('getData(filename="',
                                                 trimws(slashesFilename), '", '),
                              fullCall, fixed=TRUE);
    }

    filenameArgument <- gsub(", ", ",\n        ", filenameArgument, fixed=TRUE);

    if (!silent) {
      cat("\n\nYou have selected a file. Based on your call and the filename",
          "and directory (path) you selected, this is the",
          "command you can use to read the datafile without",
          "a dialog, for example in an R script:\n\n");
      cat(filenameArgument, ";\n\n", sep="");
    }

    filenameArgument <- filename;

  }

  extension <- tolower(gsub(".*\\.(.*)$", '\\1', filenameArgument));

  #   ### Set error message
  #   errorMessage <- sub("\\[defaultErrorMessage\\]",
  #                       paste0("Specified file ('", filenameArgument,
  #                              "') does not have an extension identifying ",
  #                              "it as a readable filetype (identified extension is '",
  #                              extension, "', valid extensions are: '",
  #                              paste(supportedFormats, collapse="', '"), "')."),
  #                       errorMessage);

  errorMessage <- sub("\\[defaultErrorMessage\\]",
                      paste0("Specified file ('", filenameArgument,
                             "') cannot be imported."),
                      errorMessage);

  if (!file.exists(filename)) {
    stop("Specified file ('", filenameArgument,
         "') does not exist. Note that R is case sensitive, so make sure ",
         "your capitalisation is correct!")
  } else if (extension == "csv") {
    dat <- utils::read.csv(filename, stringsAsFactors=stringsAsFactors, ...);
  } else if (extension == "tsv") {
    dat <- utils::read.delim(filename, stringsAsFactors=stringsAsFactors, ...);
  } else {
    if (!requireNamespace("rio", quietly = TRUE)) {
      stop("Package \"rio\" needed to load .sav format. Please install it using `install.packages('rio');`.",
           call. = FALSE)
    } else {
      dat <- rio::import(filename, encoding=encoding, fread=FALSE);
      if (applyRioLabels) {
        dat[] <- lapply(dat, function(x) {
          if (is.null(attr(x, 'labels'))) {
            return(x);
          } else {
            return(factor(x, levels=attr(x, 'labels'), labels=names(attr(x, 'labels'))));
          }
        });
      }
    }
  }

  #   } else if (!(extension %in% supportedFormats)) {
  #     ### Show error if the file doesn't exist or has the wrong extension
  #     stop(errorMessage);
  #   } else {
  #     if ((extension == "rda") || (extension == "rdata")) {
  #       dat <- load(filename);
  #       dat <- get(dat);
  #     } else if (extension == "sav") {
  #       dat <- suppressWarnings(read.spss(filename, use.value.labels=use.value.labels,
  #                               to.data.frame=to.data.frame, ...));

  #       dat <- read.spss(filename, use.value.labels=use.value.labels,
  #                        to.data.frame=to.data.frame, ...);
  #       cat("Note that a warning like 'Unrecognized record type 7, subtype ## encountered in system file'",
  #           "is no cause for concern; the file is read normally.\n");

  #       dat <- tryCatch({
  #         read.spss(filename, use.value.labels=use.value.labels,
  #                   to.data.frame=to.data.frame, ...);
  #       }, warning=function(w) {
  #         if (grepl("Unrecognized record type 7, subtype [0123456789]+ encountered in system file", w)) {
  #           return(suppressWarnings(read.spss(filename, use.value.labels=use.value.labels,
  #                                   to.data.frame=to.data.frame, ...)));
  #          }
  #          else {
  #            return(read.spss(filename, use.value.labels=use.value.labels,
  #                             to.data.frame=to.data.frame, ...));
  #          }
  #       });

  #     } else if (extension == "csv") {
  #       dat <- read.csv(filename, stringsAsFactors=stringsAsFactors, ...);
  #     } else if (extension == "tsv") {
  #       dat <- read.delim(filename, stringsAsFactors=stringsAsFactors, ...);
  #     } else if (extension == "ods") {
  #
  #       stop("Sorry, I currently do not know how to import OpenOffice files. If you do, ",
  #            "please contact me and I'll add this as well!\nOf course, you can always export from ",
  #            "LibreOffice or OpenOffice to .csv (comma separated values) and load that file.");

  #       if (!is.element('ROpenOffice', installed.packages()[, 1])) {
  #          stop("To load OpenOffice or LibreOffice files, I need package 'ROpenOffice', ",
  #               "which is not on CRAN. Please visit http://omegahat.org for instructions, ",
  #               "or you can try to downloads and install it yourself directly using:\n\n",
  #               "install.packages('ROpenOffice', repos = 'http://www.omegahat.org/R', type = 'source');\n\n",
  #               "Note that you might need specific tools to compile this source package ",
  #               "(see Details in the install.packages() help, displayed with:\n\n?install.packages;");
  #       }
  #       require('ROpenOffice');
  #       dat <- read.ods(filename, ...);
  #     }
  #     else if ((extension == "xls") || (extension == "xlsx")) {
  #       if (!is.element('XLConnect', installed.packages()[, 1])) {
  #         stop("To load Excel (.xls or .xlsx) files, I need package 'XLConnect', ",
  #              "which in turn requires Java. Please install it yourself if you wish to ",
  #              "use this. You can install it using:\n\n",
  #              "install.packages('XLConnect')\n\nOf course, you can always export from ",
  #              "Excel to .csv (comma separated values) and load that file.");
  #       }
  #       else {
  #         wb <- XLConnect::loadWorkbook(filename, ...);
  #         dat <- XLConnect::readWorksheet(wb, sheet=1);
  #         if (requireNamespace"XLConnect", quietly=TRUE) {
  #           wb <- XLConnect::loadWorkbook(filename, ...);
  #           dat <- XLConnect::readWorksheet(wb, sheet=1);
  #         } else {
  #           stop("To load Excel (.xls or .xlsx) files, I need package 'XLConnect', ",
  #                "which in turn requires Java. Please install it yourself if you wish to ",
  #                "use this. You can install it using:\n\n",
  #                "install.packages('XLConnect')\n\nOf course, you can always export from ",
  #                "Excel to .csv (comma separated values) and load that file.");
  #         }
  #      }
  #    }

  ### Store the file where we got this dataframe
  attr(dat, "fileName") <- filename;
  ### Store the call
  attr(dat, "getDataCall") <- filenameArgument;

  ### Return the resuls
  return(dat);
  #  }
}

#' @rdname basicSPSStranslation
#' @export
getDat <- function(..., dfName="dat", backup=TRUE) {
  dat <- getData(...);
  if (exists(dfName, envir=sys.frame(-1)) && backup) {
    backupName <- paste0(dfName, '_backup_',
                         format(Sys.time(), "%Y%m%d_%H%M%S"));
    assign(backupName,
           value=get(dfName, envir=sys.frame(-1)),
           envir=sys.frame(-1));
    warning("NOTE: an object called '", dfName, "' already existed; I renamed ",
            "it to '", backupName, "'.");
  }
  assign(dfName, value=dat, envir=sys.frame(-1));
  cat("The data has been stored in a dataframe called '",
      dfName, "'. That means that if you want to repeat this command and ",
      "store the dataframe with the same name, you have to use:\n\n",
      dfName, " <- getData('", attributes(dat)$getDataCall, "');\n\n",
      sep="");
}
