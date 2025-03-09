#' Convert 'rectangular' or spreadsheet-format data to one or more sources
#'
#' These functions first import data from a 'data format', such as spreadsheets
#' in `.xlsx` format, comma-separated values files (`.csv`), or SPSS data
#' files (`.sav`). You can also just use R data frames (imported however you
#' want). These functions then use the columns you specified to convert these
#' data to one (`oneFile=TRUE`) or more (`oneFile=FALSE`) `rock`
#' source file(s), optionally including class instance
#' identifiers (such as case identifiers to identify participants, or location
#' identifiers, or moment identifiers, etc) and using those to link the
#' utterances to attributes from columns you specified. You can also precode
#' the utterances with codes you specify (if you ever would want to for some
#' reason).
#'
#' @param data The data frame containing the data to convert.
#' @param file The path to a file containing the data to convert.
#' @param importArgs Optionally, a list with named elements representing
#' arguments to pass when importing the file.
#' @param output If `oneFile=TRUE` (the default), the name (and path)
#' of the file in which to save the processed source (if it is `NULL`, the
#' resulting character vector will be returned visibly instead of invisibly).
#' Note that the ROCK convention is to use `.rock` as extension.
#' If `oneFile=FALSE`, the path to which to write the
#' sources (if it is `NULL`, as a result a list of character vectors will be
#' returned visibly instead of invisibly).
#' @param omit_empty_rows Whether to omit rows where the values in the columns
#' specified to convert to utterances are all empty (or contain only
#' whitespace).
#' @param cols_to_utterances The names of the columns to convert to utterances,
#' as a character vector.
#' @param cols_to_ciids The names of the columns to convert to class instance
#' identifiers (e.g. case identifiers), as a named character vector, with
#' the values being the column names in the data frame, and the names being the
#' class instance identifiers (e.g. `"sourceId"`, `"fieldId"`, `"caseId"`, etc).
#' @param cols_to_codes The names of the columns to convert to codes (i.e.
#' codes appended to every utterance), as a character vector. When writing codes,
#' it is not possible to also write multiple utterance columns
#' (i.e. `utterance_classId` must be `NULL`).
#' @param cols_to_attributes The names of the columns to convert to attributes,
#' as a named character vector, where each name is the name of the class
#' instance identifier to attach the attribute to. If only one column is passed
#' in `cols_to_ciids`, names can be omitted and a regular unnamed character
#' vector can be passed.
#' @param utterance_classId When specifying multiple columns with utterances,
#' and `utterance_classId` is not `NULL`, the column names are considered to be
#' class instance identifiers, and specified above each utterance using the
#' class identifier specified here (e.g. "`utterance_classId="originalColName"`"
#' yields something like "`[[originalColName=colName_1]]`" above all utterances
#' from the column named `colName_1`). When writing multiple utterance columns,
#' it is not possible to also write codes (i.e. `cols_to_codes` must be `NULL`).
#' @param oneFile Whether to store everything in one source, or create one
#' source for each row of the data (if this is set to `FALSE`, make sure that
#' `cols_to_sourceFilename` specifies one or more columns that together
#' uniquely identify each row; also, in that case, `output` must be an existing
#' directory on your PC).
#' @param cols_to_sourceFilename The columns to use as unique part of the
#' filename of each source. These will be concatenated using
#' `cols_in_sourceFilename_sep` as a separator. Note that the final string
#' *must* be unique for each row in the dataset, otherwise the filenames for
#' multiple rows will be the same and will be overwritten! By default, the
#' columns specified with class instance identifiers are used.
#' @param cols_in_sourceFilename_sep The separator to use when concatenating
#' the `cols_to_sourceFilename`.
#' @param sourceFilename_prefix,sourceFilename_suffix Strings that are
#' prepended and appended to the `col_to_sourceFilename` to create the full
#' filenames. Note that `.rock` will always be added to the end as extension.
#' @param ciid_labels The labels for the class instance identifiers. Class
#' instance identifiers have brief codes used in coding (e.g. 'cid' is the
#' default for Case Identifiers, often used to identify participants) as well
#' as more 'readable' labels that are used in the attributes (e.g. 'caseId' is
#' the default class instance identifier for Case Identifiers). These can be
#' specified here as a named vector, with each element being the label and
#' the element's name the identifier.
#' @param ciid_separator The separator for the class instance identifier - by
#' default, either an equals sign (`=`) or a colon (`:`) are supported, but
#' an equals sign is less ambiguous.
#' @param attributesFile Optionally, a file to write the attributes to if you
#' don't want them to be written to the source file(s).
#' @param preventOverwriting Whether to prevent overwriting of output files.
#' @param encoding The encoding of the source(s).
#' @param silent Whether to suppress the warning about not editing the cleaned source.
#'
#' @return A source as a character vector.
#'
#' @export
#'
#' @rdname convert_to_source
#'
#' @examples ### Get path to example files
#' examplePath <-
#'   system.file("extdata", package="rock");
#'
#' ### Get a path to file with example data frame
#' exampleFile <-
#'   file.path(examplePath, "spreadsheet-import-test.csv");
#'
#' ### Read data into a data frame
#' dat <-
#'   read.csv(exampleFile);
#'
#' ### Convert data frame to a source
#' source_from_df <-
#'   convert_df_to_source(
#'     dat,
#'     cols_to_utterances = c("open_question_1",
#'                            "open_question_2"),
#'     cols_to_ciids = c(cid = "id"),
#'     cols_to_attributes = c("age", "gender"),
#'     cols_to_codes = c("code_1", "code_2"),
#'     ciid_labels = c(cid = "caseId")
#'  );
#'
#' ### Show the result
#' cat(
#'   source_from_df,
#'   sep = "\n"
#' );
convert_df_to_source <- function(data,
                                 output = NULL,
                                 omit_empty_rows = TRUE,
                                 cols_to_utterances = NULL,
                                 cols_to_ciids = NULL,
                                 cols_to_codes = NULL,
                                 cols_to_attributes = NULL,
                                 utterance_classId = NULL,
                                 oneFile = TRUE,
                                 cols_to_sourceFilename = cols_to_ciids,
                                 cols_in_sourceFilename_sep = "=",
                                 sourceFilename_prefix = "source_",
                                 sourceFilename_suffix = "",
                                 ciid_labels = NULL,
                                 ciid_separator = "=",
                                 attributesFile = NULL,
                                 preventOverwriting = rock::opts$get(preventOverwriting),
                                 encoding = rock::opts$get(encoding),
                                 silent = rock::opts$get(silent)) {

  delimiterString <- rock::opts$get('delimiterString');
  attributeContainer <- rock::opts$get('attributeContainers')[1];
  codeDelimiters <- rock::opts$get('codeDelimiters');

  if (!is.data.frame(data)) {
    stop("As `data`, you must pass a data frame!");
  }

  if (!is.null(cols_to_codes) && !is.null(utterance_classId)) {
    stop("Either one or both of `cols_to_codes` and `utterance_classId` ",
         "must always be `NULL`!");
  }

  allCols <-
    c(cols_to_utterances,
      cols_to_ciids,
      cols_to_codes,
      cols_to_attributes);

  if (!all(allCols %in% names(data))) {
    stop("Not all columns you specified exist in the data frame ",
         "you passed. Missing the following column(s): ",
         vecTxtQ(allCols[!(allCols %in% names(data))]), ".");
  }

  if (is.null(ciid_labels)) {
    if (!is.null(cols_to_ciids)) {
      ciid_labels <-
        stats::setNames(
          names(cols_to_ciids),
          nm = names(cols_to_ciids)
        );
    }
  }

  sourceList <- list();
  attributeList <- list();
  attributesAsYamlList <- list();

  if (omit_empty_rows) {
    oldData <- data;
    rowsWithUtterances <-
      nchar(
        trimws(
          unlist(
            apply(
              oldData[, cols_to_utterances, drop=FALSE],
              1,
              paste0,
              collapse = "",
              simplify = FALSE
            )
          )
        )
      ) > 0;

    data <- oldData[rowsWithUtterances, ];

    emptyRows <- nrow(oldData) - nrow(data);

    if (emptyRows > 0) {
      msg("Deleted ", emptyRows, " 'empty rows' from the data (rows with no ",
          "data in the columns you specified to convert to utterances), ",
          "leaving ", sum(rowsWithUtterances), " rows with utterances.\n",
          silent=silent);
    } else {
      msg("No 'empty rows' found: every row had data in the columns you ",
          "specified to convert to utterances.\n", silent=silent);
    }
  }

  ### Create a vector with any codes we want to add

  if (!is.null(cols_to_codes)) {
    codeVector <-
      apply(
        data[, cols_to_codes, drop=FALSE],
        1,
        function(row) {
          return(
            paste0(
              codeDelimiters[1],
              as.vector(row),
              codeDelimiters[2],
              collapse = " "
            )
          );
        }
      );
    codeVector <- paste0(
      " ",
      codeVector
    );
  } else {
    codeVector <- rep("", nrow(data));
  }

  for (i in 1:nrow(data)) {

    sourceList[[i]] <- "";

    if (!is.null(cols_to_ciids)) {

      ### Add the class instance identifiers

      for (j in seq_along(cols_to_ciids)) {
        sourceList[[i]] <-
          c(sourceList[[i]],
            paste0(
              codeDelimiters[1],
              names(cols_to_ciids)[j],
              ciid_separator,
              data[i, cols_to_ciids[j], drop=FALSE],
              codeDelimiters[2]
            )
          );
      }

      ### Add the utterances and codes

      if (is.null(utterance_classId)) {
        for (j in cols_to_utterances) {
          sourceList[[i]] <-
            c(sourceList[[i]],
              "",
              paste0(data[i, j], codeVector[i])
            );
        }
      } else {
        for (j in cols_to_utterances) {
          sourceList[[i]] <-
            c(sourceList[[i]],
              "",
              paste0(
                codeDelimiters[1],
                utterance_classId,
                ciid_separator,
                j,
                codeDelimiters[2]
              ),
              "",
              paste0(data[i, j])
            );
        }
      }

      ### Create an object with attributes

      if (!is.null(cols_to_attributes)) {

        currentAttributes <-
          stats::setNames(
            c(list(as.character(data[i, cols_to_ciids])),
              as.list(as.character(data[i, cols_to_attributes]))
            ),
            nm = c(ciid_labels[names(cols_to_ciids)],
                   cols_to_attributes)
          );

        attributeList[[i]] <-
          currentAttributes;

        attributesAsYamlList[[i]] <-
          attributeList_to_yaml(
            currentAttributes,
            delimiterString = delimiterString,
            attributeContainer = attributeContainer
          );

      } else {

        attributeList <- c();

      }

    }

    ### Add empty line to the end
    sourceList[[i]] <-
      c(sourceList[[i]], "");

  }

  if (length(attributeList) > 0) {

    allAttributes_as_yaml <-
      attributeList_to_yaml(
        attributeList,
        delimiterString = delimiterString,
        attributeContainer = attributeContainer
      );

  }

  ### Check whether we should save the attributes to one separate file

  if (!is.null(attributesFile)) {

    if (file.exists(attributesFile) && preventOverwriting) {
      warning("The file you specified to save the attributes to, '",
              attributesFile,
              "', exists, and `preventOverwriting` is set to TRUE, ",
              "so not writing the attributes to disk! Pass ",
              "`preventOverwriting=FALSE` to override this.\n\nNote that ",
              "because you did pass a value for `attributesFile`, the ",
              "attributes will not be stored in the source(s), so unless ",
              "the attributes that were already stored in that file ",
              "are correct, you may want to re-run this command and pass ",
              "another filename, set `preventOverwriting` to `FALSE`, or ",
              "pass `NULL` for `attributesFile`.");
    } else {
      con <- file(description=attributesFile,
                  open="w",
                  encoding=encoding);
      writeLines(text=allAttributes_as_yaml,
                 con=con);
      close(con);
    }

  }

  if (oneFile) {

    allInOneSource <-
      unlist(
        sourceList
      );

    if ((length(attributeList) > 0) && is.null(attributesFile)) {

      completeAllInOneSource <-
        c(allInOneSource,
          "",
          "",
          allAttributes_as_yaml);

    } else {

      completeAllInOneSource <- allInOneSource;

    }

    if (is.null(output)) {
      msg("Nothing specified as `output`, returning produced source visibly.\n",
          silent=silent);
      return(completeAllInOneSource);
    } else {
      if (preventOverwriting && (file.exists(output))) {
        warning("The file you specified to save the output to, '",
                output,
                "', exists, and `preventOverwriting` is set to TRUE, ",
                "so not writing the source to disk! Pass ",
                "`preventOverwriting=FALSE` to override this.");
      } else {
        con <- file(description=output,
                    open="w",
                    encoding=encoding);
        writeLines(text=completeAllInOneSource,
                   con=con);
        close(con);
        msg("Wrote the produced source to the file specified as `output` (",
            output, ").\n",
            silent=silent);
      }
      return(invisible(completeAllInOneSource));
    }

  } else {

    if (!dir.exists(output)) {
      stop("You indicated that you wanted to write the produced sources ",
           "to directory '", output, "', but it doesn't seem to exist.");
    }

    res <- list();

    filenames_to_write_to <-
      file.path(
        output,
        paste0(
          sourceFilename_prefix,
          apply(
            data[, cols_to_ciids],
            1,
            paste,
            sep = cols_in_sourceFilename_sep
          ),
          sourceFilename_suffix,
          ".rock"
        )
      );

    for (i in seq_along(sourceList)) {

      if (is.null(attributesFile)) {

        res[[i]] <-
          c(sourceList[[i]],
            "",
            attributesAsYamlList[[i]],
            "");

      } else {

        res[[i]] <-
          c(sourceList[[i]]);

      }

    }

    if (is.null(output)) {
      msg("Nothing specified as `output`, returning a list of the produced ",
          "sources visibly.\n",
          silent=silent);
      return(res);
    } else {

      for (i in seq_along(sourceList)) {

        if (preventOverwriting && (file.exists(filenames_to_write_to[i]))) {
          warning("A file you specified to save the output to, '",
                  filenames_to_write_to[i],
                  "', exists, and `preventOverwriting` is set to TRUE, ",
                  "so not writing the source to disk!");
        } else {
          con <- file(description=filenames_to_write_to[i],
                      open="w",
                      encoding=encoding);
          writeLines(text=res[[i]],
                     con=con);
          close(con);
        }

      }

      msg("Wrote ", length(res), " produced sources to files in the ",
          "directory specified as `output` (",
          output, ").\n",
          silent=silent);


      return(invisible(res));
    }

  }

}

attributeList_to_yaml <- function(attributeList,
                                  delimiterString,
                                  attributeContainer) {

  attributeList <-
    list(attributeList);

  names(attributeList) <-
    attributeContainer;

  attributes_as_yaml <-
    c(delimiterString,
      unlist(
        strsplit(
          yaml::as.yaml(
            attributeList
          ),
          "\n",
          fixed = TRUE
        )
      ),
      delimiterString,
      ""
    );

  return(attributes_as_yaml);

}

# dat <- openxlsx::read.xlsx(here::here("inst", "extdata", "spreadsheet-import-test.xlsx"))
# write.csv(dat, here::here("inst", "extdata", "spreadsheet-import-test.csv"), row.names=FALSE)

import_and_convert_to_source <- function(file,
                                         importFunction,
                                         importArgs = NULL,
                                         silent = rock::opts$get(silent),
                                         ...) {

  if (file.exists(file)) {

    dat <-
      do.call(
        importFunction,
        c(list(file),
          importArgs)
      )

  } else {
    stop("The file you specified to import (", file, ") does ",
         "not seem to exist.");
  }

  msg("Imported a data frame with ", ncol(dat), " columns and ",
      nrow(dat), " rows. Converting it to a source.\n",
      silent = silent);

  return(
    convert_df_to_source(
      data = dat,
      silent = silent,
      ...
    )
  );

}

#' @rdname convert_to_source
#' @export
convert_csv_to_source <- function(file,
                                  importArgs = NULL,
                                  omit_empty_rows = TRUE,
                                  output = NULL,
                                  cols_to_utterances = NULL,
                                  cols_to_ciids = NULL,
                                  cols_to_codes = NULL,
                                  cols_to_attributes = NULL,
                                  oneFile = TRUE,
                                  cols_to_sourceFilename = cols_to_ciids,
                                  cols_in_sourceFilename_sep = "=",
                                  sourceFilename_prefix = "source_",
                                  sourceFilename_suffix = "",
                                  ciid_labels = NULL,
                                  ciid_separator = "=",
                                  attributesFile = NULL,
                                  preventOverwriting = rock::opts$get(preventOverwriting),
                                  encoding = rock::opts$get(encoding),
                                  silent = rock::opts$get(silent)) {

  importFunction <- utils::read.csv;

  return(
    import_and_convert_to_source(
      file = file,
      output = output,
      importFunction = importFunction,
      importArgs = importArgs,
      omit_empty_rows = omit_empty_rows,
      cols_to_utterances = cols_to_utterances,
      cols_to_ciids = cols_to_ciids,
      cols_to_codes = cols_to_codes,
      cols_to_attributes = cols_to_attributes,
      oneFile = oneFile,
      cols_to_sourceFilename = cols_to_sourceFilename,
      cols_in_sourceFilename_sep = cols_in_sourceFilename_sep,
      sourceFilename_prefix = sourceFilename_prefix,
      sourceFilename_suffix = sourceFilename_suffix,
      ciid_labels = ciid_labels,
      ciid_separator = ciid_separator,
      attributesFile = attributesFile,
      preventOverwriting = preventOverwriting,
      encoding = encoding,
      silent = silent
    )
  );

}

#' @rdname convert_to_source
#' @export
convert_csv2_to_source <- function(file,
                                   importArgs = NULL,
                                   omit_empty_rows = TRUE,
                                   output = NULL,
                                   cols_to_utterances = NULL,
                                   cols_to_ciids = NULL,
                                   cols_to_codes = NULL,
                                   cols_to_attributes = NULL,
                                   oneFile = TRUE,
                                   cols_to_sourceFilename = cols_to_ciids,
                                   cols_in_sourceFilename_sep = "=",
                                   sourceFilename_prefix = "source_",
                                   sourceFilename_suffix = "",
                                   ciid_labels = NULL,
                                   ciid_separator = "=",
                                   attributesFile = NULL,
                                   preventOverwriting = rock::opts$get(preventOverwriting),
                                   encoding = rock::opts$get(encoding),
                                   silent = rock::opts$get(silent)) {

  importFunction <- utils::read.csv2;

  return(
    import_and_convert_to_source(
      file = file,
      output = output,
      importFunction = importFunction,
      importArgs = importArgs,
      omit_empty_rows = omit_empty_rows,
      cols_to_utterances = cols_to_utterances,
      cols_to_ciids = cols_to_ciids,
      cols_to_codes = cols_to_codes,
      cols_to_attributes = cols_to_attributes,
      oneFile = oneFile,
      cols_to_sourceFilename = cols_to_sourceFilename,
      cols_in_sourceFilename_sep = cols_in_sourceFilename_sep,
      sourceFilename_prefix = sourceFilename_prefix,
      sourceFilename_suffix = sourceFilename_suffix,
      ciid_labels = ciid_labels,
      ciid_separator = ciid_separator,
      attributesFile = attributesFile,
      preventOverwriting = preventOverwriting,
      encoding = encoding,
      silent = silent
    )
  );

}


#' @rdname convert_to_source
#' @export
convert_xlsx_to_source <- function(file,
                                   importArgs = list(),
                                   omit_empty_rows = TRUE,
                                   output = NULL,
                                   cols_to_utterances = NULL,
                                   cols_to_ciids = NULL,
                                   cols_to_codes = NULL,
                                   cols_to_attributes = NULL,
                                   oneFile = TRUE,
                                   cols_to_sourceFilename = cols_to_ciids,
                                   cols_in_sourceFilename_sep = "=",
                                   sourceFilename_prefix = "source_",
                                   sourceFilename_suffix = "",
                                   ciid_labels = NULL,
                                   ciid_separator = "=",
                                   attributesFile = NULL,
                                   preventOverwriting = rock::opts$get(preventOverwriting),
                                   encoding = rock::opts$get(encoding),
                                   silent = rock::opts$get(silent)) {

  if (requireNamespace("openxlsx", quietly = TRUE)) {

    importFunction = openxlsx::read.xlsx;

  } else {
    stop("To import .sav files, you need to have the {openxlsx} package ",
         "installed. You can install it with:\n\n  ",
         "install.packages('openxlsx');\n");
  }

  return(
    import_and_convert_to_source(
      file = file,
      output = output,
      importFunction = importFunction,
      importArgs = importArgs,
      omit_empty_rows = omit_empty_rows,
      cols_to_utterances = cols_to_utterances,
      cols_to_ciids = cols_to_ciids,
      cols_to_codes = cols_to_codes,
      cols_to_attributes = cols_to_attributes,
      oneFile = oneFile,
      cols_to_sourceFilename = cols_to_sourceFilename,
      cols_in_sourceFilename_sep = cols_in_sourceFilename_sep,
      sourceFilename_prefix = sourceFilename_prefix,
      sourceFilename_suffix = sourceFilename_suffix,
      ciid_labels = ciid_labels,
      ciid_separator = ciid_separator,
      attributesFile = attributesFile,
      preventOverwriting = preventOverwriting,
      encoding = encoding,
      silent = silent
    )
  );

}

#' @rdname convert_to_source
#' @export
convert_sav_to_source <- function(file,
                                  importArgs = NULL,
                                  omit_empty_rows = TRUE,
                                  output = NULL,
                                  cols_to_utterances = NULL,
                                  cols_to_ciids = NULL,
                                  cols_to_codes = NULL,
                                  cols_to_attributes = NULL,
                                  oneFile = TRUE,
                                  cols_to_sourceFilename = cols_to_ciids,
                                  cols_in_sourceFilename_sep = "=",
                                  sourceFilename_prefix = "source_",
                                  sourceFilename_suffix = "",
                                  ciid_labels = NULL,
                                  ciid_separator = "=",
                                  attributesFile = NULL,
                                  preventOverwriting = rock::opts$get(preventOverwriting),
                                  encoding = rock::opts$get(encoding),
                                  silent = rock::opts$get(silent)) {

  if (requireNamespace("haven", quietly = TRUE)) {

    importFunction = haven::read_sav;

  } else {
    stop("To import .sav files, you need to have the {haven} package ",
         "installed. You can install it with:\n\n  ",
         "install.packages('haven');\n");
  }

  return(
    import_and_convert_to_source(
      file = file,
      output = output,
      importFunction = importFunction,
      omit_empty_rows = omit_empty_rows,
      importArgs = importArgs,
      cols_to_utterances = cols_to_utterances,
      cols_to_ciids = cols_to_ciids,
      cols_to_codes = cols_to_codes,
      cols_to_attributes = cols_to_attributes,
      oneFile = oneFile,
      cols_to_sourceFilename = cols_to_sourceFilename,
      cols_in_sourceFilename_sep = cols_in_sourceFilename_sep,
      sourceFilename_prefix = sourceFilename_prefix,
      sourceFilename_suffix = sourceFilename_suffix,
      ciid_labels = ciid_labels,
      ciid_separator = ciid_separator,
      attributesFile = attributesFile,
      preventOverwriting = preventOverwriting,
      encoding = encoding,
      silent = silent
    )
  );

}
