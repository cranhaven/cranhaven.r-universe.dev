sanitizeForDiagrammer <- function(dat,
                                  columns = names(dat),
                                  regExReplacements = opts$get("diagrammerSanitization")) {

  newData <- dat;

  columns <- columns[columns %in% names(dat)];

  newData[, columns] <-
    lapply(
      dat[, columns, drop=FALSE],
      function(column) {
        for (i in seq_along(regExReplacements)) {
          column <- gsub(regExReplacements[[i]][1],
                         regExReplacements[[i]][2],
                         column);
        }
        return(column);
      });

    return(newData);

  }
