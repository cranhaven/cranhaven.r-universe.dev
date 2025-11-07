setGeneric('writeSolar', function(object, file,
                                  complete = FALSE, day = FALSE,
                                  timeScales = c('i', 'd', 'm', 'y'), sep = ',',
                                  ...){
    standardGeneric('writeSolar')})

setMethod('writeSolar', signature = (object = 'Sol'),
          definition = function(object, file, complete = FALSE, day = FALSE,
                                timeScales = c('i', 'd', 'm', 'y'), sep = ',', ...){
              name <- strsplit(file, '\\.')[[1]][1]
              ext <- strsplit(file, '\\.')[[1]][2]
              timeScales <- match.arg(timeScales, several.ok = TRUE)
              if ('i' %in% timeScales) {
                  zI <- as.data.tableI(object, complete = complete, day = day)
                  write.table(zI,
                              file = file, sep = sep, row.names = FALSE, ...)
              }
              if ('d' %in% timeScales) {
                  zD <- as.data.tableD(object, complete = complete, day = day)
                  write.table(zD,
                              file = paste(name, 'D', ext, sep = '.'),
                              sep = sep, row.names = FALSE, ...)
              }
              if ('m' %in% timeScales) {
                  zM <- as.data.tableM(object, complete = complete, day = day)
                  write.table(zM,
                              file = paste(name, 'M', ext, sep = '.'),
                              sep = sep, row.names = FALSE, ...)
              }
              if ('y' %in% timeScales) {
                  zY <- as.data.tableY(object, complete = complete, day = day)
                  write.table(zY,
                              file = paste(name, 'Y', ext, sep = '.'),
                              sep = sep, row.names = FALSE, ...)
              }
          })
