library(itertools)

# Write a data frame to disk, using one file per column.
# The optional fprefix argument is used to partially
# specify the name of these files.
writedf <- function(df, fprefix=as.character(substitute(df))) {
  # Handle fprefix argument
  if (! is.character(fprefix))
    stop('fprefix must be character')
  if (length(fprefix) == 1)
    fprefix <- sprintf('%s_%02d', fprefix, seq_along(df))

  # Write each column of "df" to a file
  for (icol in seq_along(df)) {
    p <- fprefix[icol]
    cls <- class(df[[icol]])[1]
    type <- if (cls == 'numeric') 'double' else cls

    if (type == 'factor') {
      writeBin(as.integer(df[[icol]]), sprintf('%s_factor.col', p))
      writeLines(levels(df[[icol]]), sprintf('%s_factor.lev', p))
    } else if (type == 'character') {
      writeLines(df[[icol]], sprintf('%s_character.col', p))
    } else if (type == 'integer') {
      writeBin(df[[icol]], sprintf('%s_integer.col', p))
    } else if (type == 'double') {
      writeBin(df[[icol]], sprintf('%s_double.col', p))
    } else {
      stop('unsupported type: ', type)
    }
  }
}

# Read a data frame from files containing column data.
# The arguments "n", "start", and "col.names" can be used
# to read part of the data frame.
#
# Although you must specify a value for "n", you can
# specify a value that is larger than the number of elements
# in the files.  This will allocate too much memory, so
# try not to overestimate by too much.
readdf <- function(filenames, n, start=1, col.names) {
  it <- ireaddf(filenames, n, start, col.names, chunkSize=n)
  df <- nextOr(it)
  nextOr(it, NULL)
  df
}

# This is a writedf combiner factory function


#' Create an object that contains a combiner function
#'
#' Create an object that contains a combiner function.
#'
#'
#' @param filenames Names of files to write column data to.
#' @keywords utilities
#' @export writedf.combiner
writedf.combiner <- function(filenames) {
  opencol <- function(i) {
    # Extract the type of data from the file name
    m <- regexpr('(factor|character|integer|double)', filenames[i])
    if (m < 1)
      stop('illegal file name: ', filenames[i])
    type <- substr(filenames[i], m, attr(m, 'match.length') + m - 1L)

    if (type == 'factor') {
      conn <- file(filenames[i], 'wb')
      lfile <- sub('\\..+$', '.lev', filenames[i])
      if (lfile == filenames[i])
        lfile <- paste(filenames[i], '.lev', sep='')
    } else if (type == 'character') {
      conn <- file(filenames[i], 'wt')
      lfile <- NULL
    } else if (type == 'integer') {
      conn <- file(filenames[i], 'wb')
      lfile <- NULL
    } else if (type == 'double') {
      conn <- file(filenames[i], 'wb')
      lfile <- NULL
    } else {
      stop('error: type = ', type)
    }

    list(type=type, conn=conn, lfile=lfile)
  }

  combine <- function(...) {
    # Write each column of "df" to a file
    dfs <- list(...)
    for (icol in seq_along(columndata)) {
      colinfo <- columndata[[icol]]

      if (colinfo$type == 'factor') {
        for (df in dfs) {
          if (! is.null(df)) {
            levsvar <- sprintf('levels.%d', icol)
            levs <- get(levsvar)
            if (is.null(levs)) {
              assign(levsvar, levels(df[[icol]]), inherits=TRUE)
            } else {
              if (any(levs != levels(df[[icol]]))) {
                # XXX should this be an error?
                # XXX should a try to fix the problem?
                warning('inconsistent levels found for column ', icol)
              }
            }
            writeBin(as.integer(df[[icol]]), colinfo$conn)
          }
        }
      } else if (colinfo$type == 'character') {
        for (df in dfs) {
          if (! is.null(df))
            writeLines(df[[icol]], colinfo$conn)
        }
      } else if (colinfo$type == 'integer') {
        for (df in dfs) {
          if (! is.null(df))
            writeBin(df[[icol]], colinfo$conn)
        }
      } else if (colinfo$type == 'double') {
        for (df in dfs) {
          if (! is.null(df))
            writeBin(df[[icol]], colinfo$conn)
        }
      } else {
        stop('unsupported type: ', colinfo$type)
      }
    }
    NULL
  }

  closeallcolumns <- function() {
    # Close all column data files
    for (i in seq_along(columndata)) {
      colinfo <- columndata[[i]]
      close(colinfo$conn)
      if (! is.null(colinfo$lfile)) {
        levsvar <- sprintf('levels.%d', i)
        levs <- get(levsvar)
        if (is.null(levs)) {
          warning(sprintf('not writing levels file for column %d\n', i))
        } else {
          writeLines(levs, colinfo$lfile)
        }
      }
    }
  }

  columndata <- lapply(seq_along(filenames), opencol)
  for (i in seq_along(filenames))
    assign(sprintf('levels.%d', i), NULL)

  # Construct and return the combiner object
  obj <- list(combine=combine, close=closeallcolumns)
  class(obj) <- c('combiner')
  obj
}


testreaddf <- function(n=1000, s=c(1, 8, 9, 12)) {
  filenames <- Sys.glob('fifty1_*.col')
  col.names <- c('AOU', 'RouteDataID', 'countrynum', 'statenum',
                 'Route', 'RPID', 'year', 'rteNo',
                 'species', 'stopNo', 'count', 'rtestopNo')
  readdf(filenames[s], n, col.names=col.names[s])
}

testireaddf <- function(n=200000000, chunkSize=1000000) {
  library(foreach)
  filenames <- Sys.glob('fifty1_*.col')
  col.names <- c('AOU', 'RouteDataID', 'countrynum', 'statenum',
                 'Route', 'RPID', 'year', 'rteNo',
                 'species', 'stopNo', 'count', 'rtestopNo')

  nfilenames <- sub('fifty1', 'mod', filenames, fixed=TRUE)
  print(nfilenames)
  cobj <- writedf.combiner(nfilenames)

  # Copy the sub-data frames from one set of files to another
  cat('Starting to read "fifty1" data files...\n')
  foreach(df=ireaddf(filenames, n, col.names=col.names,
                     chunkSize=chunkSize),
          .combine=cobj$combine, .maxcombine=3) %do% {
    df
  }
  cobj$close()

  # Check that the two sets of files contain identical data frames
  cat('Compare the two sets of data files...\n')
  r <- foreach(df1=ireaddf(filenames, n, col.names=col.names,
                           chunkSize=chunkSize),
               df2=ireaddf(nfilenames, n, col.names=col.names,
                           chunkSize=chunkSize),
               .combine='all', .maxcombine=50) %do% {
    identical(df1, df2)
  }

  print(r)

  cat('Finished\n')
}
