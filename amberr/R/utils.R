#' Display search result metrics
#' @keywords internal
.reportListMetrics <- function(results) {
  if (!is.null(results) && !is.null(results$total)) {
    message(
      "results: ",
      ifelse(
        is.null(results$found),
        length(results$data),
        results$found
      ),
      "/",
      results$total,
      " skip: ",
      results$skip,
      " limit: ",
      results$limit
    )
  }
}

#' Converts a NULL value to NA
#' @keywords internal
.nullToNA <- function(x) {
  ifelse(is.null(x), NA, x)
}

#' Convert list to vector
#' @keywords internal
.unlist <- function (x) {
  if (is.list(x)) {
    unlist(lapply(x, function (val) {
      .nullToNA(val)
    }))
  } else {
    .nullToNA(x)
  }
}

#' Turns a dictionary in JSON format into a "Opal" one (variables and categories)
#' @keywords internal
.makeDictionary <- function(vars) {
  toAttributeKey <- function(attr) {
    key <- attr$name
    if ("namespace" %in% names(attr)) {
      key <- paste0(attr$namespace, "::", key)
    }
    if ("locale" %in% names(attr)) {
      key <- paste0(key, ":", attr$locale)
    }
    key
  }

  n <- length(vars)
  if (n > 0) {
    name <- rep(NA, n)
    entityType <- rep(NA, n)
    valueType <- rep(NA, n)
    unit <- rep(NA, n)
    referencedEntityType <- rep(NA, n)
    mimeType <- rep(NA, n)
    repeatable <- rep(FALSE, n)
    occurrenceGroup <- rep(NA, n)
    index <- rep(NA, n)
    variables.attributes <- list()

    categories.variable <- c()
    categories.name <- c()
    categories.missing <- c()
    categories.attributes <- list()

    # read variables
    for (i in 1:n) {
      var <- vars[[i]]
      name[i] <- var$name
      entityType[i] <- var$entityType
      valueType[i] <- var$valueType
      unit[i] <- .nullToNA(var$unit)
      referencedEntityType[i] <- .nullToNA(var$referencedEntityType)
      mimeType[i] <- .nullToNA(var$mimeType)
      repeatable[i] <-
        ifelse(is.null(var$isRepeatable), FALSE, var$isRepeatable)
      occurrenceGroup[i] <- .nullToNA(var$occurrenceGroup)
      index[i] <- ifelse(is.null(var$index), i, var$index)

      # read variable's attributes
      if (!is.null(var$attributes)) {
        for (attribute in var$attributes) {
          key <- toAttributeKey(attribute)
          if (!(key %in% names(variables.attributes))) {
            a <- list()
            a[[key]] <- rep(NA, n)
            variables.attributes <- append(variables.attributes, a)
          }
          variables.attributes[[key]][i] <- attribute$value
        }
      }

      # read variable's categories
      catn <- length(var$categories)
      cat.name <- rep(NA, catn)
      cat.missing <- rep(NA, catn)
      cat.attributes <- list()
      if (catn > 0) {
        for (j in 1:catn) {
          cat <- var$categories[[j]]
          cat.name[j] <- cat$name
          cat.missing[j] <-
            ifelse(!is.null(cat$isMissing), cat$isMissing, FALSE)
          if (!is.null(cat$attributes)) {
            for (attribute in cat$attributes) {
              key <- toAttributeKey(attribute)
              if (!(key %in% names(cat.attributes))) {
                a <- list()
                a[[key]] <- rep(NA, catn)
                cat.attributes <- append(cat.attributes, a)
              }
              cat.attributes[[key]][j] <- attribute$value
            }
          }
        }

        lg <-
          length(categories.name) # original length before appending
        categories.variable <-
          append(categories.variable, rep(var$name, catn))
        categories.name <- append(categories.name, cat.name)
        categories.missing <-
          append(categories.missing, cat.missing)
        for (col in names(cat.attributes)) {
          if (!(col %in% names(categories.attributes))) {
            # init with NAs
            categories.attributes[[col]] <- rep(NA, lg)
          } else {
            # complete with NAs
            times <- lg - length(categories.attributes[[col]])
            categories.attributes[[col]] <-
              append(categories.attributes[[col]], rep(NA, times))
          }
          categories.attributes[[col]] <-
            append(categories.attributes[[col]], cat.attributes[[col]])
        }
      }
    }

    # build output data frames
    variables <-
      data.frame(
        name,
        entityType,
        valueType,
        unit,
        referencedEntityType,
        mimeType,
        repeatable,
        occurrenceGroup,
        index,
        stringsAsFactors = FALSE
      )
    for (col in names(variables.attributes)) {
      variables[[col]] <- variables.attributes[[col]]
    }
    categories <-
      data.frame(
        variable = categories.variable,
        name = categories.name,
        missing = categories.missing,
        stringsAsFactors = FALSE
      )
    for (col in names(categories.attributes)) {
      times <-
        length(categories.name) - length(categories.attributes[[col]])
      if (times > 0) {
        categories.attributes[[col]] <-
          append(categories.attributes[[col]], rep(NA, times))
      }
      categories[[col]] <- categories.attributes[[col]]
    }
    list(variables = variables, categories = categories)
  }
}

#' @keywords internal
.formatDate <- function(date) {
  if (inherits(date, "POSIXt") || inherits(date, "Date"))
    format(date, "%Y-%m-%d %H:%M")
  else
    date # let it go?
}
