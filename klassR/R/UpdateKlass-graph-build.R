#' Build a directed graph of code changes based on a Klass classification
#'
#' @param classification The ID of the desired classification.
#'
#' @param date The date which the edges of the graph should be directed towards.
#'
#'   Defaults to the current year plus one, which ensures the graph is directed
#'   to the most recent codes.
#'
#' @return An \code{igraph} object with the vertexes representing codes, and
#'   edges representing changes between codes. The direction of the edges
#'   represent changes towards the date specified in \code{date}.
#'
#' @export
#'
#' @examples
#' library(klassR)
#'
#' # Build a graph directed towards the most recent codes
#' \dontrun{
#' klass_131 <- klass_graph(131)
#' }
#'
#' # Build a graph directed towards valid codes in 2020.
#' \dontrun{
#' klass_131_2020 <- klass_graph(131, "2020-01-01")
#' }
#'
klass_graph <- function(classification, date = NULL) {
  if (is.null(classification)) stop("Please provide a classification ID.")

  ## Downloading codes and code changes

  changes_url <- paste0(
    "https://data.ssb.no/api/klass/v1/classifications/",
    classification, "/changes?from=0001-01-01"
  )

  api_endringer <- jsonlite::fromJSON(GetUrl2(changes_url),
    flatten = TRUE
  )[["codeChanges"]]

  codes_url <- paste0(
    "https://data.ssb.no/api/klass/v1/classifications/",
    classification, "/codes?from=0001-01-01"
  )

  api_alle <- jsonlite::fromJSON(GetUrl2(codes_url),
    flatten = TRUE
  )[["codes"]]


  ## Calculating code variants and changes. A code may have more variants than
  ## are present in `api_alle`, since combinations of codes into an already
  ## existing code do not generate a new entry in this table. See `find_dates`.

  variants <-
    do.call(rbind, lapply(unique(api_alle$code),
      find_dates,
      api_alle = api_alle,
      api_endringer = api_endringer
    ))

  variants$validFrom <- as.Date(variants$validFrom)
  variants$validTo <- as.Date(variants$validTo)

  variants <- variants[, c(
    "code", "name", "variant",
    "validFrom", "validTo"
  )]

  changes <- api_endringer[, c("oldCode", "changeOccurred", "newCode")]

  changes$variantFrom <- mapply(
    FUN = find_variant_from,
    x = changes$oldCode,
    changeOccurred = changes$changeOccurred,
    MoreArgs = list(variants = variants),
    SIMPLIFY = TRUE
  )

  changes$variantTo <- mapply(
    FUN = find_variant_to,
    x = changes$newCode,
    changeOccurred = changes$changeOccurred,
    MoreArgs = list(variants = variants),
    SIMPLIFY = TRUE
  )

  ## `variant_changes` represents additional changes that are inferred from code
  ## variants where the `validTo` and `validFrom` values are "adjacent", e.g.
  ## variant 1 having a `validTo` of "1964-01-01" and variant 2 having a
  ## `validFrom` of "1964-01-01". In these cases we construct a change between
  ## the two variants on the assumption that the variants are connected.

  variant_changes <-
    do.call(rbind, lapply(split(variants, variants$code), function(df) {
      # We check if each variant has a `validTo` that matches the next variant's
      # `validFrom`. The last variant always gets `FALSE`, since there is no
      # next variant to check against.
      df$adjacent <- c(df$validTo[-nrow(df)] == df$validFrom[-1], FALSE)

      # We keep rows where `validTo` is equal to a `validFrom` value in another
      # row, or where `validTo` is either the highest value or NA. We wrap this
      # call in suppressWarnings to silence warnings that are due to all values
      # of `validTo` being NA. This is usually the case when there's only one
      # variant of a code.
      suppressWarnings({
        df <- df[df$adjacent |
          df$validTo == max(df$validTo, na.rm = TRUE) |
          is.na(df$validTo), ]
      })

      # Preparing the changes table. These columns represent the "from" side of
      # the change, so we simply copy the variables already present.
      df$oldCode <- df$code
      df$newCode <- df$code
      df$variantFrom <- df$variant
      df$changeOccurred <- as.character(df$validTo)

      # Constructing the "to" side of the change. We set `variantTo` to the next
      # `variant` if `validTo` matches that variant's `validFrom`. This ensures
      # that we don't connect two variants that aren't adjacent in time, e.g. if
      # a code has been discontinued and then reused after a period of time.
      df$variantTo <- ifelse(df$adjacent, df$variant + 1, NA)

      # We exclude rows where variantTo is NA, since these aren't changes. This
      # also excludes rows representing the last variant of a code (see
      # `max_validTo`). We also only keep the newly constructed columns.
      df <- df[!is.na(df$variantTo), c(
        "oldCode", "changeOccurred", "newCode",
        "variantFrom", "variantTo"
      )]

      return(df)
    }))

  # The final changes table is the combination of changes identified earlier and
  # the changes inferred from evaluating the dates in the variant table. We wrap
  # the call in `unique` in order to remove duplicates, since some of the
  # inferred changes are present in `changes`.
  all_changes <- unique(rbind(changes, variant_changes))

  ## Calculating vertices and edges.

  klass_vertices <- variants

  klass_vertices$vertex <- as.character(1:nrow(klass_vertices))

  all_changes$changeOccurred <- as.Date(all_changes$changeOccurred)

  klass_edges <-
    merge(all_changes,
      stats::setNames(
        klass_vertices[, c("code", "variant", "vertex")],
        c("oldCode", "variantFrom", "vertexFrom")
      ),
      by = c("oldCode", "variantFrom"),
      all.x = TRUE
    )

  klass_edges <-
    merge(klass_edges,
      stats::setNames(
        klass_vertices[, c("code", "variant", "vertex")],
        c("newCode", "variantTo", "vertexTo")
      ),
      by = c("newCode", "variantTo"),
      all.x = TRUE
    )

  ## Building graph

  graph <-
    igraph::graph_from_edgelist(
      as.matrix(
        klass_edges[, c("vertexFrom", "vertexTo")]
      )
    )

  graph <- igraph::set_edge_attr(
    graph = graph,
    name = "changeOccurred",
    value = klass_edges$changeOccurred
  )

  no_edges <- klass_vertices[!klass_vertices$vertex %in% igraph::V(graph)$name, ]

  graph <- igraph::add_vertices(
    graph = graph,
    nv = nrow(no_edges),
    attr = list(name = no_edges$vertex)
  )

  # Redirecting edges only if date is not specified.
  # Removes the need to check dates in update_klass_node(), since we can simply follow
  # outgoing edges to reach the code valid at `date`.

  if (!is.null(date)) {
    graph <-
      igraph::reverse_edges(
        graph = graph,
        eids = igraph::E(graph)[igraph::E(graph)$changeOccurred > date]
      )
  }

  klass_vertices <-
    klass_vertices[match(igraph::V(graph)$name, klass_vertices$vertex), ]

  # Building attributes table and applying attributes to vertices. We later
  # extract these attributes when we traverse the graph in update_klass_node()

  attributes <- klass_vertices
  attributes$vertex <- NULL

  names(attributes)[names(attributes) == "name"] <- "label"

  for (i in seq_along(attributes)) {
    graph <-
      igraph::set_vertex_attr(
        graph = graph,
        name = names(attributes)[i],
        value = attributes[, i]
      )
  }

  return(graph)
}

#' Find the variant of a code corresponding to a change *from* a specific code.
#'
#' @param x The code that is being changed
#' @param changeOccurred The date the change occurred
#' @param variants The variants lookup-table.
#'
#' @return The variant corresponding to the code \code{x} at date
#' \code{changeOccurred}.
#'
#' @seealso [find_variant_to]
#'
#' @keywords internal
#'
find_variant_from <- function(x, changeOccurred, variants) {
  variants[variants$code == x &
    changeOccurred > variants$validFrom &
    (changeOccurred <= variants$validTo | is.na(variants$validTo)), ]$variant
}

#' Find the variant of a code corresponding to a change *to* a specific code.
#'
#' @inheritParams find_variant_from
#'
#' @inherit find_variant_from return
#'
#' @seealso [find_variant_from()]
#'
#' @keywords internal
#'
find_variant_to <- function(x, changeOccurred, variants) {
  variants[variants$code == x &
    changeOccurred >= variants$validFrom &
    (changeOccurred < variants$validTo | is.na(variants$validTo)), ]$variant
}


#' For a given Klass code, produce a table of dates describing the valid-from
#' and valid-to dates of all versions of the code
#'
#' @param code A Klass code
#' @param api_alle A table of all codes in the classification. See example.
#' @param api_endringer A table of all changes in the classification See
#'   example.
#'
#' @return A \code{data.frame} with number of rows equal to the number of
#'   variants of the combination of code and name, determined by the changes the
#'   code has been involved in. The \code{data.frame} has two columns:
#'
#'   \itemize{
#'    \item{"validFrom"}
#'    \item{"validTo"}
#'   }
#'
#' @keywords internal
#'
find_dates <- function(code, api_alle, api_endringer) {
  dates_df <- api_alle[api_alle$code == code, ]

  dates_df <- dates_df[c(
    "name",
    "validFromInRequestedRange",
    "validToInRequestedRange"
  )]

  names(dates_df)[2:3] <- c("validFrom", "validTo")

  dates_changed <- api_endringer[
    (api_endringer$oldCode == code) | (api_endringer$newCode == code),
  ][["changeOccurred"]]

  if (all(dates_changed %in% unlist(dates_df[c("validFrom", "validTo")]))) {
    dates_df <- dates_df[
      order(dates_df$validFrom),
      c("name", "validFrom", "validTo")
    ]

    dates_df$variant <- 1:nrow(dates_df)
    dates_df$code <- code

    return(dates_df)
  } else {
    dates_df <- dates_df[, c("validFrom", "validTo")]

    # `api_alle` does not give information on codes combining with already
    # existing codes. We use `api_endringer` to expand the dates table to
    # include periods based on when a code has been either combined with
    # another code or a code has split into multiple codes.

    for (date in c(dates_df$validFrom, dates_df$validTo, dates_changed)) {
      if (date %in% c(dates_df$validFrom, dates_df$validTo)) {
        next()
      } else {
        dates_df$rn <- 1:nrow(dates_df)

        row_to_edit <-
          dates_df[date >= dates_df$validFrom &
            (date < dates_df$validTo | is.na(dates_df$validTo)), ]

        if (nrow(row_to_edit) > 1) stop("Multiple changes matched!")

        old_row <- row_to_edit
        old_row$validTo <- date

        new_row <- row_to_edit
        new_row$validFrom <- date

        dates_df <-
          rbind(
            dates_df[
              !dates_df$rn %in% row_to_edit$rn,
              c("validFrom", "validTo")
            ],
            old_row[, c("validFrom", "validTo")],
            new_row[, c("validFrom", "validTo")]
          )
      }
    }

    dates_df <- dates_df[
      order(dates_df$validFrom),
      c("validFrom", "validTo")
    ]

    dates_df$variant <- 1:nrow(dates_df)

    dates_df$name <-
      mapply(find_name,
        validFrom = dates_df$validFrom,
        validTo = dates_df$validTo,
        MoreArgs = list(
          code = code,
          api_alle = api_alle
        ),
        SIMPLIFY = TRUE
      )

    dates_df$code <- code

    return(dates_df[c("code", "name", "variant", "validFrom", "validTo")])
  }
}

#' Find the name of a code valid at a specific date.
#'
#' @inheritParams find_dates
#'
#' @param validFrom The date the code is valid from, in \code{YYYY-MM-DD} format.
#'
#' @param validTo The date the code is valid to, in \code{YYYY-MM-DD} format.
#'
#' @return The name of the code.
#'
#' @keywords internal
#'
find_name <- function(code, validFrom, validTo, api_alle) {
  koder <- api_alle[api_alle$code == code, ]

  if (is.na(validTo)) {
    name <- koder[is.na(koder$validToInRequestedRange), ]$name
  } else {
    name <- koder[
      validFrom >= koder$validFromInRequestedRange &
        (validTo <= koder$validToInRequestedRange |
          is.na(koder$validToInRequestedRange)),
    ][["name"]]
  }

  if (length(name) == 0) {
    return(NA)
  } else {
    return(name)
  }
}
