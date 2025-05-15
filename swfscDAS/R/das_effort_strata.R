#' Split effort by strata
#'
#' Split DAS effort where it intersects with a stratum boundary
#'
#' @param x an object of class \code{das_df},
#'   or a data frame that can be coerced to class \code{das_df}
#' @param ... ignored
#' @param strata.files list of path(s) of the stratum CSV file(s);
#'   see \code{\link{das_effort}}
#'
#' @details This function should only be called by \code{\link{das_effort}},
#'   i.e. it should not be called by users in their personal scripts.
#'   Practically speaking, this functions splits the effort line wherever it crosses a stratum line.
#'   This point of intersection is interpolated;
#'   specifically, it is determined using \code{\link[sf:geos_binary_ops]{st_intersection}}.
#'   Thus, any effort will be first split at these effort-stratum boundary intersection points,
#'   and then using the specified method (e.g. condition).
#'
#' @return The data frame x, with 1) columns added that
#'   indicate a) if the point was in a particular stratum (see \code{\link{das_intersects_strata}}), and
#'   b) the index of the stratum in \code{strata.files}
#'   (column name 'stratum'; 0 if the point intersects with no strata), and
#'   2) two rows added for each strata crossing
#'   that occurs between something other than an E and R.
#'   These rows are necessary because of how \code{das_effort} processes effort.
#'   The added rows are the same as the event previous to the strata crossing, except:
#'   \itemize{
#'     \item They have the event code "strataE" and "strataR", respectively
#'     \item Their coordinates are the coordinates of the intersection of
#'       the effort line and the stratum boundary
#'     \item Their 'idx_eff' values are plus 0.4 and 0.5, respectively
#'     \item The second added row has the same stratum info as the point
#'       immediately after the stratum boundary crossing
#'   }

das_effort_strata <- function(x, ...) UseMethod("das_effort_strata")

#' @name das_effort_strata
das_effort_strata.data.frame <- function(x, ...) {
  das_effort_strata(as_das_df(x), ...)
}

#' @name das_effort_strata
das_effort_strata.das_df <- function(x, strata.files, ...) {
  # Check that none of the strata overlap
  strata.list <- lapply(strata.files, .das_pts2poly_vertices) #duplication, oh well
  strata.int <- st_intersection(st_sf(do.call(c, strata.list)))
  # janky way of checking that intersection is not a linestring
  strata.int.class <- lapply(st_geometry(strata.int), class)
  strata.int.class.poly <- vapply(strata.int.class, function(i) {
    any(i %in% c("POLYGON", "MULTIPOLYGON"))
  }, as.logical(1))
  if (any(strata.int$n.overlaps > 1 & strata.int.class.poly))
    stop("Error: one or more stratum polygons overlap - ",
         "please ensure that the polygons do not overlap ",
         "(they can share a boundary)")


  x.strata <- das_intersects_strata(x, strata.files, strata.which = TRUE)

  ces.new <- which(x.strata$Event == "R")
  strata.new <- which(x.strata$strata_which != lag(x.strata$strata_which))

  # As necessary, add in a point with event 'strata' wherever effort changes strata
  strata.new.todo <- setdiff(strata.new, ces.new)
  if (length(strata.new.todo) > 0) {
    strata.list.poly <- lapply(strata.list, st_cast, "POLYGON") #in case any are MULTIPOLYGON
    strata.list.lines <-  lapply(strata.list.poly, st_cast, "LINESTRING")

    pts.toadd <- lapply(rev(strata.new.todo), function(i, das.df, strata.list.lines) {
      das.df.curr <- das.df %>% slice(i-1, i)
      pts.sf <- das.df.curr %>%
        mutate(Lon_sf = .data$Lon, Lat_sf = .data$Lat) %>%
        st_as_sf(coords = c("Lon_sf", "Lat_sf"), crs = 4326, agr = "constant")

      # Convert both effort and strata poly to lines
      das.line <- st_sfc(st_linestring(as.matrix(das.df.curr[, c("Lon", "Lat")])), crs = 4326)
      z <- das.df.curr$strata_which
      poly.line <- strata.list.lines[[ifelse(z[1] == 0, z[2], z[1])]]
      # poly.line <- strata.list.lines[[max(das.df.curr$strata_which)]]

      # Intersect lines, and extract coordinates from point of intersection
      das.poly.int <- suppressMessages(st_intersection(das.line, poly.line))
      if (!inherits(das.poly.int, "sfc_POINT"))
        stop("Error intersection the strata and DAS data - please report this as an issue")

      das.poly.coords <- round(unname(st_coordinates(das.poly.int)[1, ]), 4)

      # 'New' point will have same data as i-1 b/c we haven't made it to i yet
      idx.eff <- das.df.curr$idx_eff[1]
      df.out2 <- bind_cols(
        das.df.curr %>% select(.data$Event:.data$line_num) %>% slice(1),
        das.df.curr %>% select(.data$idx_eff:.data$strata_which) %>% slice(2)
      )

      das.df.curr %>%
        slice(1) %>%
        bind_rows(df.out2) %>%
        mutate(Event = c("strataE", "strataR"), Lon = das.poly.coords[1], Lat = das.poly.coords[2],
               idx_eff = c(idx.eff+0.4, idx.eff+0.5))
    }, das.df = x.strata, strata.list.lines = strata.list.lines)

    for (i.idx in seq_along(pts.toadd)) {
      x.strata <- x.strata %>%
        add_row(pts.toadd[[i.idx]], .before = rev(strata.new.todo)[i.idx])
    }

  }

  x.strata
}
