##
#' @title Compute summary statistic for groupings of units using given method (typically, mean)
#'
#' @description Computes summary statistics for groupings (given as vector) of units in ena data using given method (typically, mean); computes summary statistic for point locations and edge weights for each grouping
#'
#' @export
#'
#' @param enaset An \code{\link{ENAset}} or a vector of values to group.
#' @param by A vector of values the same length as units. Uses rotated points for group positions and normed data to get the group edge weights
#' @param method A function that is used on grouped points. Default: mean().  If `enaset` is an ENAset, enaset$points.rotated will be groups using `mean` regardless of `method` provided
#' @param names A vector of names to use for the results. Default: unique(by)
#'
#' @examples
#' data(RS.data)
#'
#' codeNames = c('Data','Technical.Constraints','Performance.Parameters',
#'   'Client.and.Consultant.Requests','Design.Reasoning','Collaboration');
#'
#' accum = ena.accumulate.data(
#'   units = RS.data[,c("UserName","Condition")],
#'   conversation = RS.data[,c("Condition","GroupName")],
#'   metadata = RS.data[,c("CONFIDENCE.Change","CONFIDENCE.Pre","CONFIDENCE.Post")],
#'   codes = RS.data[,codeNames],
#'   window.size.back = 4
#' )
#'
#' set = ena.make.set(
#'   enadata = accum
#' )
#'
#' means = ena.group(set, "Condition")
#'
#'
#' @return A list containing names, points, and edge weights for each of the unique groups formed by the function
##
ena.group <- function(
  enaset = NULL,
  by = NULL,
  method = mean,
  names = as.vector(unique(by))
) {
  run.method = function(pts, m = method) {
    to_matrix <- class(pts)[1];
    points.dt = pts;

    if(is.logical(by)) {
      points.dt.means = points.dt[by, { lapply(.SD, m) }, .SDcols = find_dimension_cols(points.dt) | find_code_cols(points.dt)];
      if(length(names) == 1) {
        points.dt.means[['ENA_GROUP_NAME']] <- as.ena.metadata(names)
      }
    }
    else if(all(by %in% colnames(pts))) {
      points.dt.means <- points.dt[,
                          {lapply(.SD, function(x) {
                            get(paste0("as.", class(x)[1]))(m(x))
                          })},
                          by = by,
                          .SDcols = find_dimension_cols(points.dt) | find_code_cols(points.dt)
                        ];
      points.dt.means[, ENA_GROUP_NAME := do.call(paste, c(.SD, sep = ".")) , .SDcols = c(by)]
      points.dt.means <- points.dt.means[, !find_meta_cols(points.dt.means), with = F]
      set(points.dt.means, j = "ENA_GROUP_NAME", value = as.ena.metadata(points.dt.means[["ENA_GROUP_NAME"]]))
    }
    else {
      to_what <- get(paste0("as.", class(pts[[which(!find_meta_cols(pts))[1]]])[1]))
      to_cols <- names(which(!find_meta_cols(pts)))

      points.dt.means = as.data.frame(aggregate(as.matrix(points.dt), by = list(by), FUN = m)) #"mean"))
      set(points.dt.means, j = "Group.1", value = as.ena.metadata(points.dt.means$Group.1))
      colnames(points.dt.means)[colnames(points.dt.means) == "Group.1"] <- "ENA_GROUP_NAME"
      set(x = points.dt.means, j = to_cols, value = lapply(points.dt.means[, to_cols], to_what))
      points.dt.means <- as.data.table(points.dt.means)

      # agg.df[as.vector(unique(group.by)),]u
      # return (points.dt.means[as.vector(unique(by)),]);
      return(as.ena.matrix(points.dt.means[which(points.dt.means$ENA_GROUP_NAME %in% unique(by)),], to_matrix))
    }

    return(as.ena.matrix(points.dt.means, to_matrix));
  }

  if(is.character(method)) {
    method = get(method)
  }

  if(is(enaset, "ENAset")) {
    enaset <- ena.set(enaset);
  }

  if (is(enaset, "ena.set")) {
    pts <- run.method(enaset$points)
    return(list(
      "names" = pts$ENA_GROUP_NAME,
      "points" = pts,
      "line.weights" = run.method(enaset$line.weights)
    ));
  }
  else {
    return(run.method(enaset))
  }
}
