#' Visualising individual response patterns
#'
#' @param data A dataframe with the dataset containing
#' the responses.
#' @param row A vector with indices of the rows for which
#' you want the individual response patterns. These can be
#' either the relevant row numbers, or if character row
#' names are set, the names ot the rleevant rows.
#' @param columns A vector with the names of the variables
#' you want the individual response patterns for.
#' @param dataName,title Optionally, you can override the
#' dataset name that is used in the title; or, the title
#' (the dataset name is only used in the title).
#'
#' @return A [ggplot2::ggplot()].
#' @export
#'
#' @examples ### Get a dataset
#' dat <- ufs::bfi;
#'
#' ### Show the individual responses for
#' ### the tenth participant
#' irpplot(dat, 10, 1:20);
#'
#' ### Set some missing values
#' dat[10, c(1, 5, 15)] <- NA;
#'
#' ### Show the individual responses again
#' irpplot(dat, 10, 1:20);
irpplot <- function(data,
                    row,
                    columns,
                    dataName = NULL,
                    title = paste("Row", row, "in dataset", dataName)) {
  dataName <-
    ifelse (is.null(dataName),
            deparse(substitute(data)),
            dataName);
  colNames <- names(data[, columns]);
  rowData <-
    as.numeric(data[row, colNames]);
  missingRows <- seq_along(colNames)[is.na(rowData)];
  res <-
    ggplot2::ggplot(data=data.frame(x=seq_along(colNames),
                                    y=rowData),
                    mapping=ggplot2::aes_string(x='x', y='y'));

  for (i in missingRows) {
    res <- res +
      ggplot2::geom_segment(x = -1*i, xend = -1*i,
                            y = min(rowData, na.rm=TRUE),
                            yend = max(rowData, na.rm=TRUE),
                            color="red",
                            linetype="dashed");
  }

  res <- res +
    ggplot2::geom_point(na.rm=TRUE) +
    ggplot2::geom_line(na.rm=TRUE) +
    ggplot2::scale_x_continuous(trans = "reverse",
                                breaks = seq_along(colNames),
                                labels = colNames) +
    ggplot2::scale_y_continuous(breaks=seq(from = min(rowData, na.rm=TRUE),
                                           to = max(rowData, na.rm=TRUE))) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank()) +
    ggplot2::labs(title = title,
                  x = NULL,
                  y = NULL) +
    ggplot2::coord_flip();

  return(res);
}
