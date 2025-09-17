#' Find the column of mtch.data with the most rows containing an element of ids
#'
#' @param mtch.data A data.frame
#' @param ids A vector of row ids to match
#'
#' @returns A vector of column names with the most matches.
#' @export
#'
#' @examples
#' data(omicann)
#' data(omicdat)
#' lsn.data <- omicann[[1]]
#' mtx.rows <- rownames(omicdat[[1]])
#' test <- find_id_clm(lsn.data,mtx.rows)
find_id_clm=function(mtch.data,ids)
{
  mtch.data=cbind.data.frame(row.names=rownames(mtch.data),
                             mtch.data)

  mtch.mtx=apply(mtch.data,2,is.element,ids) # check each column of mtch.data for matches
  n.mtch=colSums(matrix(mtch.mtx))                   # compute number of matches
  mtch.clm=which.max(n.mtch)                 # find column with most matches
  mtch.clm=colnames(mtch.data)[mtch.clm]
  return(mtch.clm)
}
