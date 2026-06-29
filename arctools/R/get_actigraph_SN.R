#' Get Actigraph device serial number
#'
#' Read Actigraph device serial number from ActiLife accelerometry data
#' file.
#'
#' @param fpath_full A string scalar. An absolute path to ActiLife accelerometry
#' data file.
#'
#' @return String scalar. Actigraph device serial number.
#' @export
#'
#' @examples
#' fpath_full_i <- system.file("extdata", extdata_fnames[1], package = "arctools")
#' get_actigraph_SN(fpath_full_i)
#'
get_actigraph_SN = function(fpath_full){

  f_conn_obj <- file(fpath_full)
  f_lines <- readLines(f_conn_obj, n = 2)
  close(f_conn_obj)

  sn_out <- strsplit(x = f_lines[2], split = " ")[[1]][3]

  return(sn_out)
}
