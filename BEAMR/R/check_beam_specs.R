#' Check that beam.specs satisfies all necessary conditions
#'
#' @param beam.specs A data.frame with column name, mtx, and mdl
#' @param mtx.names A vector with the names of the data matrices (beam.data$mtx.data)
#'
#' @returns A data.frame of beam.specs if all conditions satisfied, otherwise throws an error
#' @export
#'
#' @examples
#' data(beam_dat)
#' data(beam_specs)
#' test_specs <- check_beam_specs(beam_specs, names(beam_dat$mtx.data))
check_beam_specs=function(beam.specs,mtx.names)
{
  if (any(!is.element(beam.specs[,"mtx"],mtx.names)))
    stop("Each mtx in beam.specs must be in names(beam.data$mtx.data).")

  mtx.row=grepl("mtx.row",beam.specs[,"mdl"],fixed=TRUE)
  if (any(!mtx.row))
    stop("Each model string must include mtx.row in the formula.")

  md.row=grepl("data=main.data",beam.specs[,"mdl"])
  if (any(!md.row))
    stop("Each model string must include data=main.data.")

  if (!is.element("name",colnames(beam.specs)))
  {
    name=paste0(beam.specs[,"mtx"],".",
                beam.specs[,"mdl"])
    beam.specs=cbind(beam.specs,
                     name=name)
  }

  return(beam.specs)
}
