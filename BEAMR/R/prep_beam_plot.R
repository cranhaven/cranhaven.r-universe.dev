#' Prepare for BEAM plotting
#'
#' Add a "plot" column to beam.specs, which includes string of plot commands.
#'
#' @param beam.data Result of prep.beam.data
#' @param beam.specs A data.frame of strings with columns name, mtx, mdl (string with R model with mtx.row)
#'
#' @returns An updated beam.specs object that includes the column "plot"
#' @export
#'
#' @examples
#' data(clinf)
#' data(omicdat)
#' data(omicann)
#' data(setdat)
#' test.beam.data <- prep_beam_data(main.data=clinf, mtx.data=omicdat,
#'                                  mtx.anns=omicann, set.data=setdat,
#'                                  set.anns=NULL, n.boot=10, seed=123)
#' specs <- prep_beam_specs(beam.data=test.beam.data, endpts=c("MRD29", "EFS", "OS"),
#'                          firth=TRUE)
#' plot.specs <- prep_beam_plot(beam.data=test.beam.data, beam.specs=specs)
prep_beam_plot <- function(beam.data, beam.specs){
  # check that beam.data is a beam.data object
  if(!inherits(beam.data, "beam.data"))
    stop("beam.data must be a beam.data object from prep_beam_data")
  # check beam.specs colnames are correct
  if(!all.equal(colnames(beam.specs), c("name", "mtx", "mdl")))
    stop("beam.specs must be the output from the prep_beam_specs function")
  if(!inherits(beam.specs, "data.frame"))
    stop("beam.specs must be the output from the prep_beam_specs function")
  # Extract main.data (clinical data)
  mainData <- beam.data$main.data
  plot.vec <- c()
  for(i in 1:nrow(beam.specs)){
    temp.ep.name <- sub(".*\\.", "", beam.specs$name[i])
    omic <- beam.specs$mtx[i]
    if(!(temp.ep.name %in% colnames(mainData)))
      stop(paste0("Endpoint ", temp.ep.name, " not found in columns of beam.data$main.data"))
    temp.ep.vec <- mainData[,which(colnames(mainData)==temp.ep.name)]
    temp.ep.type <- class(temp.ep.vec)
    omic.vec <- beam.data$mtx.data[[omic]][1,]
    omic.type <- ifelse(length(unique(omic.vec))<=4, "factor", class(omic.vec))
    if(inherits(temp.ep.vec, "Surv")){
      if(omic.type=="factor"){
        plot.vec[i] <- paste0("survminer::ggsurvplot(survfit(", temp.ep.name, "~mtx.row.fac, data=main.data), data=main.data, legend='right', legend.title='",
                              omic,"', legend.labs=c(paste0(levels(main.data$mtx.row.fac))), ylab='",
                              temp.ep.name," Probability', font.x=12, font.y=12)")

      } # if omic type factor
      else{
        # continuous omic - dichotomize at median
        plot.vec[i] <- paste0("survminer::ggsurvplot(survfit(", temp.ep.name, "~(mtx.row>median(mtx.row, na.rm=TRUE)), data=main.data), data=main.data, legend='right', legend.title='",
                              omic,"', legend.labs=c('High', 'Low'), ylab='",
                              temp.ep.name," Probability', font.x=12, font.y=12)")
      } # else omic type numeric
    } # if surv
    else if(inherits(temp.ep.vec, "numeric")|inherits(temp.ep.vec, "integer")){
      uni.len <- length(unique(temp.ep.vec))
      if(uni.len<=4){
        if(omic.type=="factor"){
          # factor factor make mosaic plot - remove na
          plot.vec[i] <- paste0("ggplot2::ggplot(data=main.data)+ggmosiac::geom_mosaic(aes(x=product(", temp.ep.name, ",mtx.row)), na.rm=TRUE) + ggmosaic::geom_mosaic_text(aes(x=product(",
                                temp.ep.name,", mtx.row), label=after_stat(.wt)), na.rm=TRUE, color='white') + ggplot2::xlab('",
                                omic,"')+ggplot2::ylab('", temp.ep.name,"')+ggplot2::theme(legend.position='none')")
        }# end omic type factor
        else{
          # factor endpoint with continuous omic make boxplot
          plot.vec[i] <- paste0("ggplot2::ggplot(data=main.data)+ggplot2::geom_boxplot(aes(x=as.factor(",
                                temp.ep.name, "), y=mtx.row)) + ggplot2::ylab('", omic,"')+ggplot2::xlab('",
                                temp.ep.name,"')")
        } # end else omic not type factor
      } # end if numeric has uni.len<=4
      else{
        if(omic.type=="factor"){
          # continuous endpoint, factor omic, make boxplot
          plot.vec[i] <- paste0("ggplot2::ggplot(data=main.data)+ggplot2::geom_boxplot(aes(x=mtx.row.fac, y=",
                                temp.ep.name,")) + ggplot2::ylab('", temp.ep.name,"')+ggplot2::xlab('",omic,"')")
        } # end if omic type factor
        else{
          # continuous endpoint, continuous omic, make scatterplot
          plot.vec[i] <- paste0("ggplot2::ggplot(data=main.data)+ggplot2::geom_point(aes(x=mtx.row, y=",
                                temp.ep.name,")) + ggplot2::ylab('", temp.ep.name,"')+ggplot2::xlab('",omic,"')")

        } # end else omic type not factor
      } # end else numeric truly continuous
    } # end else if numeric
    else if (inherits(temp.ep.vec, "factor")){
      if(omic.type=="factor"){
        plot.vec[i] <- paste0("ggplot2::ggplot(data=main.data)+ggmosaic::geom_mosaic(aes(x=product(", temp.ep.name, ",mtx.row)), na.rm=TRUE) + ggmosaic::geom_mosaic_text(aes(x=product(",
                              temp.ep.name,", mtx.row), label=after_stat(.wt)), na.rm=TRUE, color='white') + ggplot2::xlab('",
                              omic,"')+ggplot2::ylab('", temp.ep.name,"')+ggplot2::theme(legend.position='none')")
      }# end if omic type factor
      else{
        plot.vec[i] <- paste0("ggplot2::ggplot(data=main.data)+ggplot2::geom_boxplot(aes(x=as.factor(",
                              temp.ep.name, "), y=mtx.row)) + ggplot2::ylab('", omic,"')+ggplot2::xlab('",
                              temp.ep.name,"')")
      } # end else not factor
    } # end else if factor
    else{
      stop(paste0(temp.ep.name, " is not a recognized variable type. Please create plot input as a column in beam.specs."))
    }
  } # for length beam.specs
  beam.specs$plot <- plot.vec
  return(beam.specs)
} # end function


