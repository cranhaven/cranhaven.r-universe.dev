#' Print summary information about a beam.data object
#'
#' @param x An object of class "beam.data"
#' @param ... Other arguments passed to or from other methods
#'
#' @returns Messages about the beam.data object
#' @importFrom utils head
#' @export
#'
#' @examples
#' data(beam_dat)
#' print(beam_dat)
print.beam.data=function(x,...)

{
  beam.data <- unclass(x)
  beam.data <- as.list(beam.data)
  cat(paste0("main.data: ",
             nrow(beam.data$main.data)," rows and ",
             ncol(beam.data$main.data)," columns. \n \n"))

  print(beam.data$main.data[1:min(5, nrow(beam.data$main.data)), 1:min(5, ncol(beam.data$main.data))])

  n.mtx=length(beam.data$mtx.data)

  cat(paste0("\nmtx.data: \n"))

  mtx.names=names(beam.data$mtx.data)
  for (i in 1:n.mtx)
  {
    cat(paste0("  mtx.data ",mtx.names[i],": ",
               ncol(beam.data$mtx.data[[i]])," columns linked to ",
               sum(!is.na(beam.data$main.data[,paste0(mtx.names[i],".clm")])),
               " rows of main.data. \n"))
  }
  for(i in 1:n.mtx){
    cat(paste0("\n", mtx.names[i],": \n"))
    print(beam.data$mtx.data[[i]][1:min(5, nrow(beam.data$mtx.data[[i]])),1:min(5, ncol(beam.data$mtx.data[[i]]))])
  }

  cat(paste0("\n","mtx.anns: \n"))
  n.anns=length(beam.data$mtx.anns)
  if (n.anns>0)
  {
    ann.names=names(beam.data$mtx.anns)
    for (i in 1:n.anns)
    {
      cat(paste0("  ",ann.names[i],": ",
                 nrow(beam.data$mtx.anns[[i]])," rows and ",
                 ncol(beam.data$mtx.anns[[i]])," columns. \n"))
    }
    for(i in 1:n.anns){
      cat(paste0("\n", ann.names[i],": \n"))
      print(beam.data$mtx.anns[[i]][1:min(5, nrow(beam.data$mtx.anns[[i]])),1:min(ncol(beam.data$mtx.anns[[i]]), 5)])
    }
  }

  cat(paste0("\n","anns.mtch: \n"))
  print(beam.data$anns.mtch)

  cat(paste0("\n","set.data: ",
             nrow(beam.data$set.data)," rows assigning sets to data.mtx rows. \n"))

  print(head(beam.data$set.data))


  cat(paste0("\n \n","set.anns: ",
             nrow(beam.data$set.anns)," rows of set annotations."))

  cat(paste0("\n \n","boot.index: ",
             nrow(beam.data$boot.index)," rows and ",
             ncol(beam.data$boot.index)," columns of bootstrap indices. \n"))
  print(beam.data$boot.index[1:min(5, nrow(beam.data$boot.index)), 1:min(5, ncol(beam.data$boot.index))])

}
