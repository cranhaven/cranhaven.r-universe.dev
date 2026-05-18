#' Read multiple files exported from Thermec Master-Z tester
#'
#' @description Read data from multiple files with structurized file names, then generate
#' a summary table. It will also be available for the files from other tester apparatus by
#' correct setting.
#' @param Cdl An handmade double list to determine selected conditions.
#' @param wd Work directory.
#' @param ftype File type to be read. Default value is ".csv".
#' @param Straincln An integer to specify column for Strain in your data. Default value
#' is 7 means the 7th column contains strain data, in the files exported from Thermec
#' Master-Z tester.
#' @param Stresscln An integer to specify column for Strain in your data. Default value
#' is 8 means the 8th column contains stress data, in the files exported from Thermec
#' Master-Z tester.
#' @param startrow An integer to ignore the prefix rows for testing conditions. Default
#' value is 29.
#'
#' @import VBTree utils stats
#' @importFrom methods as
#' @return A matrix-like summary table for all input files.
#' @export API4TMZ
#'
#' @examples
#' variable1 <- c("factor11", "factor12", "factor13")
#' variable2 <- c("factor21", "factor22")
#' variable3 <- c("factor31", "factor32", "factor33", "factor34")
#' conditions <- list(variable1, variable2, variable3)
#'
#' \dontrun{
#' SummaryTable <- API4TMZ(conditions, "/Your_Data_Directory/")
#' }
#' @keywords APIfunction
API4TMZ <- function(Cdl, wd, ftype=".csv", Straincln=7, Stresscln=8, startrow=29){

  calc_len <- function(data){
    result<-ifelse(is.null(nrow(data)),length(data),nrow(data))
    return(result)
  }

  vert <- function(object){
    if(is.list(object)){
      object<-cbind(object)
    }
    return(object)
  }

  cache <- function(x,length.out=calc_len(x),fill=NULL,preserveClass=TRUE)
  {
    xclass<-class(x)
    input<-lapply(vert(x),unlist)
    results<-as.data.frame(lapply(input,rep,length.out=length.out))
    if(length.out> calc_len(x) && !is.null(fill))
    {
      results<-t(results)
      results[(length(unlist(x))+1):length(unlist(results))]<-fill
      results<-t(results)
    }
    if(preserveClass)
      results<-as2(results,xclass)
    return(results)
  }

  as2 <- function(object,class)
  {
    object<-as.matrix(object)
    if(class=='factor')
      return(as.factor(as.character(object)))
    if(class=='data.frame')
      return(as.data.frame(object))
    else
      return(as(object,class))
  }


  if(all(is.character(unlist(Cdl)))==F){
    stop("input list must double list.", call. = FALSE)
  } else{
    # Cdl: a double list for conditions
    # wd: the working directory, defined by user

    # make names
    name <- trvs(dl2vbt(Cdl))
    name <- unlist(name[,1])
    len_name <- length(name)

    # initialize temp data
    temp_data <- read.csv(paste(wd, name[1], ftype, sep = ""), header = F)
    temp_data <- as.matrix(temp_data[startrow:dim(temp_data)[1],])
    temp_data <- temp_data[which(temp_data[,2]==" 1"), c(Straincln, Stresscln)]
    data <- temp_data

    arbi_combine <-function(..., fill=NULL)
    {
      entry <-list(...)
      entry<-lapply(entry, vert)
      maxlength<-max(unlist(lapply(entry, calc_len)))
      bufferedInputs<-lapply(entry, cache, length.out=maxlength, fill, preserveClass=FALSE)
      return(Reduce(cbind.data.frame, bufferedInputs))
    }

    # make data summary table
    clnnames <- c()
    i <- 1
    for(i in 1:len_name){
      temp_data <- read.csv(paste(wd, name[i],".csv", sep = ""), header = F)
      temp_data <- as.matrix(temp_data[startrow:dim(temp_data)[1],])
      temp_data <- temp_data[which(temp_data[,2]==" 1"), c(Straincln, Stresscln)]
      temp_data <- apply(temp_data, 2, as.numeric)
      temp_clnnames <- c(paste("Strain", name[i], sep = "-"), paste("Stress", name[i], sep = "-"))
      clnnames <- append(clnnames, temp_clnnames)
      data <- arbi_combine(data, temp_data)
    }
    data <- data[,-c(1:2)] # Delete initial temp data
    colnames(data) <- clnnames
    data <- data[complete.cases(data),]
    data <- apply(data, 2, as.numeric)
  }
  return(data)
}

#' Read multiple files exported from Thermec Master-Z tester
#'
#' @description Read data from multiple files with structurized file names, then generate
#' a summary data frame. It will also be available for the files from other tester apparatus by
#' correct setting.
#' @param makeidx A boolean value to control the index column, inserted in the first column.
#' Default setting is FALSE.
#' @param ... Arguments to be passed to \code{\link[TPMplt:API4TMZ]{API4TMZ}}.
#'
#' @return A summary data frame for all input files.
#' @export TMZdatainput
#'
#' @examples
#' variable1 <- c("factor11", "factor12", "factor13")
#' variable2 <- c("factor21", "factor22")
#' variable3 <- c("factor31", "factor32", "factor33", "factor34")
#' conditions <- list(variable1, variable2, variable3)
#'
#' \dontrun{
#' SummaryTable <- TMZdatainput(Cdl=conditions, wd="/Your_Data_Directory/")
#' }
#' @keywords APIfunction
TMZdatainput <- function(makeidx=FALSE, ...){
  data <- API4TMZ(...)
  title <- colnames(data)

  result <- data.frame(data)

  colnames(result)[-1] <- title
  colnames(result)[1] <- "idx"
  result <- as.data.frame(result)
  if(makeidx==FALSE){
    result <- result[,-1]
  }
  return(result)
}
