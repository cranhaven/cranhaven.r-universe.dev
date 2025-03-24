#' Enter or edit rating data
#' 
#' Enter or edit rating data. Function enterratdata creates a dataframe in a
#' format that can be processed by SSrat functions \code{\link{calcgroup}} and
#' \code{\link{calcallgroups}}.
#' 
#' The SSrat conformant dataframe has repondent identifiers and ratings
#' received. The respondent identifiers are schoolid, groupid, respid en
#' optionally respname. The ratings received are named \code{r01, r02 .. rn},
#' where n is the last rated, with a maximum of r99. Multiple groups can be
#' entered. It is not possible (or sensible) to add other variables.
#' 
#' @param dataframe Ssrat conformant dataframe. If NULL a new dataframe is
#' created (default). Otherwise, the dataframe is opened for editing.
#' @param resplabel Boolean, default is FALSE. When TRUE, a column resplabel is
#' added as the first column of the dataframe. If dataframe already has a
#' column resplabel, this id preserved.
#' @return A data.frame with columns: \item{resplabel}{An identification label
#' of the rater (and of the assessed). Optional.} \item{schoolid
#' }{Identification number of the secondary group level, most often a school.}
#' \item{groupid }{Identification number of the first group level, most often a
#' school class.} \item{respid }{Identification number of the rater (and of the
#' assessed).} \item{r01..r0n }{Ratings received by respid = 1 .. respid = n,
#' with a maximum ofr99. Respid's < 10 are padded with a zero.}
#' @note To allow for the combination of groups with different sizes in a
#' single file, it is important to enumerate the n respondents from 1 to n,
#' respectively use the columnnames r01 to rn for the received ratings. These
#' column names are padded with a zero (r01, r02 etc.) to allow for easy
#' ordening of these columns after a merge. Background data of each respondent
#' can be added to the output data.frame of \code{\link{calcgroup}} or
#' \code{\link{calcallgroups}}.
#' @author Hans Landsheer
#' @seealso \code{\link{calcgroup}} \code{\link{calcallgroups}}
#' \code{\link{readratdatafixed}} \code{\link{edit}}
#' @keywords datagen & Functions for generating data sets
#' @examples
#' 
#' # These examples require human intervention and are therefore placed in a
#' # don't run section. They can only be excuted interactively.
#' # create a new data frame with rating data
#' \dontrun{df=enterratdata()}
#'  
#' # edit existing data frame
#' \dontrun{df=enterratdata(df)}
#'  
#' # add respondent names to the dataframe
#' \dontrun{df=enterratdata(df, resplabel=TRUE)}
#'  
#' #create a new data frame with rating data and respondent names
#' \dontrun{df=enterratdata(resplabel=TRUE)}
#' 
#' @importFrom utils edit
#' 
#' @export
enterratdata <- function(dataframe=NULL, resplabel=F){
  if (is.null(dataframe)) {
    if (resplabel==T) {
    dataframe=data.frame(resplabel=character(), schoolid=numeric(0), groupid=numeric(0), respid=numeric(0),
                      r01=numeric(0), r02=numeric(0), r03=numeric(0), stringsAsFactors=FALSE)}
    else {dataframe=data.frame(schoolid=numeric(0), groupid=numeric(0), respid=numeric(0),
                              r01=numeric(0), r02=numeric(0), r03=numeric(0), stringsAsFactors=FALSE)}
  } else {
    if (!("resplabel" %in% colnames(dataframe)) & resplabel){
      # add column resplabel
      resplabel=rep("", nrow(dataframe))
      dataframe=data.frame(resplabel, dataframe, stringsAsFactors=FALSE)
    }
  } 
  #suppressWarnings(edit(dataframe))
  edit(dataframe)
}

# #create a new data frame with rating data
# df=enterratdata()
# 
# # edit existing data frame
# df=enterratdata(df)
# 
# # add respondent names to the dataframe
# df=enterratdata(df, resplabel=T)
# 
# #create a new data frame with rating data and respondent names
# df=enterratdata(resplabel=T)
