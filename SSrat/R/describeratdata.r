#' Show various features of the SSrat dataframe 
#' 
#' describeratdata shows the following features: Number of unique school,
#' number of unique groups of raters/assessed, and for each group, the school
#' to which the group belongs, the group identifier, the ratings that are
#' actually used, the count of assessors, the count of assessed, the number of
#' missing ratings, the bias in the ratings, the number of ratings less than
#' the midrating, the number of ratings equal to the midrating, the number of
#' ratings larger than the midrating. Function describeratdata is useful for a
#' first check of the ratings./cr N.B. Unavailable values (NA) on the diagonal
#' are not counted as mssing ratings.
#' 
#' 
#' @param dataframe The dataframe should have the following columns: schoolid
#' groupid respid, and for n raters the columns r01 .. rn, with a maximum of
#' r99. Function \code{\link{readratdatafixed}} creates an appropriate data
#' frame from a text file with ratings.
#' @return A data.frame with for each group: \item{schoolid }{The school
#' identifier} \item{groupid }{The group identifier} \item{scores }{The scores
#' that have been used by the raters} \item{assessors }{The number of
#' assessors} \item{assessed }{The number of assessed} \item{missing }{The
#' number of missing ratings, not counting the missings on the diagonal}
#' \item{bias }{The bias in the ratings, defined as the mean of the ratings
#' minus the midscore value (R)} \item{ltR }{The number of ratings, less than
#' the midscore value (R)} \item{R }{The number of ratings, equal to the
#' midscore value (R)} \item{gtR }{The number of ratings, greater than the
#' midscore value (R)}
#' @author Hans Landsheer 
#' @seealso \code{\link{readratdatafixed}}
#' @keywords utilities Utilities
#' @examples
#' 
#' data(example6.rat)
#' describeratdata(example6.rat)
#' data(example7.rat)
#' describeratdata(example7.rat)
#' data(example1a.rat)
#' describeratdata(example1a.rat)
#' 
#' @export
describeratdata <- function(dataframe) {
  schoolgroups = unique(cbind(dataframe$schoolid, dataframe$groupid))
  # i=1
  out=data.frame(schoolid=numeric(0), groupid=numeric(0), scores=character(0), assessors=numeric(0), 
                 assessed=numeric(0), missing=numeric(0), bias=numeric(0), "ltR"=numeric(0), 
                 R=numeric(0), "gtR"=numeric(0), stringsAsFactors = F)
  for (i in 1:nrow(schoolgroups)) {
    #i=1
    sel = dataframe$schoolid == schoolgroups[i, 1] & dataframe$groupid == 
      schoolgroups[i, 2]
    lastrow = ncol(dataframe)
    #maxnrratings = as.numeric(gsub("[r]", "", names(dataframe)))
    maxnrratings=sum(grepl("^r\\d", names(dataframe)))
    grr = dataframe[sel, (lastrow - maxnrratings + 1):lastrow]
    grr = grr[, !apply(grr, 2, function(x) all(is.na(x)))]
    d = diag(as.matrix(grr))
    maxnrratings = ncol(grr)
    nra = nrow(grr)
    v = c(as.matrix(grr))
    m = sum(is.na(v)) - min(nra, maxnrratings)
    scale = max(grr, na.rm=T) 
    switch(EXPR=scale,{scale=3},{scale=3},{scale=3},{scale=5},{scale=5},{scale=7},{scale=7},{scale=9},{scale=9})
    R=(scale+1)/2
    bias=round(mean(as.matrix(grr-R),na.rm=T),2)
    ltR = sum(grr<R, na.rm=T)
    isR = sum(grr==R, na.rm=T)
    gtR = sum(grr>R, na.rm=T)
    scores=paste(sort(unique(v)),collapse=",") #length(scores)
    if (m < 0 | !all(is.na(d))) 
      warning(paste("Number of missing values is negative in schoolid", schoolgroups[i, 1], "groupid",
                  schoolgroups[i, 2], "because of numeric diagonal values"), immediate.=F, noBreaks. = F)
    out[i,]=data.frame(schoolgroups[i, 1],schoolgroups[i, 2], scores, nra, 
              maxnrratings, m, bias, ltR, isR, gtR, stringsAsFactors = FALSE)
  }
  out
}


# data(example6.rat)
# describeratdata(example6.rat)
# dataframe=example6.rat
# bias=round(mean(as.matrix(grr),na.rm=T),2)-4
