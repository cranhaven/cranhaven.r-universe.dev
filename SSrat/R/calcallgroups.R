#dataframe=DF.ex6


#' Calculates the social status determinations of multiple groups. 
#' 
#' Calculates the social status determinations from a SSrat compliant
#' dataframe, as described in \code{\link{readratdatafixed}}.\cr The dataframe
#' may contain multiple groups. This function can only be used when the rating
#' scale is equal for all groups.\cr Basically, this function calls the
#' \code{\link{calcgroup}} function as often as there are groups in the SSrat
#' compliant dataframe.\cr The resulting dataframe with social sttaus
#' determinations can be analyzed with standard tools for statistical analyis,
#' such as linear regression (\code{lm}). Other data, such as background
#' variables can be added to the dataframe.
#' 
#' See function \code{\link{calcgroup}} for more details.
#' 
#' @param dataframe The input dataframe with the rating data. This dataframe
#' should have columns schoolid, groupid, respid, and for n raters n columns
#' named "r01", "r02" .. "r<n>", with a maximum of r99.
#' @param alpha The significance levels to be applied to the probability
#' distributions of the four total scores that have been derived from the
#' ratings. By choosing an appropriate alpha, the user can fine tune the
#' criterion for the status determination. A list of various alphas can be
#' supplied. Default is the list (.10, .05, .01).
#' @param NBcriteria A boolean. When TRUE, the classification criteria of
#' Newcomb & Bukowski (1983) will be applied, in stead of the SSrat criteria.
#' These criteria are applicable to three-point rating scales. When this option
#' is selected for longer scales, the scale midpoint is recoded to 2, all
#' scores larger than the midscore are recoded to 3 and all scores lower than
#' the midscore are recoded to 1. When another recoding scheme is preferred,
#' the input ratings themselves should be recoded (use \code{recode} from
#' package \code{car}). 
#' 
#' @return \item{dataframe}{dataframe with the most relevant output of SSrat,
#' calculated for each rated respondent} \item{dataframe$schoolid}{school id as
#' entered in the input dataframe} \item{dataframe$groupid}{group id as entered
#' in the input dataframe} \item{dataframe$respid}{respondent id as entered in
#' the input data frame} \item{dataframe$nrAss}{number of assessors who have
#' given a valid rating} \item{dataframe$tr.S}{total rating Sympathy}
#' \item{dataframe$tr.A}{total rating Antipathy} \item{dataframe$tr.P}{total
#' rating Preference} \item{dataframe$tr.I}{total rating Impact}
#' \item{dataframe$SS.xx}{Social Determination as attributed by SSrat, applying
#' alpha = .xx. Defaults to SS.10, SS.05 and SS.01 }
#' @note Use \code{\link{calcgroup}} for generating more
#' detailed information on social status determination.
#' @author Hans Landsheer
#' @seealso \code{\link{readratdatafixed}} \code{\link{calcgroup}}
#' @references Coie, J. D., Dodge, K. A., & Coppotelli, H.(1982). Dimensions
#' and types of social status: A cross-age perspective. Developmental
#' Psychology, 18, 557-570.\cr Coie, J.D., & Dodge, K.A. (1983). Continuities
#' and changes in children's social status: A five-year longitudinal study.
#' Merril-Palmer Quarterly, 29, 261-282.\cr Newcomb, A. F., & Bukowski, W. M.
#' (1983). Social impact and social preference as determinants of children's
#' peer group status. Developmental Psychology, 19, 856-867.\cr Maassen, G. H.
#' and Landsheer, J. A. (1998). SSRAT: The processing of rating scales for the
#' determination of two-dimensional sociometric status. Behavior Research
#' Methods Instruments &amp; Computers, 30(4), 674-679.
#' @keywords manip Data Manipulation datagen Functions for generating data sets
#' @examples
#' 
#' data(example5.rat)
#' calcgroup (school=1, group=23, data=example5.rat, scalelength="3")$dataframe
#' calcallgroups(example5.rat)
#' 
#' #readmultiple groups
#' data(example6.rat)
#' calcallgroups(example6.rat)
#' 
#' data(example7.rat)
#' calcallgroups(example7.rat) # Wrong!
#' calcgroup (school=1, group=1, dataframe=example7.rat, scalelength="7")$dataframe
#' calcgroup (school=1, group=3, dataframe=example7.rat, scalelength="7")$dataframe
#' calcgroup (school=2, group=1, dataframe=example7.rat, scalelength="7")$dataframe
#' 
#' #use names
#' data(example1a.rat)
#' calcallgroups(example1a.rat)
#' 
#' @export
calcallgroups <- function(dataframe, alpha = c(0.10, 0.05, 0.01), NBcriteria=F) {
  schoolgroups = unique(cbind(dataframe$schoolid, dataframe$groupid))  #sch=4;gr=20

  lastcol = ncol(dataframe)
  ratingnames = grepl("^r\\d", names(dataframe))
  # count columns with name rn
  maxrated = sum(ratingnames)
  srn = sort(names(dataframe)[ratingnames])
  # ratings may be padded with NA
  dataframe = data.frame(dataframe[!ratingnames],dataframe[srn])
  maxnrrated = maxrated - 1
  # get ratings
  grr = dataframe[, (lastcol - maxnrrated):lastcol]
  
  scale = max(grr, na.rm=T)
  switch(EXPR=scale,{scale=3},{scale=3},{scale=3},{scale=5},{scale=5},{scale=7},{scale=7},
         {scale=9},{scale=9})
  DF = data.frame()
  #i=2
  for (i in 1:nrow(schoolgroups)) {
    out = calcgroup(schoolid = schoolgroups[i, 1], groupid = schoolgroups[i,2], 
                    dataframe , scale, alpha, NBcriteria)
    DF = rbind(DF, out$dataframe)
  }
  DF
}
