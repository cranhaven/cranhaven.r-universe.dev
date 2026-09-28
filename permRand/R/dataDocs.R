

######################################################################
##
## File Name: dataDocs.R
## Description: Description of Data Included in Package
## Date Created: 8/26/2025
## Last Updated: 8/26/2025
##
######################################################################

## Data: emptyQC

#' A dataset containing the example non-randomized serum sample file.
#'
#' @name testR
#' @docType data
#' @author Michelle Mellers
#' @keywords data
#' @format A data frame with four datasets:
#' \describe{
#'   \item{serumIDs}{Serum Sample IDs Linked with StudyIDs}
#'   \item{serumLoc}{Serum ID packing list}
#'   \item{emptyQC}{Empty QC containers packing list}
#'   \item{motherQC}{QC samples to be aliquoted packing list}
#' }
#'
NULL
######################################################################

#' A dataset containing serum sample IDs linked with studyIDs.
#'
#' @name serumIDs
#' @docType data
#' @author Michelle Mellers
#' @keywords data
#' @format Serum Sample IDs linked with studyIDs
#' \describe{
#'   \item{studyID}{Study Subject ID}
#'   \item{event}{Sample Time}
#'   \item{serumID}{Serum ID}
#' }
#'
NULL
######################################################################


#' A dataset containing the a packing list with serum samples.
#'
#' @name serumLoc
#' @docType data
#' @author Michelle Mellers
#' @keywords data
#' @format Packing list of serum samples
#' \describe{
#'   \item{serumID}{Serum ID}
#'   \item{rack}{Rack}
#'   \item{row}{Row}
#'   \item{col}{Column}
#' }
#'
NULL
######################################################################

#' A dataset containing locations of empty QC samples.
#'
#' @name emptyQC
#' @docType data
#' @author Michelle Mellers
#' @keywords data
#' @format Empty QC samples
#' \describe{
#'   \item{serumID}{Serum ID}
#'   \item{rack}{Rack}
#'   \item{row}{Row}
#'   \item{col}{Column}
#' }
#'
NULL
######################################################################

#' A dataset containing locations of the mother QC samples.
#'
#' @name motherQC
#' @docType data
#' @author Michelle Mellers
#' @keywords data
#' @format Mother QC samples
#' \describe{
#'   \item{serumID}{Serum ID}
#'   \item{rack}{Rack}
#'   \item{row}{Row}
#'   \item{col}{Column}
#' }
#'
NULL
######################################################################

#' A dataset containing QC sample IDs and packing location.
#'
#' @name QCMaster
#' @docType data
#' @author Michelle Mellers
#' @keywords data
#' @format QC sample IDs and Packing Location
#' \describe{
#'   \item{serumID}{Serum ID}
#'   \item{rack}{Rack}
#'   \item{row}{Row}
#'   \item{col}{Column}
#'   \item{event}{Sample Time}
#'   \item{motherSerumID}{Mother Serum ID}
#'   \item{motherRack}{Mother Rack}
#'   \item{motherRow}{Mother Row}
#'   \item{motherCol}{Mother Column}
#'   \item{studyID}{Study ID}
#' }
#'
NULL
######################################################################

#' A dataset containing information for QC and serum samples.
#'
#' @name serumMaster3
#' @docType data
#' @author Michelle Mellers
#' @keywords data
#' @format Serum and QC sample Information
#' \describe{
#'   \item{serumID}{Serum ID}
#'   \item{studyID}{Study ID}
#'   \item{event}{Sample Time}
#'   \item{rack}{Rack}
#'   \item{row}{Row}
#'   \item{col}{Column}
#'   \item{ccID}{Group ID}
#'   \item{caseControl}{Case or Control Indicator}
#'   \item{QCsamp}{Indicates if QC sample}
#' }
#'
NULL
######################################################################

#' A dataset containing randomized QC and serum samples.
#'
#' @name serumRand
#' @docType data
#' @author Michelle Mellers
#' @keywords data
#' @format Randomized Serum and QC sample Information
#' \describe{
#'   \item{serumID}{Serum ID}
#'   \item{studyID}{Study ID}
#'   \item{event}{Sample Time}
#'   \item{rack}{Rack}
#'   \item{row}{Row}
#'   \item{col}{Column}
#'   \item{ccID}{Group ID}
#'   \item{caseControl}{Case or Control Indicator}
#'   \item{QCsamp}{Indicates if QC sample}
#'   \item{batchN}{Batch Number}
#'   \item{loc}{Location}
#' }
#'
NULL
######################################################################
#' A dataset containing switched and randomized QC and serum samples.
#'
#' @name serumSwitch
#' @docType data
#' @author Michelle Mellers
#' @keywords data
#' @format Switched and Randomized Serum and QC sample Information
#' \describe{
#'   \item{serumID}{Serum ID}
#'   \item{studyID}{Study ID}
#'   \item{event}{Sample Time}
#'   \item{ccID}{Group ID}
#'   \item{caseControl}{Case or Control Indicator}
#'   \item{QCsamp}{Indicates if QC sample}
#'   \item{batchN}{Batch Number}
#'   \item{loc}{Location}
#'   \item{outoforder}{Marker if Switched}
#' }
#'
NULL
######################################################################
#' A dataset containing switched and randomized QC and serum samples
#'  with packing locations.
#'
#' @name serumSwitchP
#' @docType data
#' @author Michelle Mellers
#' @keywords data
#' @format Switched and Randomized Serum and QC sample Information
#' \describe{
#'   \item{serumID}{Serum ID}
#'   \item{rack}{Rack}
#'   \item{row}{Row}
#'   \item{col}{Column}
#'   \item{studyID}{Study ID}
#'   \item{event}{Sample Time}
#'   \item{ccID}{Group ID}
#'   \item{caseControl}{Case or Control Indicator}
#'   \item{QCsamp}{Indicates if QC sample}
#'   \item{batchN}{Batch Number}
#'   \item{loc}{Location}
#'   \item{outoforder}{Marker if Switched}
#' }
NULL
######################################################################
