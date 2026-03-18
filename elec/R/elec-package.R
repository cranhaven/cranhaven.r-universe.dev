

#' @title
#' Statistical Election Audits Package
#' 
#' @description
#' This is a collection of functions written to do various sorts of statistical
#' election audits.  There are also functions to generate simulated voting
#' data, and simulated ``truth'' so as to do simulations to check
#' charactaristics of these methods.  The package includes two data sets
#' consisting of actual reported voting results for races held November, 2008,
#' in California.  It also includes actual audit date for one of these races.
#' 
#' \tabular{ll}{ Package: \tab elec\cr Type: \tab Package\cr Version: \tab
#' 0.1\cr Date: \tab 2009-01-14\cr License: \tab GPL (>= 2)\cr LazyLoad: \tab
#' yes\cr }
#' 
#' There are three general audit styles implemented in this package.  For each
#' style there are two main computational tasks provided: estimate the needed
#' sample size and expected workload, and calculate $P$-values for a given
#' audit result.  The three methods are CAST (see
#' \code{\link{CAST.calc.sample}} and \code{\link{CAST.audit}}, the Trinomial Bound
#' (see \code{\link{tri.calc.sample}} and \code{\link{trinomial.audit}}), and the
#' Kaplan-Markov (KM) Bound (see \code{\link{KM.calc.sample}} and
#' \code{\link{KM.audit}}).
#' 
#' The examples primarily use a data set included in the package,
#' \code{\link{santa.cruz}} and \code{\link{santa.cruz.audit}}, which holds the
#' ballot counts for a Santa Cruz, CA race that we audited using these methods.
#' See \code{\link{trinomial.bound}} for how these data were analyzed.  The
#' \code{\link{yolo}} data set holds precinct level counts for a race in Yolo
#' county.
#' 
#' There are also many functions allowing for construction of new audit methods
#' and simulations.  This includes methods that generate fake race data that
#' can be used for computational simulations to assess the efficay of different
#' auditing approaches (see, e.g., \code{\link{make.sample}} and
#' \code{\link{make.truth}}).
#' 
#' The package grew out of an earlier, disorganized package that implemented
#' general routines for election auditing.  Pieces of this package are used by
#' the aforementioned cleaner methods, but all the individual functions are
#' still there for specific uses, such as making different tests.  Start with
#' \link{stark.test}, which has an index of these pieces in its ``see also''
#' section.
#' 
#' If you find yourself confused, please contact the maintainer, L. Miratrix,
#' for help.  This will help improve the clarity of the package a great deal.
#' 
#' @name elec-package
#' @aliases elec-package elec
#' @docType package
#' @author Luke W. Miratrix
#' 
#' 
#' Maintainer: Luke W. Miratrix <luke@@vzvz.org>
#' @references CAST and KM were developed by Philip B. Stark.  The Trinomial
#' bound was developed by Luke W. Miratrix and Philip B. Stark.
#' 
#' For general papers on election auditing see the list at
#' http://www.stat.berkeley.edu/~stark/Vote/index.htm.
#' 
#' In particular, for the trinomial bound, see Luke W. Miratrix and Philip B.
#' Stark. (2009) Election Audits using a Trinomial Bound (in press).
#' 
#' For the KM bound see Stark, P.B., 2009. Risk-limiting post-election audits:
#' P-values from common probability inequalities.
#' 
#' For an overview of the races and the methods, see Joseph Lorenzo Hall,
#' Philip B. Stark, Luke W. Miratrix, Elaine Ginnold, Freddie Oakley, Tom
#' Stanionis, and Gail Pellerin. (2009) Implementing Risk-Limiting Audits in
#' California.
#' 
#' @keywords package
#' 
#' @importFrom graphics abline axis contour points segments text
#' @importFrom stats pbinom runif uniroot phyper
#' @importFrom utils head
NULL










#' @title
#' Marin Measure B Reported Results
#' 
#' @description 
#' These are the reported vote totals from the 2009 election in Marin, CA for
#' Measure B.
#' 
#' Note the vote totals for the VBM strata are made up.  The batches are the
#' ``Decks'', which could not be individually tallied with ease.  The
#' work-around was complex.  See the references, below.
#' 
#' @name marin
#' @docType data
#' @format A data frame with 544 observations on the following 5 variables.
#' \describe{ \item{PID}{Batch ID} \item{strata}{There are two
#' levels, \code{ST-IB} \code{ST-VBM} for in-precinct and Vote-by-Mail.}
#' \item{tot.votes}{total ballots cast in the batch.}
#' \item{Yes}{Number recorded for Yes} \item{No}{Number
#' recorded for No} }
#' @references See J. L. Hall, L. W. Miratrix, P. B. Stark, M. Briones, E.
#' Ginnold, F. Oakley, M. Peaden, G. Pellerin, T. Stanionis, and T. Webber.
#' Implementing risk-limiting audits in california.  USENIX EVT/WOTE in press,
#' July 2009.
#' @source Marin, CA 2009 reported election results.
#' @keywords datasets
#' @examples
#' 
#' data(marin)
#' marin = elec.data( marin, C.names=c("Yes","No") )
#' 
#' # Hand fixing error bound due to unknown
#' # vote totals in the VBM decks
#' marin$V$e.max = maximumMarginBound(marin)
#' sum( marin$V$e.max )   # 7.128
#' vbm = marin$V$strata=="ST-VBM"
#' marin$V[ vbm, "e.max" ] = 2 * marin$V[ vbm, "tot.votes" ] / marin$margin
#' 
#' sum( marin$V$e.max )   # 9.782
#' 
#' 
NULL










#' @title
#' Santa Cruz Election Data
#' 
#' @description 
#' \code{santa.cruz} and \code{santa.cruz.audit} hold data from a Santa Cruz
#' County, CA, contest held in November, 2008, for County Supervisor in the 1st
#' District.  The competitive candidates were John Leopold and Betty Danner.
#' According to the semi-official results provided to us by the Santa Cruz
#' County Clerk's office, Leopold won with votes on 45\% of the 26,655 ballots.
#' Danner received the votes on 37\% of the ballots.  The remaining ballots
#' were undervoted, overvoted, or had votes for minor candidates.
#' 
#' \code{santa.cruz.audit} holds the audit totals for the random sample of
#' precincts selected for the audit.  Note the \code{santa.cruz.audit} vote
#' counts are larger for some precincts due the missing provisional ballot
#' counts in the semi-official results.
#' 
#' @name santa.cruz.audit
#' @docType data
#' @format A data frame with 16 observations on the following 4 variables.
#' \describe{ \item{PID}{Precinct IDs (unique) for all precincts
#' involved in race} \item{leopold}{Total number of ballots marked for
#' John Leopold.} \item{danner}{Total number of ballots marked for
#' Betty Danner.} \item{count}{The number of times precinct was sampled
#' in the PPEB sample taken.} }
#' @seealso \link{santa.cruz}. For an illustration of analyzing this data, see
#' the example in \code{\link{trinomial.bound}}.
#' @source Santa Cruz County, CA, Clerk Gail Pellerin, and their staffs, which
#' we thank for their generous cooperation and the considerable time and effort
#' they spent counting ballots by hand in order to collect these data.
#' @keywords datasets
#' @examples
#' 
#' data(santa.cruz.audit)
#' data(santa.cruz)
#' santa.cruz = elec.data(santa.cruz, C.names=c("leopold","danner"))
#' trinomial.audit( santa.cruz, santa.cruz.audit )
#' 
NULL





#' @title
#' Santa Cruz Election Data
#' 
#' @description 
#' \code{santa.cruz} and \code{santa.cruz.audit} hold data from a Santa Cruz
#' County, CA, contest held in November, 2008, for County Supervisor in the 1st
#' District.  The competitive candidates were John Leopold and Betty Danner.
#' According to the semi-official results provided to us by the Santa Cruz
#' County Clerk's office, Leopold won with votes on 45\% of the 26,655 ballots.
#' Danner received the votes on 37\% of the ballots.  The remaining ballots
#' were undervoted, overvoted, or had votes for minor candidates.
#' 
#' \code{santa.cruz} holds the semi-official results for the race.
#' \code{santa.cruz.audit} holds the audit totals for the random sample of
#' precincts selected for the audit.  Note the \code{santa.cruz.audit} vote
#' counts are larger for some precincts due the missing provisional ballot
#' counts in the semi-official results.
#' 
#' @name santa.cruz
#' @docType data
#' @format A data frame with 152 observations on the following 5 variables.
#' \describe{ \item{PID}{Precinct IDs (unique) for all precincts
#' involved in race} \item{r}{Total number of registered voters in the
#' precinct.} \item{tot.votes}{Total number of ballots cast in the
#' precinct.} \item{leopold}{Total number of ballots marked for John
#' Leopold.} \item{danner}{Total number of ballots marked for Betty
#' Danner.} }
#' @seealso \link{santa.cruz.audit}
#' @source Santa Cruz County, CA, Clerk Gail Pellerin, and their staff.
#' @keywords datasets
#' @examples
#' 
#' data(santa.cruz)
#' elec.data( santa.cruz, C.names=c("danner","leopold") )
#' 
NULL





#' @title
#' Yolo County, CA Election Data
#' 
#' @description
#' This is for measure W in Yolo County, CA, November 2008.  The file includes
#' precinct-level reports.
#' 
#' In the actual audit, 6 precincts were selected (see example) and audited by
#' hand-to-eye count by a group of 4 people cross-checking each other. One of
#' the 6 batches had underreported the "yes" votes by 1, and one had
#' overreported the "yes" votes by 1.  There were no other errors.
#' 
#' @name yolo
#' @docType data
#' @format A data frame with 114 observations on the following 8 variables.
#' \describe{ \item{PID}{ Unique identifier for the batches of ballots}
#' \item{Pct}{The precinct id of the batch} \item{how}{Vote by
#' mail (VBM) or walk-in (PCT)} \item{b}{Number of votes cast in that
#' unit} \item{under}{Number of undervotes (ballots not voted).}
#' \item{over}{Number of overvotes (where someone marked both yes and
#' no).} \item{y}{Reported number of valid ballots marked yes.}
#' \item{n}{Reported number of valid ballots marked no.} }
#' @references See Stark et al. for papers using this data to illustrate
#' risk-limiting audits of election data.
#' @source Yolo County, CA.  Special thanks to Freddie Oakley and Tom
#' Stanionis.
#' @keywords datasets
#' @examples
#' 
#' # Make an elec.data object out of precicnt-level results
#' data(yolo)
#' yolo = elec.data( yolo, C.names=c("y","n","under","over"), tot.votes.col="b" ) 
#' 
#' # Look at different sample sizes and cuts for setting aside
#' # small precincts
#' CAST.calc.opt.cut( yolo, beta=0.75, stages=1, t=5, plot=TRUE )
#' 
#' print( yolo )
#' 
#' # Get details of the audit plan -- expected work, etc.
#' ap <- CAST.calc.sample( yolo, beta=0.75, stages=1, t=5, small.cut=5 )
#' print( ap )
#' 
#' # Draw a sample (seed not used for actual audit)
#' CAST.sample(yolo, ap, seed=12345678)
#' 
#' 
#' 
NULL



