\name{edhw}
\alias{edhw}
\title{
Wrapper function for manipulation of the EDH dataset
}
\description{
A function to obtain variable data and perform transformations on the \code{EDH} dataset.
}
\usage{
edhw(x = "EDH", vars, as = c("df", "list"), type = c("long", "wide", "narrow"), 
     split, select, addID, limit, id, na.rm, ldf, province, gender, rp, ...)
}

\arguments{
  \item{x}{
   a list object name with fragments of the \code{EDH} dataset (optional)
}
  \item{vars}{
   vector of variables of interest from \code{x}; if \code{x=NULL}, the entire \code{EDH} dataset is taken (optional)
}
  \item{as}{
   format to return the output; either as a \code{"list"} or a data frame \code{"df"} object.
}
  \item{type}{
   type format of data frame; either \code{"long"} or \code{"wide"} (\code{"narrow"} not yet implemented)
}
  \item{split}{
   divide the data into groups by id? (optional and logical)
}
  \item{select}{
   vector with \code{"people"} variables (optional)
}
  \item{addID}{
   add identification to the output? (optional and logical)
}
  \item{limit}{
   integer or vector to limit the returned output. Ignored if \code{id} is specified (optional)
}
  \item{id}{
   select only \code{hd_nr} records (optional, integer or character)
}
  \item{na.rm}{
   remove entries with NA data? (optional and logical)
}
  \item{ldf}{
   is \code{x} list of data frames? (optional and logical)
}
  \item{province}{
   name or abbreviation of Roman province in \code{EDH} as in \code{\link{rp}} dataset
}
  \item{gender}{
   gender of people in \code{EDH}: \code{male} or \code{female}
}
  \item{rp}{
   customized list of Roman provinces as in \code{\link{rp}} dataset
}
  \item{\dots}{
   optional arguments if needed.
}
}
\details{
This is an interface to extract attribute variables from the \code{EDH} dataset attached to this package either as a built-in dataset or as external data. 
EDH dataset is a built-in data set of Latin epigraphy retrieved from the Epigraphic Database Heidelberg API repository where epigraphs or inscriptions 
in this dataset are recorded in a list object of 84701 items (until 10-11-2020) with at least one of the following 47 (or more) attribute names in the list:


\code{"ID"}, \code{"commentary"}, \code{"fotos"}, \code{"country"}, 
\code{"depth"}, \code{"diplomatic_text"}, \code{"edh_geography_uri"}, \code{"findspot"}, 
\code{"findspot_ancient"}, \code{"findspot_modern"}, \code{"geography"}, \code{"height"}, \code{"id"}, 
\code{"language"}, \code{"last_update"}, \code{"letter_size"}, \code{"literature"}, 
\code{"material"}, \code{"military"}, \code{"modern_region"}, \code{"not_after"}, 
\code{"not_before"}, \code{"people"} (which is a list with: \code{"person_id"}, \code{"nomen"}, \code{"cognomen"}, 
\code{"praenomen"}, \code{"name"}, \code{"gender"}, \code{"status"}, \code{"tribus"}, 
\code{"origo"}, \code{"occupation"}, \code{"age: years"}, \code{"age: months"}, \code{"age: days"}), 
\code{"present_location"}, \code{"province_label"}, \code{"religion"},  \cr  \code{"responsible_individual"}, 
\code{"social_economic_legal_history"}, \code{"transcription"}, \cr  \code{"trismegistos_uri"}, 
\code{"type_of_inscription"}, \code{"type_of_monument"}, \code{"uri"}, \code{"width"}, \cr 
\code{"work_status"}, and \code{"year_of_find"}. 


The input in \code{x}, however, can be fragments of the \code{EDH} dataset or from 
the Epigraphic Database Heidelberg API obtained by functions \code{\link{get.edh}} or \code{\link{get.edhw}}
with the "\code{\link[rjson]{rjson}}" format, or transformed data organized, for example, by provinces. 
When \code{x} is explicit, it must be at least a list object with a comparable structure to the \code{EDH} dataset. 
Argument \code{ldf} is a flag when the input in \code{x} is a created list of data frames that are 
organised by variables rather than by records as in the \code{EDH} dataset. 
The return of the output is either \code{as} a list with \code{list} or by default as a data frame with option \code{df}.

The extraction from \code{EDH} is typically through argument \code{vars} in the function, and in case that \code{vars} is missing, then it takes all entries in \code{x}. 
Ad hoc arguments are the \code{EDH} entries \code{province} and \code{gender} for entering a Roman province 
and people's gender in \code{x} as a data frame; otherwise, these arguments are ignored. 
When \code{province} is used, it is possible to refer to a customized list of provinces with argument "\code{rp}"; 
otherwise, dataset \code{\link{rp}} is the default where names and abbreviations are accepted. 

By default, this wrapper returns a list object with or without a numerical `ID' identification provided by the \code{addID} argument. 
When the output is a data frame, the ordering of the variables is alphabetically and, if desired, it is possible to remove missing data 
from the output by activating \code{na.rm} and work with complete cases.

Arguments \code{id} and \code{limit} serve to reduce the returned output either to some Epigraphic Database number or to numbers, 
which are specified by \code{hd_nr}, or else by limiting the amount of the returned output. 
\code{limit} here is like the \code{limit} argument of function \code{\link{get.edh}}, but in 
this case the offset can be specified as a sequence. 
While \code{limit} is a faster way to get to entries in the \code{EDH} dataset, argument \code{id} is for 
referring to precisely one or more \code{hd_nr}s in the Epigraphic Database Heidelberg API.

Component \code{"people"} is a separated list in the \code{EDH} dataset, and it should be considered as 
a separate case from the rest of the variables. 
In the case that the output is a data frame, the default output is a `long' \code{type} table; that is records can 
appear in different rows and each variable is assigned into a single column, and with this option is possible to 
\code{select} \code{"people"} variables like gender and origin. 
When choosing people variables with \code{select} and a data frame output, then \code{"people"} attribute must be in \code{vars}.

By setting \code{"wide"} in \code{type}, it is possible to place the different people from a single entry 
column by column in the data frame and each record has a single row. Finally, argument \code{split} allows for 
dividing the data in the data frame into groups by `id', which corresponds to the HD number of inscription 
in the \code{EDH} dataset. 

}
\value{
A list or a data frame with a long or wide format, depending on the input arguments. 

Argument \code{province} with no \code{vars} returns a list of lists.
}
\references{
\emph{Epigraphic Database Heidelberg -- Data Reuse Options}, (Online; retrieved on 16 June 2019). URL https://edh-www.adw.uni-heidelberg.de/data

https://edh-www.adw.uni-heidelberg.de/data/api (database retrieved on November 2020)

https://github.com/sdam-au/sdam/tree/master/data

https://github.com/mplex/cedhar/tree/master/pkg/sdam/data

}
\author{
Antonio Rivero Ostoic
}
\note{
\code{Warning} messages are given for the \code{EDH} dataset as the input, and when choosing 
the \code{province} argument alone. 
}
\section{Warning }{
\code{EDH} is a built-in dataset in the development and legacy version of the package but, 
because of its size, re not part of the CRAN distribution. Functions \code{\link{edhw}} and \code{\link{edhwpd}} 
download \code{EDH} from another repository in References. 
}

\seealso{
\code{\link{get.edh}}, \code{\link{get.edhw}}, \code{\link{rp}}, \code{\link{edhwpd}}, \code{\link{prex}}, 
\code{\link{plot.dates}}, \code{\link{cln}}, \code{\link[rjson]{rjson}}
}
\examples{
\dontrun{
# load dataset
data(EDH)

# make a list for three variables in 'EDH' for first 4 entries
edhw(vars=c("type_of_inscription", "not_after", "not_before"), limit=4 )

# as before, but also select 'gender' from 'people'
edhw(vars=c("people", "not_after", "not_before"), select="gender", limit=4 )}
}
\keyword{ data }
\keyword{ manip }
\keyword{ utilities }