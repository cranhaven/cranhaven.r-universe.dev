\name{get.edh}
\alias{get.edh}
\title{
Get data from the Epigraphic Database Heidelberg API
}
\description{
A function to obtain data from the Epigraphic Database Heidelberg REST like API repository.
}
\usage{
get.edh(search = c("inscriptions", "geography"), url =
                 "https://edh.ub.uni-heidelberg.de/data/api", hd_nr,
                 province, country, findspot_modern, findspot_ancient,
                 year_not_before, year_not_after, tm_nr, transcription,
                 type, bbox, findspot, pleiades_id, geonames_id,
                 offset, limit, maxlimit = 4000, addID, printQ)
}
\arguments{
  \item{search}{
	whether the search is on \code{inscriptions} \emph{or} on \code{geography}.
}
  \item{url}{
	open data repository API
}
  \item{hd_nr}{
	HD number of inscription
}
  \item{province}{
	ancient Roman province name
}
  \item{country}{
	actual country name
}
  \item{findspot_modern}{
	actual location name findspot
}
  \item{findspot_ancient}{
	ancient location name findspot
}
  \item{year_not_before}{
	year, not before (integer, BC years are negative)
}
  \item{year_not_after}{
	year, not after (integer, BC years are negative)
}
  \item{tm_nr}{
	trismegistos ID (integer)
}
  \item{transcription}{
	automatic leading and trailing truncation (brackets are ignored)
}
  \item{type}{
	type of inscription (case insensitive)
}
  \item{bbox}{
	bounding box with character format \code{bbox = "minLong, minLat, maxLong, maxLat"}
}
  \item{findspot}{
	level of village, street etc. (add leading and/or trailing)
}
  \item{pleiades_id}{
	Pleiades identifier of a place (integer)
}
  \item{geonames_id}{
	Geonames identifier of a place (integer)
}
  \item{offset}{
	clause to specify which row to start from retrieving data (optional and integer)
}
  \item{limit}{
	clause to limit the number of results (optional and integer)
}
  \item{maxlimit}{
	maximum limit of the query (integer, default 4000)
}
  \item{addID}{
	add identification to the output? (optional and logical)
}
  \item{printQ}{
	print also query? (optional and logical)
}
}
\details{
Since with the \code{inscriptions} option the \code{id} "component" of the 
output list is not with a numeric format, then the function adds an \code{ID} 
at the beginning of the list with the identifier with a numerical format. 
\code{hd_nr} has not the same value as \code{ID} nor \code{id}. 
In case you want to grab several items from the Epigraphic Database Heidelberg API 
use function \code{\link{get.edhw}}.

A list with the of valid values from the EDH API for the ancient Roman provinces 
that are also available in dataset \code{\link{rp}} are

\tabular{llllll}{
\code{"Ach"}  \tab  Achaia                           \tab  \code{"Cor"}  \tab  Corsica                       \tab  \code{"Mes"}  \tab  Mesopotamia                   \cr 
\code{"Aeg"}  \tab  Aegyptus                         \tab  \code{"Cre"}  \tab  Creta                         \tab  \code{"MoI"}  \tab  Moesia inferior               \cr 
\code{"Aem"}  \tab  Aemilia (Regio VIII)             \tab  \code{"Cyp"}  \tab  Cyprus                        \tab  \code{"MoS"}  \tab  Moesia superior               \cr 
\code{"Afr"}  \tab  Africa Proconsularis             \tab  \code{"Cyr"}  \tab  Cyrene                        \tab  \code{"Nar"}  \tab  Narbonensis                   \cr 
\code{"AlC"}  \tab  Alpes Cottiae                    \tab  \code{"Dac"}  \tab  Dacia                         \tab  \code{"Nor"}  \tab  Noricum                       \cr 
\code{"AlG"}  \tab  Alpes Graiae                     \tab  \code{"Dal"}  \tab  Dalmatia                      \tab  \code{"Num"}  \tab  Numidia                       \cr 
\code{"AlM"}  \tab  Alpes Maritimae                  \tab  \code{"Epi"}  \tab  Epirus                        \tab  \code{"PaI"}  \tab  Pannonia inferior             \cr 
\code{"AlP"}  \tab  Alpes Poeninae                   \tab  \code{"Etr"}  \tab  Etruria (Regio VII)           \tab  \code{"PaS"}  \tab  Pannonia superior             \cr 
\code{"ApC"}  \tab  Apulia et Calabria (Regio II)    \tab  \code{"Gal"}  \tab  Galatia                       \tab  \code{"Pic"}  \tab  Picenum (Regio V)             \cr 
\code{"Aqu"}  \tab  Aquitania                        \tab  \code{"GeI"}  \tab  Germania inferior             \tab  \code{"Rae"}  \tab  Raetia                        \cr 
\code{"Ara"}  \tab  Arabia                           \tab  \code{"GeS"}  \tab  Germania superior             \tab  \code{"ReB"}  \tab  Regnum Bospori                \cr 
\code{"Arm"}  \tab  Armenia                          \tab  \code{"HiC"}  \tab  Hispania citerior             \tab  \code{"Rom"}  \tab  Roma                          \cr 
\code{"Asi"}  \tab  Asia                             \tab  \code{"Inc"}  \tab  Provincia incerta             \tab  \code{"Sam"}  \tab  Samnium (Regio IV)            \cr 
\code{"Ass"}  \tab  Assyria                          \tab  \code{"Iud"}  \tab  Iudaea                        \tab  \code{"Sar"}  \tab  Sardinia                      \cr 
\code{"Bae"}  \tab  Baetica                          \tab  \code{"LaC"}  \tab  Latium et Campania (Regio I)  \tab  \code{"Sic"}  \tab  Sicilia, Melita               \cr 
\code{"Bar"}  \tab  Barbaricum                       \tab  \code{"Lig"}  \tab  Liguria (Regio IX)            \tab  \code{"Syr"}  \tab  Syria                         \cr 
\code{"Bel"}  \tab  Belgica                          \tab  \code{"Lug"}  \tab  Lugdunensis                   \tab  \code{"Thr"}  \tab  Thracia                       \cr 
\code{"BiP"}  \tab  Bithynia et Pontus               \tab  \code{"Lus"}  \tab  Lusitania                     \tab  \code{"Tra"}  \tab  Transpadana (Regio XI)        \cr 
\code{"BrL"}  \tab  Bruttium et Lucania (Regio III)  \tab  \code{"LyP"}  \tab  Lycia et Pamphylia            \tab  \code{"Tri"}  \tab  Tripolitania                  \cr 
\code{"Bri"}  \tab  Britannia                        \tab  \code{"MaC"}  \tab  Mauretania Caesariensis       \tab  \code{"Umb"}  \tab  Umbria (Regio VI)             \cr 
\code{"Cap"}  \tab  Cappadocia                       \tab  \code{"MaT"}  \tab  Mauretania Tingitana          \tab  \code{"Val"}  \tab  Valeria                       \cr 
\code{"Cil"}  \tab  Cilicia                          \tab  \code{"Mak"}  \tab  Macedonia                     \tab  \code{"VeH"}  \tab  Venetia et Histria (Regio X)  \cr 
}

And the valid values for \code{country} entries are abbreviated country names where the inscription was located. 

\tabular{llllll}{
\code{"ad"}  \tab  Andorra                 \tab  \code{"gr"}  \tab  Greece                  \tab  \code{"pl"}  \tab  Poland                \cr 
\code{"al"}  \tab  Albania                 \tab  \code{"hr"}  \tab  Croatia                 \tab  \code{"pt"}  \tab  Portugal              \cr 
\code{"am"}  \tab  Armenia                 \tab  \code{"hu"}  \tab  Hungary                 \tab  \code{"rks"} \tab  Kosovo                \cr 
\code{"at"}  \tab  Austria                 \tab  \code{"il"}  \tab  Israel                  \tab  \code{"ro"}  \tab  Romania               \cr 
\code{"az"}  \tab  Azerbaijan              \tab  \code{"iq"}  \tab  Iraq                    \tab  \code{"rs"}  \tab  Serbia                \cr 
\code{"ba"}  \tab  Bosnia and Herzegovina  \tab  \code{"it"}  \tab  Italy                   \tab  \code{"ru"}  \tab  Russia                \cr 
\code{"be"}  \tab  Belgium                 \tab  \code{"jo"}  \tab  Jordan                  \tab  \code{"sa"}  \tab  Saudi Arabia          \cr 
\code{"bg"}  \tab  Bulgaria                \tab  \code{"kg"}  \tab  Kyrgyzstan              \tab  \code{"sd"}  \tab  Sudan                 \cr 
\code{"ch"}  \tab  Switzerland             \tab  \code{"kz"}  \tab  Kazakhstan              \tab  \code{"se"}  \tab  Sweden                \cr 
\code{"cy"}  \tab  Cyprus                  \tab  \code{"lb"}  \tab  Lebanon                 \tab  \code{"si"}  \tab  Slovenia              \cr 
\code{"cz"}  \tab  Czech Republic          \tab  \code{"li"}  \tab  Liechtenstein           \tab  \code{"sk"}  \tab  Slovakia              \cr 
\code{"de"}  \tab  Germany                 \tab  \code{"lu"}  \tab  Luxembourg              \tab  \code{"sm"}  \tab  San Marino            \cr 
\code{"dk"}  \tab  Denmark                 \tab  \code{"ly"}  \tab  Libyan Arab Jamahiriya  \tab  \code{"sy"}  \tab  Syrian Arab Republic  \cr 
\code{"dz"}  \tab  Algeria                 \tab  \code{"ma"}  \tab  Morocco                 \tab  \code{"tj"}  \tab  Tajikistan            \cr 
\code{"eg"}  \tab  Egypt                   \tab  \code{"mc"}  \tab  Monaco                  \tab  \code{"tn"}  \tab  Tunisia               \cr 
\code{"es"}  \tab  Spain                   \tab  \code{"md"}  \tab  Moldova                 \tab  \code{"tr"}  \tab  Turkey                \cr 
\code{"fr"}  \tab  France                  \tab  \code{"me"}  \tab  Montenegro              \tab  \code{"ua"}  \tab  Ukraine               \cr 
\code{"gb"}  \tab  United Kingdom          \tab  \code{"mk"}  \tab  Macedonia               \tab  \code{"uz"}  \tab  Uzbekistan            \cr 
\code{"ge"}  \tab  Georgia                 \tab  \code{"mt"}  \tab  Malta                   \tab  \code{"va"}  \tab  Vatican City State    \cr 
\code{"gi"}  \tab  Gibraltar               \tab  \code{"nl"}  \tab  Netherlands             \tab  \code{"ye"}  \tab  Yemen                 \cr 
}


}
\value{
A list object with at least one the following items:

\item{"commentary"}{ }
\item{"fotos"}{ }
\item{"country"}{ }
\item{"depth"}{ }
\item{"diplomatic_text"}{ }
\item{"edh_geography_uri"}{ }
\item{"findspot"}{ }
\item{"findspot_ancient"}{ }
\item{"findspot_modern"}{ }
\item{"geography"}{ }
\item{"height"}{ }
\item{"id"}{ }
\item{"language"}{ }
\item{"last_update"}{ }
\item{"letter_size"}{ }
\item{"literature"}{ }
\item{"material"}{ }
\item{"military"}{ }
\item{"modern_region"}{ }
\item{"not_after"}{ }
\item{"not_before"}{ }
\item{"people"}{This item is another list with at least one the following items: \cr  \cr  
\code{"person_id"} \cr  \code{"nomen"} \cr  \code{"cognomen"} \cr  \code{"praenomen"} \cr  \code{"name"} \cr  \code{"gender"} \cr  \code{"status"} \cr  \code{"tribus"} \cr  
\code{"origo"} \cr  \code{"occupation"} \cr  \code{"age: years"} \cr  \code{"age: months"} \cr  \code{"age: days"} }
\item{"present_location"}{ }
\item{"religion"}{ }
\item{"province_label"}{ }
\item{"responsible_individual"}{ }
\item{"social_economic_legal_history"}{ }
\item{"transcription"}{ }
\item{"trismegistos_uri"}{ }
\item{"type_of_inscription"}{ }
\item{"type_of_monument"}{ }
\item{"uri"}{ }
\item{"width"}{ }
\item{"work_status"}{ }
\item{"year_of_find"}{ }
\item{"ID"}{(Optional), only if \code{addID} is set to \code{TRUE}.}

The query is also printed if specified by \code{printQ}. 
}
\references{
https://edh.ub.uni-heidelberg.de/data/api
}
\author{
Antonio Rivero Ostoic
}
\note{
This function requires "\code{\link[rjson]{rjson}}", and is for the [EDH] database [API] at the URL in references starting in year January 2022, 
and changes in the URL should be updated with the url option. 

Search options \code{"photos"} and \code{"bibliography"} are not supported.
}


\section{Warning }{
For queries having more than 4000 records, the server can produce a timeout break to be handled by \code{offset}.
}

\seealso{
\code{\link{get.edhw}}, \code{\link{edhw}}, \code{\link{edhwpd}}, \code{\link{rp}}, \code{\link{plot.map}}, \code{\link{simil}}, \code{\link[rjson]{rjson}}
}
\examples{
\dontrun{
# get inscriptions from EDH database API
get.edh(findspot_modern="madrid")}
}
\keyword{ IO }
\keyword{ utilities }
