

\name{read_PDT}
\alias{read_PDT}
\title{
read PDT data from archival one or multiple tags
}
\description{
reads PDT data (PAT-style Depth Temperature profiles) from archival tags by \href{https://wildlifecomputers.com/}{Wildlife Computers}).
The PDT file can contain data from one or multiple tags.
\cr\cr\strong{What are PDTs?}
\cr PDT data provides minimum and maximum water temperatures during a user-programmed interval (usually 24h) at 8 to 16 depths. The sampled depths are thereby rounded (binned) to multiples of 8 and include the minimum and maximum depth bins as well as the 6 to 14 most frequent depth bins at which the tagged animal was located. The total number of depth bins (8 or 16) also depends on the tagged animals' behaviour. If the animal was in waters deeper than 400 m during the summary data period, the range of temperature at 16 depth bins will be reported, otherwise 8.
\cr\cr\strong{Why using PDT data?}
\cr Despite its low resolution, PDT data can give accurate information on the in-situ thermal stratification of the water column (e.g. thermocline depth, stratification index, ocean heat content) experienced by the tagged animal, as illustrated by Bauer et al. (2015). Accordingly, PDT data can provide precious insights into the relations between animal behaviour and environmental conditions. See the example section below on how to obtain thermal stratification indicators of the water column from PDT data.

For instance, daily PDT data can be interpolated and then visualized using functions \link{interpolate_PDTs} and \link{image_TempDepthProfiles}, respectively. This faciliates the analysis of temporal changes of temperature profiles, for instance, in relation to animal behaviour (e.g. diving behaviour).



}

\references{
Bauer, R., F. Forget and JM. Fromentin (2015) Optimizing PAT data transmission: assessing the accuracy of temperature summary data to estimate environmental conditions. Fisheries Oceanography, 24(6): 533-539, 
\doi{10.1111/fog.12127}
}


\usage{
read_PDT(pdt_file, folder, sep=",",date_format,lang_format="en",tz="UTC")
}
 

\arguments{
\item{pdt_file}{
character string indicating the name of a standard PDT-file. The Date-vector of the file is expected to be or the format "\%H:\%M:\%S \%d-\%b-\%Y, tz='UTC'".
}

\item{folder}{
path to pdt-file.
}

\item{sep}{
the field separator character. Values on each line of the file are separated by this character (default is ',').
}

\item{date_format, lang_format, tz}{
character strings indicating the date format, language format and the corresponding time zone, defined by the vectors Date and Time (by default: date_format="\%H:\%M:\%S \%d-\%b-\%Y", lang_format="en", tz='UTC')
If formatting fails, please check as well the input language format, defined by \code{lang_format} (and use abbrviations such as "en" for English,"es" for Spanish, "fr" for French, etc.) as well.
}
}

\value{A \link{data.frame} with the columns:\cr
"pdt_file", "DeployID", "Ptt", "NumBins", "Depth", "MinTemp", "MaxTemp", "datetime", "date", "MeanPDT"
\cr \bold{Attention:  Column "MeanPDT" is not measured} but calculated as the average of "MinTemp" and "MaxTemp" values.
}

\seealso{\link{bin_TempTS}, \link{interpolate_PDTs}, \link{image_TempDepthProfiles}} 


\author{
Robert K. Bauer
}

\examples{
## step I) read sample PDT data file:
path <- system.file("example_files",package="RchivalTag")
PDT <- read_PDT("104659-PDTs.csv",folder=path)
head(PDT)

## step II) interpolate average temperature fields (MeanPDT) from PDT file:
# m <- interpolate_PDTs(PDT)
# str(m)
# m$sm

## step III) calculate thermal stratifcation indicators per day (and tag):
# strat <- get_thermalstrat(m, all_info = TRUE)
# strat <- get_thermalstrat(m, all_info = FALSE)

## step IV) plot interpolated profiles:
# image_TempDepthProfiles(m$station.1)


}

