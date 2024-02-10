#' Presence-only records of the echinoid \emph{Ctenocidaris nutrix} (Kerguelen Plateau)
#'
#' @description Dataset that contains the presence of the echinoid species \emph{Ctenocidaris nutrix} reported from several campaigns including RV Marion Dufresne MD03 1974 & MD04 1975, POKER 2 (2010) and PROTEKER 2013, 2014, 2015.  \cr \emph{Ctenocidaris nutrix} (Thomson 1876) is a broad range species, distributed from -70.5W to 143.7E and -76.13 to -47.18S in the Southern Ocean. The species is mainly found around the Kerguelen Plateau, and near Weddell Sea and Scotia Ridge regions. The species is known from littoral waters down to 800m. It is a carnivorous and direct developer species that breeds its youngs (David et al. 2005). \emph{Ctenocidaris nutrix} is considered as an indicator species of Vulnerable Marine Ecosystems (VME) by the CCAMLR.
#'
#' See Guillaumot et al. (2016) for more details
#'
#'
#'@usage data('ctenocidaris.nutrix')
#'
#'
#'@format A data frame containing 125 occurrences and 13 descriptive variables
#'\itemize{
#' \item \emph{id}                         \cr   Occurrence number indicator
#' \item \emph{scientific.name}           \cr   Species scientific name
#' \item \emph{scientific.name.authorship}   \cr   Author of the species description
#' \item \emph{genus}                       \cr Genus scientific name and its associated author
#' \item \emph{family}                      \cr Family scientific name and its associated author
#' \item \emph{order.and.higher.taxonomic.range} \cr Order scientific name and its associated author
#' \item \emph{decimal.Longitude}           \cr Longitude in decimal degrees
#' \item \emph{decimal.Latitude}            \cr Latitude in decimal degrees
#' \item \emph{depth}                       \cr Depth in meters
#' \item \emph{campaign}                    \cr Campaign origin of the data
#' \item \emph{reference}                   \cr Campaign reference
#' \item \emph{vessel}                      \cr Campaign vessel}
#'


#'@references
#'David B, Chone T, Mooi R, De Ridder C (2005) Antarctic Echinoidea. Synopses of the Antarctic Benthos 10.
#'
#'Guillaumot C, A Martin, S Fabri-Ruiz, M Eleaume & T Saucede (2016). Echinoids of the Kerguelen Plateau: Occurrence data and environmental setting for past, present, and future species distribution modelling, Zookeys, 630: 1-17.
#'
#'Thomson CW (1876) Notice of some peculiarities in the mode of propagation of certain echinoderms of the southern seas. J. Linn. Soc. London 13: 55-79.
#'
#'
#'@examples
#'data('ctenocidaris.nutrix')
#'x <- ctenocidaris.nutrix
#'# plot of the occurrences:
#'# selecting the species according to the campaigns
#'ctenocidaris7475 <- base::subset(x,x$year==1974 | x$year==1975)
#'ctenocidaris20102015 <- base::subset(x,x$campaign=='POKER II' | x$campaign=='PROTEKER')
#'
#'# drawing the background (depth)
#'library(grDevices)
#'blue.palette <- colorRampPalette(c('blue','deepskyblue','azure'))(100)
#'data('predictors1965_1974')
#'depth <- raster :: subset(predictors1965_1974, 1)
#'
#'raster::plot(depth, col=blue.palette,main= "Ctenocidaris nutrix occurrences")
#'
#'# adding the occurrences data to the background
#'points(ctenocidaris7475[,c('decimal.Longitude','decimal.Latitude')],
#'       col='orange',pch=16)
#'points(ctenocidaris20102015[,c('decimal.Longitude','decimal.Latitude')],
#'       col='darkgreen',pch=16)
#'legend('bottomleft',
#'        legend=c('Ctenocidaris nutrix 1974-1975','Ctenocidaris nutrix 2010-2015'),
#'        col= c('orange','darkgreen'), pch= c(15, 15),cex=0.9)
#'
#'
"ctenocidaris.nutrix"



