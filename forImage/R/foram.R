#' Foraminiferal genera data for \code{forImage} examples
#'
#' This `data.frame` contains protoplasm occupancy mean and standard deviation data of 72 foraminifera genera.
#' These genera data are originally from Freitas and others (2019)
#' @docType data
#' @keywords `data.frame`
#' @format A `data.frame` with 72 rows and 3 variables:
#' \describe{
#'   \item{genera}{foraminifera genera}
#'   \item{mean}{mean protoplasm occupancy percentage \code{(pco)}}
#'   \item{sd}{standard deviation of mean \code{(pco)}}
#'   \item{n}{number of specimens}
#'   \item{model}{fitted geometric model in Freitas and others (2019)}
#' }
#'
#' @details Foraminifera genera with \code{(pco)} available:
#' \tabular{rlll}{
#'  \tab"ammonia"\tab"amphistegina"\tab"angulogerina"\cr
#'  \tab"archaias"\tab"asterotrochammina"\tab"bolivina"\cr
#'  \tab"buliminella"\tab"cancris"\tab"caronia"\cr
#'  \tab"cassidulina"\tab"cibicides"\tab"cibicidoides"\cr
#'  \tab"cornuspira"\tab"cribroelphidium"\tab"cymbaloporetta"\cr
#'  \tab"deuterammina"\tab"disconorbis"\tab"discorbia"\cr
#'  \tab"discorbinella"\tab"discorbis"\tab"discorbitina"\cr
#'  \tab"edentostomina"\tab"elphidium"\tab"eoeponidella"\cr
#'  \tab"eponides"\tab"fissurina"\tab"fursenkoina"\cr
#'  \tab"glabratella"\tab"globocassidulina"\tab"hanzawaia"\cr
#'  \tab"hauerina"\tab"heronallenia"\tab"laevipeneroplis"\cr
#'  \tab"lepidodeuterammina"\tab"loxostomina"\tab"miliolinella"\cr
#'  \tab"mullinoides"\tab"mychostomina"\tab"neoconorbina"\cr
#'  \tab"nonionella"\tab"nonioninoides"\tab"pararotalia"\cr
#'  \tab"paratrochammina"\tab"patellina"\tab"planispirillina"\cr
#'  \tab"planorbulina"\tab"planulina"\tab"pyrgo"\cr
#'  \tab"quinqueloculina"\tab"rectocibicides"\tab"remaneicella"\cr
#'  \tab"reophax"\tab"rosalina"\tab"rotaliammina"\cr
#'  \tab"rotorbinella"\tab"rotorbis"\tab"sahulia"\cr
#'  \tab"sigmavirgulina"\tab"siphonina"\tab"sorites"\cr
#'  \tab"spirillina"\tab"spiroloculina"\tab"spirorbina"\cr
#'  \tab"tetrataxiella"\tab"textularia"\tab"triloculina"\cr
#'  \tab"triloculinella"\tab"trochammina"\tab"webbinella"\cr
#'  \tab"wiesnerella"
#' }
#'
#' @name data_pco
NULL

#' Foraminiferal photomicrograph for \code{forImage} examples
#'
#'
#' Contains image file of one foraminifer.
#' These files are originally from previous work - Freitas and others (2019)
#'
#' @docType data
#' @keywords `data.frame`
#' @format Image objects with variable dimension and resolution.
#'
#' @name exampleImage
NULL

#' Ammonia size data
#'
#' A `data.frame` containing the size and other attributes of species from genus \emph{Ammonia}.
#'  The variables are as follows:
#'
#' @format A `data.frame` with 867 rows and 7 variables:
#' \describe{
#'   \item{species}{species of genus Ammonia from which raw data were gathered}
#'   \item{ind}{number of individuals by species}
#'   \item{h}{test height in um (82.05--390.65)}
#'   \item{d_one}{test minor diameter in  um (20--220)}
#'   \item{d_two}{test major diameter in  um (75.1--364.6)}
#'   \item{area}{test surface area in um2 (4438--107903)}
#'   \item{pco}{percent cell occupancy of the test (11--100)}
#' }
"ammonia"

#' Amphistegina size data
#'
#' A `data.frame` containing the size and other attributes of genus \emph{Amphistegina}.
#'  The variables are as follows:
#'
#' @format A `data.frame` with 167 rows and 5 variables:
#' \describe{
#'   \item{ind}{number of individuals}
#'   \item{h}{test height in um (60.0--579.0)}
#'   \item{d_one}{test diameter in  um (111.3--1193.1)}
#'   \item{area}{test surface area in um2 (9722--3027282)}
#'   \item{pco}{percent cell occupancy of the test (3.408--100)}
#' }
"amphistegina"

#' Angulogerina size data
#'
#' A `data.frame` containing the size and other attributes of species from genus \emph{Angulogerina}.
#'  The variables are as follows:
#'
#' @format A `data.frame` with 100 rows and 6 variables:
#' \describe{
#'   \item{ind}{number of individuals}
#'   \item{h}{test height in um (81.75--380.91)}
#'   \item{d_one}{test minor diameter in  um (34--156)}
#'   \item{d_two}{test major diameter in  um (66.04--237.34)}
#'   \item{area}{test surface area in um2 (4654--41915)}
#'   \item{pco}{percent cell occupancy of the test (6.818--100)}
#' }
"angulogerina"

#' Asterotrochammina size data
#'
#' A `data.frame` containing the size and other attributes of species from genus \emph{Asterotrochammina}.
#'  The variables are as follows:
#'
#' @format A `data.frame` with 335 rows and 7 variables:
#' \describe{
#'   \item{species}{species of genus Asterotrochammina from which raw data were gathered}
#'   \item{ind}{number of individuals by species}
#'   \item{h}{test height in um (12--72)}
#'   \item{d_one}{test minor diameter in  um (77.86--265.50)}
#'   \item{radius}{test radius in  um (38.93--132.75)}
#'   \item{area}{test surface area in um2 (4761--57577)}
#'   \item{pco}{percent cell occupancy of the test (15--100)}
#' }
"asterotrochammina"

#' Bolivina size data
#'
#' A `data.frame` containing the size and other attributes of species from genus \emph{Bolivina}.
#'  The variables are as follows:
#'
#' @format A `data.frame` with 628 rows and 7 variables:
#' \describe{
#'   \item{species}{species of genus Bolivina from which raw data were gathered}
#'   \item{ind}{number of individuals by species}
#'   \item{d_one}{test minor diameter in um (72.37--502.12)}
#'   \item{h}{test height (thickness) in um (12.5--83.0)}
#'   \item{d_two}{test major diameter in  um (60.60--226.21)}
#'   \item{area}{test surface area in um2 (3187--72244)}
#'   \item{pco}{percent cell occupancy of the test (9.72--100)}
#' }
"bolivina"

#' Cibicidoides size data
#'
#' A `data.frame` containing the size and other attributes of species from genus \emph{Cibicidoides}.
#'  The variables are as follows:
#'
#' @format A `data.frame` with 118 rows and 7 variables:
#' \describe{
#'   \item{species}{species of genus Cibicidoides from which raw data were gathered}
#'   \item{ind}{number of individuals by species}
#'   \item{h}{test height in um (24--117)}
#'   \item{d_one}{test minor diameter in  um (76.46--266.85)}
#'   \item{radius}{test radius in  um (38.23--133.43)}
#'   \item{area}{test surface area in um2 (4591--55928)}
#'   \item{pco}{percent cell occupancy of the test (11.87--100)}
#' }
"cibicidoides"

#' Discorbinella size data
#'
#' A `data.frame` containing the size and other attributes of species from genus \emph{Discorbinella}.
#'  The variables are as follows:
#'
#' @format A `data.frame` with 318 rows and 7 variables:
#' \describe{
#'   \item{species}{species of genus Discorbinella from which raw data were gathered}
#'   \item{ind}{number of individuals by species}
#'   \item{h}{test height in um (15--72)}
#'   \item{d_one}{test minor diameter in  um (67.69--197.43)}
#'   \item{radius}{test radius in  um (33.85--98.71)}
#'   \item{area}{test surface area in um2 (3599--30614)}
#'   \item{pco}{percent cell occupancy of the test (9.673--100)}
#' }
"discorbinella"

#' Laevipeneroplis size data
#'
#' A `data.frame` containing the size and other attributes of species from genus \emph{Laevipeneroplis}.
#'  The variables are as follows:
#'
#' @format A `data.frame` with 79 rows and 7 variables:
#' \describe{
#'   \item{species}{species of genus Laevipeneroplis from which raw data were gathered}
#'   \item{ind}{number of individuals by species}
#'   \item{h}{test height in um (40--633.9)}
#'   \item{d_one}{test minor diameter in  um (47--624.2)}
#'   \item{area}{test surface area in um2 (8827--306012)}
#'   \item{pco}{percent cell occupancy of the test (15.24--100)}
#'   \item{d_two}{test major diameter in  um (169.2--544.0) - The NA's related to one species of this genus that don't required this measure}
#' }
"laevipeneroplis"

#' Loxostomina size data
#'
#' A `data.frame` containing the size and other attributes of species from genus \emph{Loxostomina}.
#'  The variables are as follows:
#'
#' @format A `data.frame` with 31 rows and 7 variables:
#' \describe{
#'   \item{ind}{number of individuals by species}
#'   \item{d_one}{test minor diameter in um (101.4--534.6)}
#'   \item{h}{test height in um (28.0--80.0)}
#'   \item{d_two}{test major diameter in  um (89.76--261.17)}
#'   \item{area}{test surface area in um2 (6636--76089)}
#'   \item{pco}{percent cell occupancy of the test (9.88--100)}
#' }
"loxostomina"

#' Nonionella size data
#'
#' A `data.frame` containing the size and other attributes of species from genus \emph{Nonionella}.
#'  The variables are as follows:
#'
#' @format A `data.frame` with 208 rows and 7 variables:
#' \describe{
#'   \item{species}{species of genus Nonionella from which raw data were gathered}
#'   \item{ind}{number of individuals by species}
#'   \item{d_one}{test minor diameter in um (93.39--564.98)}
#'   \item{h}{test height in um (19.50--294.00)}
#'   \item{d_two}{test major diameter in  um (82.99--547.67)}
#'   \item{area}{test surface area in um2 (5783--222371)}
#'   \item{pco}{percent cell occupancy of the test (15.51--100)}
#' }
"nonionella"

#' Patellina size data
#'
#' A `data.frame` containing the size and other attributes of species from genus \emph{Patellina}.
#'  The variables are as follows:
#'
#' @format A `data.frame` with 79 rows and 6 variables:
#' \describe{
#'   \item{ind}{number of individuals by species}
#'   \item{h}{test height in um (24--66)}
#'   \item{d_one}{test minor diameter in um (84.84--247.46)}
#'   \item{radius}{test radius in  um (42.42--123.73)}
#'   \item{area}{test surface area in um2 (5653--87838)}
#'   \item{pco}{percent cell occupancy of the test (9.94--100)}
#' }
"patellina"

#' Quinqueloculina size data
#'
#' A `data.frame` containing the size and other attributes of species from genus \emph{Quinqueloculina}. As this genus has a wide morphological variation, two types of models can be applied to calculate its volume. Therefore the variables are grouped by model as well as by species. It is also an example that the \code{model} argument can be a `data.frame` column.
#'  The variables are as follows:
#'
#' @format A `data.frame` with 688 rows and 10 variables:
#' \describe{
#'   \item{species}{species of genus Quinqueloculina from which raw data were gathered}
#'   \item{ind}{number of individuals by species}
#'   \item{h}{test height in um (107.4--944.4)}
#'   \item{d_one}{test minor diameter in um (26.94--389.73)}
#'   \item{d_two}{test major diameter in  um (18.52--662.23)}
#'   \item{area}{test surface area in um2 (6294--431271)}
#'   \item{pco}{percent cell occupancy of the test (13.76--100)}
#'   \item{length}{test length in um (44.02--609.00)}
#'   \item{width}{test width in um (83.72--688.32)}
#'   \item{model}{geometric model ('10hl'--'17fs')}
#' }
"quinqueloculina"


#' Rectocibicides size data
#'
#' A `data.frame` containing the size and other attributes of genus \emph{Rectocibicides}.
#'  The variables are as follows:
#'
#' @format A `data.frame` with 199 rows and 4 variables:
#' \describe{
#'   \item{ind}{number of individuals}
#'   \item{h}{test height in um (22.00--101.50)}
#'   \item{area}{test surface area in um2 (3450--674280)}
#'   \item{pco}{percent cell occupancy of the test (10--100)}
#'
#' }
"rectocibicides"

#' Spirillina size data
#'
#' A `data.frame` containing the size and other attributes of species from genus \emph{Spirillina}.
#'  The variables are as follows:
#'
#' @format A `data.frame` with 42 rows and 6 variables:
#' \describe{
#'   \item{ind}{number of individuals by species}
#'   \item{h}{test height in um (12--44)}
#'   \item{d_one}{test minor diameter in um (73.71--105.14)}
#'   \item{radius}{test radius in  um (36.86--123.73)}
#'   \item{area}{test surface area in um2 (4268--34730)}
#'   \item{pco}{percent cell occupancy of the test (7.10--100)}
#' }
"spirillina"

#' Textularia size data
#'
#' A `data.frame` containing the size and other attributes of species from genus \emph{Textularia}.
#'  The variables are as follows:
#'
#' @format A `data.frame` with 84 rows and 7 variables:
#' \describe{
#'   \item{species}{species of genus Textularia from which raw data were gathered}
#'   \item{ind}{number of individuals by species}
#'   \item{h}{test height in um (113.6--1034.4)}
#'   \item{d_one}{test minor diameter in um (40--306)}
#'   \item{d_two}{test major diameter in  um (103.8--552.6)}
#'   \item{area}{test surface area in um2 (8235--336929)}
#'   \item{pco}{percent cell occupancy of the test (10.46--100)}
#' }
"textularia"
