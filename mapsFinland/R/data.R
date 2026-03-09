#'
#' Make map of Finland
#'
#' @param url url of shapefile (character)
#' @param dir folder of shape file (character)
#' @return sf object
#' @note url must contain WSF Shapefile. Check available maps from http://geo.stat.fi/geoserver/web/
#' @export
#' @examples
#' hv.alueet<-mkMapFinland(url="https://tinyurl.com/hvalueet2022")
#' require(ggplot2)
#' ggplot(hv.alueet) +geom_sf()+ggtitle("Wellbeing services counties")
mkMapFinland<-function(url,dir)
{
  lv.temp <- tempfile()
  lv.tempd <- tempdir()
  if(missing(dir)){
    lv.dir<-paste0(lv.tempd,"/lvfinmapddir")

    utils::download.file(url=url,lv.temp, mode="wb")
    utils::unzip(lv.temp,exdir = lv.dir)
    lv.1<-sf::st_read(dsn=lv.dir, quiet=TRUE)
    unlink(lv.dir,recursive = TRUE)
    names(lv.1)<-tolower(names(lv.1))
  }
  else{
    lv.1<-sf::st_read(dsn=dir, quiet=TRUE)
    names(lv.1)<-tolower(names(lv.1))

  }
  # lv.1$nimi<-stringi::stri_enc_toascii(lv.1$nimi)
  # lv.1$namn<-stringi::stri_enc_toascii(lv.1$namn)
  # lv.1$name<-stringi::stri_enc_toascii(lv.1$name)
  lv.1
}

#' @title Map of Finland with welfare counties (hyvinvointialueet) boundaries
#'
#' @description A data set containing map of Finland with welfare counties (hyvinvointialueet) boundaries
#'
#' @format A data frame with map boundaries:
#' \describe{
#'   \item{ vuosi}{ Year }
#'   \item{ hyvinvoint}{ Code }
#'   \item{ nimi}{ Name in Finnish}
#'   \item{ namn}{ Name in Swedish}
#'   \item{ name }{ Name in English}
#'   \item{geometry}{Map polygon}
#' }
#' @source <http://geo.stat.fi/geoserver/>
"hyvinvointialueet2023"

#' @title Map of Finland with district (seutukunta) boundaries
#'
#' @description A data set containing map of Finland with district (seutukunta) boundaries
#'
#' @format A data frame with map boundaries:
#' \describe{
#'   \item{ vuosi}{ Year }
#'   \item{ seutukunta}{ Code }
#'   \item{ nimi}{ Name in Finnish}
#'   \item{ namn}{ Name in Swedish}
#'   \item{ name }{ Name in English}
#'   \item{geometry}{Map polygon}
#' }
#' @source <http://geo.stat.fi/geoserver/>
"seutukunnat2019"

#' @title Map of Finland with parishes (kunta) boundaries
#'
#' @description A data set containing map of Finland with parishes (kunta) boundaries
#'
#' @format A data frame with map boundaries:
#' \describe{
#'   \item{ vuosi}{ Year }
#'   \item{ kunta}{ Code }
#'   \item{ nimi}{ Name in Finnish}
#'   \item{ namn}{ Name in Swedish}
#'   \item{ name }{ Name in English}
#'   \item{geometry}{Map polygon}
#' }
#' @source <http://geo.stat.fi/geoserver/>
"kunnat2019"
#' @title A data set containing map of Finland with county (maakunta) boundaries
#' @description A data set containing map of Finland with county (maakunta) boundaries
#'
#' @format A data frame with map boundaries:
#' \describe{
#'   \item{ vuosi}{ Year }
#'   \item{ maakunta}{ Code }
#'   \item{ nimi}{ Name in Finnish}
#'   \item{ namn}{ Name in Swedish}
#'   \item{ name }{ Name in English}
#'   \item{  til_vuosi }{ til_vuosi}
#'   \item{ vaesto  }{ vaesto}
#'   \item{  vaesto_p }{vaesto_p }
#'   \item{   miehet}{ miehet}
#'   \item{  miehet_p }{ miehet_p}
#'   \item{  naiset }{naiset }
#'   \item{  naiset_p }{ naiset_p}
#'   \item{  ika_0_14 }{ ika_0_14}
#'   \item{   ika_0_14p}{ ika_0_14p}
#'   \item{  ika_15_64 }{ ika_15_64}
#'   \item{  ika_15_64p }{ ika_15_64p}
#'   \item{  ika_65_ }{ ika_65_}
#'   \item{   ika_65_p}{ ika_65_p}
#'   \item{geometry}{Map polygon}
#' }
#' @source <http://geo.stat.fi/geoserver/>
"maakunta2019"

#' @title A data set containing map of Finland with regiona (suuralue) boundaries
#' @description A data set containing map of Finland with regiona (suuralue) boundaries
#'
#' @format A data frame with map boundaries:
#' \describe{
#'   \item{ vuosi}{ Year }
#'   \item{ suuralue}{ Code }
#'   \item{ nimi}{ Name in Finnish}
#'   \item{ namn}{ Name in Swedish}
#'   \item{ name }{ Name in English}
#'   \item{  til_vuosi }{ til_vuosi}
#'   \item{ vaesto  }{ vaesto}
#'   \item{  vaesto_p }{vaesto_p }
#'   \item{   miehet}{ miehet}
#'   \item{  miehet_p }{ miehet_p}
#'   \item{  naiset }{naiset }
#'   \item{  naiset_p }{ naiset_p}
#'   \item{  ika_0_14 }{ ika_0_14}
#'   \item{   ika_0_14p}{ ika_0_14p}
#'   \item{  ika_15_64 }{ ika_15_64}
#'   \item{  ika_15_64p }{ ika_15_64p}
#'   \item{  ika_65_ }{ ika_65_}
#'   \item{   ika_65_p}{ ika_65_p}
#'   \item{geometry}{Map polygon}
#' }
#' @source <http://geo.stat.fi/geoserver/>
"suuralue2019"
#' @title A data set containing map of Finland with commuter region (työssäkäyntialue) boundaries
#' @description A data set containing map of Finland with commuter region (työssäkäyntialue) boundaries
#'
#' @format A data frame with map boundaries:
#' \describe{
#'   \item{ vuosi}{ Year }
#'   \item{ tkalue}{ Code }
#'   \item{ nimi}{ Name in Finnish}
#'   \item{ namn}{ Name in Swedish}
#'   \item{ name }{ Name in English}
#'   \item{geometry}{Map polygon}
#' }
#' @source <http://geo.stat.fi/geoserver/>
"tyossakayntialue2019"

#' @title A data set containing area codes
#' @description A data set containing area codes
#'
#' @format A data frame :
#' \describe{
#'   \item{ Kuntanumero}{Kuntanumero}
#'   \item{ Kunnannimi}{Kunnannimi}
#'   \item{ Ruotsinkielilinennimi}{Ruotsinkielilinennimi}
#'   \item{ Manner-SuomijaAhvenanmaa}{Manner-SuomijaAhvenanmaa}
#'   \item{ NUTS2-koodi}{NUTS2-koodi}
#'   \item{ NUTS2-alueennimisuuralue}{NUTS2-alueennimisuuralue}
#'   \item{ AVI-nro}{AVI-nro}
#'   \item{ AVI-nimi}{AVI-nimi}
#'   \item{ Ely15nro}{Ely15nro}
#'   \item{ Ely15-nimi}{Ely15-nimi}
#'   \item{ Ely9nro}{Ely9nro}
#'   \item{ Ely9-nimi}{Ely9-nimi}
#'   \item{ Maakuntanro}{Maakuntanro}
#'   \item{ Maakunnannimi}{Maakunnannimi}
#'   \item{ Seutukuntanro}{Seutukuntanro}
#'   \item{ Seutukunnannimi}{Seutukunnannimi}
#'   \item{ Kuntamuotokoodi}{Kuntamuotokoodi}
#'   \item{ Kuntamuoto}{Kuntamuoto}
#' }
#' @source <http://geo.stat.fi/geoserver/>
"Alueluokat_ja_kuntanumerot_2019"
#' @title A data set containing map of Finland with hospital district (sairaanhoitopiiri) boundaries
#' @description A data set containing map of Finland with hospital district (sairaanhoitopiiri) boundaries
#'
#' @format A data frame with map boundaries:
#' \describe{
#'   \item{ sp_name}{ Name of hospital district}
#'   \item{ sp_code }{ Code of hospital district}
#'   \item{geom}{Map polygon}
#' }
#' @source <http://geo.stat.fi/geoserver/>
"SHP2019"
#'
#' @title A data set containing map of Finland with ERVA (erityisvastuualue) boundaries
#' @description A data set containing map of Finland with ERVA (erityisvastuualue) boundaries
#'
#' @format A data frame with map boundaries:
#' \describe{
#'   \item{ erva_name}{ Name of erityisvastuualue district}
#'   \item{ erva_code }{ Code of erityisvastuualue district}
#'   \item{geom}{Map polygon}
#' }
#' @source <http://geo.stat.fi/geoserver/>
"erva2019"
#'
#' @title A data set containing map of Finland with urbanisation category boundaries
#' @description A data set containing map of Finland with with urbanisation category boundaries
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' ggplot(taajama2019) +geom_sf(aes(fill = kr_name))+
#' labs(fill = "Kaupunkimaisuus")+
#'   theme(legend.title = element_text(size = 6),legend.text = element_text(size=6),
#'   legend.key.size = unit(10, "pt"),legend.key.width = unit(4,"pt"),
#'   legend.key.height = unit(4,"pt"),
#'    legend.position = c(0.22,0.52))
#'    }
#' @format A data frame with map boundaries:
#' \describe{
#'   \item{ kr_name}{ Name of urbanisation category}
#'   \item{ kr_code }{ Code of urbanisation category}
#'   \item{geom}{Map polygon}
#' }
#' @source <http://geo.stat.fi/geoserver/>
"taajama2019"
#'
#' @title A data set containing map of Finland with voting district (vaalipiiri) boundaries on 2019
#' @description A data set containing map of Finland with voting district (vaalipiiri) boundaries
#'
#' @format A data frame with map boundaries:
#' \describe{
#'   \item{ vaalipiiri}{ Name of voting district (vaalipiiri)}
#'   \item{ vuosi }{ Year}
#'   \item{ nimi }{ nimi}
#'   \item{ namn }{ namn}
#'   \item{ name }{ name}
#'   \item{geometry}{Map polygon}
#' }
#' @source <http://geo.stat.fi/geoserver/>
"vaalipiiri2019"
