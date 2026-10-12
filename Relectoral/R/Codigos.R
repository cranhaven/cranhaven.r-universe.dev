#########################
# ID codes of the Spanish Autonomous Communities
# They are the codes that the Ministry of Home Office (MIR) uses
##############################
#'@title Utilities. ID codes of the Spanish Autonomous Communities
#'
#'@description With this function you can obtain the ID codes of the Autonomous Communities
#'of Spain used by the Ministry of Home Affairs to code their data. This codification [can also be seen in this link](https://github.com/Miguelro/Electoral/blob/master/Diseno_Registros_MIR/Diseno_Registros.pdf).
#'It should be noted that this coding system does not fully coincide with the
#'[Autonomous Community codes of Spanish National Institute of Statistics](https://www.ine.es/daco/daco42/codmun/cod_ccaa.htm).
#'This function does not take any parameters.
#'
#'@param ... No parameters are required for this function.
#'
#'@return Return a data.frame with two columns. The first column contains the ID code of the
#'Autonomous Community and the second field contains the name of that region.
#'
#'@examples
#'get_CCAA()
#'
#'
#'
#'@export
#'@md

get_CCAA <- function(...){
  codigos <- data.frame(
    cod=c("01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19"),
    nombre=c(
      "Andaluc\u00EDa",
      "Arag\u00F3n",
      "Asturias",
      "Baleares",
      "Canarias",
      "Cantabria",
      "Castilla-La Mancha",
      "Castilla y Le\u00F3n",
      "Catalu\u0169a",
      "Extremadura",
      "Galicia",
      "Madrid",
      "Navarra",
      "Pa\u00EDs Vasco",
      "Regi\u00F3n de Murcia",
      "La Rioja",
      "Comunidad Valenciana",
      "Ceuta",
      "Melilla"
    )
  )
  return(codigos)
}
#get_CCAA()

#'@title Utilities. ID codes of the provinces of Spain
#'
#'@description With this function, the name of all the Spanish provinces is obtained,
#'along with their ID codes, which are the same as those provided by the Spanish National
#'Institute of Statistics [on this website](https://www.ine.es/daco/daco42/codmun/cod_provincia.htm).
#'This function does not take any parameters.
#'
#'@param ... No parameters are required for this function.
#'
#'@return It returns a data.frame with two columns. The first column contains the ID code of the
#'province and the second column contains the name of the province.
#'
#'@examples
#'get_Provincias()
#'
#'
#'
#'@export
#'@md


get_Provincias <- function(...){
  codigos <- data.frame(
  cod=c("01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19",
        "20","21","22","23","24","25","26","27","28","29",
        "30","31","32","33","34","35","36","37","38","39",
        "40","41","42","43","44","45","46","47","48","49",
        "50","51","52"),
  nombre=c("\u00C1lava","Albacete","Alicante","Almer\u00EDa",
           "\u00C1vila","Badajoz","Baleares, Illes","Barcelona",
           "Burgos","C\u00E1ceres","C\u00E1diz",
           "Castell\u00F3n","Ciudad Real","C\u00F3rdoba","Coru\u0169a, A",
           "Cuenca","Girona","Granada","Guadalajara","Gipuzkoa",
           "Huelva","Huesca","Ja\u00E9n","Le\u00F3n","Lleida","Rioja, La","Lugo",
           "Madrid","M\u00E1laga","Murcia","Navarra","Ourense","Asturias",
           "Palencia","Palmas, Las","Pontevedra",
           "Salamanca","San. Cr. Tenerife","Catabria",
           "Segovia","Sevilla","Soria","Tarragona",
           "Teruel","Toledo","Valencia","Valladolid",
           "Bizkaia","Zamora","Zaragoza",
           "Ceuta","Melilla")
  )
  return(codigos)
}

get_Provincias()

