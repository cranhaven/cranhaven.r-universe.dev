### Functions that implement the capability of reading data from the Spanish Ministry of Home Office (MIR)
##if (!require("readxl")) install.packages("readxl");require("readxl")

### Reading of aggregated data at municipal level ####

#'@title Download.  Aggregated data at the municipal level obtained from the MIR (Spanish Ministry of Home Office)
#'
#'
#'@description  This function serves to download the data in Excel format at the municipal level and
#'creates a data.frame in your R environment with that information. Note: The fields returned are all of character type,
#'so if you need to perform operations with them, you will have to convert the necessary fields to numeric.
#'
#'@param Ano The election year in four-digit format (YYYY). Can be numeric or text.
#'@param Mes The election month. It has to be of character format. For example "06", corresponding to June.
#'@param Tipo The type of elections to which the data to be downloaded correspond: Congress (you must pass to the function the word "Congreso") or European (you must pass to the function the word "Europeas").
#'@param Ruta It is the local path from your computer where the zip files from the Spanish Ministry of Home Office (MIR) will be dowloaded.
#'@param Borrar It is a Boolean variable that indicates whether or not the downloaded files are deleted after creating the data.frame in R.
#'
#'@return Object of type 'tbl_df' with the data at municipal level.
#'
#'@import readxl
#'@import utils
#'
#'@examples
#'c<-Agregado_Mun_MIR(1989,"06",Tipo = "Europeas","F:/")
#'
#'
#'@export
Agregado_Mun_MIR<-function(Ano,Mes,Tipo,Ruta,Borrar=TRUE){
  out <-tryCatch(
    {
      temp<-paste0(Ruta,"temp.zip")
      url1 <- "http://www.infoelectoral.mir.es/infoelectoral/docxl/"
      if(Tipo=="Congreso"){
        url <- paste0(url1,"02_",as.character(Ano),Mes,"_1.zip")
        fic<-paste0(Ruta,"02_",as.character(Ano),Mes,"_1.xlsx")
      }else if(Tipo=="Europeas"){
        url <- paste0(url1,"07_",as.character(Ano),Mes,"_1.zip")
        fic<-paste0(Ruta,"07_",as.character(Ano),Mes,"_1.xlsx")
      }else{
        stop("The type of elections chosen is not correct. You can only choose between 'Congreso' (Congress) or 'Europeas' (European)")
      }

      download.file(url,temp)
      unzip(zipfile=temp, exdir=Ruta)
      dat <- read_xlsx(fic,col_names = TRUE,skip = 5)
      if(Borrar){
        unlink(temp)
        unlink(fic)
      }
      return(dat)
    },
    warning = function(cond){
      message("A warning has been generated, the path may not exist or cannot be written to")
      message("The warning message is the following:")
      message(cond)
      return(NULL)
    },
    error= function(cond){
      message("The path provided may not exist or cannot be written to")
      message("The error message is the following:")
      message(cond)
      return(NA)
    }
  )
  return(out)
}
# Suprimo los mensajes de esta función
#suppressMessages(Agregado_Mun_MIR())

#c<-Agregado_Mun_MIR(1989,"06",Tipo = "Europeas","F:/",Borrar = TRUE)

#### Reading of aggregate data at the provincial level #######

#'@title Download. Reading of aggregate data at the provincial level obtained from the MIR (Spanish Ministry of Home Office)
#'
#'
#'@description This function is used to download the data in Excel format at the
#'provincial level and upload a data.frame with that information into R. Note: The
#'returned fields are all of character format, so if you need to perform
#'operations, you will have to convert the required fields to numeric.
#'
#'@param Ano The election year in four-digit format (YYYY). Can be numeric or text.
#'@param Mes The election month. It has to be of character format. For example "06", corresponding to June.
#'@param Tipo The type of elections to which the data to be downloaded correspond: Congress (you must pass to the function the word "Congreso") or European (you must pass to the function the word "Europeas").
#'@param Ruta It is the local path from your computer where the zip files from the Spanish Ministry of Home Office (MIR) will be dowloaded.
#'@param Borrar It is a Boolean variable that indicates whether or not the downloaded files are deleted after creating the data.frame in R.
#'
#'@return Object of type 'tbl_df' with the data at provincial level.
#'
#'
#'@examples
#'
#'c<-Agregado_Prov_MIR(2019,"05",Tipo = "Europeas","F:/")
#'
#'
#'@export

Agregado_Prov_MIR<-function(Ano,Mes,Tipo,Ruta,Borrar=TRUE){
  out <-tryCatch(
    {
      temp<-paste0(Ruta,"temp.zip")
      url1 <- "http://www.infoelectoral.mir.es/infoelectoral/docxl/"
      if(Tipo=="Congreso"){
        url <- paste0(url1,"PROV_02_",as.character(Ano),Mes,"_1.zip")
        fic<-paste0(Ruta,"PROV_02_",as.character(Ano),Mes,"_1.xlsx")
      }else if(Tipo=="Europeas"){
        url <- paste0(url1,"PROV_07_",as.character(Ano),Mes,"_1.zip")
        fic<-paste0(Ruta,"PROV_07_",as.character(Ano),Mes,"_1.xlsx")
      }else{
        stop("The type of elections chosen is not correct. You can only choose between 'Congreso' (Congress) or 'Europeas' (European)")
      }

      download.file(url,temp)
      unzip(zipfile=temp, exdir=Ruta)
      #En el año 2015 los datos comienzan una linea más abajo.POr eso incluyo esto
      #Generaba un error
      if(as.integer(Ano)==2015){
        dat <- read_xlsx(fic,col_names = FALSE,skip = 5)
      } else{
        dat <- read_xlsx(fic,col_names = FALSE,skip = 4)
      }

      if(Tipo %in% c("Congreso","Europeas" )){
        n<-ncol(dat)
        for(i in 1:n){
          if(trimws(dat[2,i])=='Votos'){
            dat[2,i]=paste0("V_",dat[1,i])
          }else if(trimws(dat[2,i])=='Diputados'){
            dat[2,i]=paste0("D_",dat[1,i-1])
          }
        }
        # Pongo nombres a las columnas
        colnames(dat)<-dat[2,]
        #Quito las dos primeras filas
        dat<-dat[-c(1,2),]
      }

      if(Borrar){
        unlink(temp)
        unlink(fic)
      }
      return(dat)
    },
    warning = function(cond){
      message("A warning has been generated, the path may not exist or cannot be written to")
      message("The warning message is the following:")
      message(cond)
      return(NULL)
    },
    error= function(cond){
      message("The path provided may not exist or cannot be written to")
      message("The error message is the following:")
      message(cond)
      return(NA)
    }
  )
  return(out)
}
# Suprimo los mensajes de esta función
#suppressMessages(Agregado_Prov_MIR())
#c<-Agregado_Prov_MIR(2019,"05",Tipo = "Europeas","G:/")


#### Lectura datos desagregados #####

#'@title Download. Disaggregated data at polling station level obtained from the MIR (Spanish Ministry of Home Office)
#'
#'
#'@description This function serves to download the data in Excel format at polling station
#'level and then uploads a data.frame with that information into R. Note: The
#'returned fields are all of character format, so if you need to perform numeric
#'operations, you will have to convert the required fields to numeric.
#'
#'@param Ano The election year in four-digit format (YYYY). Can be numeric or text.
#'@param Mes The election month. It has to be of character format. For example "06", corresponding to June.
#'@param Tipo The type of elections to which the data to be downloaded correspond: Congress (you must pass to the function the word "Congreso") or European (you must pass to the function the word "Europeas").
#'@param Ruta It is the local path from your computer where the zip files from the Spanish Ministry of Home Office (MIR) will be dowloaded.
#'@param Borrar It is a Boolean variable that indicates whether or not the downloaded files are deleted after creating the data.frame in R.
#'
#'@return Object of type 'tbl_df' with the data at polling station level.
#'
#'
#'@examples
#'
#'c2<-Desagregados_Mesa_Mir(2019,"04",Tipo = "Congreso", Ruta = "F:/",Borrar = TRUE)
#'
#'
#'@export
Desagregados_Mesa_Mir<-function(Ano,Mes,Tipo,Ruta,Borrar=TRUE ){
  out <-tryCatch(
    {
        temp<-paste0(Ruta,"temp.zip")
        url1 <- "http://www.infoelectoral.mir.es/infoelectoral/docxl/apliextr/"
        if(Tipo=="Congreso"){
          url <- paste0(url1,"02",as.character(Ano),Mes,"_MESA.zip")
          #fic<-paste0(Ruta,"PROV_02_",as.character(Ano),Mes,"_1.xlsx")
        }else if(Tipo=="Europeas"){
          url <- paste0(url1,"07",as.character(Ano),Mes,"_MESA.zip")
          #fic<-paste0(Ruta,"PROV_07_",as.character(Ano),Mes,"_1.xlsx")
        }else{
          stop("The type of elections chosen is not correct. You can only choose between 'Congreso' (Congress) or 'Europeas' (European)")
        }

        download.file(url,temp)
        if(Tipo == "Congreso"){
          ti<-"02"
        }else if(Tipo == "Europeas"){
          ti<-"07"
        }
        message("Generating file dat01")
        dat01<-read.fwf(unz(temp,paste0("01",ti,substr(Ano,3,4),Mes,".DAT")),
                      widths = c(2,4,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1),
                      col.names = c(
                        "Tipo","Ano","Mes","Nvuelta","F01","F02","F03",
                        "F04","F05","F06","F07","F08","F09","F10",
                        "F11","F12","F0510","F0610","F0710","F0810"
                      ))
        message("Generating file dat02")

        dat02<-read.fwf(unz(temp,paste0("02",ti,substr(Ano,3,4),Mes,".DAT")),
                        widths = c(2,4,2,1,1,2,2,2,4,5,5,5,5),
                        col.names = c(
                          "Tipo","Ano","Mes","Nvuelta","Tambito",
                          "Ambito","Dia","Mes2","Ano2","HoraAp",
                          "HoraC","HoraAvan1","HoraAvan2"
                        ))
        message("Generating file dat03")

        dat03<-read.fwf(unz(temp,paste0("03",ti,substr(Ano,3,4),Mes,".DAT")),
                        widths = c(2,4,2,6,50,150,6,6,6),
                        col.names = c("Tipo","Ano","Mes","CodCan",
                                      "Sigla","Denominacion","Cod_Prov",
                                      "Cod_CCAA","Cod_Nacion"
                        ))
        message("Generating file dat04")

        dat04<-read.fwf(unz(temp,paste0("04",ti,substr(Ano,3,4),Mes,".DAT")),
                        widths = c(2,4,2,1,2,1,3,6,3,1,25,25,25,1,2,2,4,10,1),
                        col.names = c(
                          "Tipo","Ano","Mes","Nvuelta","Cprov","Cdist",
                          "Cmun","CodCan","Norden","Tipo","Nombre","Ape1","Ape2",
                          "Sexo","NaciDia","NaciMes","NaciAno","Dni","Elegido"
                        ))
        message("Generating file dat05")

        dat05<-read.fwf(unz(temp,paste0("05",ti,substr(Ano,3,4),Mes,".DAT")),
                        widths = c(2,4,2,1,2,2,3,2,100,1,3,3,3,8,5,8,
                                   8,8,8,8,8,8,8,8,3,8,8,1),
                        col.names = c(
                          "Tipo","Ano","Mes","Nvuelta","Cccaa","Cprov","Cmun",
                          "Cdist","Cnombre","Cdist2","Cpj","Cdipu","Ccomarca",
                          "Poblacion","Nmesas","CensoINE","CensoEscrutinio",
                          "CERE","TvCERE","Vavan1","Vavan2","Vblanco","Vnulos",
                          "Vcandidaturas","Nescanos","Vsi","Vno","Doficiales"
                        ))
        message("Generating file dat06")

        dat06<-read.fwf(unz(temp,paste0("06",ti,substr(Ano,3,4),Mes,".DAT")),
                        widths = c(2,4,2,1,2,3,2,6,8,3),
                        col.names = c(
                          "Tipo","Ano","Mes","Nvuelta","Cprov","Cmun",
                          "Cdist","CodCan","VotosCand","Ncandi"
                        ))
        message("Generating file dat07")

        dat07<-read.fwf(unz(temp,paste0("07",ti,substr(Ano,3,4),Mes,".DAT")),
                        widths = c(2,4,2,1,2,2,1,50,8,5,8,8,8,8,8,8,8,8,8,
                                   6,8,8,1),
                        col.names = c(
                          "Tipo","Ano","Mes","Nvuelta","Cccaa","Cprov",
                          "Cdist","NomAmbito",
                          "Poblacion","Nmesas","CensoINE","CensoEscrutinio",
                          "CERE","TvCERE","Vavan1","Vavan2","Vblanco","Vnulos",
                          "Vcandidaturas","Nescanos","Vsi","Vno","Doficiales"
                        ))
        message("Generating file dat08")

        dat08<-read.fwf(unz(temp,paste0("08",ti,substr(Ano,3,4),Mes,".DAT")),
                        widths = c(2,4,2,1,2,2,1,6,8,5),
                        col.names = c(
                          "Tipo","Ano","Mes","Nvuelta","Cccaa",
                          "Cprov","Cdist","CodCan","Votos","Ncand"
                        ))
        message("Generating file dat09")

        dat09<-read.fwf(unz(temp,paste0("09",ti,substr(Ano,3,4),Mes,".DAT")),
                        widths = c(2,4,2,1,2,2,3,2,4,1,7,7,7,7,7,7,7,7,
                                   7,7,7,1),
                        col.names = c(
                          "Tipo","Ano","Mes","Nvuelta","Cccaa",
                          "Cprov","Cmun","Cdist","Csecc","Cmesa","Tcenso",
                          "CERA","CERE","VotantesCERE","Vavan1","Vavan2",
                          "Vblanco","Vnulos","Vcandidaturas","Vsi","Vno","Oficiales"
                        ))

        message("Generating file dat10 (it might take a while)")

        dat10<-read.fwf(unz(temp,paste0("10",ti,substr(Ano,3,4),Mes,".DAT")),
                        widths = c(2,4,2,1,2,2,3,2,4,1,6,7),
                        col.names = c(
                          "Tipo","Ano","Mes","Nvuelta","Cccaa",
                          "Cprov","Cmun","Cdist","Csecc","Cmesa","CodCan","Tvotos"
                        ))

        final<-list("dat01"=dat01,"dat02"=dat02,"dat03"=dat03,"dat04"=dat04,
                    "dat05"=dat05,"dat06"=dat06,"dat07"=dat07,"dat08"=dat08,
                    "dat09"=dat09,"dat10"=dat10)
        return(final)
        if(Borrar){
          # Borro ficheros
          unlink(temp)
        }
      },
      warning = function(cond){
        message("A warning has been generated, the path may not exist or cannot be written to")
        message("The warning message is the following:")
        message(cond)
        return(NULL)
      },
      error= function(cond){
        message("The path provided may not exist or cannot be written to")
        message("The error message is the following:")
        message(cond)
        return(NA)
      }
      )
  return(out)
}
# Suprimo los mensajes de esta función
#suppressMessages(Desagregados_Mesa_Mir())

#c2<-Desagregados_Mesa_Mir(2019,"04",Tipo = "Congreso", Ruta = "G:/",Borrar = TRUE)

