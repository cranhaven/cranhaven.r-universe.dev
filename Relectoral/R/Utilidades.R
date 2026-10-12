#This file contains a series of utility functions.
#'@title Utilities. Get data for Bochsler method (2010)
#'
#'@section Utilities
#'
#'@description The standardised party nationalisation index developed by Bochsler (2010)
#'can be calculated using an excel spreadsheet, which can be downloaded from the following
#'website: [https://www.bochsler.eu/pns/](https://www.bochsler.eu/pns/). This Excel file
#'contains a macro to calculate the match nationalisation index, which is widely used
#'nowadays. The aim of this function is to create an Excel sheet that
#'is directly loaded with the data provided by the Spanish Ministry of the Interior (MIR),
#'[http://www.infoelectoral.mir.es/](http://www.infoelectoral.mir.es/).
#'This data is arranged in such a way that it is only necessary to copy and paste it into
#'the macro created by Bochsler. This makes it much easier to load the macro function with
#'data. In the Excel sheet that is created, the last row contains the totals that are the
#'ones to be placed in row 8 of Daniel Bochsler's macro function.
#'
#'@param Ano This value corresponds to the year of the data that the user wants to download.
#'This data can be numerical or character (numerical is preferred).
#'@param Mes This is the month in which the election was held. It must be a character
#'with two digits, i.e. "04" is valid but "4" is not.
#'@param RutaDescarga A path to the local hard disk where the Ministry of the Interior's Excel
#'file will be downloaded must be indicated. Once the download is completed the downloaded
#'file will be automatically deleted.
#'@param RutaSalida It is a path of the local hard disk where the Excel sheet generated with
#'the data will be deposited to feed Daniel Bochsler's macro. The resulting Excel workbook
#'is called 'Bochsler.xlsx'.
#'
#'@return With this function you obtain an Excel sheet called 'Bochsler.xlsx' placed in the
#'route that has been indicated with the parameter 'RutaSalida'.
#'
#'@import xlsx
#'
#'@examples
#'
#'Bochsler(2019,"04","F:/","F:/")
#'Bochsler(2016,"06","F:/","F:/")
#'
#'@export
#'@md
#if (!require("xlsx")) install.packages("xlsx"); require("xlsx")
Bochsler <- function(Ano,Mes,RutaDescarga,RutaSalida){
  out <-tryCatch(
    {
      if( class(Mes) != "character") stop("The month must be a character")
      if(nchar(Mes) != 2) stop("The month parameter must contain 2 characters")
      if(is.null(RutaDescarga)) stop("Some value must be provided for the route 'RutaDescarga'")
      if(is.null(RutaSalida)) stop("Some value must be provided for the route 'RutaSalida'")
      # Creo el libro excel

      wb<-createWorkbook(type="xlsx")
      CellStyle(wb, dataFormat=NULL, alignment=NULL,
                border=NULL, fill=NULL, font=NULL)
      sheet <- createSheet(wb, sheetName = "Datos")
      # Descargo los datos del MIR
      datos_MIR<-suppressMessages(Agregado_Prov_MIR(Ano,Mes,"Congreso",RutaDescarga,Borrar=T))
      if(is.null(datos_MIR) || is.na(datos_MIR)){
        return(NULL)
      }
      # Elimino la fila de los totales,
      datos_MIR <-datos_MIR[1:52,]
      # Tengo que sacar la columna de votos válidos
      if(as.integer(Ano) %in% c(1977,1979,1982)){
        column = 8
      } else if(as.integer(Ano) %in% c(1986,1989,1993,1996,2000,2004,2008)){
        column=12
      } else {
        column=13
      }

      valid <- as.data.frame(datos_MIR[,column])
      valid <- as.data.frame(apply(valid,2,as.integer))
      # Mediante expresión regular selecciono las columnas que tienen el número de votos
      columnas_Votos<-grep("V_",colnames(datos_MIR))
      # Me quedo solo con las columna de votos
      datos_MIR_Votos<-as.data.frame(datos_MIR[,columnas_Votos])
      #Uno la columna del total votos
      datos_MIR_Votos<-cbind(valid,datos_MIR_Votos)

      datos_MIR_Votos <- as.data.frame(apply(datos_MIR_Votos,2,as.integer))
      #calculo los totales
      totales<-colSums(datos_MIR_Votos)
      # Añado los totales

      datos_MIR_Votos[53,]<-totales
      #Ordenos las columnas de mas a menos votos
      datos_MIR_Votos<-datos_MIR_Votos[,order(totales,decreasing = T),]

      # Añado la columna del nombre de la provincia
      provin <-datos_MIR[3]
      provin[53,1]<-"Totales"

      datos_MIR_Votos<-cbind(provin,datos_MIR_Votos)

      addDataFrame(datos_MIR_Votos,sheet,startRow=1, startColumn=1)
      saveWorkbook(wb,paste0(RutaSalida,"/Bochsler.xlsx"))
      message("Excel sheet called 'Bochsler.xlsx' created on the requested route")
      return(TRUE)
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

#Bochsler(2016,"06","F:/","F:/")


### Mathematical Injustice in provinces ###
#'@title Utilities. Disaggregated Mathematical Injustice Calculation
#'
#'@section Utilities
#'
#'@description The function of this same package called 'InjusticeM()',
#'allows to obtain the mathematical injustice for the dataset that is going
#'going to be used for the study. However, if we want to obtain the mathematical injustice for
#'different geographical areas, a significant effort and time is required
#'to prepare and execute the data. With this function, this process is
#'greatly expedited, if what we want to evaluate are results to the
#'Congress of Deputies in Spain (or its counterpart in other countries),
#'since thanks to this function, the information from the Ministry of
#'Interior can be automatically extracted and processed directly for
#'each province and national data, and the result is returned in a
#'list() type object, where each element corresponds to the result
#'obtained for each province or the total for the whole nation under study.
#'
#'Likewise, if you have another type of election, you can use the parameter
#'Auto=FALSE, and two data.frame will be provided with the characteristics
#'that will be indicated later. In this way the mathematical injustices
#'will be calculated for each region that is indicated in those data.frames
#'
#'@param Ano (Mandatory if Auto=TRUE)  It is the value of the year of the data
#'to be downloaded. This parameter can be numeric or character (better numeric).
#'@param Mes (Mandatory if Auto=TRUE) This is the month in which the election was
#'held. It must be a character with two digits, i.e. "04" is valid but "4" is not.
#'@param Ruta (Mandatory if Auto=TRUE) A path to the local hard disk where the data
#'from the Ministry of the Interior will be saved must be indicated. Once the download
#'is completed, the downloaded file will be automatically deleted.
#'@param Auto Its default value is TRUE and indicates that the procedure is automatic,
#'with another value the non-automatic process will be executed.
#'@param d_votos (Mandatory if Auto != TRUE). It is a data.frame with the first
#'column containing the name of the region, and then a column for each party
#'to be evaluated, which will contain the votes obtained in each region
#'under study.
#'@param d_escanos (Mandatory if Auto != TRUE).It is a data.frame with the first
#'column containing the name of the region, and then a column for each party to
#'be evaluated, which will contain the seats obtained in each region covered.
#'
#'@return The result is a list with 53 elements (for the automatic procedure), so
#'that each element is a matrix containing the data of the calculated mathematical
#'injustice. The 'names' in this list are those of Spanish provinces, or the
#'expression 'Total' if they are the data of all Spain.
#'When the procedure is not automatic, a list is also obtained containing a
#'number of elements equal to the number of regions provided plus one, as there
#'is an element called 'Total' which refers to all the territories provided as a whole.
#'
#'@examples
#'f<-InjusticiaM_desagregada(2019,"04","F:/") # Aggregated mode
#'
#'da1 <- data.frame( # Contains the votes
#'    Reg=c("Alava","Albacete","Madrid","Barcelona","Valladolid"),
#'      PSOE=c(400,300,0,50,25),
#'      PP=c(300,200,10,150,2),
#'      Cs=c(400,0,3,300,45),
#'      Uno=c(465,23,341,263,0))
#'
#'da2 <- data.frame( # Contains the seats
#'      Reg=c("Alava","Albacete","Madrid","Barcelona","Valladolid"),
#'      PSOE=c(4,3,0,0,0),
#'      PP=c(2,3,0,1,0),
#'      Cs=c(4,0,0,2,1),
#'       Uno=c(3,0,3,2,0))
#'
#'f2<- InjusticiaM_desagregada(Auto=FALSE,d_votos=da1,d_escanos = da2) #No agregegado
#'@export
InjusticiaM_desagregada<-function(Ano,Mes,Ruta,Auto=TRUE,d_votos,d_escanos){
  #Primero compruebo si Auto es TRUE
  if(Auto == TRUE){
    out <-tryCatch(
      {
        if( class(Mes) != "character") stop("The month must be a character")
        if(nchar(Mes) != 2) stop("The month parameter must contain 2 characters")
        if(is.null(Ruta)) stop("Some value must be provided for the route 'Ruta'")
        datos_MIR<-suppressMessages(Agregado_Prov_MIR(Ano,Mes,"Congreso",Ruta,Borrar=TRUE))
        if(is.null(datos_MIR) || is.na(datos_MIR)){
          return(NULL)
        }
        # Me quedo con los datos de cada provincia
        datos_MIR<-datos_MIR[1:52,]
        # Saco los nombre de las provincias
        Cprov<-datos_MIR[,3]
        # Añado en la ultima posición el epigrafe "Total"
        Cprov[53,]<-"Total"
        #me quedo con los votos
        # Mediante expresión regular selecciono las columnas que tienen el número de votos
        columnas_Votos<-grep("V_",colnames(datos_MIR))
        dat_votos<- as.data.frame(datos_MIR[,columnas_Votos])
        # paso los datos a numéricos
        dat_votos<-as.data.frame(apply(dat_votos,2,as.integer))
        #calculo los totales
        totales_voto<-colSums(dat_votos)
        #Añado ahora los votos totales
        dat_votos[53,]<-totales_voto
        #Hago lo mismo que antes pero con los escaños
        columnas_escanos<-grep("D_",colnames(datos_MIR))
        dat_escanos<-as.data.frame(datos_MIR[,columnas_escanos])
        # paso los datos a numéricos
        dat_escanos<-as.data.frame(apply(dat_escanos,2,as.integer))
        #calculo los totales
        totales_escanos<-colSums(dat_escanos)
        #Añado ahora los votos totales
        dat_escanos[53,]<-totales_escanos

        #Creo una lista donde almaceno los resultados.
        res<-list()
        # lo calculo para cada provincia
        for(i in 1:52){
          # Saco vector logico si el partido ha tenido votos o no en esa provincia
          chek<-dat_votos[i,]>0
          dat_votos_prov<-dat_votos[i,chek]
          dat_escanos_prov<-dat_escanos[i,chek]
          dat_in2<-data.frame(
            par=substr(colnames(dat_votos)[chek],3,nchar(colnames(dat_votos))[chek]),
            vot=as.numeric(dat_votos_prov),
            sea=as.numeric(dat_escanos_prov), stringsAsFactors = FALSE
          )

          res[[i]]<-InjusticiaM(dat_in2)*10000 # multiplico por esta cantidad para no sacar cifras muy bajas
        }
        #Ahora lo hago para el total nacional
        dat_in<-data.frame(
          par=substr(colnames(dat_votos),3,nchar(colnames(dat_votos))),
          vot=as.numeric(dat_votos[53,]),
          sea=as.numeric(dat_escanos[53,]),stringsAsFactors = FALSE
        )

        res[[53]]<-InjusticiaM(dat_in)*10000
        #Asigno los nombres de las provincias y el total
        names(res)<-as.data.frame(Cprov)[,1]
        return(res)
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
  } # Fin de AUTO == TRUE
  else{
        if(missing(d_votos)) stop("A data.frame must be provided with the voting data")
        if(missing(d_escanos)) stop("A data.frame must be provided with the seat data")
        if(class(d_votos) != "data.frame") stop("The parameter 'd_votos' must be a data.frame")
        if(class(d_escanos) != "data.frame") stop("The parameter 'd_escanos' must be a data.frame")
        # Compruebo que el numero de columnas de ambos ficheros es el mismo
        if(ncol(d_votos) != ncol(d_escanos)) stop("The number of columns in the two data.frames must be the same")
        # compruebo que el numero de filas es el mismo
        if(nrow(d_votos) != nrow(d_escanos)) stop("The number of rows in the two data.frame must be the same")


        #Comparo nombre columnas es el mismo en los dos data.frame
        a <- colnames(d_votos) == colnames(d_escanos)

        if(sum(a) != ncol(d_votos)) stop("The column names of the two data.frame must be the same")
        #Comparo que los nombres de las regiones son los mismo
        b <- as.character(d_votos[,1]) == as.character(d_escanos[,1])
        if(sum(b) != nrow(d_votos)) stop("The region names ('primera columa') of the two data.frame
                                          must be the same")

        #Hechas las comprobaciones comienzo el cálculo

        #Saco los votos totales
        T_votos <- colSums(d_votos[,-c(1)])
        #Añado la fila de los totales
        d_votos[,1] <- as.character(d_votos[,1])
        d_votos <- rbind(d_votos,c("Totales",T_votos))
        # hago lo mismo para los escaños
        T_escanos <- colSums(d_escanos[,-c(1)])
        #Añado la fila de los totales
        d_escanos[,1] <- as.character(d_escanos[,1])
        d_escanos <- rbind(d_escanos,c("Totales",T_escanos))


        #Creo una lista donde almaceno los resultados.
        res<-list()
        # Ahora calculo las injuticias matemáticas
        for( i in 1:nrow(d_escanos)){
          # Saco vector logico si el partido ha tenido votos o no en esa region
          chek<-d_votos[i,-c(1)]>0
          # Saco los nombres de los partidos con votos
          parti <- colnames(d_votos)[-c(1)][chek]
          #Saco los votos
          voto <- d_votos[i,-c(1)][chek]
          #Saco los escanos
          esc <- d_escanos[i,-c(1)][chek]
          #browser()
          #Genero el data.frame para pasarlo a la función InjusticiaM()
          dat_in2<-data.frame(
            par=parti,
            vot=as.integer(voto),
            sea=as.integer(esc), stringsAsFactors = FALSE
          )

          res[[i]]<-InjusticiaM(dat_in2)*10000 # multiplico por esta cantidad para no sacar cifras muy bajas

        } #fin del for
        names(res) <- d_votos[,1]
        return(res)
      }# Fin del else
}

### Generar ficheros .bazi ####

#'@title Utilities. Generation of *.bazi files
#'
#'@description BAZI is a freely available Java program that implements various
#'methods of distributing seats for proportional representation systems. It
#'offers the possibility of using divisor methods as well as quota methods.
#'The program can be downloaded from [https://www.math.uni-augsburg.de/htdocs/emeriti/pukelsheim/bazi/](https://www.math.uni-augsburg.de/htdocs/emeriti/pukelsheim/bazi/)
#'Instructions on its use can be found at the following webpage:
#'[https://opus.bibliothek.uni-augsburg.de/opus4/frontdoor/index/index/docId/601](https://opus.bibliothek.uni-augsburg.de/opus4/frontdoor/index/index/docId/601).
#'With this function you obtain flat ASCII files, with extension '.bazi' that
#'structures the data in the form that can be understood by BAZI program and
#'obtain the results that can be accessed with this application. This function
#'has been designed so that it can be used both automatically or manually, that is
#'to say, it will work autonomously if after giving the corresponding parameters
#'it connects to the databases of the Spanish Ministry of the Interior, downloads
#'the results and generates the file with extension ".bazi". The manual procedure,
#'is designed to be provided with the appropriate data and that this function is
#'responsible for transforming them to the format that BAZI understands to
#'generate the results for which it is programmed. Let's see below how to
#'proceed in each case.
#'
#'First of all, it should be pointed out that the "camino" parameter must be
#'provided both with the manual and the automatic procedure, and it should indicate
#'the path to be used to store the file with the ".bazi" extension that is
#'generated as a result of this process. It must also be taken into account
#'that a path with writing permission for R must be provided.
#'
#'The manual or automatic procedure is indicated by the "Auto" parameter of
#'the Bazi() function, so that if "TRUE" (default value) is valid, the automatic
#'procedure will be executed and otherwise the manual one.
#'
#'@param Ano . For automatic procedure only. This value indicates the year of the data
#'you want to download. This data can be numerical or of character type (better numeric)
#'@param Mes . For automatic procedure only. This value refers to the month in which the
#'election was held. It must be of character type with two digits always  i.e. "04" is valid but "4" is not.
#'@param camino . For manual and automatic procedure. You must indicate a local
#'hard disk path where you download the file from the Ministry of Interior (once
#'the process is finished it will be deleted automatically), and then the file will
#'be written with the extension '.bazi' ready to be used by BAZI. Please note that
#'this path must indicate a disk space with write permission so that the R program
#'can save the results into it.
#'@param cota . Only for automatic procedure. It will be the electoral threshold that
#'the user wants to use. In the case of the Congress of Deputies in Spain it is 3 per
#'cent (parties with less than 3 per cent of the valid votes do not take part in the
#'distribution), and it is the value that has been assigned by default.
#'@param Auto . To choose whether to use manual and automatic procedure. If it is equal to TRUE,
#'the automatic procedure is executed, otherwise the manual procedure is carried out.
#'@param votes . Only needed for manual procedure. It must be a dataframe type
#'object with the following column structure: The first column must contain the
#'name of the electoral districts to be considered. And then there will be a column
#'for each political party to be taken into account, in such a way that one column
#'will be identified by the acronym of the corresponding party, and will contain
#'the votes of that formation in each of the districts where it has been presented
#'as an electoral option. In those places where it has not been elegible, or it has
#'not overcome the electoral threshold that is to be taken into account, a value of
#'zero is added, in such a way that the Bazi() function takes this into account
#'so as not to count these cases in the generation of the final file (the file
#'with the extension ".bazi").
#'@param seats . Only needed for manual procedure.  It is a two-column dataframe.
#'The first column contains the names of the electoral districts (which must have
#'the same name and be in the same order as those provided in the "votes" dataframe)
#'and the second column indicates the number of Members of Parliament to be elected in each electoral district.
#'@param Titulo . Only needed for manual procedure. It must be a string of
#'characters that are the ones that head the file with extension ".bazi" that
#'is generated.
#'@return The result is a flat ASCII file with its own information structure to
#'be read by BAZI Java program and which has the following generic name 'Congreso_AAAA_MM.bazi',
#'where YYYY is the year of the electoral process and MM is the two-digit month.
#'In the case of using a manual procedure the file will have the name 'Congreso.bazi'.
#'
#'
#' @examples
#' Bazi(Ano = 2019,"04","F:/")
#'
#' # Manual example
#'
#'vo <- data.frame(
#'  circu=c("c1","c2","c3","c4","c5","c6","c7","c8","c9"),
#'  p1=c(200,300,0,0,250,360,145,0,0),
#'  p2=c(0,0,450,467,576,346,234,0,437),
#'  p3=c(243,567,0,0,345,634,456,634,0),
#'  p4=c(0,367,384,134,267,0,0,364,146),
#'  p5=c(345,123,234,254,123,543,342,45,0),
#'  p6=c(23,45,234,0,0,354,254,56,123)
#')
#'
#'se <- data.frame(
#'  circu=c("c1","c2","c3","c4","c5","c6","c7","c8","c9"),
#'  dip=c(3,4,5,3,2,6,2,4,3)
#')
#'
#'Bazi(camino="F:/",Auto=FALSE, votes=vo,seats=se, Titulo="Fichero_prueba")
#'
#'
#'@export
#'@md
Bazi<-function(Ano=0,Mes,camino="",cota=3,Auto=TRUE,votes,seats,Titulo=""){
  out <-tryCatch(
    {
      if(camino == "") stop("A path must be indicated where to write the file")
      if(Auto){
        # Comprobaciones
        if(Ano == 0) stop("The year must be provided in 4-digit format")
        if(nchar(as.character(Ano)) != 4) stop("The provided year must have 4 digits")
        if(class(Mes) != "character") stop("The month provided must be of character type")
        if(nchar(Mes) != 2 ) stop("The month must have two digits")


        sink(paste0(camino,"Congreso_",Ano,"_",Mes,".bazi"))
        cat(paste("=TITEL=","Congess elections, Year:",Ano,"Month:",Mes,"\n"))
        cat("=METHODE= DivAbr\n")
        cat("=AUSGABE= vert./horiz., Div/Quote, kodiert\n")
        cat("=EINGABE= Candidatura, Votos, ---\n")
        cat("=DISTRIKTOPTION= separat\n")
        datos_Mir<-suppressMessages(Agregado_Prov_MIR(Ano,Mes,"Congreso",Ruta=camino))
        datos_Mir<-datos_Mir[0:52,]


        cprov<-apply(datos_Mir[,2],2,as.integer)
        datos_Mir<-datos_Mir[order(cprov),]
        Provincias<-apply(datos_Mir[,3],2,as.character)

        # Tengo que sacar la columna de votos válidos para luego aplicar el 3 por ciento
        if(as.integer(Ano) %in% c(1977,1979,1982)){
          column = 8
        } else if(as.integer(Ano) %in% c(1986,1989,1993,1996,2000,2004,2008)){
          column=12
        } else {
          column=13
        }

        valid <- as.data.frame(datos_Mir[,column])
        valid <- as.data.frame(apply(valid,2,as.integer))
        # Mediante expresión regular selecciono las columnas que tienen el número de votos
        columnas_Votos<-grep("^(V_)",colnames(datos_Mir))
        # Me quedo solo con las columna de votos
        datos_MIR_Votos<-as.data.frame(datos_Mir[,columnas_Votos])
        datos_MIR_Votos <- as.data.frame(apply(datos_MIR_Votos,2,as.integer))
        #calculo los totales
        totales<-colSums(datos_MIR_Votos)
        #Ordenos las columnas de mas a menos votos
        datos_MIR_Votos<-datos_MIR_Votos[,order(totales,decreasing = TRUE)]

        #Hago lo mismo que antes pero con los escaños
        columnas_escanos<-grep("^(D_)",colnames(datos_Mir))
        datos_MIR_escanos<-as.data.frame(datos_Mir[,columnas_escanos])
        datos_MIR_escanos <- as.data.frame(apply(datos_MIR_escanos,2,as.integer))
        #Ordenos las columnas de mas a menos votos ##### Lo meto nuevo
        datos_MIR_escanos<-datos_MIR_escanos[,order(totales,decreasing = TRUE)]

        for(i in 1:nrow(datos_Mir)){
          Dipu <-sum(datos_MIR_escanos[i,])#N. Diputados
          #Me quedo con los partidos que tienen algun voto
          Si_Voto<-datos_MIR_Votos[i,]>0
          datos_voto<-datos_MIR_Votos[i,Si_Voto]
          datos_escanos<-datos_MIR_escanos[i,Si_Voto]
          #Aplico el minimo voto para oder conseguir escaño
          tope<-valid[i,1]*(cota/100)
          corte<-datos_voto[1,]>tope
          #Me quedo con los partidos con votos encima topa
          datos_voto<-datos_voto[,corte]
          datos_escanos<-datos_escanos[,corte]
          cat(paste("=DISTRIKT=",Provincias[i,1],"\n"))
          cat(paste("=MANDATE=",Dipu,"\n"))
          cat("=DATEN=\n")
          a<-colnames(datos_voto)
          siglas<- substr(a,3,nchar(a))
          #Recorro las columnas
          #browser()
          for(j in 1:ncol(datos_escanos)){
            cat(paste0("\"",siglas[j],"\"","\t",datos_voto[1,j]," ",datos_escanos[1,j],"\n"))
          }
        }
        cat("=INFO=\n")
        cat(paste("Congress Elections, Year:",Ano,"Month:",Mes,"\n"))
        cat("=ENDE=")

        sink()
        message(paste0("File: ",camino,"Congreso_",Ano,"_",Mes,".bazi' generated successfully"))
      } else{ # Fin si procedimiento automatico. Comienzo procedimiento manual
        # Comprobaciones iniciales
        if(nchar(Titulo) == 0) stop("A title must be provided for this manual procedure")
        if(nchar(camino) == 0) stop("You must specify a path to save the file")
        if(class(votes) != "data.frame") stop("The votes data must be provided using a data.frame")
        if(class(seats) != "data.frame") stop("The seats data must be provided using a data.frame")
        if(nrow(votes) != nrow(seats)) stop("The number of rows in the file containing the votes does
                                            not match the rows of the file with the seat data")
        if( sum(votes[,1] == seats[,1]) != nrow(votes) ) stop("The electoral districts in the file of
                                                              votes and seats do not match or are
                                                              not in the same order")
        #abro la conexion para generar el fichero correspondiente
        sink(paste0(camino,"Congreso",".bazi"))
        cat(paste("=TITEL=",Titulo,"\n"))
        cat("=METHODE= DivAbr\n")
        cat("=AUSGABE= vert./horiz., Div/Quote, kodiert\n")
        cat("=EINGABE= Candidatura, Votos, ---\n")
        cat("=DISTRIKTOPTION= separat\n")

        # separo los nombres de las circunscripciones de los votos
        circuns <- votes[,1]
        votes <- votes[,2:ncol(votes)]

        #Voy generando el fichero con extensión .bazi
        # itero para cada circunscripción
        #browser()
        for(i in 1:nrow(votes)){
          cat(paste("=DISTRIKT=",circuns[i],"\n"))
          cat(paste("=MANDATE=",seats[i,2],"\n"))
          cat("=DATEN=\n")
          # Quito las columnas con valores de votos a cero
          ceros <- votes[i,]>0
          votes2 <- votes[i,ceros]
          # Ordeno las columnas de más a menos votos
          votes2 <- votes2[order(votes2,decreasing = TRUE)]

          for(j in 1:ncol(votes2) ){
            cat(paste0("\"",colnames(votes2)[j],"\"","\t",votes2[j],"\n"))
          }
        }
        cat("=INFO=\n")
        cat(paste("Election :",Titulo ,"\n"))
        cat("=ENDE=")

        sink()
        message(paste0("File: ",camino,"Congreso",".bazi' generated successfully"))

      } # Fin del else
      return(TRUE)
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

} # Fin de la función
