###########################################
# Creo la función para el paquete Relectoral
########################################
#if (!require("dplyr")) install.packages("dplyr"); require("dplyr")
globalVariables(c("CCAA"))

# Función para generar el csv. No ofrecida en Relectoral
#'@import utils
generar_CSV <- function(data,fichero){
  #Data es un dataframe con primera columna las siglas de los partidos
  # Fichero es el nombre y patch del fichero que se quiere crear
  ##### Genero las restricciones de acceso correspondientes

  if(file.access(names = fichero,mode = 2) == 0) stop("El fichero csv que se quiere generar, no se puede crear
                                    porque no hay permisos de escritura o bien ya existe")
  colnames(data)[1] <- c("PARTIDOS")
  a <- data.frame(PARTIDOS="PARTIDOS")
  data$PARTIDOS <- substring(data$PARTIDOS,3)
  data <- rbind(a,data)

  data$PARTIDOS <- paste0(data$PARTIDOS,",")
  write.table(data[,c(1)],fichero,quote = FALSE,row.names=FALSE,col.names = FALSE)
  message(paste0("\u00A1 \u00A1 \u00A1 Generado el fichero de tipo csv: ",fichero,"\u0021 \u0021 \u0021 "))
}

generar_VRta <- function(datos,panes){
  # datos es un dataframes. primera columna nivel medio (CCAA), segunda columna niveles
  #inferiores (provincias). Despues una columna para cada partido, con nombre el logo del
  #partido y conteniendo los votos a ese partido en cada nivel inferior
  #panes es el camino de fichero de tipo csv contenendo información sobre panes y no panes



  panes <- read.csv(panes,header=TRUE,stringsAsFactors = FALSE)
  #### Genero las restricciones correspondientes
  if(ncol(panes) !=2 ) stop("El fichero conteniendo el regionalismo ( PANES ) no tiene dos columnas")
  colnames(panes) <- c("PARTIDOS","PANE")
  panes$PANE <- as.logical(panes$PANE)
  suma_total <- rowSums(datos[,3:ncol(datos)])
  a <- datos[,3:ncol(datos)]
  suma_panes <- rowSums(a[,panes$PANE])  #Sumo quedandome solo con las columnas que son PANE
  #Añadimos al resultado final el nombre de provincia
  porc_panes_prov <- data.frame(cbind(datos[,c(2)],(suma_panes/suma_total)*100))
  colnames(porc_panes_prov)<-c("C_PROv","VRta")

  # Para Comunidades Autónomas
  votos_CCAA <- datos[,-c(2)] %>% group_by_at(1) %>% summarise_all(list(sum))
  suma_total <- rowSums(votos_CCAA[,-c(1)])
  a <- votos_CCAA[,-c(1)]
  suma_panes <- rowSums(a[,panes$PANE])
  porc_panes_CCAA <- data.frame(cbind(votos_CCAA[,c(1)]),(suma_panes/suma_total)*100)
  colnames(porc_panes_CCAA) <- c("CCAA","VRta_CCAA")

  # Calculo ahora el VRtaD
  # Tengo que introducir el VRta de la C.A en cada provincia, lo hago ccon un merge
  tem <- merge(datos[,c(1,2)],porc_panes_prov,by.x='Nombre de Provincia',by.y = 'C_PROv')
  resul<-merge(tem,porc_panes_CCAA,by.x = 'Nombre de Comunidad',by.y='CCAA')
  resul$VRta <- as.numeric(as.character(resul$VRta))

  resul$VRtaD <- resul$VRta-resul$VRta_CCAA
  resul <- resul[,c(2,5)]

  #Calculamos ahora VRtaD para CCAA
  # Calculamos el apoyo panes a nivel nacional
  # Total votos por partidos
  T_vot_part <- sum(colSums(datos[,3:ncol(datos)]))
  T_vot_part_PANES <-sum(colSums(datos[,3:ncol(datos)][panes$PANE]))
  # VRtad nacional
  POR_PANES_NACIONAL <-(T_vot_part_PANES/T_vot_part)*100
  # PAra cada C.A cada porcentaje PANES le resto  POR_PANES_NACIONAL
  VRtaD_CCAA <- porc_panes_CCAA[2]-
    matrix(POR_PANES_NACIONAL,ncol = nrow(porc_panes_CCAA))
  VRtaD_CCAA <- cbind(porc_panes_CCAA[1],VRtaD_CCAA)

  resul_Total <-list(VRta_Inferior=porc_panes_prov,
                     VRta_Medio=porc_panes_CCAA,
                     VRtaD_inferior=resul,
                     VRtaD_Medio=VRtaD_CCAA)
  return(resul_Total)
}

#'@title Dimensionality of voting. Regionalism
#'
#'
#'@description With this function we calculate the regionalism indexes described in the book
#'"Analysis of electoral data" from Pablo Oñate y Francisco A. Ocaña (page 48). The reader is
#'referred to this publication in order to understand the meaning of the indicators computed
#'with this function. More precisely, the Regionalist Voting Index (VRta), the Differentiated
#'Regionalist Voting Index (VRtaD), and the Differentiated Regional Voting Index (VRD) will be calculated.
#'
#'@import dplyr
#'
#'@param Ano It is the year of the electoral process to be dealt with. It must be a four-digit numerical
#'value. If a manual procedure is used, no value is needed for this parameter.
#'@param Mes It is the month of the electoral process, it must be **a string with two numerical characters**,
#'associated with the month in which the elections took place. If a manual procedure is used, no value is
#'required for this parameter.
#'@param RutaDescarga It should be a string indicating the way to download the file from the Spanish Ministry of Home Office.
#'This file is automatically removed if the process finish successfully. The user has to be aware that the
#'path provided must correspond to a computer location where the user has read and write permissions,
#'otherwise the process will not be completed. If a manual procedure is used, no value is required for this parameter.
#'@param Auto It can take the logical values TRUE or FALSE. By default, it has the value TRUE to indicate that
#'the user wants the process to be automatic. In the case of wanting a manual process, this parameter will take the value FALSE.
#'@param datos In this parameter you must enter the values that will be processed if manual processing is
#'chosen, i.e. "Auto=FALSE". It must be a data.frame with the following structure: The first column must
#'contain the name of the intermediate unit (in the case of Spain the autonomous regions), in the second
#'column must include the name of the lower units (in the case of Spain the name of the provinces), and then
#'there must be a column for each political party containing the votes obtained by that party in each lower
#'unit (for Spain in each province). The value of the name for each of these columns must be the acronyms
#'that identify each political party.
#'@param PANES Here the user should provide the complete path of the csv-type file containing, for each
#'party, the information on whether that party is regionalist or not. "1" will denote that the party is
#'regionalist or nationalist and "0" that it is not. These values will appear in the second column of
#'that csv file. The first column must have the acronym of each political party under study.
#'@param Generate_PANES A boolean type parameter, by default its value is FALSE. In case it is TRUE,
#'the csv file indicated in the "PANES" parameter will be generated. The first line of this file will
#'be the following: "PARTIES,PANE", which will be the name of the variables with which the function
#'will work internally. In this case, the csv file generated will contain a column with the acronyms of
#'the political parties, and then, manually, "1" will be written down if the match is "PANE" (nationalist or regionalist party) and 0 if it
#'is "NO PANE" (non-nationalist or non-regionalist party). That is to say, with the parameter Generate_PANES = TRUE, a help system is obtained to
#'obtain the file that must be passed with the "PANES" parameter. But this csv file can also be generated
#'in a completely manual way, if the user wishes.
#'
#'@return It returns a list of all the indicators obtained with this function. This initial list,
#'in turn, contains two other lists:
#'
#'1. VRtaD. It is composed of three other lists, each one containing a data.frame with the following information:
#'**$VRta_Provincias**: contains the values of the VRta indicator at the lowest level (in the case of
#'Spain, the provinces). **$VRta_CCAA** contains the VRta indicator at the intermediate level (in the
#'case of Spain, at an autonomous region level). **$VRtaD** contains the indicator VRtaD at the lowest level.
#'
#'2. VRD. Composed of three other lists, each one containing a data.frame with the following information:
#'**$inferior_medio** contains the VRD value of the lower level (provinces in Spain) with respect to the
#'intermediate level (autonomous regions in Spain).  **$inferior_superior** contains the VRD value for the
#'lower level with respect to the upper level (State level). **$medio_superior** contains the value of the
#'VRD index at the intermediate level with respect to the upper one.
#'
#'@examples
#'r <- Regionalismo(Ano=2019,Mes = "11",RutaDescarga = "F:/",
#'  PANES = system.file("regionalismo","Regionalismo.csv", package="Relectoral"))
#'
#'@export
#'@md

Regionalismo <- function(Ano=0,Mes="",RutaDescarga="",Auto=TRUE,datos="",PANES="",Generate_PANES=FALSE){
  out <-tryCatch(
    {
      # Ano es el año a cuatro dígitos. Mes es numero de mes a dos dítos.
      #RutaDescarga es donde se descarga el zip proveniente del MIR. Auto es para proceso autom. (descarga MIR) o no
      #PANES es el nombre , con el pacth,de fichero CSV que contiene información sobre PANES
      #Generate_PANES boolean para indicar si se hay que generar fichero csv con las siglas de los partidos o no
      # datos es un data.frame con una estructura similar a la que se descarga del MIR
      if(Auto==TRUE & Ano==0) stop("Since auto=TRUE, a four-digit year must be provided")
      if(Auto==TRUE & ((Mes=="") || (nchar(Mes) !=2) || (class(Mes) != "character")))
        stop("Since auto=TRUE the month must be provided as a two-digit character format")
      if(Generate_PANES==FALSE & file.exists(PANES)==FALSE) stop("No csv file has been provided or the one
                                                                 indicated with the parameter is not found")

      if(Auto == TRUE){
        data <- suppressMessages(Agregado_Prov_MIR(Ano,Mes,Tipo="Congreso",RutaDescarga, Borrar=TRUE))
        # Miro la columna que comienza por "V_"
        V_idx <-  which(substr(colnames(data),1,2)=="V_")
        V_idx <- c(1,3,V_idx) #Añado las columnas 1 y 2
        datos <- data[1:52,V_idx]  #Selecciono esas columnas y elimino las filas con el total

      } else {datos = datos}
      datos=as.data.frame(datos)
      # Para unificar doy esta denominación al fichero de entrada
      colnames(datos)[c(1,2)]<-c("Nombre de Comunidad","Nombre de Provincia")
      # Si queremos crear el fichero de tipo csv con información de PANES y NO PANES
      if(Generate_PANES == TRUE){
        # Genero restricciones para la generación del fichero CSV

        if(class(datos) !="data.frame") stop( "To generate the csv file the data.frame must
                                              be provided with the appropriate structure, containing
                                              information about the political parties and votes.")
        a <- as.data.frame(colnames(datos)[3:ncol(datos)])
        colnames(a) <- "PARTIDOS"
        generar_CSV(a,PANES)
        return()
      }



      #############################
      # Calculo del VRD
      ##################################



      colnames(datos)[3:ncol(datos)] <- substring(colnames(datos)[3:ncol(datos)],3)


      #Convierto la columna de votos-escaños en numericos
      datos[,3:ncol(datos)]<-apply(datos[,3:ncol(datos)],2,as.integer)
      sumrows <- rowSums(datos[,3:ncol(datos)])
      votos_porc <- (datos[,3:ncol(datos)]/sumrows)*100
      votos_porc <- cbind(datos[,c(1,2)],votos_porc)
      colnames(votos_porc)[c(1,2)] <- c("CCAA","Prov")
      votos_porc_ccaa <- votos_porc[,-c(2)] %>% group_by(CCAA) %>% summarise_all(list(mean))
      votos_porc_ccaa <- merge(votos_porc[,c(1,2)],votos_porc_ccaa, by="CCAA")

      votos_porc <- votos_porc[order(votos_porc$Prov),]
      votos_porc_ccaa <- votos_porc_ccaa[order(votos_porc_ccaa$Prov),]

      resul_parc <- abs(votos_porc[,3:ncol(datos)] - votos_porc_ccaa[,3:ncol(datos)])
      resul <- rowSums(resul_parc)*0.5
      #Añadimos al resultado final el nombre de provincia
      resul <- data.frame(cbind(votos_porc_ccaa[,c(2)],resul))
      colnames(resul)<-c("Nivel_Inferior","VRD")

      # Calcullamos VRD de nivel inferior (provincias) frente al superior (Estado)
      # Calculamos la suma por coulumna
      col_sum <-colSums(datos[,3:ncol(datos)])
      # Calculamos los porcentajes para cada partido
      col_sum_porcen <- (col_sum/sum(col_sum))*100
      # para poder hacer la resta repetimos esta fila 52 veces
      col_sum_porcen <- as.data.frame(do.call("rbind",replicate(nrow(votos_porc),col_sum_porcen,simplify=FALSE)))
      resul_prov_Total <- abs(votos_porc[,3:ncol(datos)]-col_sum_porcen)
      resul_prov_Total <- rowSums(resul_prov_Total)*0.5
      # Añado las provincias
      resul_prov_Total<-data.frame(cbind(votos_porc[,c(2)],resul_prov_Total))
      colnames(resul_prov_Total)<-c("Provinc.","VRD")

      # Calculamos lo anterior pero para el nivel intermedio, es decir CCAA frente al superior

      votos_porc_ccaa <- votos_porc[,-c(2)] %>% group_by(CCAA) %>% summarise_all(list(mean))
      # Calculamos la suma por coulumna
      col_sum <-colSums(datos[,3:ncol(datos)])
      # Calculamos los porcentajes para cada partido
      col_sum_porcen <- (col_sum/sum(col_sum))*100
      # para poder hacer la resta repetimos esta fila 52 veces
      col_sum_porcen <- as.data.frame(do.call("rbind",replicate(nrow(votos_porc_ccaa),col_sum_porcen,simplify=FALSE)))
      resul_CCAA_TOTAL <- abs(votos_porc_ccaa[,-c(1)]-col_sum_porcen)
      resul_CCAA_TOTAL <- rowSums(resul_CCAA_TOTAL)*0.5
      # Añadimos ahora el nombre de la CCAA
      resul_CCAA_TOTAL <- data.frame(cbind(votos_porc_ccaa[,c(1)],resul_CCAA_TOTAL))
      colnames(resul_CCAA_TOTAL) <- c("CCAA","VRD")




      #Devuelvo los resultados de VRD
      resulVRD <-list(inferior_medio = resul, inferior_superior = resul_prov_Total,
                      medio_superior= resul_CCAA_TOTAL)

      ##############################
      # Calculos con PANES, NOPANES
      ###############################
      res_PANES<- generar_VRta(datos,PANES)

      # Devuelvo los resultados totales
      devolver <- list(VRtaD=res_PANES,VRD=resulVRD)
      return(devolver)
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

#r <-Regionalismo(Ano=2019,Mes = "11",RutaDescarga = "F:/")
#r <- Regionalismo(Ano=2019,Mes = "11",RutaDescarga = "F:/" ,
#                 PANES = "G:/Elecciones/TFM/Datos/Regionalismo.csv")



