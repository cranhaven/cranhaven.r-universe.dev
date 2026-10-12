#if(!require("Relectoral")) install.packages("Relectoral"); require("Relectoral")
#if(!require("ggplot2")) install.packages("ggplot2"); require("ggplot2")
#if (!require("dplyr")) install.packages("dplyr"); require("dplyr")
globalVariables(c("Partidos","Desproporcionalidad","Comunidad"))

#'@title Utilities. Obtaining electoral indicators from aggregated data
#'
#'@description With this function, the following indicators of electoral disproportionality can be
#'obtained for aggregated data:
#'
#'* Sainte Lagüe (SL)
#'* RAE (R)
#'* RAE Corrected (Rco)
#'* Loosemore & Hanby (LH)
#'* Gallagher's Minimum Squares index (Gcm)
#'* Maximum deviation index (Lmax)
#'* Cox & Shugart index (Cs)
#'* Corrected Cox & Shugart index (CS_correg)
#'* Tukey's line (LT)
#'
#'All the values of these indicators are in an output data.frame where the names of the columns
#'coincide with the identification of these indicators which appear between brackets above.
#'
#'Likewise the following voting size indicators are also obtained:
#'
#'* Electoral and Parliamentary fragmentation (F)
#'* Effective number of political parties (N)
#'* Hyperfractionality index (Hiper)
#'* Molinar's effective number of political parties (NP)
#'* Concentration of vote (Con)
#'* Competitiveness between parties (Comp)
#'
#'The electoral and parliamentary versions are calculated for these voting size indicators.
#'Their values are stored in the output of this function within a data.frame with the
#'name of the columns indicated with the initials shown above in parentheses and the suffix '_electoral' is added
#'to indicate that it is the electoral version or the suffix '_parliamentary' to indicate
#'that it is the parliamentary version.
#'
#'**Important note**. With this function you can get the results automatically or manually.
#'With the automatic form, the data are downloaded from the Spanish Ministry of Home Office. With
#'the manual form, you have to provide the data using a data.frame with the structure that will
#'be indicated later.
#'
#'@import dplyr
#'
#'@param Ano Required only for automatic procedure. It is the year of the electoral process that
#'the user wants to study. It must be a four-digit numerical value.
#'@param Mes Required only for automatic procedure.is the month of the electoral process, it must
#'be a string composed of two characters, associated with the month in which the elections were held.
#'@param RutaDescarga Required only for automatic procedure. must be a string indicating the local path
#'of the user's computer where to download the file from the Spanish Ministry of Home Office (MIR).
#'This file is then automatically deleted once the process is completed. The user has to bear in mind that this path must indicate a
#'place where R has read and write permission, otherwise the process will not be completed.
#'@param Auto It can take the logical values TRUE or FALSE. By default it has the value TRUE to indicate
#'that the user wants the automatic procedure. In the case of wanting a manual procedure, this parameter will take the FALSE value.
#'@param datos Required only for manual procedure. It must be a data.frame with three columns:
#'The first column containing the name of the parties, the second column the number of votes and
#'the third column the number of seats that the political party in question obtains.
#'
#'@return Returns a data.frame with four elements:
#'
#'1.- Its name is 'dat' and it is a data.frame containing the votes and seats of each candidate party,
#'in absolute terms and in percentages, also the accumulated distribution of these values
#'and a last column containing an indicator of electoral disproportionality that is calculated
#'as the difference between the percentage of votes and the percentage of seats obtained.
#'
#'2.- Its name is 'grafico' and it is a ggpplot object where the difference between the percentage of
#'votes and seats is represented in a bar chart.
#'
#'3.- Its name is 'In_despro' and it is a data.fame containing the values of the disproportionality
#'indices indicated above.
#'
#'4.- Its name is 'In_dimen' and it is a dataframe with the values of the voting dimension indicators
#'indicated above. Both the electoral and parliamentary versions are provided.
#'
#'
#'@examples
#'d<-AgregadosIndi(2019,"04",RutaDescarga = "F:/")
#'
#'
#'@export
#'@md

AgregadosIndi <- function(Ano=0,Mes="",RutaDescarga="",Auto=TRUE,datos=""){
  out <- tryCatch(
    {
      if(Auto==TRUE){
        if(Ano == 0) stop("It must be provided a year in 'Ano'")
        if(Mes=="") stop("It must be provided a month with a two-character format in 'Mes'")
        if(RutaDescarga=="") stop("A download path must be provided in 'RutaDescarga'")
      }else{
        if(class(datos) != "data.frame") stop("It must be provided a data.frame in parameter 'datos'")
        if(ncol(datos) != 3) stop("The data.frame provided in parameter 'datos' must contain 3 columns")
      }
      #Creamos el data.frame que se va a procesar
      if(Auto){
        data <- suppressMessages(Agregado_Prov_MIR(Ano,Mes,Tipo="Congreso",RutaDescarga, Borrar=TRUE))
        # Miro la columna que comienza por "V_"
        start_idx <-  which(substr(colnames(data),1,2)=="V_")[1]
        #Convierto la columna de votos-escaños en numericos
        data[,start_idx:ncol(data)]<-apply(data[,start_idx:ncol(data)],2,as.integer)
        #Me quedo solo con las columnas que tienen votos-escaños
        data <-data[1:52,start_idx:ncol(data)]

        # obtenemos la suma de todas las columnas
        #data2 <- data[nrow(data),start_idx:ncol(data)]
        data2<- as.data.frame(t(colSums(data)))
        # Me quedo solo con las columna que tienen los votos (Lo valores lógicos)
        idx_votos <- substr(colnames(data2),1,2)=="V_"
        #Ahora  me quedo por un lado con las columna de los votos
        #y por otro con la de los escaños
        data3 <- t(data2[,idx_votos])
        data4 <- t(data2[,!idx_votos])
        # Gener el data.frame con las tres columnas que necesito: Nombre partidos,
        # N. votos; y n. escaños
        data5 <- data.frame(Partidos=substr(rownames(data3),3,nchar(rownames(data3))),
                            Votos=data3,Escanos=data4,stringsAsFactors = FALSE)
        # Convierto la columna de votos y escaños a valores numéricos
        data5[,c(2,3)] <- apply(data5[,c(2,3)],2,as.integer)
        # Este data.frame es el que entra al proeso
      }else{
        # Si se facilita un data.frame va a salir un data.frama llamado también
        # data5 y con los mismos nombres de las columnas
        data5 <-datos
        # Llamo a las columnas igual que en el proceso automático
        colnames(data5)<-c("Partidos","Votos","Escanos")
      }

      # Ahora ya genero los resultados
      data5$Porc_votos <- round(data5$Votos/sum(data5$Votos),4)*100
      data5$Porc_escanos <- round(data5$Escanos/sum(data5$Escanos),4)*100
      data5$Acum_porc_votos <- cumsum(data5$Porc_votos)
      data5$Acum_porc_escanos <- cumsum(data5$Porc_escanos)
      data5$Desproporcionalidad <- round(data5$Porc_escanos - data5$Porc_votos,2)
      p<-ggplot(data5,aes(x=Partidos,y=Desproporcionalidad))+
        geom_bar(stat='identity') +
        coord_flip() +
        scale_x_discrete(limits=rev(data5$Partidos))+
        geom_hline(yintercept = 0, color="blue") +
        geom_text(aes(x = Partidos,y = max(Desproporcionalidad) + 0.1,
                      label = Desproporcionalidad),size=2.5)
      #Calculo indicadores de desproporcionalidad
      # Indice Sainte_Lague (SL)
      idx_sainte_lag <- Sainte_Lague(data5$Votos,data5$Escanos)
      # Indice de Rae (R)
      idx_rae <- Rae(data5$Votos,data5$Escanos)
      #Indice Rae corregido (Rco)
      idx_rae_corr <- Rae_corregido(data5$Votos,data5$Escanos,correc=0.5)
      # Indice Loos Hanby (LH)
      idx_loos_hanb <- Loos_Hanby(data5$Votos,data5$Escanos)
      # Índice Gallagher (Gcm)
      idx_Gallagher <- Gallagher(data5$Votos,data5$Escanos)
      # Índice máxima desviación
      idx_L_max <-L_max(data5$Votos,data5$Escanos)
      # Índice de Cox Shugart (CS)
      idx_Cox_Shugart <- Cox_Shugart(data5$Votos,data5$Escanos)
      # Índice Cox Shugart corregido
      idx_Cox_Shugart_corr <- Cox_Shugart_correg(data5$Votos,data5$Escanos)
      # ïndice desproporcionalidad línea Tukey (LT)
      idx_L_Tukey <- L_Tukey(data5$Votos,data5$Escanos)
      # Todos estos indicadores de desproporcionalidad los incluyo en un data.frame
      despr_res=data.frame(SL=idx_sainte_lag)
      despr_res$R<-idx_rae
      despr_res$Rco <- idx_rae_corr
      despr_res$LH <- idx_loos_hanb
      despr_res$Gcm <- idx_Gallagher
      despr_res$Lmax <- idx_L_max
      despr_res$CS <- idx_Cox_Shugart
      despr_res$CS_correg <- idx_Cox_Shugart_corr
      despr_res$LT <- idx_L_Tukey

      # Añado los indicadores de dimension del voto
      #Fragmentación electoral y parlamentario (F)
      idx_F <- fragmentacion_rae(data5$Votos,data5$Escanos)
      # Numero efectivo de partidos
      idx_N <- nep(data5$Votos,data5$Escanos)
      # Indice hiperfraccionamiento.N. efecti par. Kesselman & Wildgen
      idx_Hiper <-hiper(data5$Votos,data5$Escanos)
      #Numero efectivo partidos Molinar (NP)
      idx_NP <-nepMolinar(data5$Votos,data5$Escanos)
      # Concentracion de voto
      idx_Con <- concentracion(data5$Votos,data5$Escanos)
      # Competitividad electoral
      idx_Comp <- competitividad(data5$Votos,data5$Escanos)

      # Todos estos indicadores los incluyo en un data.frame
      dimension <- data.frame(F_electoral=as.numeric(idx_F[1]),
                              F_parlamen=as.numeric(idx_F[2]))
      #Numero efectivo partidos
      dimension$N_electoral <- as.numeric(idx_N[1])
      dimension$N_parlamen <- as.numeric(idx_N[2])
      # Hiperfraccionamiento..N. efecti par. Kesselman & Wildgen
      dimension$Hiper_electoral <- as.numeric(idx_Hiper[1])
      dimension$Hiper_parlamen <- as.numeric(idx_Hiper[2])

      #Numero efectivo partidos Molinar (NP)
      dimension$NP_electoral <- as.numeric(idx_NP[1])
      dimension$NP_parlamen <- as.numeric(idx_NP[2])



      # Concentración voto
      dimension$Con_electoral <- as.numeric(idx_Con[1])
      dimension$Con_parlamen <- as.numeric(idx_Con[2])

      #Competitividad electoral
      dimension$Comp_electoral <- as.numeric(idx_Comp[1])
      dimension$Comp_parlamen <- as.numeric(idx_Comp[2])


      #preparo la salida
      res <- list(dat=data5,grafico=p,In_despro= despr_res,In_dimen=dimension)
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
}

#Pruebo la función anterior
#d<-AgregadosIndi(2019,"04",RutaDescarga = "F:/")

#'@title Utilities. Obtaining electoral indicators from disaggregated data
#'
#'@description With this function, the same results will be obtained as with
#'the 'AgregadosIndi()' function, but for each of the regions listed in the
#'data provided. Specifically, when downloading data from the Spanish Ministry
#'of Home Office, the indicated data will be obtained both for each Autonomous
#'Community and for each province. The format of the output resulting from this
#'function is indicated below.
#'
#' **IMPORTANT NOTE:** Again, data can be obtained automatically or provided manually.
#'
#'@param Ano Required only for automatic procedure. It is the year of the electoral process that
#'the user wants to study. It must be a four-digit numerical value.
#'@param Mes Required only for automatic procedure.is the month of the electoral process, it must
#'be a string composed of two characters, associated with the month in which the elections were held.
#'@param RutaDescarga Required only for automatic procedure. must be a string indicating the local path
#'of the user's computer where to download the file from the Spanish Ministry of Home Office (MIR).
#'This file is then automatically deleted once the process is completed. The user has to bear in mind that this path must indicate a
#'place where R has read and write permission, otherwise the process will not be completed.
#'@param Auto It can take the logical values TRUE or FALSE. By default it has the value TRUE to indicate
#'that the user wants the automatic procedure. In the case of wanting a manual procedure, this parameter will take the FALSE value.
#'@param datos_v Required only for manual procedure. is a data.frame with at least three columns.
#'The first column contains the name of the geographical unit that groups the elements of the
#'third column, which in the case of Spain can initially be the name of the Autonomous Community.
#'The second column is alphabetical and contains the code of the geographical unit appearing in
#'the third column. In the case of Spain, normally the province code will be the one of the Spanish
#'National Statistics Institute (INE). The third column is also of character format and contains the
#'name of the electoral disctric, in the case of Spain this will normally be the name of the province.
#'The fourth and following columns will refer to a specific political party, and the name of the column
#'is recommended to coincide with the acronym of the political party in question.
#'@param datos_d Required only for manual procedure. It is a data.frame with the same structure as 'datos_v',
#'the only thing that from the fourth column onwards will contain the seats obtained by each political party
#'for each of the territorial units included in the rows. **The order of these columns** must be the same as
#'that of 'datos_v' to include the votes.
#'
#'@return The output consists of a list that contains two lists:
#'
#'1. The fisrt list contains the layer of Autonomous Communities or geographical grouping units. There is a list
#'for each Autonomous Community or grouping unit. The order of this list corresponds to the order in which
#'the Autonomous Communities or grouping units appear in the input data.frame. After selecting an Autonomous
#'Community or grouping unit, another list is obtained with the same four elements that
#'contained the output of the function 'AgregadosIndi()' but for that specific geographical grouping unit.
#'
#'2. The second list contains the layer of provinces or disaggregated electoral districts. In this case,
#'a list is also obtained for each province or disaggregated unit, with the identifier equal to that
#'shown for the province or disaggregated unit in the input files. Finally, for each province, a list
#'is obtained with the four elements that were available with the output of the function 'AgergadosIndi()'.
#'
#'@examples
#'d2<-DesAgregadosIndi(2019,"04",RutaDescarga = "F:/")
#'
#'
#'@export
#'@md


# A continuación defino la función para datos desagregados
DesAgregadosIndi <- function(Ano=0,Mes="",RutaDescarga="",Auto=TRUE,datos_v="",datos_d=""){
  out <- tryCatch(
    {
      if(Auto==TRUE){
        if(Ano == 0) stop("It must be provided a year in 'Ano'")
        if(Mes=="") stop("It must be provided a month with a two-character format in 'Mes'")
        if(RutaDescarga=="") stop("A download path must be provided in 'RutaDescarga'")
      }else{
        if(datos_v=="") stop("A data.frame must be provided with voting data in parameter 'datos_v'")
        if(datos_d=="") stop("A data.frame must be provided with seat data in parameter 'datos_d'")
        if(class(datos_v) != "data.frame") stop("It must be provided a data.frame containing the votes in parameter 'datos_v'")
        if(class(datos_d) != "data.frame") stop("It must be provided a data.frame containing the seats in parameter 'datos_d'")
      }

      # Si es automatico genero dos data.frame. El primero conteniendo votos
      # y el segundo los escaños obtenidos
      if(Auto){
        data <- suppressMessages(Agregado_Prov_MIR(Ano,Mes,Tipo="Congreso",RutaDescarga, Borrar=TRUE))
        #Me quedo con las tres primeras columna que contienen las datos de identifi.
        #Provincia y Comunidad Autónoma
        iden <- data[1:50,1:3]
        # Miro la columna que comienza por "V_"
        start_idx <-  which(substr(colnames(data),1,2)=="V_")[1]
        #Me quedo solo con las columnas que tienen votos o diputados
        data22 <- data[1:50,start_idx:ncol(data)]
        #idx_votos vale TRUE si es una columna de votos. FALSE en caso contrario
        idx_votos <- substr(colnames(data22),1,2)=="V_"
        #data33 contiene los valores de los votos
        data33 <- data22[,idx_votos]
        # Añado lod datos de indentificación territorial
        data33 <- cbind(iden,data33)
        # data44 contiene los valores de los diputados obtenido
        data44 <- data22[,!idx_votos]
        # Añado lod datos de indentificación territorial
        data44 <- cbind(iden,data44)
        # Como salidad tengo dos data.frame data33 y data44, ambos tienen en las
        # tres primeras columnas los datos de identificación territorial y luego
        # d33 los datos de los votos  y d44 los datos de los diputados
      }else{ #si no tomo los data frame que se incluyen en los parámetros de la función
        # Para unificar estructura datos obtengo como salida una estructura
        #similar a la obtenida en el caso anterior
        data33 <- datos_v # Contiene los votos
        data44 <- datos_d #contiene los diputados

      }
      # tanto en el caso manual como en el automático obtengo dos ficheros. En ambos
      # la primera columna contiene el nombre de la C. Autónoma. La segunda el código
      # de la provincia y la tercera el nombre de la provincia. El resto de los campos
      # se corresponden con las candidaturas y el data33 contiene los votos y data44
      # contienen los diputados obtenidos en cada provincia

      res_prov <-list() # En esta lista van todas las respuestas
      # Itero ahora sobre cada provincia
      for(h in 1:nrow(data33)){
        cprov <- data33[h,2]
        # Cojo los nombres de los partidos políticos
        d1<-colnames(data33[4:ncol(data33)])
        d1 <- substr(d1,3,nchar(d1))
        #Cojo los votos de esa provincia
        d2<-t(data33[h,4:ncol(data33)])
        #Cojo los diputados de esa provincia
        d3 <- t(data44[h,4:ncol(data33)])
        # Ahora integro todo en un data.frame
        data5 <-data.frame(Partidos=d1,Votos=d2,Escanos=d3,stringsAsFactors = FALSE)
        colnames(data5) <- c("Partidos","Votos","Escanos")
        # Convierto la columna de votos y escaños a valores numéricos
        data5[,c(2,3)] <- apply(data5[,c(2,3)],2,as.integer)
        # Me quedo solo con los partidos que tienen algún voto
        votos <- data5[,2]>0
        data5 <-data5[votos,]
        #browser()
        #Ahora paso estos datos a la función AgregadosInd
        r <- AgregadosIndi(Ano=0,Mes="",RutaDescarga="",Auto = FALSE,datos=data5)
        # El resultado lo incluyo en una lista
        res_prov[[cprov]]<- r
      }
      # Hago lo propio agregando por Comunidades Autónomas
      res_CCAA <- list() # Aquí meto los resultados de las Comunidades Autónoma
      #Quito lo nombres y código de provincia
      data333 <- data33[,-c(2,3)]
      #Cambio el nombre de la primera variable
      colnames(data333)[1]<-"Comunidad"
      # Cambio a valores numéricos los votos
      data333[,2:ncol(data333)] <-apply(data333[,2:ncol(data333)],2,as.integer)
      # Obtengo la suma de votos para cada Comunidad Autónoma
      data333 <-data333 %>% group_by(Comunidad) %>% summarise_all(funs(sum))
      #Hago lo mismo pero para los diputados
      data444 <- data44[,-c(2,3)]
      #Cambio nombre primera variable
      colnames(data444)[1]<-"Comunidad"
      #Cambio a valores numéricos los escaños obtenidos
      data444[,2:ncol(data444)] <-apply(data444[,2:ncol(data444)],2,as.integer)
      # Obtengo la suma de escaños para cada Comunidad Autónoma
      data444 <-data444 %>% group_by(Comunidad) %>% summarise_all(funs(sum))
      for(h in 1:nrow(data333)){
        # Cojo los nombres de los partidos políticos
        d1<-colnames(data333[2:ncol(data333)])
        d1 <- substr(d1,3,nchar(d1))
        #Cojo los votos de esa Comunidad autónoma
        d2<-t(data333[h,2:ncol(data333)])
        #Cojo los diputados de esa Comunidad Autónoma
        d3 <- t(data444[h,2:ncol(data444)])
        # Ahora integro todo en un data.frame
        data5 <-data.frame(Partidos=d1,Votos=d2,Escanos=d3,stringsAsFactors = FALSE)
        colnames(data5) <- c("Partidos","Votos","Escanos")
        # Me quedo solo con los partidos que tienen algún voto
        votos <- data5[,2]>0
        data5 <-data5[votos,]
        #Ahora paso estos datos a la función AgregadosInd
        r <- AgregadosIndi(Ano=0,Mes="",RutaDescarga="",Auto = FALSE,datos=data5)
        # El resultado lo incluyo en una lista
        res_CCAA[[as.character(h)]]<- r

      }
      # Genero la respuesta
      res_total <- list(CCAA=res_CCAA,PROV= res_prov)
      return(res_total)
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

#Pruebo la función anterior
#d2<-DesAgregadosIndi(2019,"04",RutaDescarga = "F:/")



