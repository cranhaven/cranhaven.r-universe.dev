#' SPIFromParameter
#'
#' Calculation of standardized precipitation index from alpha and beta parameter
#' of the Gamma function
#' @author Iván Arturo Ayala Bizarro
#' @export

SPIFromParameters <- function(Pmon, scale =3, Param_Alpha, Param_Beta)
#--------------------------------------------------------------------------
# AUTOR         : Iván Arturo Ayala Bizarro
# PARÁMETROS    :
# PROPÓSITO     : Calcula los SPI a partir de parámetros calculados alpha y beta
#--------------------------------------------------------------------------
{
    #### Datos de entrada
    icol <- 3                   # Número de columna inicio de cálculo  [Y, M, Dat1, Dat2 ...]
    iMes <-Pmon[,2]             # Indicador Mes
    ncal <- length(Pmon)        # Número total de columas

    #### Cálculo para cada estación
    mmZ <- c()              # Matriz de SPIs

    ii<-2                   # Ubicación del primera columa del parámetro según el archivo
    cont <- 0
    for(i in icol:ncal)
    {
        #i <-3
        cat(sprintf('********** Calculation Station %s \n',colnames(Pmon[i])))
        cont <- cont+1
        Ps <- Pmon[,i]            # Estación de análisis

        #### Parámetros
        alphaMes <-Param_Alpha[,ii]     # Parámetros de análisis
        betaMes <-Param_Beta[,ii]       # Parámetros de análisis

        #### Cálculo de sumas móviles
        P <-sumaMovilesSerie(Ps,scale)

        #### Parámetros
        Z <- c()                # SPIs
        mA <- c()               # lista de parámetros alpha MV
        mB <- c()               # lista de parámetros beta MV
        mAGA <- c()             # lista de parámetros alpha AG
        mBGA <- c()             # lista de parámetros beta AG
        cNSMV <- c()            # Coef. de Nash Máxima Verosimilitud
        cNSGA <- c()            # Coef. de Nash Algoritmos Genéticos

        #### Análisis mensual
        for(mes in 1:12)
        {
            # Precipitación columna mensual desde la serie
            Pm <- c()
            j <-1
            for (k in 1:length(P))
            {
                if (mes==iMes[k])
                {
                    Pm[j]=P[k]
                    j <-j+1
                }
            }

            Pm1 <- Pm[!is.na(Pm)]           # Precipitación sin valores NA
            Pm2 <- Pm[is.na(Pm)]            # Precipitación con valores NA

            nd  <- length(Pm1)              # Total de valores
            Pnn <- Pm1[Pm1!= 0]             # Lista de elementos no nulos
            nn  <- length(Pnn)              # Cantidad de datos con valores no nulos
            m   <- nd-nn                    # Cantidad de datos con valores nulos

            #### Cálculo de los parámetros alpha y Beta en F(Gamma)
            alpha <-alphaMes[mes]
            beta  <-betaMes[mes]

            #### Cálculo de los SPIs
            Z1 <-calcSPI(Pm1,alpha, beta,m,nd)
            Z2 <- c(Pm2,Z1)  # insertar los valores NA

            # Ubicar los resultado en la posición total de  un vector
            ubic <-mes
            for (k in 1:length(Z2))
            {
                Z[ubic]<- Z2[k]
                ubic<-ubic+12
            }
        }

        mmZ<- cbind(mmZ,Z)
        ii <- ii+1
    }
    # Save file .txt
    Zm<- cbind(Pmon[,1],Pmon[,2],mmZ)
    write.table(Zm, file = paste("SPI_PARAM_",as.character(scale),".txt", sep=""),row.names=F, na="",col.names=F, sep="\t")

    #### Plot SPI
    dataFrame <-data.frame(Pmon[,1],Pmon[,2],mmZ)
    colnames(dataFrame) <- colnames(Pmon)
    plotSerieStandart(dataFrame, ylabel = paste('SPI_PARAM_',as.character(scale), sep=""))
    cat('\nSPIFromParameters: Successful Analysis\n')
    cat(sprintf('See the results:  %s \n', getwd()))
}

