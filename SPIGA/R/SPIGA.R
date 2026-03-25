#' SPIGA
#'
#' Calculation of standardized precipitation index, using the Genetic Algorithm method (SPIGA)
#' @author Iván Ayala-Bizarro
#' @export

SPIGA <- function(Pmon, scale =3, population=500, maxIter = 50, plotGA=FALSE, plotCDF=FALSE)
#--------------------------------------------------------------------------
# AUTOR         : Iván Arturo Ayala Bizarro
# PARÁMETROS    :
# PROPÓSITO     : Calcula el SPI mediante la técnica del algoritmo Genético
#--------------------------------------------------------------------------
{
    iMes <-Pmon[,2]    # Indicador Mes
    rango<-50


    #### Datos de entrada
    icol <- 3                       # Número de columna inicio de cálculo  [Y, M, Dat1, Dat2 ...]
    ncal <- length(Pmon)        # Número total de columas

    #### Cálculo para cada estación
    mmZ <- c()              # Matrix  SPIs
    mmA <- c()              # Matrix parameters alpha MV
    mmB <- c()              # Matrix parameters beta MV
    mmAGA <- c()            # Matrix parameters alpha GA
    mmBGA <- c()            # Matrix parameters beta GA

    ii<-2                   # Ubicación de la primera columa del parámetro según el archivo
    cont <- 0
    for(i in icol:ncal)
    {
        #i <-3
        cat(sprintf('********** Calculation Station %s \n',colnames(Pmon[i])))
        cont <- cont+1
        Ps <- Pmon[,i]            # Estación de análisis

        #### Parámetros
        #alphaMes <-Param_Alpha[,ii]     # Parámetros de análisis
        #betaMes <-Param_Beta[,ii]       # Parámetros de análisis

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

            #### Cálculo de los parámetros alpha y Beta MV
            param <- alphaBetaMV(Pnn,nn)
            mA[mes] <-param[1]
            mB[mes]  <-param[2]

            #### Cálculo de los parámetros alpha y Beta mediante AG
            GA <- ga(type = "real-valued",
                     fitness =  (function(x)-fitnessGamma(x[1], x[2],Pnn)),
                     min = c(0.1, 0.1), max = c(mA[mes]+rango, mB[mes]+rango),
                     popSize = population, maxiter = maxIter)
            out <-summary(GA)

            if(plotGA){
                out
                plot(GA)
                print(out)
                str(out)
            }
            mAGA[mes] <-out$solution[1]
            mBGA[mes] <-out$solution[2]

            alpha <-mAGA[mes]
            beta  <-mBGA[mes]

            #### Cálculo de Coeficientes de eficiecia
            Px <- distEmpiric(Pnn)       # probabilidad empírica
            GxGa <- sort(pgamma(Pm1, shape = mAGA[mes], scale = mBGA[mes]))
            GxMV <- sort(pgamma(Pm1, shape = mA[mes], scale = mB[mes]))
            cNSGA[mes] <- testNS(Px,GxGa)
            cNSMV[mes] <- testNS(Px,GxMV)

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

            #### Ploteo de FDA Empírica, MV y AG
            if(plotCDF){
                titleFDA <- paste0("CDF_",colnames(Pmon[i]),"mes",as.character(mes))
                pdf(paste0(titleFDA,".pdf"))
                plot(ecdf(Pm1), main=titleFDA)
                lines(sort(Pm1), sort(pgamma(Pm1, shape = mAGA[mes], scale = mBGA[mes])), col="red")
                lines(sort(Pm1), sort(pgamma(Pm1, shape = mA[mes], scale = mB[mes])), col="green")
                legend("bottomright", c("GA","ML"), lty=1,  col=c("red","green"))#,"blue"))
                dev.off()
            }
        }

        mmA <- c(mmA,mA)
        mmB <- c(mmB,mB)

        mmAGA <- c(mmAGA,mAGA)
        mmBGA <- c(mmBGA,mBGA)

        mmZ<- cbind(mmZ,Z)
        ii <- ii+1
    }

    #### Para guardar los parámetros de MV
    mmA <- matrix(mmA, nrow = 12, byrow =  F)    # Conversión a matriz
    mmB <- matrix(mmB, nrow = 12, byrow =  F)    # Conversión a matriz
    write.table(mmA, file = paste("AlphaML_",as.character(scale),".txt", sep=""),row.names=F, na="",col.names=F, sep="\t")
    write.table(mmB, file = paste("BetaML_",as.character(scale),".txt", sep=""),row.names=F, na="",col.names=F, sep="\t")

    #### Para guardar los parámetros de AG
    mmAGA <- matrix(mmAGA, nrow = 12, byrow =  F)    # Conversión a matriz
    mmBGA <- matrix(mmBGA, nrow = 12, byrow =  F)    # Conversión a matriz
    write.table(mmAGA, file = paste("AlphaGA_",as.character(scale),".txt", sep=""),row.names=F, na="",col.names=F, sep="\t")
    write.table(mmBGA, file = paste("BetaGA_",as.character(scale),".txt", sep=""),row.names=F, na="",col.names=F, sep="\t")


    #### Guardar el achivo en .txt
    Zm<- cbind(Pmon[,1],Pmon[,2],mmZ)
    write.table(Zm, file = paste("SPI_AG_",as.character(scale),".txt", sep=""),row.names=F, na="",col.names=F, sep="\t")

    #### Ploteo de barras del Coeficiente de Eficiencia
    pdf(paste("Performance_GA_ML_",as.character(scale),".pdf"))
    height <- rbind(cNSGA, cNSMV)
    mp <- barplot(height, beside = TRUE, ylim = c(0, 1), names.arg = c(1:12), col=c("aquamarine3","coral")
                  , ylab="CE", xlab="Mes", main = "Efficiency coefficients: GA Vs ML ")
    legend("topleft", c("GA","ML"), pch=15, col=c("aquamarine3","coral"), bty="n")
    dev.off()

    #### Ploteo de los SPI
    dataFrame <-data.frame(Pmon[,1],Pmon[,2],mmZ)
    colnames(dataFrame) <- colnames(Pmon)
    plotSerieStandart(dataFrame,ylabel = paste('SPIGA_',as.character(scale), sep=""))
    cat('\nSPIGA: Successful Analysis\n')
    cat(sprintf('See the results:  %s \n', getwd()))
}


