####===============================================================================================
####
#### AUTOR       : Iván Arturo Ayala Bizarro
#### FECHA       : 2015.11.16
#### PROPÓSITO   : Funciones importantes para el análisis de sequías
####
####===============================================================================================

sumaMoviles <- function(Ps, scale, rows, cols=12)
#--------------------------------------------------------------------------
# AUTOR         : Iván Arturo Ayala Bizarro
# PARÁMETROS    : data  -> [AÑO E F M A M J J A S O N D].
#                 rows -> Número de fila (Años)
#                 cols -> Número de columna
# PROPÓSITO     : Realiza la sumas mòviles de acuerdo a la escala de sequías
#                 1, 3, 4, 6, 9, 12 meses
#--------------------------------------------------------------------------
{
    #cat('**** Calculo de la sumas moviles','\n')
    list <- c()   # Crear vector vacío

    # agregar NA para los meses de inicio según la escala
    if (scale ==1)
    {
        list <- c()
    }
    else
    {
        for (i in 1:(scale-1))
        {
            list <-c(list,NA)
        }
    }

    # Sumar según la escala
    for (i in 1:(rows*cols-scale+1))
    {
        val <- 0.0
        for (j in 1:(scale))
        {   #cat("Valores = ", Ps[i+j-1], "\n")
            val <- val+Ps[i+j-1]
        }
        list <-c(list,val)
    }

    # Convertir a matriz la serie
    P <-matrix(list, ncol = cols, byrow = TRUE)

    return(P)
}

sumaMovilesSerie <- function(Ps, scale)
#--------------------------------------------------------------------------
# AUTOR         : Iván Arturo Ayala Bizarro
# PARÁMETROS    : data  -> [AÑO E F M A M J J A S O N D].
# PROPÓSITO     : Realiza la sumas mòviles de acuerdo a la escala de sequías
#                 1, 3, 4, 6, 9, 12 meses
#--------------------------------------------------------------------------
{
    if (scale != 1)
    {
        Sm <-c()   # Crear una lista vacía para las sumas móviles
        cont = 0
        # Agregar NA para los meses de inicio según la escala
        for (i in 1:(scale-1))
        {
            cont <- cont+1
            Sm[i] <-NA
        }

        # Sumar según la escala
        for (i in 1:(length(Ps)-scale+1))
        {
            cont <- cont+1
            val <- 0.0
            for (j in 1:scale)
            {   #cat("Valores = ", Ps[i+j-1], "\n")
                val <- val+Ps[i+j-1]
            }
            Sm[cont] <-val
        }
    }
    else   # Si scale =1
    {
        Sm = Ps
    }
    return(Sm)
}

alphaBetaMV <- function(Pnn,nn)
#--------------------------------------------------------------------------
# AUTOR         : Iván Arturo Ayala Bizarro
# PARÁMETROS    : Pnn  -> Columna de precipitación de un mes (valores no nulos)
#                 nn -> Número datos no nulos
# PROPÓSITO     : Calcula los parámetros alpha y betha de la función gamma
#                 mediante las relaciones conocidas por MV
#--------------------------------------------------------------------------
{

    # Cálculo de los parámetros
    Xm <- mean(Pnn)
    A <- log(Xm)-sum(log(Pnn))/nn
    alpha <-(.25/A)*(1.+sqrt(1.+4.*A/3.))
    beta <- Xm/alpha

    alpha_Beta <- c(alpha,beta)
    return(alpha_Beta)
}

calcSPI <- function(Pt,alpha, beta,m, nd)
#--------------------------------------------------------------------------
# AUTOR         : Iván Arturo Ayala Bizarro
# PARÁMETROS    : Pt -> Columna de precipitación de un mes (valores totales)
#                 m  -> Número de ceros en la columna
#                 nd -> Número datos totales
# PROPÓSITO     : Calcula los SPI a partir de los parámetros
#--------------------------------------------------------------------------
{
    # Probabilidad Acumulada función gamma
    Gx <- pgamma(Pt, shape = alpha, scale = beta)
    #print(Gx)
    # Cálculo de la probalidad de tener valores ceros (q)
    q = m/(1.*nd)

    # Inicialización de listas
    t <- c()    # lista de valores t
    Z <- c()    # lista de valores SPI
    Hx <- c()   # Matriz de distribución de probabilidades acumuladas

    # Cálculo de Hx
    for (i in 1:(nd))
    {
        Hx[i] <-q+(1.-q)*Gx[i]
    }

    # Coeficientes Z = SPI
    b0<-2.515517
    b1<-0.802853
    b2<-0.010328
    c1<-1.432788
    c2<-0.189269
    c3<-0.001308

    for (i in 1:(nd))
    {
        if ((Hx[i] > 0.) & (Hx[i] <= 0.5))
        {
            a <- sqrt(log(1./(Hx[i])**2))
            t<-c(t,a)
            Z[i] <- -(a - (b0+b1*a +b2*a**2)/(1.+c1*a+c2*a**2+c3*a**3))

        }

        else
        {
            a <- sqrt(log(1./(1.-Hx[i])**2))
            t<-c(t,a)
            Z[i]<-(a - (b0+b1*a +b2*a**2)/(1.+c1*a+c2*a**2+c3*a**3))

        }
    }

    return(Z)
}

fitnessGamma <- function(x1,x2,P)
#--------------------------------------------------------------------------
# AUTOR         : Iván Arturo Ayala Bizarro
# PARÁMETROS    : Pnn  -> Columna de precipitación de un mes (valores no nulos)
#                 nn -> Número datos no nulos
# PROPÓSITO     : Calcula la función de evaluación para minimizar
#--------------------------------------------------------------------------
{
    n <- length(P)

    # Parámetros
    alpha <- x1
    beta <- x2

    # Función de distribución acumulada
    Gx <- pgamma(P, shape = alpha, scale = beta)
    Gx <- sort(Gx)
    Gxm <- mean(Gx)
    #Gx <- sort(Gx)
    # Función de coste para la optimización

    sum1<-0.
    sum2<-0.

    for(i in 1:n)
    {
        Px <- i/(n+1)            # Probabilidad empìrica Weibull
        #Px <- (i-0.44)/(n+12)   # Probabilidad empírica Gregorten
        sum1 <- sum1+ (Px-Gx[i])**2
        sum2 <- sum2+ (Px-Gxm)**2
    }
    fit <- sum1/sum2
    #cat('estoy aqui','\n')
    return (fit)
}

distEmpiric <- function(x,m=1)
#--------------------------------------------------------------------------
# AUTOR         : Iván Arturo Ayala Bizarro
# PARÁMETROS    : x  -> serie de datos
#                 m  -> número entero: 1 - Weibull
#                                      2 - Gregorten
# PROPÓSITO     : Calcula la distribución empírica por el método elegido
#--------------------------------------------------------------------------
{
    n <- length(x)
    Px<-vector()

    if (m == 2) # Probabilidad empírica Gregorten
    {
        for(i in 1:n) {Px[i]<- (i-0.44)/(n+12)}
    }
    else if (m == 3)  # Probabilidad empírica por implementar
    {
        for(i in 1:n) {Px[i] <- i/(n+1)}
    }
    else # Probabilidad empírica Weibull
    {
        for(i in 1:n) {Px[i] <- i/(n+1)}
    }
    return (Px)
}

testNS <- function(o,c)
#--------------------------------------------------------------------------
# AUTOR         : Iván Arturo Ayala Bizarro
# PARÁMETROS    : o  -> serie de datos observados
#                 c  -> serie de datos calculados
# PROPÓSITO     : Calcula el coeficiente de eficiencia Nash-Sutcliffe (1970)
#--------------------------------------------------------------------------
{
    n <- length(o)
    om <- mean(o)     # Promedio de los observados

    sum1 = .0
    sum2 = .0

    for(i in 1:n)
    {
        sum1 = sum1+(o[i]-c[i])**2
        sum2 = sum2+(o[i]-om)**2
    }
    ns = 1.-sum1/sum2

    return (ns)
}

matrizToSerie <- function(m)
#--------------------------------------------------------------------------
# AUTOR         : Iván Arturo Ayala Bizarro
# PARÁMETROS    : m  -> matriz
# PROPÓSITO     : Convierte una matriz a serie
#--------------------------------------------------------------------------
{
    nrow <- dim(m)[1]
    ncol <- dim(m)[2]


    s <- vector()                # Vector de precipitaciones o caudales en serie.
    cont <-1
    for(i in 1:nrow)
    {
        for(j in 1:ncol)
        {
            s[cont]<-m[i,j]
            cont <- cont+1
        }
    }

    return (s)
}

plotSerieStandart <-function(P, ylabel='ANOM')
#--------------------------------------------------------------------------
# AUTOR         : Iván Arturo Ayala Bizarro
# PARÁMETROS    :
# PROPÓSITO     : Plotea datos en serie
#--------------------------------------------------------------------------
{
    #### Datos

    x_min = P[1,1]              # primer año
    x_max = P[nrow(P),1]+3       # último año
    y_min <- -3.5#min(P[,i])
    y_max <- 3.#max(P[,i])
    y_min <- 1.5*y_min
    y_max <- 1.5*y_max


    yr_frac <- as.numeric(P[,1]) + ((P[,2]-0.5)/12)
    P <- data.frame(yr_frac, P)
    nC <- ncol(P)

    icol <- 4
    mon_name <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

    for(i in icol:nC)
    {
        PP <- subset(P, P[,i]!="NA")
        nR <- nrow(PP)

        #### Crear series positivos y negativos para los colores
        y_p <- subset(PP, PP[,i] >= 0)
        y_n <- subset(PP, PP[,i] < 0)

        #### Information on Last data point in series for highlighting & legend
        nameCol <- colnames(P[i])
        last_AO <- signif(PP[nR,i],3)    # llama el último valor de la serie
        last_yr_mon <- paste(mon_name[PP[nR,3]], " ", PP[nR,2], sep = "") # Etiqueta el mes y año
        last_val_note <- paste(last_yr_mon," ", nameCol, "@", last_AO, sep="")  # Etiqueta el último valor

        #### Ploteo de anomalías climáticos generales
        pdf(paste(ylabel,nameCol,".pdf", sep=""),width=7,height=4)
        par(xaxs="i"); par(yaxs="i")
        par(oma=c(3,1,1,1)); par(mar=c(2,4,3,1))
        title <- paste(nameCol,"_", as.character(x_min), "@", last_yr_mon, sep="")
        plot(PP[,1], PP[,i], type = "n", xlab="", ylab=ylabel,
             main = title,  col = "red", ylim = c(y_min, y_max),xlim=c(x_min,x_max),las=1)

        grid(ny=NA,col = "lightgrey", lty=1)
        points(y_p[,1], y_p[,i], col="blue", type="h")
        points(y_n[,1], y_n[,i], col="red", type="h")
        abline(h=0, col="grey")
        points(PP[,1][nR], PP[,i][nR], type="p", pch=15, col="black")

        legend(x_min, 0.5*y_min,  c("wet", "drought", last_val_note), col = c("blue", "red","black"),
               text.col = "black", lty = c(1,1,0),pch=c(0,0,15),pt.cex=c(0,0,1),
               merge = F, bg = "white", bty="n", cex = .7)

        #### Generar pie de nota
        #source_note <- paste("Data Source: ", link_src)
        mtext("I. Ayala: ivan.ayala@unh.edu.pe", 1,1, adj = 0, cex = 0.5, outer=TRUE)
        mtext(format(Sys.time(), "%m/%d/ %Y"), 1, 1, adj = 1, cex = 0.5, outer=TRUE)
        #mtext(source_note, 1,0,adj=0.5, cex=0.8, outer=T)
        dev.off()
    }
}
