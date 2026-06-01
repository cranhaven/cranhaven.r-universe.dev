#' Comparacao multipla: Duncan
#'
#' \code{duncan} Realiza o teste de Duncan para comparacao
#' multipla de medias.
#' @param y Vetor numerico ou complexo contendo a variavel
#' resposta.
#' @param trt Vetor numerico ou complexo contendo os
#' tratamentos.
#' @param DFerror Grau de liberdade do residuo.
#' @param SSerror Soma de quadrados do residuo.
#' @param alpha Significancia do teste de Bootstrap.
#' @param group TRUE ou FALSE.
#' @param main Titulo.
#' @return E retornada a comparacao das medias segundo o teste
#' de Duncan.
#' @author Eric B Ferreira,
#'  \email{eric.ferreira@@unifal-mg.edu.br}
#' @author Denismar Alves Nogueira
#' @author Portya Piscitelli Cavalcanti
#' @seealso \code{\link{snk}}, \code{\link{ccboot}},
#' \code{\link{lsd}}, \code{\link{lsdb}},
#' \code{\link{scottknott}}, \code{\link{tukey}},
#' \code{\link{ccf}}.
#' @examples
#' data(ex1)
#' attach(ex1)
#' dic(trat, ig, quali = TRUE, mcomp='duncan', sigT = 0.05)
#' @export

duncan<-function (y, trt, DFerror, SSerror, alpha = 0.05, group = TRUE,    main = NULL)
{
    MSerror <- SSerror/DFerror
    name.y <- paste(deparse(substitute(y)))
    name.t <- paste(deparse(substitute(trt)))
    junto <- subset(data.frame(y, trt), is.na(y) == FALSE)
    means <- tapply.stat(junto[, 1], junto[, 2], stat = "mean")
    sds <- tapply.stat(junto[, 1], junto[, 2], stat = "sd")
    nn <- tapply.stat(junto[, 1], junto[, 2], stat = "length")
    means <- data.frame(means, std.err = sds[, 2]/sqrt(nn[, 2]),
        replication = nn[, 2])
    names(means)[1:2] <- c(name.t, name.y)
    ntr <- nrow(means)

    # criando um contador de medias abrangidas para se calcular as prob de Tukey
    k.snk<-ntr-1
    Tprob <- vector(mode="integer",k.snk)
    kk <- 1
    for (kk in 1:k.snk)  {
        alphap=1-(1-alpha)^((kk+1)-1)
        Tprob[kk] <- qtukey(1 - alphap, kk+1, DFerror)
        }
#posicao do primeiro NaN :
p.nan<-as.vector(na.action(na.omit(Tprob)))[1]
#ultimo valor que nao e NaN:
ult<-p.nan-1
#substituindo os NaNs por valores aproximados (interpolacao):
if(ntr==50) Tprob[p.nan:length(Tprob)]<-seq(Tprob[ult],3.61,length=length(Tprob)-ult)
if(ntr==100) Tprob[p.nan:length(Tprob)]<-seq(Tprob[ult],3.67,length=length(Tprob)-ult)

    # Tprob <- qtukey(1 - alpha, ntr, DFerror)

    nr <- unique(nn[, 2])
    nfila <- c("Alpha", "Error Degrees of Freedom", "Error Mean Square")
    nfila1 <- c("Distances between averages", "Critical Value of Studentized Range")
    # coloquei o vetor Tprop transposto
    nvalor <- c(alpha, DFerror, MSerror)
    nvalor1 <- rbind(t(seq(2,ntr)),t(Tprob))

    #cat("\nStudy:", main)
    #cat("\n\nHSD Test for", name.y, "\n")
    xtabla <- data.frame(...... = nvalor)
    xtabla1 <- data.frame(...... = nvalor1)
    row.names(xtabla) <- nfila
    row.names(xtabla1) <- nfila1
    #print(xtabla1)
    #print(xtabla)
    #cat("\nTreatment Means\n")
    #print(data.frame(row.names = NULL, means))
    HSD <- vector(mode="integer",k.snk)
    if (group) {
        if (length(nr) == 1) {
            kk <- 1
            for (kk in 1:k.snk) {
              HSD[kk] <- Tprob[kk] * sqrt(MSerror/nr)
                 }
            #HSD <- Tprob[1] * sqrt(MSerror/nr)
            #cat("\nHonestly Significant Difference", HSD)
        }
        else {
            nr1 <- 1/mean(1/nn[, 2])
            # criado o for para se ter um vetor de DMS
            kk <- 1
            for (kk in 1:k.snk) {
              HSD[kk] <- Tprob[kk] * sqrt(MSerror/nr1)
                 }
            #cat("\nHonestly Significant Difference", HSD)
            #cat("\nHarmonic Mean of Cell Sizes ", nr1)
            #cat("\n\nDifferent HSD for each comparison")
        }
        cat("\nTeste de Duncan \n------------------------------------------------------------------------")
        cat("\nGrupos  Tratamentos  Medias\n")
        output <- order.stat.SNK(means[, 1], means[, 2], HSD)
        cat('------------------------------------------------------------------------\n')
    }
}

order.stat.SNK<-function (treatment, means, minimum)
{
    # minimum tera que ser agora um vetor para ter n-1 DMS associadas para calcular o SNK
    n <- length(means)
##############################################################################################################################
#Isso permite que sejam colocadas quantas letras forem necessarias, e nao apenas o numero de letras existentes no alfabeto (26).
#Ou seja, permite comparar qualquer quantidade de tratamentos (Eric)
letras<-letters
 if(n>26) {
 l<-floor(n/26)
  for(i in 1:l) letras<-c(letras,paste(letters,i,sep=''))
                            }
##############################################################################################################################
    z <- data.frame(treatment, means)
    w <- z[order(z[, 2], decreasing = TRUE), ]
    M <- rep("", n)
    k <- 1
    k1 <- 0
    j <- 1
    i <- 1
    r <- 1  # iniciando o meu indice da DMS
    cambio <- n
    cambio1 <- 0
    chequeo = 0
    M[1] <- letras[k]
    while (j < n) {
        chequeo <- chequeo + 1
        if (chequeo > n)
            break
        for (i in j:n) {
            # r para a distancia entre os tratamentos sendo minimum um vetor de n-1 DMS
            if (abs(j-i) == 0) {r<-1} else {r<-abs(j-i)}
            s <- abs(w[i, 2] - w[j, 2]) <= minimum[r]
            if (s) {
                if (lastC(M[i]) != letras[k])
                  M[i] <- paste(M[i], letras[k], sep = "")
            }
            else {
                k <- k + 1
                cambio <- i
                cambio1 <- 0
                ja <- j
                for (jj in cambio:n) M[jj] <- paste(M[jj], " ",
                  sep = "")
                M[cambio] <- paste(M[cambio], letras[k], sep = "")
                for (v in ja:cambio) {
                  if (abs(v-cambio) == 0) {r<-1} else {r<-abs(v-cambio)}
                  if (abs(w[v, 2] - w[cambio, 2]) > minimum[r]) {
                    j <- j + 1
                    cambio1 <- 1
                  }
                  else break
                }
                break
            }
        }
        if (cambio1 == 0)
            j <- j + 1
    }
    w <- data.frame(w, stat = M)
    trt <- as.character(w$treatment)
    means <- as.numeric(w$means)
    for (i in 1:n) {
        cat(M[i], "\t", trt[i], "\t    ", means[i], "\n")
    }
    output <- data.frame(trt, means, M)
    return(output)
}
