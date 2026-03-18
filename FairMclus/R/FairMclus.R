#' FairMclus Clustering for Data with Sensitive Attribute
#'
#' @description Clustering for categorical and mixed-type of data, to preventing classification biases due to race,
#'              gender or others sensitive attributes.
#'              This algorithm is an extension of the methodology proposed by "Santos & Heras (2020) <doi:10.28945/4643>".
#' @usage FairMclus(f, typedata, protected, ncores, kclus, numpos)
#'
#' @param f
#' A matrix or data frame, categorical or mixed-type of data. Objects have to be in rows, variables in columns.
#' @param typedata
#' Type of data included in dataset. For categorical data should be "C" and for mixed data should be "M".
#' @param protected
#' Name of Protected attribute column included in the data (only one).
#' @param ncores
#' Number of logical cores of computer to execute parallel process (if = 0, then will take it 2 cores by default).
#' @param kclus
#' Either the number of clusters, say k,
#' @param numpos
#' A vector specifying the numerical positions included in dataset. If dataset is a categorical dataset then should be c(0).
#'
#' @details
#' The data given by "f" parameter is clustered by the FairMclus method (Santos & Heras, 2020), which
#' aims to partition the objects into k groups such that the distance from objects to the assigned cluster
#' is minimized, maintaining the ratio of the protected attribute from the original data
#'
#' All executions return the specified values, however and depending on how large the number of rows
#' in the dataset is, it will take a little longer to execute.
#'
#' If the typedata value is different from "m" or "c", the algorithm will give an error,
#' and if no protected attribute is included, the algorithm will give an error as well.
#'
#' FairMclus is a clustering algorithm for finding homogeneous and fair clusters in data files,
#' with categorical only or also with mixed-type attributes,
#' with a better grouping effect that preventing classification biases due to race, gender, social status, others.
#'
#' Stability, classification efficiency and fairness are the major benefits of FairMclus.
#'
#'
#' @return
#' An object of output "FairMclus" contain a list with following components:
#'
#' $cluster    - A vector of integers (from 1 to k) indicating the cluster to which each point is allocated.
#'
#' $fairdis    - Total Fairness distributed in the data
#'
#' $fairatio   - Fairness ratio obtained by FairMclus
#'
#' $fairclus   - A matrix with percentage of objects in each cluster and per each value of the protected attribute.
#'
#' $clusize    - A table with number of objects in each cluster.
#'
#' $fairsize   - A matrix with number of objects in each cluster and per each value of the protected attribute.
#'
#'
#' @author Carlos Santos-Mangudo,  carlossantos.csm@gmail.com
#'
#' @references
#' Santos M., C. & J. Heras, A. (2020). A Multicluster Approach to Selecting Initial Sets for Clustering of Categorical Data. Interdisciplinary Journal of Information, Knowledge, and Management, 15, 227-246, https://doi.org/10.28945/4643
#'
#' @examples
#' ### a toy-example
#' #
#' # Some required libraries to be used:
#' library(dplyr); library(utils); library(data.table); library(tidyr)
#' library(cluster); library(rlist); library(magrittr); library(irr)
#' library(stats); library(parallel); library(foreach); library(doParallel);
#' #
#' ## generate data set with 4 columns and 20 rows:
#' a <- c(1:20)                                   # name of element
#' b <- c(1:5)                                    # categorical attribute
#' c <- c(1:2)                                    # protected attribute
#' d <- rbind(matrix(rnorm(20, mean=10, sd = 1), ncol = 1))   # numerical value
#' e <- c(1:4)                                                # categorical value
#' #
#' dataM <- cbind(a,b,c,d)
#' dataC <- cbind(a,b,c,e)
#' colnames(dataM) <- colnames(dataC) <- c("V0", "V1", "V2", "V3")
#' #
#' ## run algorithm on mixed-type of data: FairMclus(dataM, "m", "V2", 0, 2, c(3))
#'
#' ## run algorithm on categorical data: FairMclus(dataC, "c", "V2", 0, 2, c(0))
#'
#' @importFrom utils head
#' @importFrom dplyr mutate filter arrange mutate_at funs summarise_at as_tibble n_distinct group_by id everything bind_cols ungroup distinct select
#' @importFrom rlist list.cbind
#' @importFrom tidyr unite
#' @importFrom parallel detectCores makeCluster stopCluster parLapply
#' @importFrom irr kappam.fleiss
#' @importFrom magrittr %$% %>%
#' @importFrom cluster clara
#' @importFrom foreach %dopar% foreach
#' @importFrom stats dist
#' @importFrom doParallel registerDoParallel
#'
#' @export
#'
FairMclus <- function(f,typedata,protected,ncores,kclus,numpos)
{
  Clus.Multi.MAX <- Clus.Multi.OPTIMO <- Freq <- value <- freq <- n <- "." <- NULL
  cat("FairMclus - A Fairness Clustering for categorical and mixed data with protected attribute", "\n"); cat("\n")
  #
  # ---------- Initial values ----
  #
  tipodata <- toupper(typedata)
  APROTECT <- protected
  data <- as_tibble(f)
  k <- kclus
  varnum <- rbind(numpos)
  numcores <- ncores

  if (tipodata != "C" & tipodata != "M") {
    stop("Type of data has to be or 'C' (categorical) or 'M' (mixed)")
  }
  if (tipodata == "C") {
    varnumcol = 0
    varnum <- cbind(0)
  }
  if (ncores == 0) {
    ncores <- 2
  }

  data <- data %>% mutate_at(vars(-1),dplyr::funs(as.factor(.)))

  if (length(colnames(data)[colnames(data) == APROTECT]) == 0) {
    stop("Protected attribute specified is not included in data file")
  }

  numobj = dim(data)[1]                 # numero de filas
  numvar = dim(data)[2]-1               # numero de variables
  vars <- names(data[-1])               # nombre de todas las variables
  optimo <- data %>% summarise_at(vars(-1),funs(n_distinct(.))) %>% t()  # dimensiones todas las variables

  if (tipodata == "M") {
    varnumcol <- dim(varnum)[2]                   # numero de variables numericas
    for (i in 1:varnumcol) {                      # calculo numero de clusters
      Va <- 2*round((numobj)**(1/3))              # regla de Rice
      if (Va < optimo[varnum[i]]) {               # usa el valor menor
        optimo[varnum[i]] <- Va
      }
    }
  }

  data1 <- data
  vp <- optimo[which(vars==APROTECT)]
  message("Dataset has ", numobj, " rows and ", numvar, " attributes", "\n")
  message(ncol(data[-1]) - varnumcol, " categorical attributes and ", varnumcol, " numeric attributes", "\n")
  message("Will use ", ncores, " logical cores of computer, and is going to be clustered in ", k, " clusters", "\n")
  message("Protected attribute -", APROTECT, "- contains ", optimo[which(vars==APROTECT)], " different values", "\n"); cat("\n")
  # ----

  # ---------- Step 1 ----
  f1 <- function(x) {clara(data1[,x],optimo[x,])$clustering}

  class_clust <- lapply(vars,f1) %>%         # cluster de cada variable
    list.cbind() %>%
    as_tibble() %>%
    unite(id, sep = "", remove = FALSE)      # union de cluster de cada variable
  # ----

  # ---------- Step 2 ----
  filtro <- class_clust %>%                         # calculo frecuencia de cluster iguales/variables
    group_by(id) %>%
    mutate(freq=n()) %>%
    ungroup() %>%
    distinct() %>%
    arrange(freq,id) %>%
    dplyr::select(id,freq,everything())


  # Funcion calculo matriz de coincidencias
  f2A <- function(x, grupo) {
    f2B <- function(y, grupo) {
      length(which(grupo[x,]==grupo[y,]))
    }
    matrizS <- lapply((1:nrow(grupo)),f2B, grupo)       # matriz de coincidencias
    return(matrizS)
  }
  #

  grupo <- data.matrix(filtro[-c(1,2)])                 # se quita columna de id y de frec
  rg <- nrow(grupo)
  matrizS <- matrix(0,nrow=rg,ncol=rg)

  cl <- parallel::makeCluster(ncores)
  doParallel::registerDoParallel(cl)
  matrizS <- parallel::parLapply(cl, 1:rg, f2A, grupo)
  parallel::stopCluster(cl)

  # Funcion poner matriz inferior por 0's
  f3 <- function(x) {(x[lower.tri(x)] <- 0);diag(x)<-0;x}

  matriz <- unlist(matrizS) %>%                     # MATRIZ DE COINCIDENCIAS MULTICLUSTER
    matrix(ncol=nrow(grupo)) %>%
    f3() %>%                                        # pone ceros debajo de la diagonal
    as_tibble() %>%
    bind_cols(filtro[,1:2]) %>%                     # añade columnas id y frec. de matriz maximos
    dplyr::select(Var=id,Freq=freq,everything())    # coloca dichas columnas al principio de la matriz

  data1 <- data1 %>% bind_cols(class_clust %>% dplyr::select(id))    # añade el maximo al elemento
  colnames(data1)[colnames(data1)=="id"] <- "Clus.Multi.MAX"          # pone titulo al maximo
  data1 <- as.data.frame(data1) %>% mutate(Clus.Multi.OPTIMO=Clus.Multi.MAX)  # añade el optimo para calcular
  data2 <- matriz[,1]                               # MATRIZ MAXIMO
  data3 <- matriz[,-(1:2)]                          # MATRIZ COINCIDENCIAS
  data4 <- matrix(0,ncol = 1,nrow = nrow(data3))    # MATRIZ POSICION RECEPTORES
  rr = nrow(data2)
  data3[rr,rr] <- numvar


  # Funcion calculo matriz de optimos
  cl <- parallel::makeCluster(ncores)
  doParallel::registerDoParallel(cl)
  results1 <- foreach(i=1:rr, .combine = "c", .packages=c("foreach", "irr", "magrittr", "stats", "dplyr")) %dopar% {

    # ---- construir la matriz de optimos
    smc <- sum(data3[i,1:rr]==max(data3[i,1:rr]))
    RECEPTOR <- as.data.frame(matrix(0, ncol=2, nrow = smc))
    EMISOR <- data2[i,1]
    RECEPTOR[,1] <- data2[which(as.vector(data3[i,])==max(data3[i,])),1]  # cluster receptores con mayor coincidencia

    # ---- calcula el kappa de fleiss de cada E+R
    for (y in smc) {
      RECEPTOR[y,2] <- data1[1:numvar+1] %>% filter(data1$Clus.Multi.OPTIMO %in% c(EMISOR,RECEPTOR[y,1])) %>%
        head() %>% kappam.fleiss() %$% value + 1
    }
    data4[i,1] <- as.numeric(which(data2[,1] == RECEPTOR[which.max(RECEPTOR[,2]),1]))[1]
  }

  parallel::stopCluster(cl)
  data4 <- as.data.frame(unlist(results1) %>% matrix(nrow=rr) %>% as_tibble())
  #
  #
  data5 <- data.frame(data2)                        # crea la matriz de optimos a partir de la matriz maximos
  for (i in 1:rr) {
    s <- data4[i,1]
    data5[i,1] <- as.character(data2[s,1])          # rellena la matriz de optimos con los mejores
  }
  for (i in 1:numobj) {
    data1$Clus.Multi.OPTIMO[which(data1$Clus.Multi.MAX == as.character(data2[i,1]))] <- data5[i,1]
  }
  k.MulClust_Optimo <- data1$Clus.Multi.OPTIMO %>% unlist() %>% data.matrix()
  # ----

  # ---------- Step 3 ----
  colnames(data1)[colnames(data1)==APROTECT] <- "VarFC"
  k2A <- as.data.frame(table(as.character(data1$Clus.Multi.OPTIMO))) %>% arrange(Freq)
  totclu=nrow(k2A)
  data1 <- as.data.frame(data1) %>% mutate(Clus.Multi.FAIRNESS=Clus.Multi.OPTIMO)
  observado <- prop.table(table(data1$VarFC))
  #
  cl <- parallel::makeCluster(ncores)
  doParallel::registerDoParallel(cl)
  #
  # Funcion para calcular distancia observado vs propuesto entre E y R
  f6 <- function(x, data1, k2A, matrizF, observado) {
    q1 <- c(which(data1$Clus.Multi.FAIRNESS == k2A$Var1[1]),                      # E + R
            which(data1$Clus.Multi.FAIRNESS == k2A$Var1[x]))
    propuesto <- table(data1$VarFC[q1])/length(q1)                  # ratio propuesto
    matrizF[x] <- dist(rbind(observado,propuesto),method = "euclidean")           # distancia propuesto/observado
    return(matrizF)
  }
  #
  #
  repeat {
    matrizF <- matrix(0, nrow = 1, ncol = totclu)

    matrizF <- parallel::parLapply(cl, 1:totclu, f6, data1, k2A, matrizF, observado)

    matrizF <- rowSums(unlist(matrizF) %>% matrix(ncol = totclu))
    matrizF[1] <- 999
    min_propuesto <- which.min(matrizF)                                       #R
    q4 <- which(data1$Clus.Multi.FAIRNESS == k2A$Var1[1])                     #E
    data1$Clus.Multi.FAIRNESS[q4] <-  as.character(k2A$Var1[min_propuesto])   #RtoE
    k2A <- as.data.frame(table(as.character(data1$Clus.Multi.FAIRNESS))) %>% arrange(Freq)
    totclu=nrow(k2A)
    if (totclu == k) {break}
  }
  #
  parallel::stopCluster(cl)
  gc()
  #
  data1 <- as.data.frame(data1) %>% mutate(k.MulClust_Fairness=0)  # crea nueva variable para clusters finales
  for (i in 1:k) {      # enumera los clusters finales
    w1 <- which(data1$Clus.Multi.FAIRNESS == k2A$Var1[i])
    data1$k.MulClust_Fairness[w1] <- i
  }
  # ----

  # ---------- Final Results ----
  totdist <- as.data.frame(table(as.character(data1$k.MulClust_Fairness)))
  colnames(totdist)[colnames(totdist)=="Var1"] <- "Cluster"
  OBS <- cbind(as.data.frame.matrix(as.data.frame(names(observado))),
               paste(round(100*observado,1),"%",sep = ""))
  OBS <- t(OBS)
  rownames(OBS)<-c("","")
  ccsum = 0
  totfair <- totsize <- matrix(0,nrow = k,ncol = vp)

  for (i in 1:k) {
    aa<-as.data.frame(table(as.character(data1$k.MulClust_Fairness)))$Var1[i]
    bb<-table(data1$VarFC[which(data1$k.MulClust_Fairness == aa)])/
      sum(table(data1$VarFC[which(data1$k.MulClust_Fairness == aa)]))
    cc<-1-dist(rbind(observado,bb),method = "euclidean")
    ccsum = ccsum + cc
    totsize[i,] <- rbind(table(data1$VarFC[which(data1$k.MulClust_Fairness == aa)]))
    totfair[i,] <- rbind(paste((round(100*(table(data1$VarFC[which(data1$k.MulClust_Fairness == aa)])/
                         sum(table(data1$VarFC[which(data1$k.MulClust_Fairness == aa)]))),1)),"%",sep = ""))
  }
  ccsum = ccsum/as.numeric(aa)

  cluster <- data1$k.MulClust_Fairness

  fairatio <- paste(round(100*ccsum, 2), "%", sep="")

  fairdis <- OBS

  clusize <- table(data1$k.MulClust_Fairness) # total obs cluster

  fairclus <- as.data.frame(totfair)

  fairsize <- as.data.frame(totsize)

  colnames(fairdis) <- colnames(fairsize) <- colnames(fairclus) <- OBS[1,]
  gc()
  # ----
  #
  return(list(cluster=cluster, fairdis=fairdis, fairatio=fairatio, fairclus=fairclus, clusize=clusize, fairsize=fairsize))
}
