####################################################
### File name: clusEvol.r
####################################################


clusEvol <- function (x=NULL,objects=NULL, time = NULL,target.vars = NULL,
                      time.base=NULL,sel.obj=NULL,init = NULL,logscale = FALSE,
                      ng = NULL,clm = "pam",scale=TRUE,clstats = FALSE,...){
  
  # x: dataframe
  # objects: variable name of objects
  # time: variable name of time 
  # labels: objects labels (same lenght as objects)
  # target.vars: selected variables for CEA
  # time.base: selected year for CEA
  # sel.obj: selected object for CEA
  # init: initiation year
  # logscale: TRUE if data should be logscaled
  # ng: number of desired clusters
  # clm: (pam,kmeans,choose)
  # ... parameters of the clm chosen
  
  # revisar paquete que recomienda el clustering del TASK VIEW
  
  datos <- x[,c(objects,time,target.vars)]
  
  # ST: if init is missing, take minimum year
  if(is.null(init)) {
    init <- min(datos[,time])
  }else{
    datos <- datos[which(datos[time]>=init),]
  }
  # END: if init is missing, take minimum year
  
  # yrs.base <- input$yrs1
  summary(datos)
  
  
  nas <- apply(datos,2,function(x) sum(is.na(x)))
  if(any(nas>0)){
    warning("Selected variables have missing values, use a balaced panel data,NA's will be replaced with mean values")
    
    if(length(target.vars)>1)
    {
      mm <- colMeans(datos[,target.vars],na.rm = TRUE)
    }else{
      mm <- mean(datos[,target.vars],na.rm = TRUE)
    }
    
    for(tv in 1:length(target.vars))
    {
      anas <- is.na(datos[,target.vars[tv]])
      datos[which(anas),target.vars[tv]] <- mm[tv]
    }
    
  }
  
  yrs <- unique(datos[,time])
  
  sol.yrs <- NULL #cheks if sel.obj is in everytime period
  for(t in 1:length(yrs))
  {
    fil <- datos[which(datos[,time]==yrs[t]),]
    sol.yrs <- c(sol.yrs,sel.obj %in% fil[,objects])
  }
  if(!all(sol.yrs))
  {
    # warning("Times with no selected object: \n")
    # print(yrs[which(sol.yrs)])
    stop("Selected object must be included in all analyzed time periods")
  }
  
  # ST: dataframe for time.base:
  refdatos <- datos[datos[,time]==time.base,c(objects,time,target.vars)]
  if(logscale & length(target.vars)>1){
    refdatos[,target.vars] <- apply(refdatos[,target.vars],2,log)
  }
  if(logscale & length(target.vars)==1){
    refdatos[,target.vars] <- log(refdatos[,target.vars])
  }
  # END: dataframe for time.base
  
  # ST: ECbase dataframe for sel.obj
  ECbase <- refdatos[refdatos[,objects]==sel.obj,c(objects,time,target.vars)]
  # END: ECbase dataframe for sel.obj
  
  results <- list()
  contador <- 1
  
  Ksol = NULL
  nbelongTot <- NULL
  kmodelSol <- NULL
  clusterStats <- NULL
  for (j in  1:length(yrs))
  {

    refdatos <- datos[which(datos[,time]==yrs[j]),c(objects,time,target.vars)]
    
    if(logscale & length(target.vars)>1){
      refdatos[,target.vars] <- apply(refdatos[,target.vars],2,log)
    }
    if(logscale & length(target.vars)==1){
      refdatos[,target.vars] <- log(refdatos[,target.vars])
    }
    
    
    refdatos[which(refdatos[,objects]==sel.obj),] <- ECbase #replace ECU values
    # Include data imputation here if needed
    
    temp1 <-  as.matrix(refdatos[,target.vars],ncol = length(target.vars))
    rownames(temp1) <- refdatos[,objects]
    
    
    # Clustering:
    if(clm =="pam")
    {
      if(scale)
      {
        clm.args <- list(x=scale(temp1),k=ng,...)
      }else{
        clm.args <- list(x=temp1,k=ng,...)
      }
      kmodel <- do.call(cluster::pam,args = clm.args)
      kmodelSol[[j]] <- kmodel
      
      if(clstats)
      {
        clusterStats[[j]] <- fpc::cluster.stats(kmodel$diss,kmodel$clustering)
      }
      
      clase <- kmodel$clustering
    }
    
    if(clm =="kmeans")
    {
      if(scale)
      {
        clm.args <- list(x=scale(temp1),centers=ng,...)
      }else{
        clm.args <- list(x=temp1,centers=ng,...)
      }
      
      kmodel <- do.call(stats::kmeans,args = clm.args)
      kmodelSol[[j]] <- kmodel
      
      if(clstats)
      {
        if(scale)
        {
          clusterStats[[j]] <- fpc::cluster.stats(stats::dist(temp1),kmodel$cluster)
        }else{
          clusterStats[[j]] <- fpc::cluster.stats((temp1),kmodel$cluster)
        }
      }
      
      
      clase <- kmodel$cluster
    }
    if(clm == "choose")
    {
      clm.args <- list(x=temp1,...)
      kmodel <- do.call(clusterSim::cluster.Sim,args = clm.args)
      kmodelSol[[j]] <- kmodel
      clase <- kmodel$optClustering
    }
    
    
    auxK = cbind(clase,yrs[j])
    Ksol = rbind(Ksol,auxK )
    
    # ST: Find sel.obj's group and data
    gbase <- list()
    for(i in 1:ng)
    {
      gbase[[i]] <- as.vector(refdatos[,1][clase==i])
    }
    
    nbelong <- grep(sel.obj,gbase)# number of group which sel.obj belongs to
    nbelongTot <- c(nbelongTot,nbelong)
    g.fin <- refdatos[match(gbase[[nbelong]],refdatos[,objects]),]# Data in sel.obj group
    g.fin$time = yrs[j]
    results[[contador]] <- g.fin
    contador <- contador+1
  }
  
  Ksol = data.frame(  objects = rownames(Ksol), cluster = Ksol[,1],time =  Ksol[,2])
  # datos: input data
  # target.vars: selected variables
  # results: data frame of negighbours of sel.obj
  # ECk: cluster that sel.obj belongs to
  # sumdat: summary statistics of "datos"
  # kmodelSol: output of clustering algorithm in each iteration
  # kmodelSol: cluster statistics in each clustering
  # sl: cluster evolution in time
  
  sl <- dplyr::bind_rows(results, .id = "time")
  
  sumdat <- summary(datos[,target.vars])

  clusEvol <- list(datos = datos,target.vars=target.vars,
                    results = results,ECk = nbelong,ECkTot = nbelongTot,
                    Clus = Ksol,sumdat=sumdat,kmodelSol=kmodelSol,
                    clusterStats=clusterStats,sl = sl,sel=c(objects,time),sel.obj=sel.obj)
  structure(c(clusEvol, call = call), class = c("clusEvol"))
}

print.clusEvol <- function(x, digits = max(3, getOption("digits") - 3), ...)
{
  if(!inherits(x,"clusEvol")) stop("Enter an object obtained from the function clusEvol\n")
  
  tn <- unique(x$Clus$time)
  sel.obj <- x$sel.obj
  # number of elements in sel.obj cluster:
  nc <- sapply(x$results,nrow)-1
  names(nc) <- tn
  # Cluster that sel.obj belongs to:
  # xs <- data.frame(time = unique(x$Clus$time),clus = x$ECkTot)
  xs <- x$ECkTot
  names(xs) <- tn
  # Clusters in time:
  tt <- table(x$Clus$time,x$Clus$cluster) 
  
  cat('\n##################################################################')
  cat('\nclusEvol: Cluster Evolution Analytics\n')
  cat('\n\nNumber of neighbours ', sel.obj,'is a group member: ', '\n')
  print(nc)
  cat('\n\nCluster that ', sel.obj, 'belongs to: ','\n')
  print(xs)
  options(digits = digits)
  cat('\nClusters in time:\n')
  print.default(tt, digits = digits, print.gap = 2,
                quote = FALSE)
}


plot.clusEvol <- function(x,target,type = "heat",plotly=FALSE,...)
{
  if(!inherits(x,"clusEvol")) stop("Enter an object obtained from the function clusEvol\n")
  oldop <- options()
  on.exit(options(oldop))
  sl <- x$sl
  time <- x$sel[2]
  objs <- x$sel[1]
  if(type=="heat" | type == "all")
  {
    p1 <-ggplot(sl,aes_string(time,objs,fill =target ))+
      geom_tile(color= "white") + 
      scale_fill_viridis(option ="C")
    
    if(plotly)
    {
      suppressWarnings(print(plotly::ggplotly(p1)))
    }else{print(p1)}
  }
  
  if(type=="line" | type == "all")
  {
    p2 <- ggplot(sl, aes_string(x=time, y=target, group=objs)) +
      geom_line(aes_string(color=objs))+
      geom_point(aes_string(color=objs))
    if(plotly)
    {
      print(plotly::ggplotly(p2))
    }else{print(p2)}
  }
  if(type=="boxplot" | type == "all")
  {
    p3 <- ggplot(sl, aes_string(x=time, y=target, color=objs)) +
      geom_boxplot()
    if(plotly)
    {
      print(plotly::ggplotly(p3))
    }else{print(p3)}
  }
 
}



