##################################################################
####### Convert netgwas.map object to cross object in qtl
#################################################################
as.cross <- function(mp, pop.type= "DH") 
{    
  RILN <- paste("RIL",1:20, sep = "")
  allow.pop <- c("BC","DH","ARIL",RILN)
  if(!(pop.type %in% allow.pop))
    stop("Population type needs to be \"BC\",\"DH\",\"ARIL\" or \"RILn\" (see ?cal.pos).")
  ptype <- pop.type
  
  obj = list( )
  obj$geno = vector("list", length = length(table(mp$map[, 2]))) #max(mp$map[, 2]))
  obj$pheno <- vector("list", length = 1)
  
  dat <- mp$allres$data
  if(is.null(dimnames(dat)[[1]])) dimnames(dat)[[1]] <- paste("ind", 1:dim(dat)[1], sep="")
  obj$pheno <- data.frame(Genotype = factor(dimnames(dat)[[1]]))
  rownames(dat) <- NULL
  
  for(i in 1: length(obj$geno))
  {
    
    obj$geno[[i]]$data <- vector("list", length = 1)
    obj$geno[[i]]$map <-  vector("list", length = 1)
    
    mar.chr <- cumsum(table(mp$map[, 2]))
    
    if(i == 1) start <- 1 else start <- mar.chr[i - 1] + 1
    obj$geno[[i]]$data <- dat[  , start:mar.chr[i]]
    
    mar.order <- as.character(mp$map$markers[start:mar.chr[i]])
    obj$geno[[i]]$map <- (1:length(as.character(mp$map$markers[start:mar.chr[i]]))) + 0.2 ##fake positions
    names(obj$geno[[i]]$map) <- mar.order
    
    class(obj$geno[[i]]) <- "A"
  }
  names(obj$geno) <- 1:length(mar.chr)
  wp <- (1:23)[allow.pop %in% ptype]
  class(obj) <- c(c("bc","dh","riself",rep("f2",20))[wp],"cross")
  if(wp %in% 4:23) co <- convert2bcsft(obj, F.gen = wp - 3, estimate.map = FALSE)
  
  return(obj)
}

quickEst <- function(object, chr, map.function = "kosambi", ...){
  if (!any(class(object) == "cross"))
    stop("Input should have class \"cross\".")
  if (missing(chr))
    chr <- names(nmar(object))
  imf <- switch(map.function, kosambi = imf.k, haldane = imf.h,
                morgan = imf.m, cf = imf.cf)
  nm <- nmar(object)
  for(i in chr){
    temp <- subset(object, chr = i)
    if(nmar(temp) != 1){
      est <- est.rf(temp)$rf
      nc <- dim(est)[1]
      er <- est[cbind(2:nc,1:(nc - 1))]
      temp$geno[[i]]$map <- c(0,cumsum(imf(er)))
      names(temp$geno[[i]]$map) <- dimnames(temp$geno[[i]]$data)[[2]]
      tempa <- argmax.geno(temp, step = 0, map.function = map.function, ...)
      tempa$geno[[i]]$data <- tempa$geno[[i]]$argmax
      tempa$geno[[i]] <- tempa$geno[[i]][-3]
      esta <- est.rf(tempa)$rf
      era <- esta[cbind(2:nc,1:(nc - 1))]
      if(class(object)[1] == "riself")
        era <- (era/2)/(1 - era)
      object$geno[[i]]$map <- c(0,cumsum(imf(era)))
      names(object$geno[[i]]$map) <- dimnames(object$geno[[i]]$data)[[2]]
    }
  }
  object
}

cal.pos <- function(netgwasmap, pop.type= NULL, map.func = "haldane", chr ){
  if(is.null(pop.type)) stop("Population type needs to be \"BC\",\"DH\",\"ARIL\" or \"RILn\" ")
  
  map <- as.cross(netgwasmap, pop.type= pop.type)  
  map <- quickEst( map, chr, map.function = map.func)
  map <- jittermap(map, amount=1e-6)
  return(map)
}
