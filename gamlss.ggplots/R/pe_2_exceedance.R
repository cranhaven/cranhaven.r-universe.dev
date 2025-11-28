################################################################################
################################################################################
################################################################################
################################################################################
pe_2_exceedance<-function(obj,
                      term = NULL,
                       fac = NULL, #additional factor/covariate to split data into
                     yterm = NULL,
                      data = NULL,
                     res = 300, 
                       how = c("median", "last"),
                  scenario = list(),
                 threshold = NULL,
                      xmin = NULL,
                      xmax = NULL,
                      ymin = NULL,
                      ymax = NULL, 
                alpharange = c(1,1), #alpha scale for raster 1,1 alpha disabled
                    raster = FALSE, #if threshold set with fac should results be drawn on raster
                   contour = TRUE, # use binning or continuous scale
                 facvalues = NULL # use if you want specific factor/covariate values
)
{
################################################################################  
#require(ggplot2)
#require(gamlss)
################################################################################ 
  xrangev <- yrva <- p_exceedance <- NULL
# checking things
if (is.null(obj) || !class(obj)[1] == "gamlss") 
    stop("Supply a standard GAMLSS model in obj")
if (is.null(terms)) {stop("The model term is not set")}
if (is.null(fac)) {stop("The additional factor is not set")}
if (is.null(data)) {data<-get(as.character(obj$call["data"]))}
   how <- match.arg(how)
# translate character as factors 
data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)],as.factor) 
# checking y  
if(is.null(yterm)) {yvalues=obj$y; yterm=obj$call$formula[[2]]}
  else{ yvalues=data[,yterm] }
  yterm <- as.character(yterm)
  # min and max for x and y's 
  # the range of values for y and x
if(is.numeric(data[,term]))
  {
  if(is.null(xmin)) xmin = min(data[,term],na.rm = TRUE)
  if(is.null(xmax)) xmax = max(data[,term],na.rm = TRUE)
    covariaterange <- seq(xmin,xmax,length.out=res)
  }
if(is.factor(data[,term])){
    covariaterange <- levels(data[,term])
  }
## should we make also distinquish between continuous and discrete distributions?
if(is.numeric(yvalues))
  {
  if(is.null(ymin)) ymin = min(yvalues, na.rm = T)
  if(is.null(ymax)) ymax = max(yvalues, na.rm = T)
    yrange <- seq(ymin, ymax, length.out=res)
  }
if(is.factor(yvalues))
  {
    yrange <- levels(yvalues)
  }
# the original data in DaTa
  DaTa <- data
if(is.factor(DaTa[,fac])) 
  {
    fac1 <- levels(DaTa[,fac])
  } else
  { fac1 <- round(quantile(DaTa[,fac],c(0.05,0.5,0.95)),3)
  }
# if factor values are specified use those instead
if(!is.null(facvalues)) {fac1 <- facvalues}
############################      
     v.names <- names(DaTa)
    dat.temp <- DaTa[1,]
dat.temp[1,] <- NA
# get the scenarios
for (i in 1:dim(DaTa)[2]) 
  {
    ma <- scenario[[v.names[i]]]
    if (is.null(ma)) 
    {
      if (how == "median") 
      {
        if (is.character(DaTa[,i])) DaTa[,i] <- as.factor(DaTa[,i])
        ma <- if (is.factor(DaTa[, i])) 
                    as.factor(levels(DaTa[, i])[which.max(table(DaTa[, i]))])
              else median(DaTa[, i])
      }
      if (how == "last") 
      {
        if (is.character(DaTa[,i])) DaTa[,i] <- as.factor(DaTa[,i])
          ma <- if (is.factor(DaTa[, i])) 
                     as.factor(levels(DaTa[, i])[which.max(table(DaTa[, i]))])
                else tail(DaTa[, i], 1)
      }
    }
    dat.temp[, i] <- ma
  }
# create new data on which the model is predicting
        DF <- expand.grid(yrva = yrange, xrangev = covariaterange,factorsplit = fac1)
       DF1 <- cbind(dat.temp[1,], DF, row.names = NULL)
DF1[,term] <- DF1$xrangev
#  as splitting factor present rename
 DF1[,fac] <- (DF1$factorsplit)
       ghj <- 1:ncol(DaTa)
        Ue <- as.data.frame(predictAll(obj,newdata=DF1[,ghj],output="matrix",type="response",data=DaTa))
# y is not needed here 
      Ue$y <- NULL
        DF <- (cbind(Ue,DF1, row.names = NULL))
rm(Ue, DF1)
      pfun <- paste0("p", obj$family[[1]])[1]
       cdf <- obj$family[1]
     nopar <- eval(parse(text=cdf))()$nopar
        DF <- as.data.frame(DF)
DF[,yterm] <- DF[,"yrva"] 
 DF[,term] <- DF[,"xrangev"] 
     survF <- switch(nopar,
             eval(call(pfun, q = DF[,yterm], mu = DF[,"mu"], lower.tail = FALSE)),
             eval(call(pfun, q = DF[,yterm], mu=DF[,"mu"], sigma=DF[,"sigma"], lower.tail=FALSE)),
             eval(call(pfun, q = DF[,yterm], mu=DF[,"mu"], sigma=DF[,"sigma"],nu=DF[,"nu"], lower.tail=FALSE)),
             eval(call(pfun, q = DF[,yterm], mu=DF[,"mu"], sigma=DF[,"sigma"],nu=DF[,"nu"], tau=DF[,"tau"],lower.tail=FALSE))
  )
  DF$p_exceedance <- survF
if(is.numeric(DF$xrangev))
{
 if(!is.numeric(DaTa[,fac]))
  {
     if(!isTRUE(contour)) 
       {
    gg <- ggplot(DF, aes( x = xrangev, y = yrva, fill = p_exceedance, 
                    alpha = p_exceedance)) +
            geom_raster(interpolate=TRUE) + 
            scale_fill_distiller(name = "Exceedance\nProbability",
                    palette = "Spectral",trans = 'reverse') +
            scale_alpha_continuous(name = "",range = alpharange, 
                    guide = "none") +
            ylab(yterm) +
            xlab(term) +
            facet_wrap(~factorsplit)
      }
     if(isTRUE(contour))
     {
    gg <- ggplot2::ggplot(DF, aes(x = xrangev, y = yrva,fill = p_exceedance,
                    alpha = p_exceedance))+
      ggplot2::geom_raster(interpolate=TRUE)+
      ggplot2::scale_fill_fermenter(name = "Exceedance\nProbability", 
                    palette="BrBG", n.breaks = 11, 
                    guide = guide_coloursteps(reverse = TRUE))+
      ggplot2::scale_alpha_continuous(name = "",range = alpharange, 
                                   guide = "none")+
      ggplot2::ylab(yterm)+
      ggplot2::xlab(term)+
      ggplot2::facet_wrap(~factorsplit) 
     }
 }
}  
if(is.numeric(DaTa[,fac]))
 {
  DF$factorsplit <- paste0(fac," = ",as.character(DF$factorsplit))
  if(isTRUE(contour))
   {
  gg <- ggplot2::ggplot(DF, ggplot2::aes(x=xrangev, y=yrva, fill=p_exceedance, 
              alpha = p_exceedance)) +
    ggplot2::geom_raster(interpolate=TRUE) + 
    ggplot2::scale_fill_distiller(name="Exceedance\nProbability", 
              palette = "Spectral",trans = 'reverse')+
    ggplot2::scale_alpha_continuous(name="", range = alpharange, guide = "none")+
    ggplot2::ylab(yterm)+
    ggplot2::xlab(term)+
    ggplot2::facet_wrap(~factorsplit)
   }
  if(isTRUE(contour)) 
   {
  gg <- ggplot2::ggplot(DF, ggplot2::aes(x = xrangev, y = yrva, fill = p_exceedance,
              alpha = p_exceedance)) + 
    ggplot2::geom_raster(interpolate=TRUE) +
    ggplot2::scale_fill_fermenter(name = "Exceedance\nProbability",
              palette = "BrBG",n.breaks = 11, guide = 
                ggplot2::guide_coloursteps(reverse = TRUE)) + 
    ggplot2::scale_alpha_continuous(name = "",range = alpharange, 
                guide="none") + 
    ggplot2::ylab(yterm) + 
    ggplot2::xlab(term) + 
    ggplot2::facet_wrap(~factorsplit)  
  }
 }
if(is.factor(DF$xrangev))
  {
  if(!is.numeric(DaTa[,fac]))
    {
      if(!isTRUE(contour)) 
      {
  gg <- ggplot2::ggplot(DF, ggplot2::aes(x = xrangev, y = yrva, fill = p_exceedance, 
                alpha = p_exceedance, group = xrangev, width=.75)) + 
    ggplot2::geom_tile()+ 
    ggplot2::scale_fill_distiller(name = "Exceedance\nProbability", 
                palette = "Spectral",trans = 'reverse') + 
    ggplot2::scale_alpha_continuous(name = "", range = alpharange, 
                guide="none")+
    ggplot2::ylab(yterm)+
    ggplot2::xlab(term)+
    ggplot2::facet_wrap(~factorsplit)      
      }
       
  if(isTRUE(contour))  
    gg<- ggplot2::ggplot(DF,ggplot2::aes(x=xrangev,y=yrva,fill=p_exceedance,
                  alpha=p_exceedance, group = xrangev, width=.75))+
      ggplot2::geom_tile()+
      ggplot2::scale_fill_fermenter(name="Exceedance\nProbability",palette="BrBG",n.breaks=11,guide=guide_coloursteps(reverse = TRUE))+
      ggplot2::scale_alpha_continuous(name="",range=alpharange,guide="none")+
      ggplot2::ylab(yterm)+
      ggplot2::xlab(term)+
      ggplot2::facet_wrap(~factorsplit)
    }
    if(is.numeric(DaTa[,fac]))
      {
      DF$factorsplit<-paste0(fac," = ",as.character(DF$factorsplit))
      if(isTRUE(contour)) 
        gg<-ggplot2::ggplot(DF,ggplot2::aes(x=xrangev,y=yrva,fill=p_exceedance,
                    alpha=p_exceedance,group = xrangev,width=.75))+
          ggplot2::geom_tile()+ 
          ggplot2::scale_fill_distiller(name="Exceedance\nProbability",
              palette = "Spectral",trans = 'reverse')+
          ggplot2::scale_alpha_continuous(name="",range=alpharange,guide="none")+
          ggplot2::ylab(yterm)+xlab(term)+
          ggplot2::facet_wrap(~factorsplit)
  if(isTRUE(contour)) gg <- ggplot2::ggplot(DF,
              ggplot2::aes(x=xrangev, y=yrva, fill=p_exceedance, 
              alpha = p_exceedance,group = xrangev, width =.75))+
      ggplot2::geom_tile()+
      ggplot2::scale_fill_fermenter(name="Exceedance\nProbability",
      palette="BrBG",n.breaks = 11, guide = guide_coloursteps(reverse = TRUE))+
      ggplot2::scale_alpha_continuous(name="",range = alpharange, guide = "none")+
      ggplot2::ylab(yterm)+
      ggplot2::xlab(term)+
      ggplot2::facet_wrap(~factorsplit)
     }
  }
  return(gg)
}
################################################################################
################################################################################
################################################################################
################################################################################
##test##
#data(rent)
#m5<-gamlss(R~pvc(Fl, by=A)+H+loc, sigma.fo=~pb(Fl)+pb(A)+H+loc,family=GA,data=rent)
# second example
#pe_2_exceedance(m5, "Fl",fac="A",res=200)
#pe_2_exceedance(m5, "Fl",fac="loc",res=200)
#pe_2_exceedance(m5, "H",fac="loc",res=200,contour=FALSE)
#pe_2_exceedance(m5, "H",fac="loc",res=200,contour=TRUE)
