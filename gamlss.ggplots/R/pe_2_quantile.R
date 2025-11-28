################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
# this file has two function at the ment 
# pe_quantile and pe_2_quantile()
# it use also the  function  pe_1_quantile()
pe_quantile <- function( obj = NULL, 
                        term = NULL, 
                    quantile = c(0.95, 0.50, 0.05),
                        data = NULL, 
                    n.points = 100, 
                         how = c("median", "last", "fixed"), 
                    scenario = list(), 
                         col = "darkblue",
                   linewidth = 1.3,
                      legend = TRUE,
                        xlim = NULL,
                        ylim = NULL,
                        zlim = NULL, 
                        bins = 30, # for contour plot
                    name.obj = NULL,
                    alpharange = c(1,1), #alpha scale for raster 1,1 alpha disabled
                    ...) # whether to plot
{
     lterm <- length(term)
  name.obj <-  if (is.null(name.obj)) deparse(substitute(obj))
if (lterm==1) {
  gg <-  pe_1_quantile(obj = obj, 
                   term = term, 
               quantile = quantile,
                   data = data,
               n.points = n.points, 
                    how = how, 
               scenario = scenario, 
                    col = col,
              linewidth = linewidth,
                   ylim = ylim,
                 legend = legend, ...)}
  else if (lterm==2) {
    gg <-  pe_2_quantile(obj,
                  terms = term,
               quantile = quantile,
                   data = data, 
               n.points = n.points, 
                    how = how, 
               scenario = list(), 
                   # col = col,
                   linewidth = linewidth,
               #  legend = legend,
                   xlim = xlim,
                   ylim = ylim,
                   zlim = zlim, 
                   bins = bins, # for contour plot
               name.obj = name.obj,
             alpharange = alpharange,...)}
  else stop("only up to two way interactions can be plotted")
  gg  
}
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
pe_2_quantile <- function(obj,
                   terms = NULL,
                 quantile = c(0.05,0.5, 0.95),
                     data = NULL,
                      how = c("median", "last", "fixed"),
                 scenario = list(),
                 n.points = 100,  # number of points needed for evaluating the function
                     xlim = NULL,
                     ylim = NULL,
                     zlim = NULL, 
                     linewidth = 1.3,
                     bins = 30, # for contour plot
                #  filled = FALSE, #for contour plot
                    title,
                name.obj = NULL,
                 alpharange = c(1,1),
                ...#alpha scale for raster 1,1 alpha disabled
)
{
################################################################################  
#require(ggplot2)
#require(gamlss)
################################################################################  
# checking things
if (is.null(obj)||!class(obj)[1]=="gamlss") 
  stop("Supply a standard GAMLSS model in obj")
if (is.null(terms))  stop("The model terms are not set")
  cdf <- x <- y <- NULL  
if (is.null(data)) {data<-get(as.character(obj$call["data"]))}
  how <- match.arg(how)
# translate any character vectors as factors 
data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)],as.factor) 
# data 
if (any(grepl("data", names(obj$call)))) 
{
   DaTa <- if (startsWith(as.character(obj$call["data"]), "na.omit"))
             eval(parse(text=as.character(obj$call["data"]))) else 
             get(as.character(obj$call["data"]))	
} 
else if (is.null(data)) stop("The data argument is needed in obj")
 v.names <- names(DaTa)
     pos <- match(terms, v.names)
    lpos <- length(pos)        
name.obj <- if (is.null(name.obj)) deparse(substitute(obj))
if (lpos<=1) stop("supply 2 terms")
if (lpos>3) stop("only up to two terms are allowed")   
WhichFactor <- sapply(DaTa[,pos], is.factor)
  if (any(WhichFactor)) # if any is factor 
  {
   # pf <- pv <- 0
    pf <- which(WhichFactor)
    pv <- if (length(pf)==2) 0 else which(!WhichFactor) 
if (pv==2)   stop("the factor should be set second in order i.e c(\"var\",\"fac\")")  
    if (length(pf)==2)
    {
       fac1 <- levels(DaTa[,pos[1]])
n.points.f1 <- nlevels(DaTa[,pos[1]])
       fac2 <- levels(DaTa[,pos[2]])
n.points.f2 <- nlevels(DaTa[,pos[2]])
         d1 <- expand.grid(f1 = fac1, f2 = fac2)
  names(d1) <- terms
        mat <- matrix(0, nrow = dim(DaTa)[1]+dim(d1)[1],ncol =dim(DaTa)[2]) 
       case <- 3 # both factors
    } else
    {
       fac <- levels(DaTa[,pos[pf]])
n.points.f <- nlevels(DaTa[,pos[pf]])
       var <- seq(min(DaTa[,pos[pv]]), max(DaTa[,pos[pv]]), length.out=n.points)
        d1 <- expand.grid(x = var, f = fac)
    terms     
 names(d1) <- terms
       mat <- matrix(0, nrow = dim(DaTa)[1]+dim(d1)[1], ncol =dim(DaTa)[2])  
      case <- 2 # one factor one continuous
    } 
  } else
  {
        x <- seq(min(DaTa[,pos[1]]), max(DaTa[,pos[1]]), length.out=n.points)
        y <- seq(min(DaTa[,pos[2]]), max(DaTa[,pos[2]]), length.out=n.points)
       d1 <- expand.grid(x = x, y = y)
names(d1) <- terms
      mat <- matrix(0, nrow = dim(DaTa)[1]+n.points^2, ncol =dim(DaTa)[2])
     case <- 1 # both continuous
  }  
       dat.temp <- as.data.frame(mat)
names(dat.temp) <- v.names  
# get the get the x_J and x_(j) values
for (i in 1:dim(dat.temp)[2])
  {
    if(pos[1]==i)                  # if the variable of interest
    {                              # new data for x1 is
      dat.temp[,i] <- c(DaTa[,i],d1[,1]) # min(x) to max(x)
    } else
      if(pos[2]==i)                   # if the variable of interest
      {                               # new data for x2 is
        dat.temp[,i] <- c(DaTa[,i],d1[,2]) # min(x) to max(x)
      }
    else                            # for all other variables
    {                               # if scenario is set gets priority
      ma <- scenario[[v.names[i]]]
      if (is.null(ma))                  # if scenario in not set
      {
        if (how=="median")          # get the median for continuous 
          # or the level with high values for factor
        {
          if (is.character(DaTa[,i])) DaTa[,i] <- as.factor(DaTa[,i])
          ma <- if(is.factor(DaTa[,i])) levels(DaTa[,i])[which.max(table(DaTa[,i]))]
          else median(DaTa[,i])
        }
        if (how=="last")       # otherwise get the last values
        {
          if (is.character(DaTa[,i])) DaTa[,i] <- as.factor(DaTa[,i])
          ma <- if(is.factor(DaTa[,i])) levels(DaTa[,i])[which.max(table(DaTa[,i]))]
          else tail(DaTa[,i],1)
        }
      }
      dat.temp[,i] <-   c(DaTa[,i],rep(ma,dim(d1)[1]))       
    }
  } # end going thought the variables  
## get the pdf and q function
    pdf <- obj$family[1]
  binom <- pdf%in%gamlss::.gamlss.bi.list # whether binomial
   qfun <- paste("q", obj$family[[1]],sep="")
   lpar <- eval(parse(text=pdf))()$nopar
if (binom) {bd <- obj$bd ; Y <- obj$y}
# predict    
   daPred <-  tail(dat.temp,  dim(d1)[1])
    pp <-  predictAll(obj, newdata = daPred, output="matrix")
    qq <- list()
   lqq <- length(quantile) 
# evalualte the q-function  
if (lqq==1)
 {
  qq[[1]] <- switch(lpar, 
               eval(call(qfun, p= quantile, mu=pp[,"mu"])),       # 1
               eval(call(qfun, p= quantile, mu=pp[,"mu"], sigma=pp[,"sigma"])),        # 2
               eval(call(qfun, p= quantile, mu=pp[,"mu"], sigma=pp[,"sigma"],  nu=pp[,"nu"])),  # 3                   
               eval(call(qfun, p= quantile, mu=pp[,"mu"], sigma=pp[,"sigma"],  nu=pp[,"nu"], tau=pp[,"tau"])))
  } else
  {
  for (i in 1:lqq)
   {
     qq[[i]] <- switch(lpar, 
                eval(call(qfun, p= quantile[i], mu=pp[,"mu"])),       # 1
                eval(call(qfun, p= quantile[i], mu=pp[,"mu"], sigma=pp[,"sigma"])),        # 2
                eval(call(qfun, p= quantile[i], mu=pp[,"mu"], sigma=pp[,"sigma"],  nu=pp[,"nu"])),  # 3                   
                eval(call(qfun, p= quantile[i], mu=pp[,"mu"], sigma=pp[,"sigma"],  nu=pp[,"nu"], tau=pp[,"tau"])))
    }
  } 
# this is the data frame to use in the plots   
   txt.title <- if (missing(title))  
     paste("Partial quantile effect for model", name.obj) else title
################################################################################
# case 1 continuous versus continuous
if (case==1)
{
      da <- data.frame(cdf=unlist(qq), x1 = rep(x,lqq), 
                       y2=as.vector(t(replicate(n.points,y))), 
                       quantile=gl(lqq,length(qq[[1]]), labels = quantile))
names(da)[c(2,3)] <- terms
  yname <-  paste(obj$call$formula[[2]])
     gg <- ggplot2::ggplot(da, ggplot2::aes_string(terms[1], terms[2], 
                        fill = "cdf", alpha = "cdf" ))+
       ggplot2::geom_raster(interpolate=TRUE) +
       ggplot2::scale_fill_fermenter(name = yname,palette = "BrBG",n.breaks = 10, 
                           guide = guide_coloursteps(reverse = TRUE)) + 
       ggplot2::scale_alpha_continuous(name = "",range = alpharange, 
                       guide="none") + 
       ggplot2::ggtitle(txt.title)+
       ggplot2::facet_wrap(~quantile) 
########################################
# different ways of plotting  
######################################## 
# da1 <- da[1:10000,]
#   x1 <- x
#   x2 <- y
# mm=matrix(da1$cdf, nrow=100, ncol=100)
# contour(x=x1, y=x2, z=mm)
#########################################
# wireframe(cdf ~ Fl * A  , data = da, groups = quantiles,
#           scales = list(arrows = FALSE),
#           drape = TRUE, colorkey = TRUE,
#           screen = list(z = 30, x = -60))
##########################################  
}
################################################################################
# case 2 continuous versus factor
if (case==2)
{
  da <- data.frame(cdf=unlist(qq), x1 = rep(daPred[, terms[1]], lqq) , 
                   y2 = rep(daPred[, terms[2]], lqq), 
                   quantile=gl(lqq,length(qq[[1]]), labels = quantile))
  names(da)[c(2,3)] <- terms
 gg= ggplot2::ggplot(data=da, ggplot2::aes_string(x=terms[pv], y="cdf", 
                        group="quantile", col="quantile"))+
   ggplot2::geom_line(linewidth=linewidth)+
   ggplot2::ggtitle(txt.title)+
   ggplot2::facet_wrap(terms[2]) 
} 
################################################################################
# case 3 factor versus factor
if (case==3)
{
  da <- data.frame(cdf=unlist(qq), x1 = rep(daPred[, terms[1]], lqq) , 
                   y2 = rep(daPred[, terms[2]], lqq), 
                   quantile=gl(lqq,length(qq[[1]]), labels = quantile))
  names(da)[c(2,3)] <- terms
  gg <- ggplot2::ggplot(data=da, ggplot2::aes_string(x=terms[1], y="cdf",
                          group="quantile", color="quantile"))+
    ggplot2::geom_line(linewidth=linewidth)+
    ggplot2::geom_point(size=linewidth+2)+
    ggplot2::facet_wrap(terms[2]) 
}
  return(gg)
}###############################################################################
################################################################################
################################################################################
################################################################################
################################################################################
##test##
#data(rent)
# m5<-gamlss(R~pvc(Fl, by=A)+H+loc, sigma.fo=~pb(Fl)+pb(A)+H+loc,family=GA,data=rent)
# # second example
# pe_2_quantile(m5, c("Fl","A") )
# pe_2_quantile(m5, c("Fl","loc"))
# pe_2_quantile(m5, c("H","loc"))
# 
# pe_quantile(m5, "Fl")
# pe_quantile(m5, "H")
