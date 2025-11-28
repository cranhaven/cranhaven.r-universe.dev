################################################################################
################################################################################
################################################################################
################################################################################
#require(ggplot2)
#require(gamlss)
#require(grid)
################################################################################
################################################################################
################################################################################
################################################################################
### function to plot fitted additive terms in a GAMLSS model
### it is using ggplot
### To DO
### i) I think I should allow first order interaction
### ii) I should take out the ask and plot everything in more than one page if 
###      necessary DONE
### iii) to allow plotting class object Partial OK it needs to go trought all smoothers
# ACTION NEEDED
### Author: Mikis Stasinopoulos
################################################################################
################################################################################
################################################################################
################################################################################
   pe_terms = fitted_terms  <- function (object, 
                        what = c("mu","sigma","nu","tau"),  
                   parameter = NULL, 
                        data = NULL, 
                       terms = NULL, 
                       envir = environment(formula(object)), 
                     partial = FALSE, 
                         rug = FALSE, 
                   rug.sides = "b",
                     rug.col = "gray",
                       alpha = 0.2, # for shades in se's
                        ylim = c("common","free"),
                       xlabs = NULL, 
                       ylabs = NULL, 
                        main = NULL, 
                    term.col = "darkred",
                   resid.col = "lightblue", 
                 resid.alpha = 0.8,
                  resid.size = 1,
                        nrow = NULL,
                        ncol = NULL,
              plots.per.page = 9,
                    one.by.one = FALSE, #interactive() && nb.fig < n.tms &&.Device != "postscript", #dev.interactive() && nb.fig < n.tms
                 surface.gam = FALSE, 
                       polys = NULL, 
               polys.scheme = "topo",
                 col.ribbon = "darksalmon", # "darkseagreen3"
                 col.shaded = "gray", # for lo
                             ...) 
{
#-------------------------------------------------------------------------------
# Local functions
# i) CheckSmoList() to chech whether they are smoothers in terms
#-------------------------------------------------------------------------------
# plotLME <- function(obj)
# {
#   OBJ <- unclass(ranef(obj))
#   sizelist <- length(OBJ)
#   #  for (i in 1:sizelist) 
#   plot(OBJ[[1]],type="h", ylab=names(OBJ)[1], main="lme fit 1st random coeff.")
# }
#-------------------------------------------------------------------------------
# draw.polys.in <-function( polys, 
#                        object = NULL, 
#                        scheme = NULL,
#                        swapcolors = FALSE,
#                        n.col = 100,...)
# {
#   ## to get the range of all polygons    
#   for (i in 1:length(polys)) {
#     yr <- range(polys[[i]][, 2], na.rm = TRUE)
#     xr <- range(polys[[i]][, 1], na.rm = TRUE)
#     if (i == 1) {
#       ylim <- yr
#       xlim <- xr
#     }
#     else {
#       if (yr[1] < ylim[1]) 
#         ylim[1] <- yr[1]
#       if (yr[2] > ylim[2]) 
#         ylim[2] <- yr[2]
#       if (xr[1] < xlim[1]) 
#         xlim[1] <- xr[1]
#       if (xr[2] > xlim[2]) 
#         xlim[2] <- xr[2]
#     }
#   }
#   ## of no object just plot the polygons
#   mar <- par("mar")
#   oldpar <- par(mar = c(2, mar[2], 2, 1))
#   if (is.null(object)) {
#     plot(0, 0, ylim = ylim, xlim = xlim, xaxt = "n", yaxt = "n", 
#          type = "n", bty = "n", ylab = "", xlab = "",...)
#     for (i in 1:length(polys)) {
#       polygon(polys[[i]], col = NA)
#     }
#   }
#   else 
#   { # if object is defined  we must two alternatives
#     ## i) it is MRF object
#     ## ii) a list which defines the values and the areas    
#     if(class(object)=="MRF")
#     {
#       y.y <- object$beta 
#       #  x.x <- object$x
#     }
#     else
#     {
#       if (!is.vector(object))  stop("object class should be MRF or a vector with names matching the areas in the polys")
#       else
#       { 
#         y.y <- object
#       }
#   
#     }
#     
#      npolys <- names(polys)
#     nobject <- names(y.y)
#     if (is.null(nobject)) stop("the object do not have names")
#     else (!is.null(nobject) && !is.null(npolys)) 
# {
#       if (!all(sort(nobject)%in% sort(npolys))) 
#         stop("object names and polys names must match")
#     }
# y.y <- y.y[npolys]
# #fv1 <- tapply(y, x, mean)
# #fv <- object$beta  
# xmin <- xlim[1]
# xlim[1] <- xlim[1] - 0.1 * (xlim[2] - xlim[1])
# n.col <- n.col
# if (is.null(scheme)||scheme=="gray")
#   newscheme <- gray(0:n.col/n.col)
# else if (scheme == "heat"){
#   newscheme <- heat.colors(n.col + 1)
#   
# }
# else if (scheme == "rainbow")
#   newscheme <- rainbow(n.col+1)
# else if(scheme == "terrain")
#   newscheme <- terrain.colors(n.col+1)
# else if(scheme == "topo")
#   newscheme <- topo.colors(n.col+1)
# else if(scheme=="cm")
#   newscheme <- cm.colors(n.col+1)
# else {scheme=scheme
#       ramp <- colorRamp(c(scheme, "white"))
#       newscheme <-  rgb(ramp(seq(0, 1, length = n.col)), maxColorValue = 255)
# }
# 
# if(swapcolors==TRUE){
#   if((scheme=="heat")||(scheme=="rainbow")||(scheme=="terrain")||(scheme=="topo")||(scheme=="cm")) 
#     newscheme=rev(newscheme)
#   else stop("swapcolors just work for few options. Please, see help file.")
# }
# zlim <- range(pretty(y.y))
# for (i in 1:length(polys)) polys[[i]][, 2] <- zlim[1] + (zlim[2] - 
#                                                            zlim[1]) * (polys[[i]][, 2] - ylim[1])/(ylim[2] - ylim[1])
# ylim <- zlim
# plot(0, 0, ylim = ylim, xlim = xlim, type = "n", xaxt = "n", 
#      bty = "n", xlab = "", ylab = "",...)
# for (i in 1:length(polys)) {
#   coli <- round((y.y[i] - zlim[1])/(zlim[2] - zlim[1]) * 
#                   n.col) + 1
#   polygon(polys[[i]], col = newscheme[coli])
# }
# xmin <- min(c(axTicks(1), xlim[1]))
# dx <- (xlim[2] - xlim[1]) * 0.05
# x0 <- xmin - 2 * dx
# x1 <- xmin + dx
# dy <- (ylim[2] - ylim[1])/n.col
# poly <- matrix(c(x0, x0, x1, x1, ylim[1], ylim[1] + 
#                    dy, ylim[1] + dy, ylim[1]), 4, 2)
# for (i in 1:n.col) {
#   polygon(poly, col = newscheme[i], border = NA)
#   poly[, 2] <- poly[, 2] + dy
# }
# poly <- matrix(c(x0, x0, x1, x1, ylim[1], ylim[2], ylim[2], 
#                  ylim[1]), 4, 2)
# polygon(poly, border = "black")
#   }
# par(oldpar)
# }
#----------------------------------------------------------------------
# the results is something like 
#     c(0,0,1,2,2,2,3,4) if smootherrs where used in positions
#     c(0,0,1,1,0,0,1,1) 
CheckSmoList <- function(termList)
  {
#     gamlss.sm.list1 <- c( "cs","scs", "ps", "pb", "cy", "pvc", "pbm",  "pbj",   
#                          "mrf",   "mrfa", "sap",  "krig",   "lo", "random",
#                          "re",  "fp", "pp", "nl","ri","ridge","fk", "la",     
#                          "tr",  "ga",   "nn", "lo","own" )
    gamlss.sm.list1 <- .gamlss.sm.list
    gamlss.sm.list2  <- paste(gamlss.sm.list1,"(", sep="")   
    # ideally this should be done autonmatically
#     gamlss.sm.list2 <- c( "cs(","scs(", "ps(", "pb(", "cy(", "pvc(", "pbm(",  "pbj(",   
#                          "mrf(",   "mrfa(", "sap(",  "krig(",   "lo(", "random(",
#                          "re(",  "fp(", "pp(", "nl(","ri(","ridge(","fk(", "la(",     
#                          "tr(",  "ga(",   "nn(", "own(" )
    lgamsmol  <- length(gamlss.sm.list1)
         lsm  <- length(termList)
          res <- rep(0, lsm) 
          att <- rep(0, lsm)
    for (i in 1:length(gamlss.sm.list2))
    {
           LL <-grepl(gamlss.sm.list2[i], termList, fixed=TRUE)
         res  <- res+LL
      att[LL] <- gamlss.sm.list1[i]
    }
    res <- cumsum(res)
    attr(res, "whichSmo") <- att
    res
  }
#----end of local function------CheckSmoList-----------------------------  
CheckSmoWithPlot <- function(termList)
{
  #gamlss.Smo.plot.list <- c( "tr", "ga")
  gamlss.Smo.plot.list1 <- c( "tr(", "ga(", "nn(", "pvc(", "mrf(", "mrfa(", "ri(",
                             "own(" , "re(", "lo(", "pcat(",  "ba(")# "ma(",
  lgamsmol  <- length(gamlss.Smo.plot.list1)
  lsm  <- length(termList)
  res <- rep(0, lsm) 
  for (i in gamlss.Smo.plot.list1)
  {
    LL <-   grepl(i, termList,  fixed=TRUE) 
    res  <- res+LL
  }
res  
}
# more local functions--------------------------------------------------
carrier <- function(term) 
  {
  if (length(term) > 1)   carrier(term[[2]])
  else eval(term, data, enclos = pf)
  }# with envir  Global
#-----------------------------------------------------------------------
carrier.name <- function(term) 
    {
    if (length(term) > 1) carrier.name(term[[2]])
    else as.character(term)
    }
#--------END of local functions-----------------------------------------
#-----------------------------------------------------------------------
########################################################################
#  begining of the proper function
########################################################################
## only for gamlss objects 
    if (!is.gamlss(object))  stop(paste("This is not an gamlss object", "\n", "")) 
           what <- if (!is.null(parameter))  {
    match.arg(parameter, choices=c("mu", "sigma", "nu", "tau"))} else  match.arg(what)
           ylim <- match.arg(ylim)
           se <- TRUE # dummy
           par(ask=FALSE)
       #  pages <-  1
       #  scheme <- match.arg(scheme)
## checking the parameter
    if (!what%in%object$par) stop(paste(what,"is not a parameter in the object","\n"))
## getting the specific term for plotting
    which.terms <- terms
## get all terms and attributes 
      par.terms <- object[[paste(what, "terms", sep=".")]]
      par.attr  <- attributes(par.terms)
          Terms <- if (is.null(terms)) lpred(object, what = what, type = "terms", se.fit = TRUE)
                                  else lpred(object, what = what, type = "terms", se.fit = TRUE, terms = terms)
# the number of terms
          n.tms <- ncol(tms <- as.matrix(Terms$fit))
## if the parameters has only a constant fitted stop
     if (n.tms == 0)
          stop("The model for ", what, " has only the constant fitted") 
## model frame   
             mf <- model.frame(object, what = what) 
## this take care if data is used         
    if (is.null(data))  # get the data from gamlss data 
           data <- eval(object$call$data, envir)
    if (is.null(data))  # if still null get from model frame
           data <- mf
    if (NROW(tms) < NROW(data)) 
     {
       use.rows <- match(rownames(tms), rownames(data))
     }
    else use.rows <- NULL
            nmt <- colnames(tms)                    # the names of terms 
## whether there are interactions in the model (they are difficult to plot)
   Interactions <- par.attr$order > 1 
## if interaction nmt has to change -------------------------------------
     if (any(Interactions))
        {
                     nmt <- nmt[!Interactions] # take out interactions
               Terms$fit <- Terms$fit[,nmt,  drop = FALSE]  
            Terms$se.fit <- Terms$se.fit[,nmt,  drop = FALSE] 
                   n.tms <- ncol(tms <- as.matrix(Terms$fit))
           # I am assuming that 'terms' will be used wisely here 
           warning("interactions have been taken out from the plots,
                  plots maybe mislealing, try pe_param() instead")
        }
#------------------------------------------------------------------------
           cn <- parse(text = nmt) # as expression
 ifSpecialSmo <- CheckSmoWithPlot(nmt) #??????????????????????????????????
whichValueSmo <- CheckSmoList(nmt)
# if (!is.null(smooth)) # I do not need this but match.fun() is very interesting 
#         smooth <- match.fun(smooth) 
    if (is.null(ylabs)) 
          ylabs <- paste("Partial for", nmt) # get the labels 
    if (is.null(main)) 
           main <- ""
    else if (is.logical(main)) 
           main <- if (main)  deparse(object$call, 500)
                  else ""
                 else if (!is.character(main)) 
                   stop("`main' must be TRUE, FALSE, NULL or character (vector).")
          main <- rep(main, length = n.tms)
            pf <- envir  # the carrier has different envir (the Global)

if (is.null(xlabs))#  get the x lables
    {
       xlabs <- unlist(lapply(cn, carrier.name))
}
        pres <- residuals(object, what = what, type="partial")
if (!is.null(which.terms)) 
        pres <- pres[, which.terms, drop = FALSE]
if (any(Interactions))
        pres <- pres[, nmt, drop = FALSE]
      is.fac <- sapply(nmt, function(i) is.factor(mf[, i])) # whether factors
      nb.fig <- prod(par("mfcol")) # the number of figures
       ylims <- ylim # default "common"
if (identical(ylims, "common"))  # whether common limit in y
    {
        suppressWarnings(   
                   {
                    Terms$se.fit <- ifelse(Terms$se.fit==Inf, NA, Terms$se.fit)
                     ylims <-  range(tms + 1.05 * 2 * Terms$se.fit, tms - 1.05 * 
                        2 * Terms$se.fit, na.rm = TRUE)
                      })
     if ( partial) 
        ylims <- range(ylims, pres, na.rm = TRUE)
     if (rug) 
        ylims[1L] <- ylims[1L] - 0.07 * diff(ylims)
    } # finnish  limits for y
########################################################################
# pages 
 grp <- fit <- x <- y <-low <- high <- pre <- NULL 
   n.plots <- n.tms
        GG <- list()     
########################################################################
########################################################################
for (i in 1:n.tms) # START of the 1:n.tms loop
  {
# if we need different y limit for each variable 
   if (identical(ylim, "free"))
    { 
               ylims <- range(tms[, i], na.rm = TRUE)
          #  if (se) 
               ylims <- range(ylims, tms[, i] + 1.05 * 2 * Terms$se.fit[, 
                            i], tms[, i] - 1.05 * 2 * Terms$se.fit[, i], 
                            na.rm = TRUE)
            if ( partial)  
              ylims <- range(ylims, pres[, i], na.rm = TRUE)
            if (rug) 
           ylims[1] <- ylims[1] - 0.07 * diff(ylims)
    }
 if (!ifSpecialSmo[i])#-------------------------------------------------
 { #   if is a normal smoother use this
#  if is a factor--------------------------------------------------------
if (is.fac[i]) # if the term is a factor
  {
            ff <- mf[, nmt[i]]
      if (!is.null(object$na.action)) 
            ff <- naresid(object$na.action, ff)
            ll <- levels(ff)
         xlims <- range(seq(along = ll)) + c(-0.5, 0.5)
            xx <- as.numeric(ff)
       if (rug) 
         {
      xlims[1] <- xlims[1] - 0.07 * diff(xlims)
      xlims[2] <- xlims[2] + 0.03 * diff(xlims)
        }
            wi <-rep(0, length(ll))
      for (j in seq(along = ll)) 
          {
        wi[j] <- which(ff == ll[j])[1]
          }
          dse <-  if (is.null(terms)) data.frame(grp = ll, fit = Terms$fit[wi,i], se = Terms$se.fit[wi,i])
                   else                data.frame(grp = ll, fit = Terms$fit[wi],   se = Terms$se.fit[wi])
          gg  <- ggplot2::ggplot(dse, 
                 ggplot2::aes(grp, fit, ymin = fit-2*se, ymax = fit+2*se))
          gg <- gg + ggplot2::geom_errorbar()+
                 ggplot2::geom_pointrange(color=term.col)+
                ggplot2::ylab(ylabs[i])+xlab(xlabs[i])+ ylim(ylims)
     if ( partial) 
        {
          da <- data.frame(grp = ff, fit = pres[, i])
          gg  <- ggplot2::ggplot(dse, 
                 ggplot2::aes(grp, fit, ymin = fit-2*se, ymax = fit+2*se))
          gg <- gg + 
            ggplot2::geom_jitter(data=da, aes(x=grp,y=fit), color=resid.col, 
                            size=resid.size, alpha=resid.alpha)+
            ggplot2::geom_errorbar(data=dse)+
            ggplot2::geom_pointrange(color=term.col)+
            ggplot2::ylab(ylabs[i])+xlab(xlabs[i])+ ylim(ylims)
          }
} # end if factor  ---------------------------------------------------
  else 
{ # here is where changes had to be made at the moment every pass
            # cn is expression
              xx <- carrier(cn[[i]]) # ds Friday, October 9, 2009 at 13:13
       if (is.factor(xx)) xx <- seq(along = levels(xx))
       if (!is.null(use.rows)) # in case some of the rows are not used 
              xx <- xx[use.rows]
           xlims <- range(xx, na.rm = TRUE)
        if (rug) 
        xlims[1] <- xlims[1] - 0.07 * diff(xlims)
              oo <- order(xx)
             dse <- data.frame(x=xx[], y=tms[, i], low=tms[, i]-2*Terms$se.fit[, i],
                            high=tms[, i]+2*Terms$se.fit[, i], xx=xx, 
                            pre=pres[, i] )
              gg <- ggplot2::ggplot(data=dse)+
                ggplot2::geom_line(data=dse, ggplot2::aes(x=x, y=y), color=term.col)+
                ggplot2::ylab(ylabs[i])+xlab(xlabs[i])+ ylim(ylims)
              gg <-  gg +  ggplot2::geom_ribbon(aes(ymin=low, ymax=high, x=x),
                                          alpha=alpha, fill=col.ribbon )
        if ( partial) 
          {
              gg <- gg+ ggplot2::geom_point(ggplot2::aes(x=x, y=pre), color=resid.col, 
                                 alpha=resid.alpha, size=resid.size)+
                ggplot2::geom_line(data=dse, aes(x=x, y=y), color=term.col)+ 
                ggplot2::geom_ribbon(aes(ymin=low, ymax=high, x=x), alpha=alpha, fill=col.ribbon) 
          }
        if (rug) 
          {
             gg <-  gg+ ggplot2::geom_rug(aes(x=x,y=y), sides=rug.sides, colour=rug.col)
          }
}  
   GG[[i]]  <- gg
} # end of all normal smoother -Now the special ones which have their own plotting function
# else  # -Now the special ones which have their own plotting function
# { # the special smoothers who have a plotting function 
#         if (attr(whichValueSmo, "whichSmo")[i]=="ga"&&surface.gam==TRUE)
#            {
#             plot(getSmo(object, what, which=whichValueSmo[i]))
#            } 
#         if (attr(whichValueSmo, "whichSmo")[i]=="ga"&&surface.gam==FALSE)
#           {
#             plot(getSmo(object, what, which=whichValueSmo[i]))
#           } 
#         if (attr(whichValueSmo, "whichSmo")[i]=="ba"&&surface.gam==TRUE)
#           {
#             plot(getSmo(object, what, which=whichValueSmo[i]))
#           } 
#         if (attr(whichValueSmo, "whichSmo")[i]=="ba"&&surface.gam==FALSE)
#           {
#             plot(getSmo(object, what, which=whichValueSmo[i]))
#           } 
#         if (attr(whichValueSmo, "whichSmo")[i]=="nn")
#           {
#              plot(getSmo(object, what, which=whichValueSmo[i]), y.lab=expression(eta))
#           } 
#         if (attr(whichValueSmo, "whichSmo")[i]=="re")
#           {
#             plotLME(getSmo(object, what, which=whichValueSmo[i])) 
#           } 
#         if (attr(whichValueSmo, "whichSmo")[i]=="mrf"||attr(whichValueSmo, "whichSmo")[i]=="mrfa") 
#           { 
#         if (is.null(polys)) 
#           { warning("no polygon information is given, null plot is produced")
#           } else
#           {
#             draw.polys.in(polys, getSmo(object, what, which=whichValueSmo[i]), scheme=polys.scheme)
#           }         
#           }
#         if (attr(whichValueSmo, "whichSmo")[i]=="tr")
#           {
#             plot(getSmo(object, what, which=whichValueSmo[i]))
#             text(getSmo(object, what, which=whichValueSmo[i]))
#           }
#         if (attr(whichValueSmo, "whichSmo")[i]=="ri")
#           {
#             plot(getSmo(object, what, which=whichValueSmo[i]))
#           }
#         if (attr(whichValueSmo, "whichSmo")[i]=="pcat")
#           {
#             plot(getSmo(object, what, which=whichValueSmo[i]))
#           }
#         if (attr(whichValueSmo, "whichSmo")[i]=="pvc")
#           {
#             plot(getSmo(object, what, which=whichValueSmo[i]))
#           }
#         if (attr(whichValueSmo, "whichSmo")[i]=="lo")
#           {
#             vis.lo(getSmo(object, what, which=whichValueSmo[i]),  col.term=term.col,
#                    col.shaded = col.shaded, col.res = resid.col )
#           }
#           if (attr(whichValueSmo, "whichSmo")[i]=="tr") 
#               text(getSmo(object, what, which=whichValueSmo[i]))
# } # end of special smoothers
} # end of the  1:n.tms lop -----------------------------------------------
if (one.by.one)
{
    oask <- devAskNewPage(one.by.one)
    on.exit(devAskNewPage(oask))
  for (i in 1:n.plots) print(GG[[i]])
} 
else
{
####################################################################### 
    define_region <- function(row, col){
          viewport(layout.pos.row=row, layout.pos.col=col) }
######################################################################  
 # if (pages > n.plots)  pages <- n.plots # if pages bigger use no of plots 
 # if (pages < 1)      pages <- 1  # set to one
if (n.plots>plots.per.page)
  {
    pages <- ceiling(n.plots/plots.per.page)  
     page <- n.plots%/%plots.per.page
      ppp <- rep(plots.per.page,page) 
    if (n.plots%%plots.per.page != 0) ppp <- c(ppp, n.plots%%plots.per.page)
    if (plots.per.page==9)
    {
      nc <- 3
      nr <- 3
      IJ <- expand.grid(j=1:nc, i=1:nr) 
    } else
    {
      if (is.null(nrow)||is.null(nrow)) stop("the nrow and ncol need to be defined") 
      if (plots.per.page> ncol*nrow) stop("the nrow or ncol has to increase") 
      nc <- ncol
      nr <- nrow
      IJ <- expand.grid(j=1:nc, i=1:nr) 
      IJ <- IJ[1:plots.per.page,]
    }  
   start <- 1
  finish <- ppp[1]
  for (pa in 1:pages)
      {
       grid.newpage()
       pushViewport(viewport(layout=grid.layout(nrow=nr,ncol=nc)))   
        for (p  in start:finish) 
        {
          print(GG[[p]], vp=define_region(IJ$i[p],IJ$j[p]))
        }
     start <- finish +1
    finish <- finish+ppp[pa+1] 
        IJ <- rbind(IJ, IJ)
      oask <- devAskNewPage(ask=TRUE)
      on.exit(devAskNewPage(oask))
        } 
  } else  
  {
   pages <- 1
     ppp <- n.plots%/%pages
     nc  <- nr <- trunc(sqrt(ppp))
    if (nc < 1)  nr <- nc <- 1
    if (nc * nr < ppp) nc <- nc + 1
    if (nc * nr < ppp) nr <- nr + 1 
     IJ <- expand.grid(j=1:nc, i=1:nr)
    grid.newpage()
    pushViewport(viewport(layout=grid.layout(nrow=nr,ncol=nc)))
    for (p  in 1:n.plots) 
    {
      print(GG[[p]], vp=define_region(IJ$i[p],IJ$j[p]))
    }
  }      
}         

invisible(GG) 
}
################################################################################
################################################################################
################################################################################
################################################################################

