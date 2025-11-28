################################################################################
################################################################################
################################################################################
################################################################################
# created on the 08-06-2021
# TO DO
# i) what is data set includes character vectors no declared as factors
# ii) not going holding the whole data but only the ones in formulae
################################################################################
################################################################################
################################################################################
################################################################################
pe_param <- function(obj = NULL, # the gamlss object
                    term = NULL, # which terms to get the derivative
                    data = NULL, # which data is needed here
                n.points = 100,  # number of points needed for evaluating the function
               parameter = c("mu", "sigma", "nu", "tau"), # which parameter
                    type = c("parameter", "eta"),
                scenario = list(), # see below (1)
                     how = c("median", "last", "fixed"),
                     col = "darkblue",
                    size = 1.3,
                name.obj = NULL,
                rug.plot = TRUE,
                 rug.col = "gray",
                rug.size = 0.5,  
               data.plot = FALSE,
                data.col = "lightblue",
               data.size = 0.1,
             factor.size = 15,
              data.alpha = 0.9,
                    bins = 30, # for contour plot
                  filled = FALSE, #for contour plot
                    ylim = NULL,
                    title) # whether to plot
{
     lterm <- length(term)
  name.obj <-  if (is.null(name.obj)) deparse(substitute(obj))
if (lterm==1) {gg <-  pe_1_param(obj = obj, 
                                term = term, 
                                data = data, 
                            n.points = n.points, 
                           parameter = parameter, 
                                type = type, 
                                 how = how,
                            scenario = scenario,  
                                 col = col,
                           data.plot = data.plot,
                            data.col = data.col,
                           data.size = data.size,
                          data.alpha = data.alpha,
                            rug.plot = rug.plot, 
                             rug.col = rug.col,
                            rug.size = rug.size,
                         factor.size = factor.size,
                                size = size, 
                            name.obj = name.obj, 
                                ylim = ylim,
                               title = title)}
else if (lterm==2) {
      gg <-  pe_2_param(obj = obj, 
                      terms = term,
                       data = data, 
                   n.points = n.points, 
                  parameter = parameter, 
                       type = type,  
                        how = how,
                   scenario = scenario,
                       size = size, 
                       bins = bins,
                     filled = filled,
                   name.obj = name.obj, 
                        col = col, 
                   data.col = data.col, 
                  data.size = data.size,
                 data.alpha = data.alpha, 
                  data.plot = data.plot,
                      title = title)}
    else stop("only up to two way interactions can be plotted")
  gg  
}

################################################################################
################################################################################
################################################################################
################################################################################
# created on the 08-06-2021
# to Do
# i) what is data set inculeds character vectors no declared as factors
################################################################################
################################################################################
################################################################################
################################################################################
pe_1_param <- function(obj = NULL, # the gamlss object
                       term = NULL, # which term to get the derivative
                       data = NULL, # which data is needed here
                   n.points = 100,  # number of points needed for evaluating the function
                  parameter = c("mu", "sigma", "nu", "tau"), # which parameter
                       type = c("parameter", "eta"),
                        how = c("median", "last", "fixed"),
                 scale.from = c("mean", "median", "none"),
                   scenario = list(), # see below (1)
                        col = "darkblue",
                       size = 1.3,
                   name.obj = NULL,
                  data.plot = FALSE,
                   data.col = "lightblue",
                  data.size = 0.1,
                 data.alpha = 0.9,
                   rug.plot = TRUE,
                    rug.col = "gray",
                   rug.size = 0.5,
                factor.size = 15,
                       ylim = NULL,
                      title) # whether to plot
{
#  scenario:a named list of the values to use for the other predictor terms. 
#  Variables omitted from this list will have values set to the median 
#  for continuous variables and 
#  the most commonly occuring level for factors 
#  or the last observation
if (is.null(obj)||!class(obj)[1]=="gamlss") stop("Supply a standard GAMLSS model in obj")
if (is.null(term))  stop("The model term is not set")
         x <-  y <- NULL
       how <- match.arg(how)
      type <- match.arg(type)
      type <- if (type=="parameter") "response" else "link"
 parameter <- match.arg(parameter)
scale.from <- match.arg(scale.from)
if (any(grepl("data", names(obj$call)))) 
  {
    DaTa <- if (startsWith(as.character(obj$call["data"]), "na.omit"))
                eval(parse(text=as.character(obj$call["data"]))) 
             else get(as.character(obj$call["data"]))	
}
else if (missing(data)) stop("The data argument is needed in obj")
#browser()
# here I may call the function which only pick the right variables not all
     # DaTa <- switch(parameter, 
     #                "mu" = Formulae2DF(list(obj$mu.formula), data=DaTa),
     #             "sigma"=Formulae2DF(list(obj$sigma.formula), data=DaTa),
     #                "nu"=Formulae2DF(list(obj$nu.formula), data=DaTa),
     #               "tau"=Formulae2DF(list(obj$tau.formula), data=DaTa)
      #           )
  v.names <- names(DaTa)
      pos <- which(v.names==term)
   #   paste(obj$call$formula[[2]])
if (pos<1) stop("supply a  term")
if (is.factor(DaTa[,pos])||is.character(DaTa[,pos])) 
{
   if (is.factor(DaTa[,pos]))
     {
         xvar <- levels(DaTa[,pos])
     n.points <- nlevels(DaTa[,pos])
 it.is.factor <- TRUE
     } else 
     {
        xvar <- attr(table(DaTa[,pos]),"names")
    n.points <- length(xvar)
it.is.factor <- TRUE
     }  
} else
  {
        xvar <-  seq(from = min(DaTa[,pos]), to=max(DaTa[,pos]), length.out=n.points)
it.is.factor <- FALSE
  }   
         mat <- matrix(0, nrow = dim(DaTa)[1]+n.points, ncol =dim(DaTa)[2])
    dat.temp <- as.data.frame(mat)
names(dat.temp) <- v.names             
## creating new data         
for (i in 1:dim(dat.temp)[2])
  {
    if(pos==i)                      # if the variable of interest
    {                               # new data for x is
dat.temp[,i]  <- if (is.factor(DaTa[,i])) 
                   as.factor(c(as.character(DaTa[,i]),as.character(xvar)))
                 else c(DaTa[,i],xvar)
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
  dat.temp[,i] <- if (is.factor(DaTa[,i])) as.factor(c(as.character(DaTa[,i]),as.character(rep(ma,n.points))))
                      else c(DaTa[,i],rep(ma,n.points))
    }
} # end going thought the variables
## predict  
# pp = attr(predict(obj,  type = "term", parameter = parameter),"constant")
fittted.star  <- predict(obj, newdata=tail(dat.temp, n.points), type = type, parameter = parameter)
if (it.is.factor) {
   value1stlevel <- fittted.star[1] 
   fittted.orig  <- fittted.star-value1stlevel
  } else 
{
aver.fittted.star <- switch(scale.from, 
                           "mean" = mean(fittted.star),
                         "median" = median(fittted.star),
                           "none" = 0 )
    fittted.orig <- fittted.star - aver.fittted.star
 #        theFun <- splinefun(xvar, fittted.orig)
} 
        name.obj <-  if (is.null(name.obj))  deparse(substitute(obj)) else name.obj
        txt.title <- if (missing(title))  
                  paste("Partial effect of",term, "for", parameter, "for model", name.obj)
                  else title
    yaxislabel <- if (type=="response") paste0("PE_param(", term, ")")
                  else                  paste0("PE_eta](", term, ")")
            da <- data.frame(y=fittted.orig,x=xvar )
        y_name <- paste(obj$call$formula[[2]])
if (it.is.factor)
    {
      pp <-  ggplot2::ggplot(data=da, ggplot2::aes(x, y))+
        ggplot2::geom_point(color=col, size=factor.size, shape="-")+
        ggplot2::ylab(yaxislabel)+ 
        ggplot2::xlab(term)+ 
        ggplot2::ggtitle(txt.title)
    if ( !is.null(ylim))
      {
        pp <- pp + ggplot2::ylim(ylim)
      }  
    if (data.plot)
     {
       if (parameter!="mu")  stop("data.plot=TRUE can be used only with parameter=\"mu\"") 
       if (type=="link") stop("it is not a good idea to plot the data with type=\"eta\"") 
       pp <- pp +
         ggplot2::geom_jitter(data = DaTa, 
                  ggplot2::aes(DaTa[,term], y=DaTa[,y_name]-value1stlevel),
                     size = data.size, alpha = data.alpha, colour = data.col)
     }
    } else 
    {
      pp <- ggplot2::ggplot(data=da) +
        ggplot2::geom_line( ggplot2::aes(x=x, y=y), color=col, size=size) +
        ggplot2::ylab(yaxislabel)+ 
        ggplot2::xlab(term)+ 
        ggplot2::ggtitle(txt.title)
      if (data.plot)
      {
        if (parameter!="mu")  stop("data.plot=TRUE can be used only with parameter=\"mu\"") 
        if (type=="link") stop("it is not a good idea to plot the data with type=\"eta\"") 
       pp <- pp +  ggplot2::geom_point(data=DaTa,aes(y =DaTa[,y_name]- aver.fittted.star,  x = DaTa[,term]),
                   size = data.size, alpha = data.alpha, colour = data.col)#
      }
      if ( rug.plot)
      {
       pp <- pp +
         ggplot2::geom_rug(data=DaTa, ggplot2::aes(x=DaTa[,pos]), col=rug.col, size=rug.size)
      }
      if ( !is.null(ylim))
      {
        pp <- pp + ggplot2::ylim(ylim)
      }  
    }          
    return(pp)
}
################################################################################
################################################################################
################################################################################
################################################################################
pe_2_param <- function(obj = NULL, # the gamlss object
                    terms = NULL, # which terms to get the derivative
                     data = NULL, # which data is needed here
                 n.points = 100,  # number of points needed for evaluating the function
                parameter = c("mu", "sigma", "nu", "tau"), # which parameter
                     type = c("parameter", "eta"),
                      how = c("median", "last", "fixed"),
                 scenario = list(), # see below (1)
                      col = "darkblue",
                     size = 1.3,
                data.plot = TRUE,
                 data.col = "lightblue",
                data.size = 0.1,
               data.alpha = 0.9,
                     bins = 30, # for contour plot
                   filled = FALSE, #for contour plot
                 name.obj = NULL,
                   title) # whether to plot
{
#  scenario:a named list of the values to use for the other predictor terms. 
#  Variables omitted from this list will have values set to the median 
#  for continuous variables and the most commonly occuring level for factors 
#  or the last observation
if (is.null(obj)||!class(obj)[1]=="gamlss") stop("Supply a standard GAMLSS model in obj")
if (is.null(terms))  stop("The model terms are not set")
              x <- y <- NULL
            how <- match.arg(how)
           type <- match.arg(type)
           type <- if (type=="parameter") "response" else "link"
      parameter <- match.arg(parameter)
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
if (lpos<=1) stop("supply 2 terms")
if (lpos>3) stop("only up to two terms are allowed")   
    WhichFactor <- sapply(DaTa[,pos], is.factor)
if (any(WhichFactor)) # if any is factor 
  {
         pf <- which(WhichFactor)
         pv <- which(!WhichFactor)
if (length(pf)==2)
     {
       fac1 <- levels(DaTa[,pos[1]])
#n.points.f1 <- nlevels(DaTa[,pos[1]])
       fac2 <- levels(DaTa[,pos[2]])
#n.points.f2 <- nlevels(DaTa[,pos[2]])
         d1 <- expand.grid(f1 = fac1, f2 = fac2)
  names(d1) <- terms
        mat <- matrix(0, nrow = dim(DaTa)[1]+dim(d1)[1],ncol =dim(DaTa)[2]) 
       case <- 3 # both factors
     } else
     {
if (pv==2)   stop("the factor should be set second in order i.e c(\"var\",\"fac\")")         
        fac <- levels(DaTa[,pos[pf]])
# n.points.f <- nlevels(DaTa[,pos[pf]])
        var <- seq(min(DaTa[,pos[pv]]), max(DaTa[,pos[pv]]), length.out=n.points)
         d1 <- expand.grid(x = var, f = fac)
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
  ## creating new data         
for (i in 1:dim(dat.temp)[2])
  {
    if(pos[1]==i)                      # if the variable of interest
    {                               # new data for x is
   dat.temp[,i] <- c(DaTa[,i],d1[,1]) # min(x) to max(x)
    } else
      if(pos[2]==i)                      # if the variable of interest
      {                               # new data for x is
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
## predict     
fittted.orig <- predict(obj, newdata=tail(dat.temp, dim(d1)[1]), type = type, parameter = parameter)
   name.obj  <-  if (is.null(name.obj))  deparse(substitute(obj)) else name.obj
   txt.title <- if (missing(title))  
      paste("Partial effect of", terms[1], "and", terms[2], "for", parameter, "for model", name.obj)
             else title
         da <- data.frame(z=fittted.orig, d1)
     y_name <- paste(obj$call$formula[[2]])
if (case==1)
    {
      pp  <-  ggplot2::ggplot(da, ggplot2::aes_string(terms[1], terms[2]))
if (data.plot)  
  pp <- pp + ggplot2::geom_point(data=DaTa, 
             ggplot2::aes(x=DaTa[,terms[[1]]], y=DaTa[,terms[[2]]]), 
                                size=data.size, alpha=data.alpha, colour=data.col)
      pp <-  if (filled)  pp + ggplot2::geom_contour_filled(
        ggplot2::aes(z = da[,1]), bins=bins/3 )
              else        pp + ggplot2::geom_contour(
                ggplot2::aes(z = da[,1]), bins=bins, size=size,  colour=col)
      pp 
      pp <- pp + ggplot2::ggtitle(txt.title)
    } 
  if (case==2)
    {
     pp <- ggplot2::ggplot(da, ggplot2::aes_string(x=terms[pv], y=da[,1], 
                                                   color=terms[pf]))+
       ggplot2::geom_line(size=size)+
       ggplot2::ggtitle(txt.title)
   if (data.plot)
    {
     if (type=="link") warning("it is not a good idea to plot the data with type=\"eta\"") 
     pp <- pp +   ggplot2::geom_point(data = DaTa,
                  ggplot2::aes(x=DaTa[,terms[pv]], y=DaTa[,y_name]), 
                             size = data.size, alpha=data.alpha, colour=data.col)  
    }
       
    }  
  if (case==3)
  {
    pp <- ggplot2::ggplot(data=da, 
          ggplot2::aes_string(x=terms[1], y=da[,1], group=terms[2], color=terms[2]))+
      ggplot2::geom_line()+
      ggplot2::geom_point(size=size+2)
    if (data.plot)  
      {
      if (type=="link") warning("it is not a good idea to plot the data with type=\"eta\"") 
     pp <-  pp +ggplot2:: geom_jitter(data = DaTa, 
                ggplot2::aes(x=DaTa[,terms[[1]]], y=DaTa[,y_name]),
                    size=data.size, alpha=data.alpha, colour=data.col)
      }
    pp <- pp +  ggplot2::ggtitle(txt.title)
    
  }
      return(pp)
}
################################################################################
################################################################################
################################################################################
################################################################################
#require(grid)
pe_param_grid <- function(model, terms, maxcol=2, maxrow=3, ylim=NULL, ...)
{  
################################################################################ 
define_region <- function(row, col){
viewport(layout.pos.row=row, layout.pos.col=col) }
################################################################################  
# function starts
  lterms <- length(terms)
if (lterms  >   maxcol*maxrow) stop("increase the maxcol or maxrow")   
#if (lterms  <= 1) stop("only one term use pe_param()")
     norow <- ceiling(lterms/maxcol)
     nocol <- if (norow == 1)  lterms  else  maxcol    
        IJ <- expand.grid(j=1:nocol, i=1:norow)
grid.newpage()
  pushViewport(viewport(layout=grid.layout(nrow=norow,ncol=nocol)))   
  GG <- list()
  # for (i in xs ) gg[[i]] <-pe_pdf(linear_3, term=i, title=i)
for (p  in 1:lterms) 
  {
  title.term <- if (length(terms[[p]])==1) terms[[p]]
                else paste(terms[[p]], collapse=":")  
    GG[[title.term]] <- pe_param(model, term=terms[[p]], 
                                    title= title.term,ylim=ylim, ...)
    print(GG[[title.term]], vp=define_region(IJ$i[p], IJ$j[p]))
  }
}
################################################################################
################################################################################
################################################################################
################################################################################
Formulae2DF <- function(formula = list(), data=NULL, weights=NULL, subset=NULL, 
                        na.action, print = TRUE  )
{
  if (is(formula,"list"))
  {
    lenList <- length(formula)
    if (lenList==0) stop("no formula detected")
    if (lenList==1) 
    {
      ff <- deparse(formula[[1]])
    } else
    {
      # the first formula  
      form <- formula(formula[[1]])
      # create y~x+   
      f1 <- paste(paste(form[[2]],form[[1]]), deparse(form[[3]]), "+")
      # now add the of the formulae    
      for (i in 2:lenList)
      {
        ff <- if (i==lenList) paste(f1, deparse(formula[[i]][[2]]))
        else paste(f1,            deparse(formula[[i]][[2]]),"+")
      } 
    }
  } else if (is(formula,"formula")) {ff  <- deparse(substitute(formula))}
  else stop("The formula argument should be a formula or a list") 
  if (!is.null(weights)) 
  {
    # formula(paste(ff[[3]], collapse = " "))
    ff <- paste(ff, deparse(substitute(weights)), sep="+")
    # ff[[3]] <- paste(ff[[3]],deparse(substitute(weights)), sep="+")
  }
  environment(ff) <- globalenv()    # do I need this
  all.vars <- get_all_vars(ff, data=data)
  mm <- match( names(all.vars),names(data),0)
  oo <- which(mm==0)
  all.vars <- all.vars[,-oo]
  if (!is.null(data)&&!inherits(data,"data.frame")) warning("data is not a data frame class attributes will be lost")
  M <- dim(all.vars)[1]
  ## subsetting             
  if (!is.null(subset)) {
    r <- if (!is.null(data))  eval(substitute(subset), data,  parent.frame())
    else eval(substitute(subset),  parent.frame())
    if (!is.logical(r)) stop("'subset' must be logical")
    all.vars <- all.vars[r,]
    M <- dim(all.vars)[1]
    if (print) cat( M, "observations left after subsetting \n" )           
  }
  # it need a futher warning here      N <- dim(all.vars)[1]  
  # na.omit   
  all.vars <- na.omit(all.vars)     # clear NA's
  N <- dim(all.vars)[1]     
  if (print) {if (M-N > 0) cat(M-N, "rows with NAs are deleted", "\n" )}
  if (print) cat( N, "observations with", dim(all.vars)[2], "variables \n")    
  attr(all.vars, "formula") <- ff
  all.vars
}
################################################################################
################################################################################
################################################################################
################################################################################
Formulae2one <- function(formula, sigma=~1, nu=~1, tau=~1, data )
{
  form <- formula(formula)
  nform <- paste(paste(form[[2]],form[[1]]), deparse(form[[3]]), "+",  
                 deparse(sigma[[2]]),"+",
                 deparse(nu[[2]]),"+",
                 deparse(tau[[2]]))[1]
  ff<- formula(paste(nform, collapse = " "))
  environment(ff) <- globalenv()
  ff
}
################################################################################
################################################################################
################################################################################
################################################################################
