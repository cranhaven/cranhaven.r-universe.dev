################################################################################
################################################################################
################################################################################
################################################################################
#   fitted_cdf()  
#   Mikis Stasinopoulos Bob Rigby Fernanda de Bastiani
#   February , 2021 
#   NOTE that for binonial type of data `to' plays the role of 
#   binomial denominator so only one binomial denominator is allowed
#   here
################################################################################
################################################################################
################################################################################
################################################################################
family_cdf<- function (family = NO(),
                           mu = NULL,
                        sigma = NULL,
                           nu = NULL,
                          tau = NULL,
                          title, 
                         from = 0,
                           to = 10,
                    no.points = 201, 
                        alpha = 0.4,
                    size.line = 1.2,
                     col.fill = hcl.colors(lobs, palette="viridis"), # see hcl.pals()
                 size.seqment = 1.5, # for discrete dist
                   plot.point = TRUE,#    ''
                   size.point = 1,   #    ''
                    plot.line = TRUE,#    ''
              #size.line.disc  = 0.2, #    ''
                   lower.tail = TRUE,
                            ...)
{
################################################################
################################################################ 
gamlss.bi.list <- .binom
        fname <- if (is.name(family)) as.character(family)
                 else if (is.character(family)) family
                 else if (is.call(family)) as.character(family[[1]])
                 else if (is.function(family)) deparse(substitute(family))
                 else if (is(family, "gamlss.family"))  family$family[1]
                 else stop("the family must be a character or a gamlss.family name")
        fam1 <- eval(parse(text=fname))   # the family to output
         fam <- as.gamlss.family(family)  # this is created so I can get things
      dorfun <- paste("p",fname,sep="")   # say dNO
       nopar <- fam$nopar                 # or fam1$nopar
        type <- fam$type
   par.names <- names(fam$parameters)
#################################################################
   txt.title <- if (missing(title))  
     { if (lower.tail) paste(fname,"cdf") else paste(fname,"survival function") }
     else title   
   if (fname%in%gamlss.bi.list) bd = to
##    whether discrete distribution or not
      y.var <- if(type=="Discrete")  seq(from, to, by=1)
              else seq(from, to, length=no.points)
## whether binomial type
  if(any(fname%in%.gamlss.bi.list)) bd <- to   
## the number of plots  
      lobs <- max(c(length(mu),length(sigma),length(nu),length(tau)))
#################################################################     
# get the parameters
if ("mu"%in%par.names)
   { if (is.null(mu)) stop("At least one value of mu has to be set")
      mu.var <- rep(mu, length = lobs) 
      if (!fam$mu.valid(mu.var))  stop( "`mu' parameter out of range")
   }
if ("sigma"%in%par.names)
   { if (is.null(sigma)) stop("At least one value of sigma has to be set") 
      sigma.var <- rep(sigma, length = lobs)
      if (!fam$sigma.valid(sigma.var))  stop( "`sigma' parameter out of range")
    }
if ("nu"%in%par.names)
   { 
      if (is.null(nu)) stop("At least one value of nu has to be set")
      nu.var <- rep(nu, length = lobs)
      if (!fam$nu.valid(nu.var))  stop( "`nu' parameter out of range")
   }
if ("tau"%in%par.names)
   { if (is.null(tau)) stop("At least one value of tau has to be set") 
      tau.var <- rep(tau, length = lobs)
      if (!fam$tau.valid(tau.var))  stop( "`tau' parameter out of range")
   }
if (!fam$y.valid(y.var))  stop( "response variable out of range")
     cdfArr <- matrix(0, nrow=length(y.var), ncol=lobs)
     
# loop over         
for (j in 1:lobs)
  {
if (fname%in%gamlss.bi.list)
{
  cdf11 <-  switch(nopar,
    paste0("p",fname,"(y.var,mu.var[j], bd=bd, lower.tail=",lower.tail,")"),
    paste0("p",fname,"(y.var,mu.var[j],sigma.var[j], bd=bd, lower.tail=",lower.tail,")"),
    paste0("p",fname,"(y.var,mu.var[j],sigma.var[j], nu.var[j], bd=bd, lower.tail=",lower.tail,")"),
    paste0("p",fname,"(y.var,mu.var[j],sigma.var[j], nu.var[j], tau.var[j],  bd=bd, lower.tail=",lower.tail,")")) 
} else
{  
  cdf11 <-  switch (nopar,
    paste0("p",fname,"(y.var,mu.var[j], lower.tail=",lower.tail,")"),
    paste0("p",fname,"(y.var,mu.var[j],sigma.var[j], lower.tail=",lower.tail,")"),
    paste0("p",fname,"(y.var,mu.var[j],sigma.var[j], nu.var[j], lower.tail=",lower.tail,")"),
    paste0("p",fname,"(y.var,mu.var[j],sigma.var[j], nu.var[j], tau.var[j], lower.tail=",lower.tail,")")
      )
}
            fy11 <- eval(parse(text=cdf11))
      cdfArr[,j] <- fy11
      if (!is.null(fam$parameters$mu)){ 
        m.title <- bquote(paste(.(fname),"(",paste(mu," = ",.(2),")")))} 
      if (!is.null(fam$parameters$sigma)) {
        m.title <-  bquote(paste(.(fname),"(",paste(mu," = ",  .(mu.var[j]),  
                                                    sep=","), 
                                 paste(sigma," = ",.(sigma.var[j])         ),")"))}
      if (!is.null(fam$parameters$nu)) {
        m.title <-  bquote(paste(.(fname),"(",paste(mu," = ", .(mu.var[j]),  
                                                    sep=","), 
                                 paste(sigma," = ",.(sigma.var[j]), sep=","),
                                 paste(   nu," = ",.(   nu.var[j]))        ,")"))}
      if (!is.null(fam$parameters$tau)){
        m.title <-  bquote(paste(.(fname),"(",paste(mu," = ", .(mu.var[j]),  
                                                    sep=","), 
                                 paste(sigma," = ",.(sigma.var[j]), sep=","),
                                 paste(   nu," = ",.(   nu.var[j]), sep=","),
                                 paste(  tau," = ",.(  tau.var[j]))       ,')'))}
}# end loop
y.title <- if(type=="Discrete")  "P(Y=y)" else  "f(y)"
############################################################
da <- data.frame(y.var,  cdfArr)
    p11 <- ggplot2::ggplot(data=da) 
if (type=="Discrete")
{
  if (lobs==1) 
  {
    
    p11 <- p11 +  #geom_hline( aes(yintercept = 0)) +
      ggplot2::geom_step(direction = "hv", 
              ggplot2::aes(x = .data[[y.var]], y = .data[[cdfArr]]),  
                size = size.point, color = col.fill[1])
      #geom_segment(mapping = aes(x=y.var, y=cdfArr, xend = y.var, yend = 0), 
       #            color=col.fill[1],  size=size.seqment)
  }
  else
  {
    for (i in 1:lobs)
    {
     # p11 <- p11 + # geom_hline( aes(yintercept = 0)) +
        p11 <- p11 + ggplot2::geom_step(direction = "hv", 
                               aes(x = .data[["y.var"]], y = .data[[paste0("X",i)]]),  
                               size= size.point, color=col.fill[i])
          #    geom_segment(mapping =  aes_string(x="y.var", y=paste0("X",i),
           #          xend = "y.var", yend = 0), 
            #         color=col.fill[i], alpha=alpha, size=size.seqment)
     # if (plot.point) p11 <- p11+geom_point( aes_string(x="y.var", y=paste0("X",i)),  
      #                  size= size.point, color=col.fill[i])
      #if (plot.line)  p11 <- p11 + geom_line( aes_string(x="y.var", y=paste0("X",i)),  
      #                  size= size.line.disc , color=col.fill[i])
    } 
  }
} else
{
  if (lobs==1) p11 = p11 + ggplot2::geom_line(color=col.fill[1], alpha=alpha, 
                                              size=size.line, aes(x=y.var, y=cdfArr))
  else
  {# more than one
    for (i in 1:lobs)
    {
      p11 <-p11 +  ggplot2::geom_line(color=col.fill[i], alpha=alpha, size=size.line, 
                            ggplot2::aes(x=.data[["y.var"]], y= .data[[paste0("X",i)]]))
    } 
  }
}  
  p11 = p11 +  ggplot2::labs(x = "y", y =  y.title)+
               ggplot2::xlim(from,to)+
               ggplot2::ggtitle(txt.title)
p11
}
################################################################################
################################################################################
################################################################################
################################################################################