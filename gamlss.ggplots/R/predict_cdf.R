################################################################################
################################################################################
################################################################################
################################################################################
#   fitted_cdf()  
#   Mikis Stasinopoulos Bob Rigby Fernanda de Bastiani
#   23 February, 2021 
#   TO DO : I have not checked binomial
#           what about legend? 
################################################################################
################################################################################
################################################################################
################################################################################
predict_cdf <- function (model,
                      newdata,
                        title,
                    from = 0,
                      to = 10,
               no.points = 201, 
                   alpha = 0.4,
               size.line = 1.2,
                col.fill = hcl.colors(lobs, palette="viridis"),# see hcl.pals()"Set 2"
            size.seqment = 1.5, # for discrete dist
              plot.point = TRUE,#    ''
              size.point = 1,   #    ''
               plot.line = TRUE,#    ''
          size.line.disc = 0.2, #  ''
              lower.tail = TRUE, 
                           ...)
{
gamlss.bi.list <- .binom  
if (!is.gamlss(model)) stop("the model should be an gamlss object")  
     family <-  if(is.null(model$call$family)) as.gamlss.family(NO) 
                 else as.gamlss.family(model$call$family)
      fname <- model$family[1]  
       type <- family$type
      nopar <- family$nopar
       pfun <- paste("p",fname,sep="")
        cdf <- eval(parse(text=pfun))
  par.names <- names(family$parameters)
  txt.title <- if (missing(title))  paste("Predicted cdf's from model",deparse(substitute(model)))
  else title  
  if (missing(newdata)) stop("the argument newdata is required")
## the number of plots  
## whether binomial type
################################################################################
################################################################################        
if (fname%in%gamlss.bi.list)  
{
         MM <- predictAll(model, newdata = newdata, output = "matrix")
         bd <- MM[,"bd"]
       lobs <- dim(newdata)[1] 
  lastcolMM <- dim(MM)[2]
         to <- max(bd)
     y.var  <-  cdfArr <- da <- list()  
     y.var..i.. <- cdfArr..i.. <- NULL 
for (i in 1:lobs)
{
    y.var[[i]] <- 0:MM[i,"bd"]
switch(nopar,
            {
    cdfArr[[i]] <- cdf(y.var[[i]],  mu=MM[i,"mu"],  bd=MM[i,"bd"], lower.tail = lower.tail)
            },  
            {
    cdfArr[[i]] <- cdf(y.var[[i]], mu=MM[i,"mu"],  sigma=MM[i,"sigma"], bd=MM[i,"bd"], lower.tail = lower.tail)
            },
            {
    cdfArr[[i]] <- cdf(y.var[[i]],  mu=MM[i,"mu"], sigma=MM[i,"sigma"], nu=MM[i,"nu"], bd=MM[i,"bd"], lower.tail = lower.tail)
            },
            {
    cdfArr[[i]] <- cdf(y.var[[i]],  mu=MM[j,"mu"], sigma=MM[i,"sigma"], nu=MM[i,"nu"], tau=MM[i,"tau"],  bd=MM[i,"bd"], lower.tail = lower.tail)  
            })  
    da[[i]] <- data.frame(y.var[[i]],  cdfArr[[i]])
}
     da0 <- data.frame(y.var=from:to)
     p11 <- ggplot2::ggplot(data=da0) 
if (lobs==1) 
    {
         p11 <- p11 +  #geom_hline( aes(yintercept = 0)) +
           ggplot2::geom_segment(data=da[[1]], ggplot2::aes(x=y.var..i.., y=cdfArr..i.., 
                                      xend = y.var..i.., yend = 0), 
                        color=col.fill[1],  size=size.seqment)
    }
      else   
    {  
for (i in 1:lobs)
         {
           p11 <- p11 + # geom_hline( aes(yintercept = 0)) +
             ggplot2::geom_segment(data=da[[i]], 
              mapping =  aes(x=y.var..i.., y=cdfArr..i.., 
              xend = y.var..i.., yend = 0), 
              color=col.fill[i], alpha=alpha, size=size.seqment)
  if (plot.point) p11 <- p11 + ggplot2::geom_point(data=da[[i]],
            ggplot2::aes(x = y.var..i.., y = cdfArr..i..), size = size.point, 
              color = col.fill[i])
  if (plot.line)  p11 <- p11 + ggplot2::geom_line(data=da[[i]],
                    ggplot2::aes(x = y.var..i.., y=cdfArr..i..),  
                          size = size.line.disc, color = col.fill[i])
         } 
    }
     p11 = p11 + ggplot2::labs(x = "y", y =  paste0(fname,"(y)"))+
       ggplot2::xlim(from,to)+
       ggplot2::ggtitle( txt.title)     
return(p11)     
} # end binomial
################################################################################
################################################################################      
        MM <- predictAll(model, newdata=newdata, output="matrix")
      lobs <- dim(newdata)[1]
  if (lobs==1) MM <- matrix(MM, nrow=1, ncol=nopar+1 ) 
# everything else not binomial type
##    whether discrete distribution or not
      y.var <- if(type=="Discrete")  seq(from, to, by=1)
                else seq(from, to, length=no.points)
  #if(any(fname%in%.gamlss.bi.list)) bd <- to   
#if (lobs==1) MM <- matrix(MM, nrow=1, ncol=nopar+1 )  
# the matrix to hold the results
     cdfArr <- matrix(0, nrow=length(y.var), ncol=lobs)
# loop over observations
for (j in 1:lobs)
      {
  switch(nopar,
         {
           cdfArr[,j] <- cdf(y.var,  mu=MM[j,"mu"], lower.tail = lower.tail)
         },  
         {
           cdfArr[,j] <- cdf(y.var, mu=MM[j,"mu"],  sigma=MM[j,"sigma"], lower.tail = lower.tail)
         },
         {
           cdfArr[,j] <- cdf(y.var,  mu=MM[j,"mu"], sigma=MM[j,"sigma"], nu=MM[j,"nu"], lower.tail = lower.tail)
         },
         {
           cdfArr[,j] <- cdf(y.var,  mu=MM[j,"mu"], sigma=MM[j,"sigma"], nu=MM[j,"nu"], tau=MM[j,"tau"], lower.tail = lower.tail) 
         })  
}  # end of look over observations
################################################################################ 
     da <- data.frame(y.var,  cdfArr)
    p11 <- ggplot(data=da) 
if (type=="Discrete")
{
  if (lobs==1) 
  {
    p11 <- p11 +  #geom_hline( aes(yintercept = 0)) +
      ggplot2::geom_step(direction = "hv", 
            ggplot2::aes_string(x="y.var", y=cdfArr),  
                size= size.line.disc, color=col.fill[1])
  }
  else   
  {  
    for (i in 1:lobs)
    {
      p11 <- p11 + # geom_hline( aes(yintercept = 0)) +
        ggplot2::geom_step(direction = "hv", 
        ggplot2::aes_string(x = "y.var", y = paste0("X",i)),  
                  size = size.line, color = col.fill[i])
      #if (plot.point) p11 <- p11+geom_point( aes_string(x="y.var", y=paste0("X",i)),  
      #                                       size= size.point, color=col.fill[i])
      #if (plot.line)  p11 <- p11 + geom_line( aes_string(x="y.var", y=paste0("X",i)),  
                                       #       size= size.line.disc, color=col.fill[i])
    } 
  }
} else # continuous 
{# one plot 
  if (lobs==1) p11 = p11 + ggplot2::geom_line(color=col.fill[1], alpha=alpha, 
                          size=size.line, aes(x=y.var, y=cdfArr))
  else
  {# more than one plot
    for (i in 1:lobs)
    {
  p11 <-p11 + ggplot2::geom_line(color=col.fill[i], alpha=alpha, 
              size=size.line,  aes_string(x="y.var", y=paste0("X",i)))
    } 
  }
}  
  p11 = p11 + ggplot2::labs(x = "y", y =  paste0(fname,"(y)"))+
    ggplot2::xlim(from,to)+
    ggplot2::ggtitle( txt.title)
p11
}
################################################################################
################################################################################
################################################################################
################################################################################


