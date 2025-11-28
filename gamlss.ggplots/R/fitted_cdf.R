################################################################
################################################################
################################################################
################################################################
#   fitted_cdf()  
#   Mikis Stasinopoulos Bob Rigby Fernanda de Bastiani
#   23 February, 2021 
#   TO DO : I have not checked binomial
#           what about legend? 
################################################################
################################################################
fitted_cdf <- function (model,
                          obs,
                        title,
                    from = 0,
                      to = 10,
               no.points = 201, 
                   alpha = 1,
               size.line = 1.2,
                col.fill = hcl.colors(lobs, palette = "viridis"),# see hcl.pals()"Set 2"
            size.seqment = 1.5, # for discrete dist
             # plot.point = TRUE,#    ''
              size.point = 1,   #    for discrete
               plot.line = TRUE,#    for binomial
          size.line.disc = 0.2, #    for binomial
              lower.tail = TRUE, # FALSE for Survival function
                           ...)
{
gamlss.bi.list <- .binom  
if (!is.gamlss(model)) stop("the model should be an gamlss object")  
     family <-  if(is.null(model$call$family)) as.gamlss.family(NO) 
                 else as.gamlss.family(model$call$family)
      fname <- model$family[1]  
       type <- family$type
      nopar <- family$nopar
       pfun <- paste("p",fname,sep = "")
        cdf <- eval(parse(text = pfun))
  par.names <- names(family$parameters)
  txt.title <- if (missing(title))  paste("Fitted cdf's from model",deparse(substitute(model)))
  else title  
        MM <- predictAll(model, output="matrix")[obs,]
      lobs <- length(obs)
if (lobs==1) MM <- matrix(MM, nrow=1 , dimnames = list("1", names(MM))) 
## the number of plots  
## whether binomial type
######################################################################
######################################################################          
if (fname%in%gamlss.bi.list)  {
         bd <- model$bd[obs]
         MM <- cbind(MM, bd)
  lastcolMM <- dim(MM)[2]
         to <- max(bd)
     y.var  <-  cdfArr <- da <- list()  
     y.var..i.. <- cdfArr..i.. <- NULL 
for (i in 1:lobs)
{
    y.var[[i]] <- 0:MM[i,lastcolMM]

    switch(nopar,
           {
             cdfArr[[i]] <- cdf(y.var[[i]],  mu=MM[i,"mu"],  bd=MM[i,"bd"])
           },  
           {
             cdfArr[[i]] <- cdf(y.var[[i]], mu=MM[i,"mu"],  sigma=MM[i,"sigma"], bd=MM[i,"bd"])
           },
           {
             cdfArr[[i]] <- cdf(y.var[[i]],  mu=MM[i,"mu"], sigma=MM[i,"sigma"], nu=MM[i,"nu"], bd=MM[i,"bd"])
           },
           {
             cdfArr[[i]] <- cdf(y.var[[i]],  mu=MM[j,"mu"], sigma=MM[i,"sigma"], nu=MM[i,"nu"], tau=MM[i,"tau"],  bd=MM[i,"bd"])  
           })  
    da[[i]] <- data.frame(y.var[[i]],  cdfArr[[i]])
}
     da0 <- data.frame(y.var = from:to)
     p11 <- ggplot2::ggplot(data = da0) 
if (lobs == 1) 
    {
         p11 <- p11 +  #geom_hline( aes(yintercept = 0)) +
           ggplot2::geom_segment(data = da[[1]], 
           mapping = ggplot2::aes(x = y.var..i.., y = cdfArr..i.., 
           xend = y.var..i.., yend = 0, lower.tail = lower.tail), 
           color = col.fill[1],  size = size.seqment)
    }
      else   
    {  
for (i in 1:lobs)
         {
           p11 <- p11 + # geom_hline( aes(yintercept = 0)) +
             ggplot2::geom_segment(data=da[[i]],
             mapping =  ggplot2::aes(x=y.var..i.., y=cdfArr..i..,
             xend = y.var..i.., yend = 0),
             color = col.fill[i], alpha = alpha, size = size.seqment)
   if (plot.line)  p11 <- p11 + ggplot2::geom_line(data=da[[i]],
                  ggplot2::aes(x=y.var..i.., y=cdfArr..i..),  
                  size = size.line.disc, color=col.fill[i])
         } 
    }
     p11 = p11 + ggplot2::labs(x = "y", y =  paste0(fname,"(y)")) +
       ggplot2::xlim(from,to) +
       ggplot2::ggtitle( txt.title)     
return(p11)     
} # end binomial
#######################################################################
#######################################################################
# everything else not binomial type
##    whether discrete distribution or not
      y.var <- if(type=="Discrete")  seq(from, to, by=1)
                else seq(from, to, length=no.points)
     cdfArr <- matrix(0, nrow=length(y.var), ncol=lobs)
# loop over observations
for (j in 1:lobs)
      {
  switch(nopar,
         {
           cdfArr[,j] <- cdf(y.var,  mu = MM[j,2], lower.tail = lower.tail)
         },  
         {
           cdfArr[,j] <- cdf(y.var, mu = MM[j,2],  sigma = MM[j,3], 
                             lower.tail = lower.tail)
         },
         {
           cdfArr[,j] <- cdf(y.var,  mu = MM[j,2], sigma = MM[j,3], 
                             nu = MM[j,4], lower.tail = lower.tail)
         },
         {
           cdfArr[,j] <- cdf(y.var,  mu = MM[j,2], sigma = MM[j,3], 
                             nu = MM[j,4], tau = MM[j,5], 
                             lower.tail = lower.tail)  
         })  
}  # end of look over observations
################################################################
        da <- data.frame(y.var,  cdfArr)
       p11 <- ggplot2::ggplot(data=da) 
if (type=="Discrete")
{
  if (lobs==1) 
  {
    p11 <- p11 +  #geom_hline( aes(yintercept = 0)) +
      ggplot2::geom_step(direction = "hv", 
      ggplot2::aes_string(x = "y.var", y = cdfArr),  
      size = size.point, color = col.fill[1])
  }
  else   
  {  
    for (i in 1:lobs)
    {
        p11 <- p11 + ggplot2::geom_step(direction = "hv", 
                     ggplot2::aes_string(x = "y.var", y = paste0("X",i)),  
                    size= size.point, color=col.fill[i])
    } 
  }
} else # continuous 
{# one plot 
  if (lobs==1) p11 = p11 + ggplot2::geom_line(color = col.fill[1], 
                           size = size.line, 
                           alpha = alpha, aes(x = y.var, y = cdfArr))
  else
  {# more than one plot
    for (i in 1:lobs)
    {
  p11 <-  p11 +  ggplot2::geom_line(color = col.fill[i], size = size.line, 
                           alpha = alpha, aes_string(x = "y.var", 
                                                     y = paste0("X",i)))
    } 
  }
}  
  p11 = p11 + ggplot2::labs(x = "y", y =  paste0(fname,"(y)")) +
    ggplot2::xlim(from,to) +
    ggplot2::ggtitle( txt.title)
p11
}
##############################################################################
##############################################################################
fitted_cdf_data <- function(model, obs, from, to, ...)  
{
  fitted_cdf(model, obs=obs, from=from, to=to, ...)+
    ggplot2::geom_vline(xintercept = model$y[obs], colour="gray")
}
#############################################################################
#############################################################################


