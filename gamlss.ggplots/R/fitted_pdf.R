################################################################################
################################################################################
################################################################################
################################################################################
#   fitted_pdf()  
#   Mikis Stasinopoulos Bob Rigby Fernanda de Bastiani
#   28 September, 2022 
#   TO DO : I have not checked binomial
#           what about legend? 
################################################################################
################################################################################
################################################################################
################################################################################
fitted_pdf <- function (model,
                          obs,
                        title,
                    from = 0,
                      to = 10,
               no.points = 201, 
                   alpha = 0.4,
                col.fill = hcl.colors(lobs, palette="viridis"),# see hcl.pals()"Set 2"
            size.seqment = 1.5, # for discrete dist
              plot.point = TRUE,#    ''
              size.point = 1,   #    ''
               plot.line = TRUE,#    ''
               size.line = 0.2, #    ''
                           ...)
{
gamlss.bi.list <- .binom  
if (!is.gamlss(model)) stop("the model should be an gamlss object")  
     family <-  if(is.null(model$call$family)) as.gamlss.family(NO) 
                 else as.gamlss.family(model$call$family)
      fname <- model$family[1]  
       type <- family$type
      nopar <- family$nopar
       dfun <- paste("d",fname,sep="")
      p_d_f <- eval(parse(text=dfun))
  par.names <- names(family$parameters)
  txt.title <- if (missing(title))  paste("Fitted pdf's from model",deparse(substitute(model)))
  else title  
         MM <- predictAll(model, output="matrix")[obs,]
       lobs <- length(obs)
if (lobs==1) MM <- matrix(MM, nrow=1 , dimnames = list("1", names(MM))) 
## the number of plots  
      
## whether binomial type
######################################################################          
if (fname%in%gamlss.bi.list)  {
         bd <- model$bd[obs]
         MM <- cbind(MM, bd)
  lastcolMM <- dim(MM)[2]
         to <- max(bd)
     y.var  <-  pdfArr <- da <- list()  
     y.var..i.. <- pdfArr..i.. <- NULL 
for (i in 1:lobs)
{
    y.var[[i]] <- 0:MM[i,lastcolMM]

switch(nopar,
            {
            pdfArr[[i]] <- p_d_f(y.var[[i]],  mu=MM[i,"mu"],  bd=MM[i,"bd"])
            },  
            {
            pdfArr[[i]] <- p_d_f(y.var[[i]], mu=MM[i,"mu"],  sigma=MM[i,"sigma"], bd=MM[i,"bd"])
            },
            {
            pdfArr[[i]] <- p_d_f(y.var[[i]],  mu=MM[i,"mu"], sigma=MM[i,"sigma"], nu=MM[i,"nu"], bd=MM[i,"bd"])
            },
            {
            pdfArr[[i]] <- p_d_f(y.var[[i]],  mu=MM[j,"mu"], sigma=MM[i,"sigma"], nu=MM[i,"nu"], tau=MM[i,"tau"],  bd=MM[i,"bd"])  
            })  
    da[[i]] <- data.frame(y.var[[i]],  pdfArr[[i]])
}
     da0 <- data.frame(y.var=from:to)
     p11 <- ggplot2::ggplot(data=da0) 
if (lobs==1) 
    {
         p11 <- p11 +  #geom_hline( aes(yintercept = 0)) +
           ggplot2::geom_segment(data=da[[1]], mapping = 
                                   ggplot2::aes(x=y.var..i.., y=pdfArr..i.., 
                                      xend = y.var..i.., yend = 0), 
                        color=col.fill[1],  size=size.seqment)
    }
      else   
    {  
for (i in 1:lobs)
         {
           p11 <- p11 + # geom_hline( aes(yintercept = 0)) +
             ggplot2::geom_segment(data=da[[i]], 
              mapping =  ggplot2::aes(x=y.var..i.., y=pdfArr..i.., 
                                       xend = y.var..i.., yend = 0), 
                          color=col.fill[i], alpha=alpha, size=size.seqment)
  if (plot.point) p11 <- p11+ 
      ggplot2::geom_point(data=da[[i]],
      ggplot2::aes(x=y.var..i.., y=pdfArr..i..), size= size.point, color=col.fill[i])
  if (plot.line)  p11 <- p11 + ggplot2::geom_line(data=da[[i]],
                      ggplot2::aes(x=y.var..i.., y=pdfArr..i..),  
                          size= size.line, color=col.fill[i])
         } 
    }
     p11 = p11 + ggplot2::labs(x = "y", y =  paste0(fname,"(y)"))+
       ggplot2::xlim(from,to)+
       ggplot2::ggtitle( txt.title)     
return(p11)     
} # end binomial
#######################################################################
# evrything else not binomial type
##    whether discrete distribution or not
      y.var <- if(type=="Discrete")  seq(from, to, by=1)
                else seq(from, to, length=no.points)
  #if(any(fname%in%.gamlss.bi.list)) bd <- to   
#if (lobs==1) MM <- matrix(MM, nrow=1, ncol=nopar+1 )  
# the matrix to hold the results
     pdfArr <- matrix(0, nrow=length(y.var), ncol=lobs)
# loop over observations
# 
for (j in 1:lobs)
      {
  switch(nopar,
         {
           pdfArr[,j] <- p_d_f(y.var,  mu=MM[j,"mu"])
         },  
         {
           pdfArr[,j] <- p_d_f(y.var, mu=MM[j,"mu"],  sigma=MM[j,"sigma"])
         },
         {
           pdfArr[,j] <- p_d_f(y.var,  mu=MM[j,"mu"], sigma=MM[j,"sigma"], nu=MM[j,"nu"])
         },
         {
           pdfArr[,j] <- p_d_f(y.var,  mu=MM[j,"mu"], sigma=MM[j,"sigma"], nu=MM[j,"nu"], tau=MM[j,"tau"])  
         })  
}  # end of look over observations
################################################################ 
     da <- data.frame(y.var,  pdfArr)
    p11 <- ggplot(data=da) 
if (type=="Discrete")
{
  if (lobs==1) 
  {
    p11 <- p11 +  #geom_hline( aes(yintercept = 0)) +
      ggplot2::geom_segment(mapping =ggplot2::aes(x=y.var, y=pdfArr, xend = y.var, yend = 0), 
                   color=col.fill[1],  size=size.seqment)
  }
  else   
  {  
    for (i in 1:lobs)
    {
      p11 <- p11 + # geom_hline( aes(yintercept = 0)) +
        ggplot2::geom_segment(mapping =  
        ggplot2::aes_string(x="y.var", y=paste0("X",i),
                                           xend = "y.var", yend = 0), 
                     color=col.fill[i], alpha=alpha, size=size.seqment)
      if (plot.point) p11 <- p11+ggplot2::geom_point( 
        ggplot2::aes_string(x="y.var", y=paste0("X",i)),  
                                             size= size.point, color=col.fill[i])
      if (plot.line)  p11 <- p11 + ggplot2::geom_line( 
        ggplot2::aes_string(x="y.var", y=paste0("X",i)),  
                                              size= size.line, color=col.fill[i])
    } 
  }
} else # continuous 
{# one plot 
  if (lobs==1) p11 = p11 + ggplot2::geom_area(fill=col.fill[1], 
                      alpha=alpha, aes(x=y.var, y=pdfArr))
  else
  {# more than one plot
    for (i in 1:lobs)
    {
  p11 <-p11 + ggplot2::geom_area(fill=col.fill[i], alpha=alpha, 
              ggplot2::aes_string(x="y.var", y=paste0("X",i)))
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
# adding data in 
fitted_pdf_data <- function(model, obs, from, to, ...)  
{
  
  fitted_pdf(model, obs=obs, from=from, to=to, ...)+
    ggplot2::geom_vline(xintercept = model$y[obs], colour="gray")
}
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
