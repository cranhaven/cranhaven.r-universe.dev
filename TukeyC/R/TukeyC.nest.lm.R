TukeyC.nest.lm <- function(x,
                           which, 
                           fl1, 
                           fl2,
                           MSE,
                           dfr,
                           sig.level,
                           round,
                           adjusted.pvalue, ...)
{
  my <- as.character(formula(x)[[2]])
  m1 <- gsub('\\:','\\+', which)
  m2 <- unlist(strsplit(which,
                        '[[:punct:]]'))  

  forminter <- as.formula(paste(my, '~', m1))
  
  aux_r <- aggregate(forminter, 
                     data = x$model,
                     function(x) r = length(x))
  reps <- aux_r[[my]]

  aux_mt <- suppressWarnings(doBy::LSmeans(x,
                                           effect = m2))

  aux_mt1 <- aux_mt$coef[,1]

  aux_mt2 <- data.frame(aux_r[1:length(names(aux_r))-1],
                        means = aux_mt1,
                        reps = reps)

  aux_mt3 <- aux_mt2[order(aux_mt2[['means']],
                           decreasing = TRUE),]

  nf1 <- unlist(strsplit(which,
                         split = ':'))[1] # nome do primeiro fator do which

  nf2 <- unlist(strsplit(which,
                         split = ':'))[2] # nome do segundo fator do which

  nf3 <- unlist(strsplit(which,
                         split = ':'))[3] # nome do terceiro fator do which

  if(is.null(fl2)){
    # Interesse apenas na interacao dupla

    f1 <- levels(x$model[,nf2]) # correspondem aos fatores que se quer comparar!

    f2 <- levels(x$model[,nf1])[fl1] # corresponde ao fator onde se esta fazendo o desdobramento!

    mt <- subset(aux_mt3, 
                 eval(parse(text = nf1)) == f2) # pegando as medias de interesse

    row.names(mt) <- paste(mt[,1],
                           mt[,2],
                           sep='/')  
  } # Interesse na interacao tripla 
  else {

    f1 <- levels(x$model[,nf3])

    f2 <- levels(x$model[,nf2])[fl2] 

    f3 <- levels(x$model[,nf1])[fl1]

    mt <- subset(aux_mt3, 
                 eval(parse(text = nf1)) == f3 & eval(parse(text=nf2)) == f2) # pegando as medias de interesse

    row.names(mt) <- paste(mt[,1],
                           mt[,2],
                           mt[,3],
                           sep='/')   

  } 

  out <- make.TukeyC.test(obj             = mt,
                          MSE             = MSE,
                          sig.level       = sig.level,
                          dfr             = dfr,
                          round           = round,
                          adjusted.pvalue = adjusted.pvalue)  

  m.inf <- m.infos.nest.lm(x         = x,
                           my        = my,
                           forminter = forminter,
                           which     = which,
                           fl1       = fl1,
                           fl2       = fl2,
                           sig.level = sig.level,
                           aux_mt    = aux_mt)

  res <- list(out  = out,
              info = m.inf)

}    
