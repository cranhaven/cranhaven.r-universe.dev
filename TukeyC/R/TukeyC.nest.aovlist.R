##
## S3 method to 'aovlist' object
##
TukeyC.nest.aovlist <- function(x,
                                which,
                                fl1,
                                fl2,
                                MSE,
                                dfr, 
                                sig.level,
                                round,
                                adjusted.pvalue,...)
{

  my <- as.character(attr(x,'terms')[[2]])
  m1 <- gsub('\\:',
             '\\+', 
             which)
 
  forminter <- as.formula(paste(my, '~', m1))

  dat <- model.frame(x)

  aux_mt1 <- aggregate(forminter, 
                       data = dat,
                       function(x) c(means = mean(x),
                                     r = length(x)))

  aux_mt2 <- aux_mt1[order(aux_mt1[[my]][,1], 
                           decreasing = TRUE),]

  aux_mt3 <- data.frame(aux_mt2[1:length(names(aux_mt2))-1],
                        means = aux_mt2[[my]][,1],
                        reps = aux_mt2[[my]][,2])

  nf1 <- unlist(strsplit(which,
                         split = ':'))[1] # nome do primeiro fator do which

  nf2 <- unlist(strsplit(which,
                         split = ':'))[2] # nome do segundo fator do which

  nf3 <- unlist(strsplit(which,
                         split = ':'))[3] # nome do terceiro fator do which

  if(is.null(fl2)){
    # Interesse apenas na interacao dupla
    f1 <- levels(model.frame(x)[,nf2]) # correspondem aos fatores que se quer comparar!


    f2 <- levels(model.frame(x)[,nf1])[fl1] # corresponde ao fator onde se estao fazendo o desdobramento!

    mt <- subset(aux_mt3, 
                 eval(parse(text = nf1)) == f2) # pegando as medias de interesse

    row.names(mt) <- paste(mt[,1],
                           mt[,2],
                           sep='/')  
  } # Interesse na interacao tripla 
  else {

    f1 <- levels(model.frame(x)[,nf3])

    f2 <- levels(model.frame(x)[,nf2])[fl2] 

    f3 <- levels(model.frame(x)[,nf1])[fl1]

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

  m.inf <- m.infos.nest.aovlist(x         = x,
                                my        = my,
                                forminter = forminter,
                                which     = which,
                                fl1       = fl1,
                                fl2       = fl2,
                                sig.level = sig.level,
                                aux_mt    = aux_mt1,
                                MSE       = MSE)

  res <- list(out  = out,
              info = m.inf)

}
