##
## S3 method to 'aovlist' object
##
TukeyC.aovlist <- function(x,
                           which           = NULL,
                           fl1             = NULL, 
                           fl2             = NULL, 
                           error           = NULL,
                           sig.level       = .05,
                           round           = 2,
                           adjusted.pvalue = 'none', ...)
{

  # Interacoes com erro experimental
  if(!is.null(fl1) & is.null(error)){

    pos_error <- length(names(x))
    SSE <- deviance(x[[pos_error]]) # experimental error
    dfr <- df.residual(x[[pos_error]])# experimental error
    MSE  <- SSE/dfr

    cl <- match.call()
    class(x) <- c('nest.aovlist',class(x)) 

    res <- TukeyC(x               = x,
                  which           = which,
                  fl1             = fl1,
                  fl2             = fl2,
                  MSE             = MSE,
                  dfr             = dfr,
                  sig.level       = sig.level,
                  round           = round,
                  adjusted.pvalue = adjusted.pvalue,
                  ...)

    res$call <- cl
    class(res) <- c('TukeyC.aovlist',
                    'TukeyC',
                    'list')
    return(res)                           

  }

  # Interacoes com outros erros
  if(!is.null(fl1) & !is.null(error)){ 

    many_errors <- unlist(strsplit(error,
                                   '\\/'))

    n_errors <- length(many_errors)

    if(n_errors > 1){# combinacao de erros!!!

      aux_SSE <- NULL
      aux_dfr <- NULL

      for(i in 1:n_errors){

        aux_SSE[i] <- deviance(x[[many_errors[i]]])
        aux_dfr[i] <- df.residual(x[[many_errors[i]]]) 
      }

      aux_MSE <- aux_SSE/aux_dfr

      factors <- unlist(strsplit(which,
                                 '[[:punct:]]'))

      aux_levels <- attr(x,'xlevel')

      aux_levels1 <- lapply(aux_levels,
                            length)

      levelss <- unlist(aux_levels1[factors])

      if(length(aux_MSE) == 2){

        cp <- c(levelss[1]-1,
                1) 

        MSE <- (cp%*%aux_MSE)/levelss[1]

        numer <- (cp%*%aux_MSE)^2
        denom <- (cp[1]*aux_MSE[1])^2/aux_dfr[1] + aux_MSE[2]^2/aux_dfr[2]
        dfr <- numer/denom 

      } else {

        cp <- c(levelss[2]*(levelss[1]-1),
                levelss[2]-1,
                1) 

        MSE <- (cp%*%aux_MSE)/prod(levelss[1:2])

        numer <- (cp%*%aux_MSE)^2
        denom <- (cp[1]*aux_MSE[1])^2/aux_dfr[1] + (cp[2]*aux_MSE[2])^2/aux_dfr[2] + aux_MSE[3]^2/aux_dfr[3]
        dfr <- numer/denom       

      } 
    }else{# nao ha combinacao de erros!!!   

      SSE <- deviance(x[[error]]) # experimental error
      dfr <- df.residual(x[[error]])# experimental error
      MSE  <- SSE/dfr

    }

    cl <- match.call()

    class(x) <- c('nest.aovlist',class(x))  

    res <- TukeyC(x               = x,
                  which           = which,
                  fl1             = fl1,
                  fl2             = fl2,
                  MSE             = MSE,
                  dfr             = dfr,
                  sig.level       = sig.level,
                  round           = round,
                  adjusted.pvalue = adjusted.pvalue,
                  ...)

    res$call <- cl
    class(res) <- c('TukeyC.aovlist',
                    'TukeyC',
                    'list')
    return(res)                            

  }

  # Aqui nao ha interesse em interacoes!!!
  if(is.null(fl1) & !is.null(error)){ 

    SSE <- deviance(x[[error]]) # experimental error
    dfr <- df.residual(x[[error]])# experimental error
    MSE  <- SSE/dfr

  } else {# Erro experimental

    pos_error <- length(names(x))
    SSE <- deviance(x[[pos_error]]) # experimental error
    dfr <- df.residual(x[[pos_error]])# experimental error
    MSE  <- SSE/dfr

  }

  my <- as.character(attr(x,'terms')[[2]]) 

  forminter <- as.formula(paste(my, 
                                '~', 
                                which)) 

  dat <- model.frame(x)
  aux_mt <- aggregate(forminter, 
                      data = dat,
                      function(x) c(means = mean(x),
                                    r = length(x)))
  
  aux_mt1 <- aux_mt[order(aux_mt[[my]][,1], 
                          decreasing = TRUE),]

  mt <- data.frame(which = aux_mt1[,1],
                   means = aux_mt1[[my]][,1],
                   reps = aux_mt1[[my]][,2])

  row.names(mt) <- aux_mt1[,1]

  out <- make.TukeyC.test(obj             = mt,
                          MSE             = MSE,
                          sig.level       = sig.level,
                          dfr             = dfr,
                          round           = round,
                          adjusted.pvalue = adjusted.pvalue)

  m.inf <- m.infos.aovlist(x         = x,
                           my        = my,
                           forminter = forminter,
                           which     = which,
                           sig.level = sig.level,
                           aux_mt    = aux_mt,
                           MSE       = MSE)

  cl <- match.call()
  res <- list(out  = out,
              info = m.inf)
  res$call <- cl

  class(res) <- c('TukeyC.aovlist',
                  'TukeyC',
                  'list')

  return(res)                    
}
