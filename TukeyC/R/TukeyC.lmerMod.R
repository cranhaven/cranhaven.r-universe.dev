TukeyC.lmerMod <- function(x,
                       which           = NULL,
                       fl1             = NULL, 
                       fl2             = NULL, 
                       error           = NULL,
                       sig.level       = .05,
                       round           = 2,
                       adjusted.pvalue = 'none', ...)   
{

  if(is.null(which)){

    which <- names(x$model)[2]

  }

  # Interacoes com erro experimental
  if(!is.null(fl1) & is.null(error)){

    MSE <- sigma(x)^2 # possivel solucao
    dfr <- df.residual(x)

    cl <- match.call()
    res <- TukeyC.nest.lmerMod(x               = x,
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
    class(res) <- c('TukeyC',
                    'list')

    return(res)                          
  }

  # Interacoes com outros erros
  if(!is.null(fl1) & !is.null(error)){ 

    many_errors <- unlist(strsplit(error,
                                   '\\/'))

    n_errors <- length(many_errors)

    if(n_errors > 1){# combinacao de erros!!!

      aux_MSE1 <- lme4::VarCorr(x)
      aux_MSE <- c(sigma(x)^2,
               attr(aux_MSE1[[many_errors[many_errors!='Residual']]],
                    'stddev')^2)
      aux_df <- unlist(strsplit(error,
                                '[[:punct:]]'))
      aux_df <- aux_df[aux_df!='Residual']
      aux_df1 <- x@frame

      aux_df2 <- sapply(aux_df,function(x)length(levels(aux_df1[[x]])))

      aux_df3 <- as.character(formula(x)[3]) 
      aux_df4 <- unlist(strsplit(aux_df3,
                                 '[[:punct:]]'))
      aux_df5 <- regexpr('[A-Z|a-z|0-9]+',aux_df4)
      aux_df6 <- unique(regmatches(aux_df4,aux_df5))

      aux_df7 <- sapply(aux_df, function(x) any(aux_df6==x))

      aux_df8 <- aux_df2[aux_df7] - 1

      dfr2 <- prod(aux_df2) - (sum(aux_df8) + 1) 
      dfr1 <- df.residual(x)
      aux_dfr <- c(dfr1,dfr2)

      factors <- unlist(strsplit(which,
                                 '[[:punct:]]'))

      aux_levels <- list()

      for(i in 1:dim(aux_df1)[2])aux_levels[[i]] <- nlevels(aux_df1[,i])

      names(aux_levels) <- names(aux_df1)

      levelss <- unlist(aux_levels[factors])

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

    } else {# nao ha combinacao de erros!!!

      anov <- anova(x)
      SSE <- anov[rownames(anov) == error,][1,2] # sum square error
      dfr <- anov[rownames(anov) == error,][1,1] # residual degrees of freedom                     
      MSE <- SSE/dfr   

    }

    cl <- match.call()
    res <- TukeyC.nest.lmerMod(x               = x,
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
    class(res) <- c('TukeyC',
                    'list')

    return(res)                         

  }

  # Aqui nao ha interesse em interacoes!!!!
  if(is.null(fl1) & !is.null(error)){#Um erro de interesse do usuario

    aux_MSE <- lme4::VarCorr(x)
    MSE <- attr(aux_MSE[[error]],'stddev')^2

    aux_df <- unlist(strsplit(error,
                              '[[:punct:]]'))
    aux_df1 <- x@frame

    aux_df2 <- sapply(aux_df,function(x)length(levels(aux_df1[[x]])))

    aux_df3 <- as.character(formula(x)[3]) 
    aux_df4 <- unlist(strsplit(aux_df3,
                               '[[:punct:]]'))
    aux_df5 <- regexpr('[A-Z|a-z|0-9]+',aux_df4)
    aux_df6 <- unique(regmatches(aux_df4,aux_df5))

    aux_df7 <- sapply(aux_df, function(x) any(aux_df6==x))

    aux_df8 <- aux_df2[aux_df7] - 1

    dfr <- prod(aux_df2) - (sum(aux_df8) + 1)

  } else { #Erro experimental

    MSE <- sigma(x)^2
    dfr <- df.residual(x) 
  }

  my <- as.character(formula(x)[[2]])

  forminter <- as.formula(paste(my, 
                                '~', 
                                which)) 

  aux_r <- aggregate(forminter, 
                     data = x@frame,
                     function(x) r = length(x))
  reps <- aux_r[[my]]

  aux_mt <- suppressWarnings(doBy::LSmeans(x,
                                           effect = which,
                                           level = 1 - sig.level))

  aux_mt1 <- aux_mt$coef[,1]

  aux_mt2 <- data.frame(means = aux_mt1,
                        reps = reps)

  row.names(aux_mt2) <- aux_r[,1]

  mt <- aux_mt2[order(aux_mt2[,1],
                      decreasing = TRUE),]

  cl <- match.call()
  out <- make.TukeyC.test(obj             = mt,
                          MSE             = MSE,
                          sig.level       = sig.level,
                          dfr             = dfr,
                          round           = round,
                          adjusted.pvalue = adjusted.pvalue)

  m.inf <- m.infos.lmerMod(x         = x,
                      my        = my,
                      forminter = forminter,
                      which     = which,
                      sig.level = sig.level,
                      aux_mt    = aux_mt)

  res <- list(out  = out,
              info = m.inf)

  res$call <- cl
  class(res) <- c('TukeyC',
                  'list')
  return(res)                        
}
