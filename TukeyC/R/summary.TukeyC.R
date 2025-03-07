##
## S3 method to sumarize 'TukeyC' object
##

summary.TukeyC <- function(object,
                           complete=TRUE, ...)
{
  if(!inherits(object,
               'TukeyC'))
    stop("Use only with \"TukeyC\" objects!")

  if(complete){
    cat('Goups of means at sig.level =',
        object$out$Sig.level,
        '\n')

    print(object$out$Result)

    cat('\nMatrix of the difference of means above diagonal and')

    cat('\nrespective p-values of the Tukey test below diagonal values\n')

    print(object$out$Diff_Prob)

  } else { 
    res <- object$out$Result

    res
  }
}                   
