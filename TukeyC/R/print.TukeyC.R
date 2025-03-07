##
## S3 method to print 'TukeyC' object
##

print.TukeyC <- function(x,
                         ...)
{
  cat('Results\n')  
  print(x$out$Result,
        ...)

  cat('\nSig.level\n',
      x$out$Sig.level)

  cat('\n\nDiff_Prob\n')
  print(x$out$Diff_Prob,
        ...)

  cat('\nMSD\n')
  print(x$out$MSD,
        ...)

}                   
