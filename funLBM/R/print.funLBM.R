print.funLBM <- function(x,...){
  cat('* Selected funLBM model:\n')
  cat('\t Basis:\t',x$basisName,'\n')
  cat('\t K:\t',x$K,'row groups\n')
  cat('\t L:\t',x$L,'columns groups\n')
  cat('\t ICL:\t',x$icl,'\n')
}