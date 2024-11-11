#' Derive threshold for importance scores and return final networks.
#'
#' INPUT
#' out.iRafNet  output from function iRafNet
#' out.perm     output from function Run_permutation 
#' TH           FDR threshold
#'
#' @export 
#"iRafNet_network" <-  function(out, ...)UseMethod("iRafNet")


iRafNet_network <- function(out.iRafNet,out.perm,TH) {
 
  M<-dim(out.perm)[2]; #-- number of permutations
  j.np<-sort(out.iRafNet[,3],decreasing=TRUE)
  FDR<-rep(0,dim(out.perm)[1]); 
  for (s in 1:length(j.np)){ 
    FP<-sum(sum(out.perm>=j.np[s]))/M
    FDR[s]<-FP/s;
    if (FDR[s]>TH) {th<-j.np[s];  break;}
  }
 
  return(out.iRafNet[out.iRafNet[,3]>th,seq(1,2)])
}