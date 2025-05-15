#' generate.columns.interval
#' @keywords internal
#' @author Jorge Arce Garro
intersection.interval<-function(Interval.1,Interval.2){
   intersection <- c(NA,NA)
   #if(Interval.1[1] <= Interval.2[2] & Interval.2[1] <= Interval.1[2] | )
   if(Interval.2[2] <= Interval.1[1]){
     intersection[1]<-0
     intersection[2]<-0
   } else if(Interval.1[2] <= Interval.2[1]){
     intersection[1]<-0
     intersection[2]<-0
   } else{
     intersection[1]<-max(Interval.1[1],Interval.2[1])
     intersection[2]<-min(Interval.1[2],Interval.2[2])
   }
   return(intersection)
}
