inventorygames <-
function(n=NA,a=NA,d=NA,h=NA,m=NA,r=NA,b=NA,model=c("EOQ","EPQ")){

if (model=="EOQ"){return(EOQcoo(n,a,d,h,m))} 
if (model=="EPQ"){return(EPQcoo(n,a,d,h,m,r,b))}
if (model!="EOQ"&model!="EPQ"){
  cat("Only EOQ and EPQ can be analyzed with this function.", sep="\n")
}
}
