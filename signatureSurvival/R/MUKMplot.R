MUKMplot <-
function(svdata,stn,gn,HR="hazard risk",time = "month", status = "status",
         sml="hv",quant=c("No",-0.2,0.2),plotmethod="plot",adjx,outdir="NULL",
         file){

  #cbdata<-cbind(datag,proteinx)
#pdf(file, width=12, height=12)
  colnms<-tolower(colnames(svdata))
  if(is.character(stn)){
    kind<-is.element(colnms,tolower(stn)) 
    sn<-which(kind==TRUE)
  }else if(is.numeric(stn)){
    sn<-stn
  }
  
  if(is.character(gn)){
    kind<-is.element(colnms,tolower(gn)) 
    ln<-which(kind==TRUE)
  }else if(is.numeric(gn)){
    ln<-gn
  }  

if(plotmethod=="plot"){
  if(file!="NULL"){
  if (outdir=="NULL"){
    pdf(file=paste0(file,".pdf"),width = 10,height = 12)
  }else{
    pdf(file=file.path(outdir, paste0(file,".pdf")), width=10, height=12)
  }

#datag<-rbind(data[,1:5],data[,1:5],data[,1:5],data[,1:5])
    oldpar <- par(no.readonly = TRUE) # code line i
    on.exit(par(oldpar))
    par(mar=c(4,5,2,1))
    par(oma=c(3,4,2,1))
    par(mfrow=c(3,3))
    par(cex.main=1.5)
# gn<-as.vector(gn)
#cbdata<-cbind(datag,proteinx)

for(mol in sn:ln){
  UKMplot(data=svdata,mol=mol,HR=HR, time=time,status=status,quant=quant,
          plotmethod="plot",adjx=adjx)
}


dev.off( ) #using dev.off( ) to return output to the terminal
  }else{
    oldpar <- par(no.readonly = TRUE) # code line i
    on.exit(par(oldpar))
    par(mar=c(4,5,2,1))
    par(oma=c(3,4,2,1))
    par(mfrow=c(3,3))
    par(cex.main=1.5)
    # gn<-as.vector(gn)
    #cbdata<-cbind(datag,proteinx)
    
    for(mol in sn:ln){
   plt<-UKMplot(data=svdata,mol=mol,HR=HR, time=time,status=status,quant=quant,
              plotmethod="plot",adjx=adjx)
    }
    plt
}

}else if(plotmethod=="ggsurvplot"){
  if(file!="NULL"){
  if (outdir=="NULL"){
    pdf(file=paste0(file,".pdf"),width = 7,height = 6)
  }else{
    pdf(file=file.path(outdir, paste0(file,".pdf")), width=7, height=6)
  }
  
  plt<-list()
  i=0
  for(mol in sn:ln){
    
    i<-i+1
   plt[[i]]<-UKMplot(data=svdata,mol,HR=HR, time=time,status=status,sml=sml,
                     quant=quant,plotmethod="ggsurvplot",adjx=adjx)
   
      } 
#     if (FALSE) {
  # Arrange and save into pdf file
    res<-arrange_ggsurvplots(plt,  print = TRUE,ncol = 2, nrow = 1,
                             risk.table.height = 0.4)
 #    }
#  print(plt)
   print(res)   
  dev.off()
  }else{
    
    plt<-list()
    i=0
    for(mol in sn:ln){
      
      i<-i+1
      plt[[i]]<-UKMplot(data=svdata,mol,HR=HR, time=time,status=status,sml=sml,
                        quant=quant,plotmethod="ggsurvplot",adjx=adjx)
      
    }
    plt
  }
  
}else{
  stop("no plot method was specifies.")
}
}
