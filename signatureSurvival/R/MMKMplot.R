MMKMplot <-
function(sdata,stn, gn,X,HR="hazard risk",status="status", time="month", 
         sml="hv",quant=c("No",-0.2,0.2),plotmethod="plot", adjx,outdir,
         file){
  ################################################################
  ## sdata: survival data
  ## gn: a vector for gene number
  ## adjx: adjust x-axise position of p-value in plot
  ## file: output file with pdf
  #################################################################
  colnms<-tolower(colnames(sdata))
 
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
  if(file=="NULL"){
    oldpar <- par(no.readonly = TRUE) # code line i
    on.exit(par(oldpar))
    par(mar=c(4,4,4,2))
    par(oma=c(1,1,1,1))
    par(mfrow=c(3,3))
    #    par(mfrow=c(1,2))
    #    par(cex.main=1.5) 
    
    
    for(mol in sn:ln){
      #  print(mol) 
    plt<- MKMplot(data=sdata,mol=mol,X=X, time=time,HR=HR,status=status, 
              quant=quant,plotmethod="plot",adjx = adjx)
    plt  
    }
  }else{
  if (outdir=="NULL"){
    pdf(file=paste0(file,".pdf"),width = 7,height = 6)
  }else{
    pdf(file=file.path(outdir, paste0(file,".pdf")), width=7, height=6)
  }
    oldpar <- par(no.readonly = TRUE) # code line i
    on.exit(par(oldpar))
    par(mar=c(4,4,4,2))
    par(oma=c(1,1,1,1))
    par(mfrow=c(3,3))
#    par(mfrow=c(1,2))
#    par(cex.main=1.5) 
    
    
  for(mol in sn:ln){
 #  print(mol) 
  MKMplot(data=sdata,mol=mol,X=X, time=time,HR=HR,status=status, 
          quant=quant,plotmethod="plot",adjx = adjx)

  }
  
 dev.off( )
  }
  }else if(plotmethod=="ggsurvplot"){
    if(file=="NULL"){
      
      plt<-list()
      i=0
      for(mol in sn:ln){
        
        i<-i+1
        plt[[i]]<-MKMplot(data=sdata,mol=mol,X=X, time=time,HR=HR,status=status,
                          sml=sml,quant=quant,plotmethod=plotmethod,adjx = adjx)
        res<-arrange_ggsurvplots(plt,  print = TRUE,ncol = 2, nrow = 1,risk.table.height = 0.4)
      print(res)  
      } 
      
    }else{
    if (outdir=="NULL"){
      pdf(file=paste0(file,".pdf"),width = 7,height = 6)
    }else{
      pdf(file=file.path(outdir, paste0(file,".pdf")), width=7, height=6)
    }
    
      plt<-list()
    i=0
    for(mol in sn:ln){

    i<-i+1
    plt[[i]]<-MKMplot(data=sdata,mol=mol,X=X, time=time,HR=HR,status=status,
                      sml=sml,quant=quant,plotmethod=plotmethod,adjx = adjx)

    } 
 #  if (FALSE) {
     # Arrange and save into pdf file
     res<-arrange_ggsurvplots(plt,  print = TRUE,ncol = 2, nrow = 1,risk.table.height = 0.4)
#   }
 print(res)   
 dev.off()
    }
  }else{
    stop("no plot method was specified.")
  }


}
