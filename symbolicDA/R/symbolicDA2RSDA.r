RSDA2SymbolicDA<-function(rsda.object=NULL,from.csv=F,file=NULL, header = TRUE, sep, dec, row.names = NULL) {

if((is.null(rsda.object) || !inherits(rsda.object,"sym.data.table")) && from.csv){
  rsda.object<-read.sym.table(file=file, header = header, sep=sep, dec=dec, row.names = row.names)
}
individualsNo<-rsda.object$N
indivA<-array("",c(individualsNo,3))
for(i in 1:individualsNo)
{
  indivA[i,1]<-i
  indivA[i,2]<-rsda.object$sym.obj.names[i]
  indivA[i,3]<-i
}
indiv<-as.data.frame(indivA)
names(indiv)<-c("num","name","label")

variablesNo<-rsda.object$M
variablesICNo<-sum(rsda.object$sym.var.types=="$I")
variablesCNo<-sum(rsda.object$sym.var.types=="$C")
variablesNNo<-sum(rsda.object$sym.var.types=="$S")
variablesNMNo<-sum(rsda.object$sym.var.types=="$M")
detailsICA<-array("",c(variablesICNo,4))
detailsCA<-array("",c(variablesCNo,4))
detailsNA<-array("",c(variablesNNo,3))
detailsNMA<-array("",c(variablesNMNo,4))
variablesA<-array("",c(variablesNo,5))
detailsICNo<-0
detailsCNo<-0
detailsNNo<-0
detailsNMNo<-0
detailsListNom<-NULL
detailsListNomModif<-NULL
indivNA<-NULL
indivNMA<-NULL
for(i in 1:variablesNo)
{
  if(rsda.object$sym.var.types[i]=="$C"){
  variablesA[i,1]<-i
  variablesA[i,2]<-rsda.object$sym.var.names[i]
  variablesA[i,3]<-rsda.object$sym.var.names[i]
  variablesA[i,4]<-"C"
  detailsCNo<-detailsCNo+1
  detailsCA[detailsCNo,1]<-0
  detailsCA[detailsCNo,2]<-0
  detailsCA[detailsCNo,3]<-as.numeric(min(rsda.object$meta[,which(dimnames(rsda.object$meta)[[2]]==rsda.object$sym.var.names[i])]))
  detailsCA[detailsCNo,4]<-as.numeric(max(rsda.object$meta[,which(dimnames(rsda.object$meta)[[2]]==rsda.object$sym.var.names[i])]))
  variablesA[i,5]<-detailsCNo
  }

  if(rsda.object$sym.var.types[i]=="$I"){
    variablesA[i,1]<-i
    variablesA[i,2]<-rsda.object$sym.var.names[i]
    variablesA[i,3]<-rsda.object$sym.var.names[i]
    variablesA[i,4]<-"IC"
    detailsICNo<-detailsICNo+1
    detailsICA[detailsICNo,1]<-0
    detailsICA[detailsICNo,2]<-0
    detailsICA[detailsICNo,3]<-as.numeric(min(rsda.object$meta[,which(dimnames(rsda.object$meta)[[2]]==rsda.object$sym.var.names[i])]))
    detailsICA[detailsICNo,4]<-as.numeric(max(rsda.object$meta[,which(dimnames(rsda.object$meta)[[2]]==rsda.object$sym.var.names[i])]))
    variablesA[i,5]<-detailsICNo
  }

  if(rsda.object$sym.var.types[i]=="$S"){
    variablesA[i,1]<-i
    variablesA[i,2]<-rsda.object$sym.var.names[i]
    variablesA[i,3]<-rsda.object$sym.var.names[i]
    variablesA[i,4]<-"N"
    detailsNNo<-detailsNNo+1
    detailsNA[detailsNNo,1]<-0
    detailsNA[detailsNNo,2]<-0
    index<-which(dimnames(rsda.object$meta)[[2]]==rsda.object$sym.var.names[i])
    nr<-as.numeric(rsda.object$meta[1,index])
    detailsNA[detailsNNo,3]<-nr
    variablesA[i,5]<-detailsNNo
    for(ii in 1:nr){
      nazwa<-dimnames(rsda.object$meta)[[2]][index+ii]
  #    if(is.null(detailsListNom)){
  #      lp=1
  #    }
  #    else{
  #      lp=nrow(detailsListNom)+1
  #    }
      detailsListNom<-rbind(detailsListNom,c(detailsCNo,ii,nazwa,nazwa))
      for(j in 1:rsda.object$N){
        if(rsda.object$meta[j,index+ii]!=0){
          indivNA<-rbind(indivNA,c(j,i,ii))
        }
      }
    }
    
    
    
  }
  
  if(rsda.object$sym.var.types[i]=="$M"){
    variablesA[i,1]<-i
    variablesA[i,2]<-rsda.object$sym.var.names[i]
    variablesA[i,3]<-rsda.object$sym.var.names[i]
    variablesA[i,4]<-"MN"
    detailsNMNo<-detailsNMNo+1
    detailsNMA[detailsNMNo,1]<-0
    detailsNMA[detailsNMNo,2]<-0
    print("tt")
    
    index<-which(dimnames(rsda.object$meta)[[2]]==rsda.object$sym.var.names[i])
    nr<-as.numeric(rsda.object$meta[1,index])
    
    detailsNMA[detailsNMNo,3]<-nr
    variablesA[i,5]<-detailsNMNo
    for(ii in 1:nr){
      nazwa<-dimnames(rsda.object$meta)[[2]][index+ii]
      #    if(is.null(detailsListNom)){
      #      lp=1
      #    }
  
          #    else{
      #      lp=nrow(detailsListNom)+1
      #    }
      detailsListNomModif<-rbind(detailsListNomModif,c(detailsCNo,ii,nazwa,nazwa))
      for(j in 1:rsda.object$N){
          indivNMA<-rbind(indivNMA,c(j,i,ii,rsda.object$meta[j,index+ii]))
      }
    }
    
    
    
  }
  
  
  
      
}
variables<-as.data.frame(variablesA)
names(variables)<-c("num","name","label","type","details")
detailsIC<-as.data.frame(detailsICA)
detailsC<-as.data.frame(detailsCA)
names(detailsIC)<-c("na","nu","min","max")
names(detailsC)<-c("na","nu","min","max")
indivICA<-array(0,c(individualsNo,variablesNo,2))
indivCA<-array(0,c(individualsNo,variablesNo))
CA<-array(0,c(individualsNo,variablesNo,1))
for(i in 1:individualsNo)
{
  for(j in 1:variablesNo)
  {
    if(rsda.object$sym.var.types[j]=="$I"){
      indivICA[i,j,1]<-rsda.object$meta[i,which(dimnames(rsda.object$meta)[[2]]==rsda.object$sym.var.names[j])[1]]
      indivICA[i,j,2]<-rsda.object$meta[i,which(dimnames(rsda.object$meta)[[2]]==rsda.object$sym.var.names[j])[2]]
    }
    if(rsda.object$sym.var.types[j]=="$C"){
      indivCA[i,j]<-rsda.object$meta[i,which(dimnames(rsda.object$meta)[[2]]==rsda.object$sym.var.names[j])[1]]
    }
  }
}
if(!is.null(detailsListNom)){
  
  colnames(detailsListNom)<-c("details_no","num","name","label")
  colnames(detailsNA)<-c("na","nu","modals")
  detailsListNom<-as.data.frame(detailsListNom)
  detailsNA<-as.data.frame(detailsNA)
}
if (!is.null(indivNA))
{
  indivNA<-as.data.frame(indivNA)
  names(indivNA)<-c("indiv","variable","value")
}

if (!is.null(indivNMA))
{
  indivNMA<-as.data.frame(indivNMA)
  names(indivNMA)<-c("indiv","variable","value","frequency")
}

resul<-list(individuals=indiv,variables=variables,detailsIC=detailsIC,detailsC=detailsC,detailsN=detailsNA,detailsListNom=detailsListNom,detailsNM=NULL,detailsListNomModif=NULL,indivIC=indivICA,indivC=indivCA,indivN=indivNA,indivListNom=NULL,indivNM=NULL,indivListNomModif=indivNMA)

class(resul)<-"symbolic"
resul
}
