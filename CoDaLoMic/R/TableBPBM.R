#' Obtaining a table with the SPBal information
#'
#' Returns a table with the percentage of variance that each SPBal has, the bacteria that goes in the numerator and denominator of the balance, the relationship between the group in the numerator and the denominator and the bacteria most influences by this SPBal.
#'@param NumSPBal  List. Output of "ObtainigValueSPBal" function.List. The component i of the list has the number of the row of the matrix \code{especie} where the bacteria in the numerator of the selected  principal balance i are placed.
#'@param DemSPBal  List. Output of "ObtainigValueSPBal" function.List. The component i of the list has the number of the row of the matrix \code{especie} where the bacteria in the denominator of the selected  principal balance i are placed.
#'@param PerVar  Vector. Output of "ObtainigValueSPBal" function. The component of the vector i contains the percentage of variance of the SPBal with numerator \code{NumSPBal[[i]]} and denominator \code{DemSPBal[[i]]}.
#'@param MatrizPBmodelo Matrix. Output of "ObtainigValueSPBal" function.  MatrixSPBal is the matrix that contains the covariates of the model. The first line es equal to 1 for all columns. The other rows contain the value of one SPBal at all time points. The selected principal balance of the row i+1 has at its numerator the bacteria placed in the rows \code{NumSPBal[[i]]} of the "especie". The selected principal balance of the row i+1 has at its denominator the bacteria placed in the rows \code{DemSPBal[[i]]} of the "especie".
#'@param Estimated.Param Vector. Column "mean" of the output of StudyingParam function.
#'@param BB The bacteria in the numerator and the denominator of the balance are considered similar if the mean of the SPBal is between (-BB,BB).Default: 0.55.
#'@param names Vector with the bacteria's names placed in the order in which appear in the rows of the matrices \code{especie} and \code{especiemodi}.
#'@param E number of bacteria in the dataset.
#'
#'@return Returns the table as formatted text. By using the \code{cat} function, you can generate a LaTeX-ready table that can be easily copied and pasted.
#'
#' @examples
#'
#'NumSPBal=list(c(3,4),3,2)
#'DemSPBal=list(1,5,4)
#'PerVar=c(41.37487, 21.08270, 19.16870)
#'MatrizPBmodelo=rbind(c(1.00000000,  1.0000000,  1.0000000,  1.0000000 , 1.0000000),
#'                     c(1.84449081, -1.3569851, -0.1388348, -0.5269079, -1.3288684),
#'                     c(0.27531685,  0.4741394, -1.9045554, -0.3581268, -0.4768543),
#'                     c( 0.07782991,  0.3492473, -0.7138882,  1.4332828, -0.7203295))
#'namesOr=c("Bact1", "Bact2","Bact3","Bact4","Bact5")
#'Estimated.Param=c(0.5, 0.3, -0.2,0.1)
#'E=5
#'
#'tat=TableBPBM(NumSPBal,DemSPBal,PerVar,MatrizPBmodelo,Estimated.Param,BB=0.55,namesOr, E)
#'cat(tat, sep = "\n")
#' @export
#'
#'
#'

TableBPBM<-function(NumSPBal,DemSPBal,PerVar,MatrizPBmodelo,Estimated.Param,BB=0.55,names, E){

  pasm=FromVectorToMatrix_BPBM(Estimated.Param,MatrizPBmodelo ,E)

  m=matrix(0,length(NumSPBal)*2,5)
  j=1
  name.bal=rep(c("NUM","DEM"), length(NumSPBal))
  for (i in 1:length(NumSPBal)){
    a=paste(NumSPBal[[i]], collapse = ",")
    b=paste(which(pasm[,i+1]!=0) , collapse = ",")
    if(mean(MatrizPBmodelo[i+1,])>-BB & mean(MatrizPBmodelo[i+1,])<BB){c="(Similar)"}else{c="(Different)"}
    d=paste(DemSPBal[[i]], collapse = ",")
    e=paste0("SPBal",i)
    f=paste0("(", round(PerVar[i],2), "%)")

    m[j,]=c(e, name.bal[j], a, round(mean(MatrizPBmodelo[i+1,]),3), b)
    m[j+1,]=c(f,name.bal[j+1],d,   c, NA)
    j=j+1+1
}

 mm=matrix(0,length(NumSPBal)*2+3,5)
 mm[1,]=c("SPBal ","NUM/DEM","Bacteria","SPBal mean"         ,"Genera most")
 mm[2,]=c("(% of variance)",NA , "in"      , "(Relation between" ,"influenced by")
 mm[3,]=c(     NA   ,NA          , "NUM/DEM",       "NUM/DEM)"      ,   "the SPBal"     )

 for (i in 1:dim(m)[1] ){ mm[i+3,]=m[i,]}

 nnames=paste(c(1:length(names)), colapse=" is " ,names)
 n.nnames=paste(nnames, collapse=", ")
 cap=paste("We rename each bacteria with a number so" ,n.nnames,collapse=" ")

 xt<-xtable::xtable(mm, caption=cap)
 hlines<-c(-1,3,seq(5,5+length(NumSPBal)*2,2)[c(1:length(NumSPBal))])

 salida <- utils::capture.output(print(xt, booktabs = TRUE, hline.after = hlines,include.rownames=FALSE, include.colnames=FALSE))

 return(salida)
 }

