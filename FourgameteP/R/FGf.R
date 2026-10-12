#' Four-gamete Test
#'
#' The function FGf allows users to quickly assess genotypic data for phylogenetic incompatibility, a sign of recombination, using the four-gamete test.
#'
#' @param x A properly formatted data frame. For details see '?FourgameteP'
#'
#' @return All data output files are organized based on names in the example: 
#'\itemize{
#'\item{ }{...............Locus1....Locus2}
#'\item{ }{ Gamete 1.......A.........A}
#'\item{ }{ Gamete 2.......A.........B}
#'\item{ }{ Gamete 3.......B.........A}
#'\item{ }{ Gamete 4.......B.........B}
#'}
#' @return Columns "Locus1" and "Locus2" in data files indicate which two columns (loci) of your data were being compared.
#' @return In data output file \strong{"TrueUnique"}, the column "Locus1A" is the first allele "A" in the column "Locus1" while "Locus2B" would indicate the second allele (B) present in the "Locus2" column. Together they make up four dilocus genotypes (or the four gametes): (Locus1A,Locus2A), (Locus1A, Locus2B), (Locus1B,Locus2A), and (Locus1B,Locus2B)
#' @return The "FourGametes?" column indicates if the program was able to find all four gametes. A value of "TRUE" indicates phylogenetic incompatibility between the tested loci. A value of “FALSE” indicates that only three or fewer gametes were found.
#' @return \strong{"FinalResults"} This file contains all locus pair comparisons and if the program was able to find any ANY combination of alleles at a locus pair for which the four gametes were found (TRUE) or if  it was only able to find three or fewer of the gametes (FALSE). 
#' @return \strong{"TrueUnique"} This file contains all of the possible combination of alleles at all locus pairs for which the four gametes can be found (TRUE). 
#' @return The number of monomorphic loci is stored in the value \strong{"MonoLoci"} and their identity in \strong{"NamesOfMonoLoci"}.
#' @return The total number of comparisons between loci that are not monomorphic is stored in \strong{"Comparisons"}
#' @return The number of these comparisons that were TRUE or False is stored in \strong{"ComparisonsTrue"} or in \strong{"ComparisonsFalse"} respectively.

#' @examples
#' n = c(2, 3, 3, 2)
#' c = c(2, 2, 4, 4)
#' d = c(2, 5, 5, 3)
#' df = data.frame(n, c, d)
#' FGf(df)
#'

#'
#' 
#' @export
FGf<-function(x){{

  Comparisons<-"processing"
  ComparisonsTrue<-"processing"
  ComparisonsFalse<-"processing"
  
  {
    b<-colnames(x)
    c<-length(b)
    colnames(x)<-c(letters[1:c])
    d<-colnames(x)
    Names<-as.data.frame(cbind(Old=c(b),New=c(d)))}

  loci<-ncol(x)
  B <- paste("x$", letters[1:26], sep="")
  RawData <- data.frame(Locus1A=numeric(), Locus2A=numeric(), Locus1B=numeric(), Locus2B=numeric(), Value=logical(), Locus1=numeric(), Locus2=numeric())
  MonoLociNames<- data.frame(Name=numeric())
  LoopCount<-Loop<-0
  Count<-0
  MonoLoci<-0

  writeLines("FourGamete Progress:")
  pb <- txtProgressBar( min = 0, max =((loci*(loci-1)/2)-1), initial=0,style=3,title="cheese")

  for(m in c(1:loci)){

    RM<-x[eval(parse(text=B[m]))!=0,letters[m]]
    lo<-unique(RM)
    if (length(unique(lo))==1){MonoLoci<-Count<-Count+1
    MonoLociNames[nrow(MonoLociNames)+1,]<-list(letters[m])}

    for(o in c(m:loci-m)){

      if (m!=m+o) {LoopCount<-Loop<-Loop+1}
      setTxtProgressBar(pb, (LoopCount/(loci*(loci-1)/2)*100),"% done")
      max<-max( eval(parse(text=B[m])))

      for (n in 1:max) {
        result<-x[eval(parse(text=B[m]))==n & eval(parse(text=B[m+o]))!=0,letters[m+o]]
        test<-unique(result)


        for (P in test) {
          ar<-x[eval(parse(text=B[m+o]))==P & eval(parse(text=B[m]))!=n & eval(parse(text=B[m]))!=0,letters[m]]
          ar <- if (identical(ar, integer(0))) NA else ar
          ar<-unique(ar)

          for (p in ar){
            ar1<-x[eval(parse(text=B[m]))==p & eval(parse(text=B[m+o]))!=P & eval(parse(text=B[m+o]))!=0,letters[m+o]]
            ar1<-unique(ar1)

            for (N in ar1){
              af<- any(match(N,result,nomatch=0))
              aff<-any(match(af,TRUE, nomatch=0))

              if (m!=m+o){  RawData[nrow(RawData)+1,]<- list( n, P, p, N, aff, letters[m],letters[m+o])}
            }}}}}}
}
  TrueData<-as.data.frame(RawData[RawData$Value==TRUE,])
  TrueUnique<-as.data.frame(unique(TrueData[c("Locus1A","Locus2A","Locus1B","Locus2B","Value","Locus1","Locus2")]))

  TrueUnique$Locus1 <- Names$Old[match(TrueUnique$Locus1, Names$New)]
  TrueUnique$Locus2 <- Names$Old[match(TrueUnique$Locus2, Names$New)]
  colnames(x)<-c(b)
  MonoLociNames$Name <- Names$Old[match(MonoLociNames$Name, Names$New)]

  TrueFinal<-as.data.frame(unique(TrueUnique[c("Locus1","Locus2","Value")]))

  AllUnique<-as.data.frame(unique(RawData[c("Value","Locus1","Locus2")]))
  FalseFinal<-AllUnique[!(duplicated(AllUnique[-1])|duplicated(AllUnique[-1], fromLast = TRUE)),]
  FalseFinal$Locus1 <- Names$Old[match(FalseFinal$Locus1, Names$New)]
  FalseFinal$Locus2 <- Names$Old[match(FalseFinal$Locus2, Names$New)]

  NamesOfMonoLoci<-as.vector(MonoLociNames$Name)
  FalseFinal<-FalseFinal[!(FalseFinal$Locus1 %in% NamesOfMonoLoci),]
  FalseFinal<-as.data.frame(FalseFinal[FalseFinal$Value==FALSE,])

  FinalResults<-rbind(TrueFinal,FalseFinal)
  rownames(FinalResults)<-NULL
  rownames(TrueUnique)<-NULL

  colnames(FinalResults)[3]<-"FourGametes?"
  colnames(TrueUnique)[5]<-"FourGametes?"

  TrueUnique<<-TrueUnique
  FinalResults<<-FinalResults
  Comparisons<<-(((loci-MonoLoci)*(loci-MonoLoci-1))/2)
  ComparisonsTrue<<-as.numeric(length(rownames(TrueFinal)))
  ComparisonsFalse<<-as.numeric(length(rownames(FalseFinal)))


  rm(loci,TrueData,AllUnique,ar,ar1,af,aff,max,n,P,test,B,m,o,
     result,N,p,Loop,RM,lo,Count,LoopCount,MonoLociNames,RawData,
     b,c,d,Names,pb,FalseFinal,TrueFinal,x)

  return("
         Analysis complete! Data in Rstudio 'Environment' tab under 'Data'")

}
