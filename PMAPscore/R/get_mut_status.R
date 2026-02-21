#' @title Converts MAF file into mutation matrix
#' @description The function `get_mut_status` uses to convert MAF file into mutation matrix.
#' @param maf_data The patients' somatic mutation data, which in MAF format.
#' @param nonsynonymous Logical, tell if extract the non-silent somatic mutations (nonsense mutation, missense mutation, frame-shif indels, splice site, nonstop mutation, translation start site, inframe indels).
#' @importFrom utils read.delim
#' @return A binary mutations matrix, in which 1 represents that a particular gene has mutated in a particular sample, and 0 represents that gene has no mutation in a particular sample .
#' @export
#' @examples
#' #load the data
#' data(maf_data)
#' #perform the function `get_mut_status`.
#' mutmatrix.example<-get_mut_status(maf_data,nonsynonymous = TRUE)


get_mut_status<-function(maf_data,nonsynonymous = TRUE){
  mutvariant<-maf_data[,c("Hugo_Symbol",
                     "Tumor_Sample_Barcode",
                     "Variant_Classification")]
  if(nonsynonymous){
    mafmut<-mutvariant[which(
      mutvariant$Variant_Classification == "Missense_Mutation" |
        mutvariant$Variant_Classification == "Frame_Shift_Del" |
        mutvariant$Variant_Classification == "Frame_Shift_Ins" |
        mutvariant$Variant_Classification == "In_Frame_Del" |
        mutvariant$Variant_Classification == "Nonsense_Mutation" |
        mutvariant$Variant_Classification == "In_Frame_Ins" |
        mutvariant$Variant_Classification == "Splice_Site" |
        mutvariant$Variant_Classification == "Nonstop_Mutation" |
        mutvariant$Variant_Classification == "Translation_Start_Site"
    ),]
  }else{
    mafmut<-mutvariant
  }
  mut_status<-matrix(data=0,nrow=length(unique(mafmut[,1])),ncol=length(unique(mafmut[,2])))
  colnames(mut_status)<-unique(mafmut[,2])
  rownames(mut_status)<-unique(mafmut[,1])
  for(i in 1:dim(mut_status)[2]){
    un_sname<-table(mafmut[which(mafmut[,2]%in%colnames(mut_status)[i]),1])
    mut_status[match(names(un_sname),rownames(mut_status)),i]<-un_sname
  }
  mut_status[mut_status>1]<-1
  return(mut_status)
}
