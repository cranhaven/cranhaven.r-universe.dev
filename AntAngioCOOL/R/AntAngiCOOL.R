#' @title AntAngioCOOL
#' @description Machine learning based package to predict anti-angiogenic peptides using heterogeneous sequence descriptors.
#' @details AntAngioCOOL is a machine learning based package to predict anti-angiogenic peptides using heterogeneous sequence descriptors.
#' @details This package consists of three different predictors according to the obtained performances on independent test set: sensitive model, specific model and accurate model. These models have been build using the gold standard dataset that published by Ramaprasad et al. (Ettayapuram Ramaprasad et al., 2015).
#' @details Four different features have been used to encode peptides:
#' @details 1- Pseudo Amino Acid Composition  (PseAAC) that has been used effectively in predicting cell penetrating peptides (Chen, Chu, Huang, Kong, & Cai, 2015). Despite the simple amino acid composition, PseAAC considers the sequence-order information of the peptide.
#' @details 2- K-mer composition that shows the fraction of all possible subsequences with length k in the given peptide. To compute k-mer composition features, reduced amino acid alphabet that proposed by Zahiri et al (Zahiri et al., 2014) has been exploited: the 20 alphabet of amino acids have been reduced to a new alphabet with size 8 according to 544 physicochemical and biochemical indices that extracted from AAIndex database (Kawashima et al., 2008) (C1={A, E}, C2={I, L, F, M, V}, C3={N, D, T, S}, C4={G}, C5={P}, C6={R, K, Q, H}, C7={Y, W}, C8={C}).  We have computed k-mer composition for k=2,3,4 for each peptide.
#' @details 3- Physico-chemical profile : To compute these features, 544 different physico-chemical indices have been extracted from AAINDEX (Kawashima et al., 2008). To remove redundant indices, a subset of indices with correlation coefficient less than 0.8 and greater than -0.8 has been selected. This feature type for 5 amino acids of N- termini (5-NT) and C-termini (5-CT).
#' @details 4- Atomic profile : A 50-dimentional feature vector has been used to encode each peptide according to its atomic properties (frequency of five types of atoms: C, H, N, O, S). For details of atomic composition for each 20 natural amino acid see Kumar et al., 2015.
#' @details References
#' @details 1- Chen, L., Chu, C., Huang, T., Kong, X., & Cai, Y.-D. (2015). Prediction and analysis of cell-penetrating peptides using pseudo-amino acid composition and random forest models. Amino Acids, 47(7), 1485-93. http://doi.org/10.1007/s00726-015-1974-5
#' @details 2- Ettayapuram Ramaprasad, A. S., Singh, S., Gajendra P. S, R., Venkatesan, S., Brem, S., Cotran, R., . Stephens, R. (2015). AntiAngioPred: A Server for Prediction of Anti-Angiogenic Peptides. PLOS ONE, 10(9), e0136990. http://doi.org/10.1371/journal.pone.0136990
#' @details 3- Kawashima, S., Pokarowski, P., Pokarowska, M., Kolinski, A., Katayama, T., & Kanehisa, M. (2008). AAindex: amino acid index database, progress report 2008. Nucleic Acids Research, 36(Database issue), D202-5. http://doi.org/10.1093/nar/gkm998
#' @details 3- Kumar, R., Chaudhary, K., Singh Chauhan, J., Nagpal, G., Kumar, R., Sharma, M., & Raghava, G. P. S. (2015). An in silico platform for predicting, screening and designing of antihypertensive peptides. Scientific Reports, 5, 12512. http://doi.org/10.1038/srep12512
#' @details 4- Zahiri, J., Mohammad-Noori, M., Ebrahimpour, R., Saadat, S., Bozorgmehr, J. H., Goldberg, T., & Masoudi-Nejad, A. (2014). LocFuse: Human protein-protein interaction prediction via classifier fusion using protein localization information. Genomics, 104(6), 496-503.
#' @author Babak Khorsand
#' @import caret
#' @import rJava
#' @import RWeka
#' @import rpart
#' @importFrom stats predict
#' @export AntAngioCOOL
#' @param Input_Sequence A peptide sequence
#' @param Classifier 1 if you want to get the prediction from model with maximum Accuracy (according to the independent test reasults), 2 if maximum Sensivity is desired and 3 if maximum Specefity is desired.
#' @param SF if True then all 2343 selected features (out of 175062 features) that had been used for prediction will be returned.
#' @param AF if True then all 175062 extracted features will be returned.
#' @return If Predicted class (Anti-angiogenic/ Not anti-angiogenic) of the input peptide and a subset of descriptors upon request.
#' @examples
#' AntAngioCOOL("AAPFLECQGN",2,SF=TRUE)
AntAngioCOOL = function(Input_Sequence,Classifier=1,SF=FALSE,AF=FALSE)
{
  Seq_Length = nchar(Input_Sequence)
  if(Seq_Length<10)
  {
    stop("Input Sequence must have length more than 10")
  }else
  {
    Onemer=Features[which(Features=="A"):which(Features=="V")]
    Twomers=Features[which(Features=="AA"):which(Features=="VV")]
    Threemers=Features[which(Features=="AAA"):which(Features=="VVV")]
    Fourmers=Features[which(Features=="AAAA"):which(Features=="VVVV")]

    # A,AA,AAA,AAAA ----
    Seq_Num=1:Seq_Length
    Sequence="z"
    Sequence= sapply(Seq_Num,function(i) c(Sequence,substr(Input_Sequence,i,i)))
    Sequence=Sequence[2,]

    Twomer_Seq="AA"
    Seq_Num=1:(Seq_Length-1)
    Twomer_Seq=sapply(Seq_Num, function(i) c(Twomer_Seq,paste(Sequence[i],Sequence[i+1],sep="")))
    Twomer_Seq=Twomer_Seq[2,1:(Seq_Length-1)]

    Threemer_Seq="AAA"
    Seq_Num=1:(Seq_Length-2)
    Threemer_Seq=sapply(Seq_Num, function(i) c(Threemer_Seq,paste(Sequence[i],Sequence[i+1],Sequence[i+2],sep="")))
    Threemer_Seq=Threemer_Seq[2,1:(Seq_Length-2)]

    Fourmer_Seq="AAAA"
    Seq_Num=1:(Seq_Length-3)
    Fourmer_Seq=sapply(Seq_Num, function(i) c(Fourmer_Seq,paste(Sequence[i],Sequence[i+1],Sequence[i+2],Sequence[i+3],sep="")))
    Fourmer_Seq=Fourmer_Seq[2,1:(Seq_Length-3)]

    Onemer_Seq_table=table(Sequence)
    Feature_Onemer=0
    Feature_Onemer = sapply(Onemer, function(x) c(Feature_Onemer,ifelse(length(grep(x,Sequence))>0,(Onemer_Seq_table[x]/length(Onemer)),0)))
    Feature_Onemer=Feature_Onemer[2,]
    names(Feature_Onemer)=Onemer

    Twomer_Seq_table=table(Twomer_Seq)
    Feature_Twomers=0
    Feature_Twomers = sapply(Twomers, function(x) c(Feature_Twomers,ifelse(length(grep(x,Twomer_Seq))>0,(Twomer_Seq_table[x]/length(Twomers)),0)))
    Feature_Twomers=Feature_Twomers[2,]
    names(Feature_Twomers)=Twomers

    Threemer_Seq_table=table(Threemer_Seq)
    Feature_Threemers=0
    Feature_Threemers = sapply(Threemers, function(x) c(Feature_Threemers,ifelse(length(grep(x,Threemer_Seq))>0,(Threemer_Seq_table[x]/length(Threemers)),0)))
    Feature_Threemers=Feature_Threemers[2,]
    names(Feature_Threemers)=Threemers

    Fourmer_Seq_table=table(Fourmer_Seq)
    Feature_Fourmers=0
    Feature_Fourmers = sapply(Fourmers, function(x) c(Feature_Fourmers,ifelse(length(grep(x,Fourmer_Seq))>0,(Fourmer_Seq_table[x]/length(Fourmers)),0)))
    Feature_Fourmers=Feature_Fourmers[2,]
    names(Feature_Fourmers)=Fourmers

    Extracted_Features=c(Feature_Onemer,Feature_Twomers,Feature_Threemers,Feature_Fourmers)

    # Amino8 ----
    Amino8 = Input_Sequence
    Amino8 = gsub("A","a",Amino8)
    Amino8 = gsub("E","a",Amino8)
    Amino8 = gsub("I","b",Amino8)
    Amino8 = gsub("L","b",Amino8)
    Amino8 = gsub("F","b",Amino8)
    Amino8 = gsub("M","b",Amino8)
    Amino8 = gsub("V","b",Amino8)
    Amino8 = gsub("N","c",Amino8)
    Amino8 = gsub("D","c",Amino8)
    Amino8 = gsub("T","c",Amino8)
    Amino8 = gsub("S","c",Amino8)
    Amino8 = gsub("G","d",Amino8)
    Amino8 = gsub("P","e",Amino8)
    Amino8 = gsub("R","f",Amino8)
    Amino8 = gsub("K","f",Amino8)
    Amino8 = gsub("Q","f",Amino8)
    Amino8 = gsub("H","f",Amino8)
    Amino8 = gsub("Y","g",Amino8)
    Amino8 = gsub("W","g",Amino8)
    Amino8 = gsub("C","h",Amino8)
    Onemer=Features[which(Features=="a"):which(Features=="h")]
    Twomers=Features[which(Features=="aa"):which(Features=="hh")]
    Threemers=Features[which(Features=="aaa"):which(Features=="hhh")]
    Fourmers=Features[which(Features=="aaaa"):which(Features=="hhhh")]

    # a,aa,aaa,aaaa ----
    Seq_Num=1:Seq_Length
    Sequence="z"
    Sequence= sapply(Seq_Num,function(i) c(Sequence,substr(Input_Sequence,i,i)))
    Sequence=Sequence[2,]

    Twomer_Seq="AA"
    Seq_Num=1:(Seq_Length-1)
    Twomer_Seq=sapply(Seq_Num, function(i) c(Twomer_Seq,paste(Sequence[i],Sequence[i+1],sep="")))
    Twomer_Seq=Twomer_Seq[2,1:(Seq_Length-1)]

    Threemer_Seq="AAA"
    Seq_Num=1:(Seq_Length-2)
    Threemer_Seq=sapply(Seq_Num, function(i) c(Threemer_Seq,paste(Sequence[i],Sequence[i+1],Sequence[i+2],sep="")))
    Threemer_Seq=Threemer_Seq[2,1:(Seq_Length-2)]

    Fourmer_Seq="AAAA"
    Seq_Num=1:(Seq_Length-3)
    Fourmer_Seq=sapply(Seq_Num, function(i) c(Fourmer_Seq,paste(Sequence[i],Sequence[i+1],Sequence[i+2],Sequence[i+3],sep="")))
    Fourmer_Seq=Fourmer_Seq[2,1:(Seq_Length-3)]

    Onemer_Seq_table=table(Sequence)
    Feature_Onemer=0
    Feature_Onemer = sapply(Onemer, function(x) c(Feature_Onemer,ifelse(length(grep(x,Sequence))>0,(Onemer_Seq_table[x]/length(Onemer)),0)))
    Feature_Onemer=Feature_Onemer[2,]
    names(Feature_Onemer)=Onemer

    Twomer_Seq_table=table(Twomer_Seq)
    Feature_Twomers=0
    Feature_Twomers = sapply(Twomers, function(x) c(Feature_Twomers,ifelse(length(grep(x,Twomer_Seq))>0,(Twomer_Seq_table[x]/length(Twomers)),0)))
    Feature_Twomers=Feature_Twomers[2,]
    names(Feature_Twomers)=Twomers

    Threemer_Seq_table=table(Threemer_Seq)
    Feature_Threemers=0
    Feature_Threemers = sapply(Threemers, function(x) c(Feature_Threemers,ifelse(length(grep(x,Threemer_Seq))>0,(Threemer_Seq_table[x]/length(Threemers)),0)))
    Feature_Threemers=Feature_Threemers[2,]
    names(Feature_Threemers)=Threemers

    Fourmer_Seq_table=table(Fourmer_Seq)
    Feature_Fourmers=0
    Feature_Fourmers = sapply(Fourmers, function(x) c(Feature_Fourmers,ifelse(length(grep(x,Fourmer_Seq))>0,(Fourmer_Seq_table[x]/length(Fourmers)),0)))
    Feature_Fourmers=Feature_Fourmers[2,]
    names(Feature_Fourmers)=Fourmers

    Extracted_Features=c(Extracted_Features,Feature_Onemer,Feature_Twomers,Feature_Threemers,Feature_Fourmers)

    # Atomic Profile ----
    Seq_Num=1:Seq_Length
    Sequence="z"
    Sequence= sapply(Seq_Num,function(i) c(Sequence,substr(Input_Sequence,i,i)))
    Sequence=Sequence[2,]

    for (i in 1:5)
    {
      temp =  Atomic_Profile[Atomic_Profile$AminoAcids==Sequence[i],2:9]
      names(temp)=paste(names(temp),"_C",i,sep = "")
      temp=sapply(temp, function(x) as.numeric(as.character(x)))
      Extracted_Features=c(Extracted_Features,temp)
      temp =  Atomic_Profile[Atomic_Profile$AminoAcids==Sequence[(Seq_Length-5+i)],2:9]
      names(temp)=paste(names(temp),"_N",i,sep = "")
      temp=sapply(temp, function(x) as.numeric(as.character(x)))
      Extracted_Features=c(Extracted_Features,temp)
    }

    # PhysicoChemical AAC ----

    for (i in 1:5)
    {
      PhysicoChemical_AAC = AAIndex193_Table[Sequence[i]]
      PhysicoChemical_AAC=PhysicoChemical_AAC[,1]
      names(PhysicoChemical_AAC)=paste(AAIndex193_Table[[1]],"_C",i,sep = "")
      PhysicoChemical_AAC
      Extracted_Features=c(Extracted_Features,PhysicoChemical_AAC)
      PhysicoChemical_AAC = AAIndex193_Table[Sequence[Seq_Length-5+i]]
      PhysicoChemical_AAC=PhysicoChemical_AAC[,1]
      names(PhysicoChemical_AAC)=paste(AAIndex193_Table[[1]],"_N",i,sep = "")
      Extracted_Features=c(Extracted_Features,PhysicoChemical_AAC)
    }

    AllFeatures=Extracted_Features
    Extracted_Features=Extracted_Features[-nzv]
    Extracted_Features=data.frame(Extracted_Features,stringsAsFactors = F)
    Extracted_Features[,1]=as.numeric(Extracted_Features[,1])
    Extracted_Features=t(Extracted_Features)
    Extracted_Features=predict(standardObj,Extracted_Features)
    if (Classifier==2)
    {
      Result=predict(modelFit2,Extracted_Features)
      Result=ifelse(Result==0,"Query peptide is NOT Anti-Angiogenic","Query peptide is Anti-Angiogenic")
    }else if (Classifier==3)
    {
      Result=predict(modelFit3,Extracted_Features,type="prob")
      Result=ifelse(Result[1,1]>Result[1,2],paste("Query peptide is NOT Anti-Angiogenic (Prediction Probability: ",round(Result[1,1],2),")",sep=""),paste("Query peptide is Anti-Angiogenic (Prediction Probability: ",round(Result[1,2],2),")",sep=""))
    }else
    {
      Result=predict(modelFit1,Extracted_Features,type="prob")
      Result=ifelse(Result[1,1]>Result[1,2],paste("Query peptide is NOT Anti-Angiogenic (Prediction Probability: ",round(Result[1,1],2),")",sep=""),paste("Query peptide is Anti-Angiogenic (Prediction Probability: ",round(Result[1,2],2),")",sep=""))
    }
    if (SF)
    {
      if (AF)
      {
        Result_List=list(Prediction_Result=Result,Selected_Features=Extracted_Features,All_Features=AllFeatures)
      }else{
        Result_List=list(Prediction_Result=Result,Selected_Features=Extracted_Features)
      }
    }else{
      if (AF)
      {
        Result_List=list(Prediction_Result=Result,All_Features=AllFeatures)
      }else{return(Result)}
    }
    return(Result_List)
  }
}
