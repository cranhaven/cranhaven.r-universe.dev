
GeneratePanelSize <- function(genomic_information, Class = c("SBS", "DBS"), SBS_order = c("COSMIC", "signeR"), 
                              ref.genome="hg19"){

  # Check input arguments
  check_genomic_info(genomic_information)
  check_ref.genome(ref.genome)
  check_Class(Class)

  if (Class == "SBS") {
    check_Types(SBS_order)
    ret <- GeneratePanelSize_SBS(genomic_information, Types=SBS_order, ref.genome=ref.genome)
  } else {
    ret <- GeneratePanelSize_DBS(genomic_information)
  }
  ret
}

GeneratePanelSize_DBS <- function(genomic_information, ref.genome="hg19") {

    SEQ_ASSAY_ID <- NULL # To remove warning when compiling

    Seq_assay_GRanges <- GRanges(seqnames=paste0("chr",genomic_information$Chromosome),
                                 IRanges(start = genomic_information$Start_Position, end=genomic_information$End_Position), 
                                 strand = "+")
    
    # get sequences
    if (ref.genome == "hg19") {
      myseq = getSeq(BSgenome.Hsapiens.UCSC.hg19::Hsapiens,Seq_assay_GRanges) 
    } else {
      myseq = getSeq(BSgenome.Hsapiens.UCSC.hg38::Hsapiens,Seq_assay_GRanges)
    }
    Seq_assay_16 = dinucleotideFrequency(myseq) # key step: calculate dinucleotide frequency
    
    #included in DBS
    dinucleotideIncluded = c("AC","AT","CC","CG","CT","GC","TA","TC","TG","TT") # only 10 starting dinucleotide
    #all possible 16
    dinucleotide_16 = colnames(Seq_assay_16)
    idx = dinucleotide_16 %in% dinucleotideIncluded
    dinucleotide_10 = dinucleotide_16[idx]
    #inclcuded 10
    assay_DBS = Seq_assay_16[,dinucleotide_10]
    #excluded 6
    assay_DBS_6 = Seq_assay_16[,dinucleotide_16[!idx]]
    #rev names
    colnames(assay_DBS_6) = rev_dinucleotide(colnames(assay_DBS_6))
    #sort alphabetically
    assay_DBS_6 = assay_DBS_6[,sort(colnames(assay_DBS_6))]
    
    dinucleotide_10_set = setNames(1:10,dinucleotide_10)
    dinucleotide_10_set_idx = dinucleotide_10_set[colnames(assay_DBS_6)]
    #sum
    assay_DBS[,dinucleotide_10_set_idx] = assay_DBS[,dinucleotide_10_set_idx]+assay_DBS_6
    
    genomic_information_assay_DBS=bind_cols(genomic_information, as.data.frame(assay_DBS))
    
    
    #sum over genes for the whole assay
    assay_DBS_10 = genomic_information_assay_DBS %>% 
      group_by(SEQ_ASSAY_ID) %>% 
      summarise_at(colnames(assay_DBS),sum)
    
    assay_DBS_re <- data.frame(t(assay_DBS_10[, dinucleotideIncluded, drop = FALSE]))/10^6
    colnames(assay_DBS_re) <- assay_DBS_10$SEQ_ASSAY_ID
    
    return(assay_DBS_re)
}


rev_dinucleotide <- function(nt.seq){
    rev.seq = NULL
    for(i in 1:length(nt.seq)){
      x <- DNAString(nt.seq[i])
      rev.seq = c(rev.seq, as.character(reverseComplement(x)))
    }
    return(rev.seq)
}
  

## The following function is to create "L" matrix
## Please use the column names of the argument "genomic_information" identical to Chromosome, Start_Position, End_Position, SEQ_ASSAY_ID as in the above example
## Chromosome: chromsome number
## Start_Position: start position of targeted panel
## End_Position: end position of targeted panel
## SEQ_ASSAY_ID: distinguish different panels
## The unit of the returned L matrix is the number of trinucleotides per million base pairs
GeneratePanelSize_SBS <- function(genomic_information, Types = c("COSMIC", "signeR"), ref.genome="hg19"){
  
  SEQ_ASSAY_ID <- NULL

  # Mutation type categories, there are 96 possible single base substitutions.
  # Alexandrov v3.2 mutation type order. .
  COSMIC <- c("A[C>A]A", "A[C>A]C", "A[C>A]G", "A[C>A]T", "C[C>A]A", 
              "C[C>A]C", "C[C>A]G", "C[C>A]T", "G[C>A]A", "G[C>A]C", 
              "G[C>A]G", "G[C>A]T", "T[C>A]A", "T[C>A]C", "T[C>A]G", 
              "T[C>A]T", "A[C>G]A", "A[C>G]C", "A[C>G]G", "A[C>G]T", 
              "C[C>G]A", "C[C>G]C", "C[C>G]G", "C[C>G]T", "G[C>G]A", 
              "G[C>G]C", "G[C>G]G", "G[C>G]T", "T[C>G]A", "T[C>G]C", 
              "T[C>G]G", "T[C>G]T", "A[C>T]A", "A[C>T]C", "A[C>T]G", 
              "A[C>T]T", "C[C>T]A", "C[C>T]C", "C[C>T]G", "C[C>T]T", 
              "G[C>T]A", "G[C>T]C", "G[C>T]G", "G[C>T]T", "T[C>T]A", 
              "T[C>T]C", "T[C>T]G", "T[C>T]T", "A[T>A]A", "A[T>A]C", 
              "A[T>A]G", "A[T>A]T", "C[T>A]A", "C[T>A]C", "C[T>A]G", 
              "C[T>A]T", "G[T>A]A", "G[T>A]C", "G[T>A]G", "G[T>A]T", 
              "T[T>A]A", "T[T>A]C", "T[T>A]G", "T[T>A]T", "A[T>C]A", 
              "A[T>C]C", "A[T>C]G", "A[T>C]T", "C[T>C]A", "C[T>C]C", 
              "C[T>C]G", "C[T>C]T", "G[T>C]A", "G[T>C]C", "G[T>C]G", 
              "G[T>C]T", "T[T>C]A", "T[T>C]C", "T[T>C]G", "T[T>C]T", 
              "A[T>G]A", "A[T>G]C", "A[T>G]G", "A[T>G]T", "C[T>G]A", 
              "C[T>G]C", "C[T>G]G", "C[T>G]T", "G[T>G]A", "G[T>G]C", 
              "G[T>G]G", "G[T>G]T", "T[T>G]A", "T[T>G]C", "T[T>G]G", 
              "T[T>G]T")

  signeR <- c("C>A:ACA", "C>A:ACC", "C>A:ACG", "C>A:ACT", "C>A:CCA", 
              "C>A:CCC", "C>A:CCG", "C>A:CCT", "C>A:GCA", "C>A:GCC", 
              "C>A:GCG", "C>A:GCT", "C>A:TCA", "C>A:TCC", "C>A:TCG", 
              "C>A:TCT", "C>G:ACA", "C>G:ACC", "C>G:ACG", "C>G:ACT", 
              "C>G:CCA", "C>G:CCC", "C>G:CCG", "C>G:CCT", "C>G:GCA", 
              "C>G:GCC", "C>G:GCG", "C>G:GCT", "C>G:TCA", "C>G:TCC", 
              "C>G:TCG", "C>G:TCT", "C>T:ACA", "C>T:ACC", "C>T:ACG", 
              "C>T:ACT", "C>T:CCA", "C>T:CCC", "C>T:CCG", "C>T:CCT", 
              "C>T:GCA", "C>T:GCC", "C>T:GCG", "C>T:GCT", "C>T:TCA", 
              "C>T:TCC", "C>T:TCG", "C>T:TCT", "T>A:ATA", "T>A:ATC", 
              "T>A:ATG", "T>A:ATT", "T>A:CTA", "T>A:CTC", "T>A:CTG", 
              "T>A:CTT", "T>A:GTA", "T>A:GTC", "T>A:GTG", "T>A:GTT", 
              "T>A:TTA", "T>A:TTC", "T>A:TTG", "T>A:TTT", "T>C:ATA", 
              "T>C:ATC", "T>C:ATG", "T>C:ATT", "T>C:CTA", "T>C:CTC", 
              "T>C:CTG", "T>C:CTT", "T>C:GTA", "T>C:GTC", "T>C:GTG", 
              "T>C:GTT", "T>C:TTA", "T>C:TTC", "T>C:TTG", "T>C:TTT", 
              "T>G:ATA", "T>G:ATC", "T>G:ATG", "T>G:ATT", "T>G:CTA", 
              "T>G:CTC", "T>G:CTG", "T>G:CTT", "T>G:GTA", "T>G:GTC", 
              "T>G:GTG", "T>G:GTT", "T>G:TTA", "T>G:TTC", "T>G:TTG", 
              "T>G:TTT")
  
  # To handle L matrix
  # define trinucleotide seq from 5' to 3'. They are the same order as the function trinucleotideFrequency()
  nt = c("A","C","G","T")
  tri_nt = paste(rep(nt,each=16,times=1),rep(nt,each=4,times=4),rep(nt,each=1,times=16), sep="")
  tri_nt_idx = setNames(1:64, tri_nt)
  
  #index for where is A, C, G, T in the middle of trinucleotide
  A_idx = c(1:4,17:20,33:36,49:52)
  C_idx = A_idx + 4
  G_idx = A_idx + 4*2
  T_idx = A_idx + 4*3
  
  tri_nt_CT = tri_nt[c(C_idx,T_idx)]
  tri_nt_AG = tri_nt[c(A_idx,G_idx)]
  
  tri_nt_CT_idx = setNames(1:32,tri_nt_CT)
  tri_nt_AG_idx = setNames(1:32,tri_nt_AG)
  
  #The complementary trinucleotide. Note: flip of 1st and 3rd position be consistent with from 5' to 3'
  tri_nt_comp = paste(rep(rev(nt),each=1,times=16),rep(rev(nt),each=4,times=4),rep(rev(nt),each=16,times=1), sep="")
  
  #This index could help extract columns A or G base mutations, and combine with C or T base mutations
  AGtoCT_idx = tri_nt_CT_idx[tri_nt_comp[c(A_idx,G_idx)]]
  
  
  Seq_assay_GRanges <- GRanges(seqnames = paste0("chr",genomic_information$Chromosome),
                               IRanges(start = genomic_information$Start_Position-1, 
                               end=genomic_information$End_Position+1), strand = "+")
  
  Seq_assay_n = length(Seq_assay_GRanges)
  
  Seq_assay_64 = matrix(0,Seq_assay_n,64)
  colnames(Seq_assay_64) = tri_nt
  
  # Get sequences
  if (ref.genome == "hg19") {
    myseq = getSeq(BSgenome.Hsapiens.UCSC.hg19::Hsapiens,Seq_assay_GRanges) 
  } else {
    myseq = getSeq(BSgenome.Hsapiens.UCSC.hg38::Hsapiens,Seq_assay_GRanges)
  }
  Seq_assay_64 = trinucleotideFrequency(myseq) # key step: calculate trinucleotide frequency
  
  Seq_assay_64_CT = Seq_assay_64[,c(C_idx,T_idx)]
  Seq_assay_64_AG = Seq_assay_64[,c(A_idx,G_idx)]
  
  Seq_assay_CT_32 = Seq_assay_64_CT+Seq_assay_64_AG[,AGtoCT_idx]
  
  genomic_information_CT_32 = bind_cols(genomic_information, as.data.frame(Seq_assay_CT_32))
  
  genomic_information_CT_32 = as_tibble(genomic_information_CT_32)
  
  #sum over genes for the whole assay
  assay_CT_32 = genomic_information_CT_32 %>% 
    group_by(SEQ_ASSAY_ID) %>% 
    summarise_at(tri_nt_CT,sum)
  assay_CT_32$assaySize = rowSums(assay_CT_32[,-1])
  
  if(Types == "COSMIC"){
    assay_CT_32_2 <- data.frame(t(assay_CT_32[, paste0(substr(COSMIC, 1, 1), substr(COSMIC, 3, 3), 
                                  substr(COSMIC, 7, 7)), drop = FALSE]))/10^6
    rownames(assay_CT_32_2) <- COSMIC
  } else if(Types == "signeR"){
    assay_CT_32_2 <- data.frame(t(assay_CT_32[, substr(signeR, 5, 7), drop = FALSE]))/10^6
    rownames(assay_CT_32_2) <- signeR  
  } 
  
  colnames(assay_CT_32_2) <- assay_CT_32$SEQ_ASSAY_ID
  
  return(assay_CT_32_2)
  
}

##################################################################################################################################################################
## The following L_matrix_generation() function can be used to generate "L" matrix.
## Please use two inputs, the first argument is the panel context matrix generated by Panel_Context_generation() function, 
## while the second argument is the Patient information with sequence assay ID (Please refer to the example included in the folder)
## For the second argument, please use the column names as below:
## PATIENT_ID: patient ID corresponds to SEQ_ASSAY_ID
## SEQ_ASSAY_ID: SEQ_ASSAY_ID contained in Panel_context
L_matrix_generation <- function(Panel_context, Patient_Info){
  
  idx <- Patient_Info$SEQ_ASSAY_ID %in% colnames(Panel_context)
  L <- Panel_context[, Patient_Info$SEQ_ASSAY_ID[idx]]
  colnames(L) <- Patient_Info$PATIENT_ID[idx]
  #if(sum(idx) != nrow(Patient_Info)){
  #  warning(sprintf("There are patients for whom the panel context has not been provided in Patient_Info. 
  #  Patient_Info contains %d sequence assays, but Panel_context has only %d sequence assays", 
  # length(unique(Patient_Info$SEQ_ASSAY_ID)), ncol(Panel_context)))
  #} 
  
  return(L)
}

GenerateLMatrix <- function(Panel_context, Patient_Info) {

  check_Panel_context(Panel_context)
  check_Patient_Info(Patient_Info)

  ret <- L_matrix_generation(Panel_context, Patient_Info)
  ret
}



