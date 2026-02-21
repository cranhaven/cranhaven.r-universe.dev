
\name{GeneratePanelSize}
\alias{GeneratePanelSize}
\title{Generate Panel Size Matrix}
\description{
   Generation of the panel size matrix given the panel information.
}
\usage{ GeneratePanelSize(genomic_information, Class = c("SBS", "DBS"), 
          SBS_order = c("COSMIC", "signeR"), ref.genome="hg19")
}
\arguments{
    \item{genomic_information}{Data frame of panel information (see details).}
    \item{Class}{A character string specifying the mutation class. Must be either
                 "SBS" (for single base substitutions) or "DBS" (for double base
                 substitutions). If "DBS" is selected, the \code{SBS_order} 
                 argument is ignored. }
    \item{SBS_order}{Mutation type order either one of "COSMIC" or "signeR" (see details).}
    \item{ref.genome}{The reference genome "hg19" or "hg38".}
}
\details{ 
The first argument 'genomic_information' should contain columns 'Chromosome', 
'Start_Position', 'End_Position', 'SEQ_ASSAY_ID'. 
The column 'Chromosome' contains chromosome number where 'Start_Position' and
'End_Position' columns are start and end positions of the targeted panel. 
The last column 'SEQ_ASSAY_ID' distinguishes different panels consisting of the result. 
Please note that the column names of 'genomic_information' identical to 'Chromosome', 
'Start_Position', 'End_Position', 'SEQ_ASSAY_ID'.
The second argument specifies mutation type order as either one of "COSMIC" or 
"signeR" where "COSMIC" corresponds to the order from the COSMIC database v3.2 and 
"signeR" corresponds to the order from the signeR package.
\bold{Note}: The result of 'GeneratePanelSize()' may NOT be an 'L' matrix. 
The GenerateLMatrix() function can be used to construct the L matrix.
The resulting augmented matrix can be used as the opportunity matrix for 'signeR()' function, 
'L' matrix for 'EstimateSigActivity()' and 'CalculateSignatureBurdens()' functions. 
Therefore, it is important the mutation type order (row names) should be the same as 
input matrix (Mutation type matrix 'V'). 
We highly recommend to confirm that both 'V' and 'L' matrices have the same mutation type 
order corresponding to one of COSMIC database v3.2 or signeR package 
(both have the same order but have different expression) to conduct the consistent analysis.

}
\value{ 
 A data frame of 96 by 'S' (the number of panels, 'SEQ_ASSAY_ID') where entries 
 denote the number of trinucleotides per million base pairs.
}
%\references{}
\author{
Donghyuk Lee <dhyuklee@pusan.ac.kr> and Bin Zhu <bin.zhu@nih.gov>
}
\examples{
  data(SimData, package="SATS")

  ## For the single base substitution (SBS) case:
  Panel_context1 <- GeneratePanelSize(genomic_information = SimData$PanelEx, Class = "SBS",  
                                      SBS_order = "COSMIC", ref.genome="hg19")
  L_mat1 <- GenerateLMatrix(Panel_context1, SimData$PatientInfo)
  Panel_context2 <- GeneratePanelSize(genomic_information = SimData$PanelEx, Class = "SBS",  
                                      SBS_order = "signeR", ref.genome="hg19")
  L_mat2 <- GenerateLMatrix(Panel_context2, SimData$PatientInfo)

  ## For the double base substitution (DBS) case:
  Panel_context_DBS <- GeneratePanelSize(genomic_information = SimData$PanelEx, Class = "DBS", 
                                         ref.genome = "hg19")
  L_mat_DBS  <- GenerateLMatrix(Panel_context_DBS, SimData$PatientInfo)

  # For more detailed usage, please refer to README and the user manual
  # in https://github.com/binzhulab/SATS/tree/main 
}
%\keyword{ mutational signatures } 
%\seealso{ \code{\link{EstimateSigActivity}} }
