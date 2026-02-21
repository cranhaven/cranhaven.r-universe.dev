
\name{GenerateLMatrix}
\alias{GenerateLMatrix}
\title{Generate an L Matrix}
\description{
   Generation of the L matrix.
}
\usage{ GenerateLMatrix(Panel_context, Patient_Info)
}
\arguments{
    \item{Panel_context}{A data frame returned from \code{\link{GeneratePanelSize}}.}
    \item{Patient_Info}{A data frame with columns 'PATIENT_ID' and 'SEQ_ASSAY_ID'.}
}
\details{The GenerateLMatrix() function relies on the 'Patient_Info' data frame to link 
each patient to their specific sequencing assay, identified by 'SEQ_ASSAY_ID'.
This is the same core information required by the genomic_information
argument in the GeneratePanelSize() function. The values in the 'SEQ_ASSAY_ID'
column will be used as the column names for the resulting L matrix, ensuring each 
column corresponds to a unique sample.

It is important to understand the distinction between the two functions. 
GeneratePanelSize() is a simpler utility that only calculates the total size 
(in base pairs) of each panel type and does not require patient-level
information. However, to generate the correctly dimensioned L matrix with
the proper trinucleotide contexts for each specific sample, you must call
GenerateLMatrix().
}

\value{ 
 A data frame (L matrix) of 96 by N, where N is the number of tumors.
}
%\references{}
\author{
Donghyuk Lee <dhyuklee@pusan.ac.kr> and Bin Zhu <bin.zhu@nih.gov>
}
\examples{
data(SimData, package="SATS")

Panel_context <- GeneratePanelSize(genomic_information=SimData$PanelEx, Class="SBS", 
                         SBS_order="COSMIC")
L_mat <- GenerateLMatrix(Panel_context, SimData$PatientInfo) 
# For more detailed usage, please refer to README and the user manual
# in https://github.com/binzhulab/SATS/tree/main.
}
%\keyword{ mutational signatures } 
\seealso{ \code{\link{GeneratePanelSize}} }

