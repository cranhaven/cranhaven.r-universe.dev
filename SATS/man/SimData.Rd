% --- Source file: SimData.Rd ---
\name{SimData}
\alias{SimData}

\title{
    Data for examples
}
\description{ 
  Simulated data as an example
}
\details{
  This file consists of the list \code{SimData} with the following objects: 
  \itemize{
    \item{V} : Simulated mutation catalog matrix of size 96 by 10027
	            generated from the Poisson distribution with the mean 
				corresponding to each element of L*(WH), where * is 
				elementwise multiplication, L is the panel size matrix 
				below, W is the profile matrix of the tumor mutation 
				burden (TrueW_TMB below), and H is the signature activity 
				matrix (TrueH below).
	
    \item{L} : Panel size matrix of size 96 by 10027.
    \item{TrueH} : Simulated signature activity matrix of size 6 by 10027 used to generate V.
    \item{TrueW_TMB} : Tumor mutation burden based signatures size 96 by 6
                        for 6 SBS signatures (SBS1, SBS2/13, SBS4, SBS5, SBS40, SBS89) 
						used to generate V.
    \item{SingleTumorEx} : A single simulated tumor (column \code{singleV}) 
                    and its sequencing context (column \code{singleL}).
    \item{PanelEx}: An example sequencing panel information with Chromosome, Start_Position,
                    End_Position, SEQ_ASSAY_ID.
    \item{PatientInfo}: An example patient data containing patients ID associated with
                        SEQ_ASSAY_ID.
  }
 }
%\examples{}
\keyword{data}
