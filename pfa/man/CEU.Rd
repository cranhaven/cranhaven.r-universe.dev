 \name{CEU}
     \docType{data}
     \alias{CEU}
     \title{CCT8 genome-wise association data on CEU population}
     \description{
       This data set uses 564 SNP genotype data and CCT8 gene expression data for 60 individuals from CEU population, where CEU stands for "Utah residents with ancestry from northern and western Europe". 
     }
     \usage{CEU}
     \format{
       A list of two objects \code{x} and \code{y}.
        \itemize{
         \item \code{x}: A matrix of dimension 60*1128. Each row corresponds to one individual. In row j, every two neighboring columns correspond to one SNP: (0,0) for "no polymorphism", (1,0) for "one nucleotide has polymorphism" and (0,1) for "both nucleotides have polymorphisms".
         \item \code{y}: A vector of dimension 60. Each element corresponds to the CCT8 gene expression level on one individual.
      }
     }
     \source{http://pngu/mgh.harvard.edu/purcell/plink/res.shtml
     	
     	ftp:/ftp.sanger.ac.uk/pub/genevar}
     \references{
       Fan, Han and Gu (2012) "Estimating False Discovery Proportion Under Arbitrary Covariance Dependence" (with discussion) JASA.
     }
     \keyword{datasets}