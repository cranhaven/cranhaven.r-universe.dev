\name{mam15}
\alias{mam15}
\alias{mam15.relltest}
\alias{mam15.mt}
\alias{mam15.ass}
\alias{mam15.aux}
\alias{mam105}
\alias{mam105.relltest}
\alias{mam105.mt}
\alias{mam105.ass}
\alias{mam105.aux}
\alias{mam26}
\alias{mam26.mt}
\alias{mam26.ass}
\alias{mam26.aux}
\docType{data}
\title{Mammal Phylogenetic Analysis for 15 trees}
\description{
  Phylogenetic analysis of six mammal species for 15 trees and 105 trees.
}
\usage{
data(mam15)
}
\format{
  mam15.mt is a matrix of size 3414 * 15.  The (i,j) element is the
  site-wise log-likelihood value at site-i for tree-j for i=1,...,3414,
  and j=1,...,15. They are constrained trees with clade (cow, seal) being fixed.

  mam15.ass is a list of length 25 for association vectors. The
  components are t1, t2, ..., t15 for trees, and e1, e2, ..., e10 for
  edges.

  mam15.relltest is an object of class \code{"relltest"} of length 25.
  
  mam15.aux is a list of tree topologies (tpl), clade patterns (cld), taxa names(tax).
  
  mam105.mt, mam105.ass, mam105.relltst, mam105.aux are those for
  105 unconstrained trees.
  
  mam26.mt, mam26.ass, mam26.aux are those for 26 trees including
  the 15 constrained trees, 10 partially resolved trees corresponding to 
  the 10 internal edges, and the star topology.
}
\details{
  An example of phylogenetic analysis of six mammal species:
  Homo sapiens (human), Phoca vitulina (harbor seal), Bos taurus (cow),
  Oryctolagus cuniculus (rabbit), Mus musculus (mouse), Didelphis
  virginiana (opossum).  The data is stored in the file \file{mam15.aa},
  which contains amino acid sequences of length N=3414 for the six
  species obtained from mtDNA (see Note below).
  Here we fix (Phovi,Bosta) as a group of taxa.
  With this constraint, we consider 15 tree topologies of the six mammals as stored in the
  file \file{mam15.tpl}; 

\preformatted{  
((Homsa,(Phovi,Bosta)),Orycu,(Musmu,Didvi)); t1
(Homsa,Orycu,((Phovi,Bosta),(Musmu,Didvi))); t2
(Homsa,((Phovi,Bosta),Orycu),(Musmu,Didvi)); t3
(Homsa,(Orycu,Musmu),((Phovi,Bosta),Didvi)); t4
((Homsa,(Phovi,Bosta)),(Orycu,Musmu),Didvi); t5
(Homsa,((Phovi,Bosta),(Orycu,Musmu)),Didvi); t6
(Homsa,(((Phovi,Bosta),Orycu),Musmu),Didvi); t7
(((Homsa,(Phovi,Bosta)),Musmu),Orycu,Didvi); t8
(((Homsa,Musmu),(Phovi,Bosta)),Orycu,Didvi); t9
(Homsa,Orycu,(((Phovi,Bosta),Musmu),Didvi)); t10
(Homsa,(((Phovi,Bosta),Musmu),Orycu),Didvi); t11
((Homsa,((Phovi,Bosta),Musmu)),Orycu,Didvi); t12
(Homsa,Orycu,(((Phovi,Bosta),Didvi),Musmu)); t13
((Homsa,Musmu),Orycu,((Phovi,Bosta),Didvi)); t14
((Homsa,Musmu),((Phovi,Bosta),Orycu),Didvi); t15
}  

  The log-likelihood values are calculated using the PAML software (Ziheng 1997)
  for phylogenetic inference. The two files \file{mam15.aa} and
  \file{mam15.tpl} are fed into PAML to generate the file
  \file{mam15.lnf} of site-wise log-likelihood values.

  Using the CONSEL software (Shimodaira and Hasegawa 2001), we convert
  \file{mam15.lnf} and \file{mam15.tpl} to a format suitable for the \pkg{scaleboot}
  package. We do not use CONSEL for calculating AU p-values, but
  use it only for file conversion. We type

\preformatted{
seqmt --paml mam15.lnf
treeass --outgroup 6 mam15.tpl > mam15.log
}

  The first line above generates \file{mam15.mt}, which is a simple text
  file containing a matrix of site-wise log-likelihood values. The
  second line above generates \file{mam15.ass} and \file{mam15.log}, which
  contain information regarding which edges are included in a tree.  A
  part of \file{mam15.log} is as follows.

\preformatted{  
# leaves: 6
6
  1 Homsa
  2 Phovi
  3 Bosta
  4 Orycu
  5 Musmu
  6 Didvi

# base edges: 10
10 6
          
    123456
  1 +++---  ;
  2 ++++--  ;
  3 +--+--  ;
  4 -+++--  ;
  5 ---++-  ;
  6 +--++-  ;
  7 -++++-  ;
  8 +++-+-  ;
  9 +---+-  ;
 10 -++-+-  ;
}

  The above defines edges named e1,...e10 (base edges) as clusters for six
  mammal species. For example, e1 = +++--- = (Homsa, Phovi, Bosta).

  The converted files are read by the \pkg{scaleboot} package in \R:

\preformatted{  
mam15.mt <- read.mt("mam15.mt")
mam15.ass <- read.ass("mam15.ass")
}

  \code{mam15.mt} is a matrix of size 3414 * 6 for the site-wise
  log-likelihood values. For testing trees, we need only \code{mam15.mt}.
  \code{mam15.ass} is used for testing edges, and it is
  a list of length 25 for association vectors for t1,t2,...,t15, and
  e1,e2,...,e10.  For example, \code{mam15.ass$t1 = 1}, indicating tree
  "t1" is included in tree "t1", and \code{mam15.ass$e1 = c(1, 5, 8)},
  indicating edge "e1" is included in trees "t1", "t5", and "t8".

  Multiscale bootstrap resampling is performed by the function
  \code{relltest}. The simplest way to get AU p-values for trees is:

\preformatted{
mam15.trees <- relltest(mam15.mt) # resampling and fitting
summary(mam15.trees) # calculates AU p-values
}

  The \code{\link{relltest}} returns an object of class \code{"relltest"}.
  It calls the function \code{\link{scaleboot}} internally with
  the number of bootstrap replicates \code{nb=10000}, and takes about 20
  mins. Typically, \code{nb=10000} is large enough, but it would be safe
  to use larger value, say \code{nb=100000} as in the examples below.
  
  Note that the default value of scales in \code{\link{relltest}} has
  a much wider range than that of CONSEL. It is
  \code{sa=9^seq(-1,1,length=13)} for \code{\link{relltest}}, and
  is \code{sa=1/seq(from=0.5,to=1.4,by=0.1)} for CONSEL.

  The \code{mam15.relltest} object in \code{data(mam15)} is similar
  to \code{mam15.trees} above, but is also calculated for
  edges using \code{mam15.ass}. We can extract the result for trees by
\preformatted{
mam15.trees <- mam15.relltest[1:15]
}
  
  The results for trees stored in the \code{mam15.trees} object above are in
  the order specified in the columns of \code{mam15.mt}. To sort it by
  increasing order of the log-likelihood difference, we can type
\preformatted{
stat <- attr(mam15.trees,"stat")  # the log-likelihood differences
o <- order(stat) # sort it in increasing order
mam15.trees <- mam15.trees[o] # same as mam15.trees in Examples
}

  Results of the fitting are shown by using the \code{print} method.
\preformatted{
> mam15.trees

Test Statistic, and Shimodaira-Hasegawa test:
    stat  shtest       
t1  -2.66 94.51 (0.07) 
t3   2.66 80.25 (0.13) 
t2   7.40 57.85 (0.16) 
t5  17.57 17.30 (0.12) 
t6  18.93 14.32 (0.11) 
t7  20.11 11.49 (0.10) 
t4  20.60 10.98 (0.10) 
t15 22.22  7.34 (0.08) 
t8  25.38  3.31 (0.06) 
t14 26.32  3.29 (0.06) 
t13 28.86  1.71 (0.04) 
t9  31.64  0.61 (0.02) 
t11 31.75  0.57 (0.02) 
t10 34.74  0.20 (0.01) 
t12 36.25  0.12 (0.01) 

Multiscale Bootstrap Probabilities (percent):
    1  2  3  4  5  6  7  8  9  10 11 12 13 
t1  86 81 77 73 68 63 58 52 46 41 36 31 28 
t3  14 19 23 27 30 32 32 31 30 27 25 22 20 
t2   0  0  0  0  1  2  4  5  7  9 10 11 11 
t5   0  0  0  0  0  1  1  2  3  5  6  6  7 
t6   0  0  0  0  1  2  3  5  6  7  8  9  9 
t7   0  0  0  0  0  0  0  1  2  3  4  5  5 
t4   0  0  0  0  0  1  2  3  4  4  5  6  6 
t15  0  0  0  0  0  0  0  0  1  1  2  2  3 
t8   0  0  0  0  0  0  0  0  0  0  1  1  1 
t14  0  0  0  0  0  0  0  1  1  2  3  4  4 
t13  0  0  0  0  0  0  0  0  0  0  1  1  2 
t9   0  0  0  0  0  0  0  0  0  0  0  1  1 
t11  0  0  0  0  0  0  0  0  0  0  0  1  1 
t10  0  0  0  0  0  0  0  0  0  0  0  0  0 
t12  0  0  0  0  0  0  0  0  0  0  0  0  0 

Numbers of Bootstrap Replicates:
1     2     3     4     5     6     7     8     9     10    11    12    13    
1e+05 1e+05 1e+05 1e+05 1e+05 1e+05 1e+05 1e+05 1e+05 1e+05 1e+05 1e+05 1e+05 

Scales (Sigma Squared):
1      2      3      4      5      6      7 8     9     10 11    12    13    
0.1111 0.1603 0.2311 0.3333 0.4808 0.6933 1 1.442 2.080 3  4.327 6.241 9.008 

AIC values of Model Fitting:
    poly.1   poly.2  poly.3  sing.3  
t1  89483.40  964.33  964.75  966.33 
t3  75434.97 1750.22 1306.50 1752.22 
t2  29361.29  403.41   36.33   -6.21 
t5  23893.19  260.44   -0.22  -14.11 
t6  35791.26  330.50    4.31   -2.49 
t7  15221.10   93.59  -10.33  -12.04 
t4  29790.60  453.95    5.22   -7.57 
t15  6874.98   46.16  -10.48  -17.08 
t8   1747.13   -6.88  -12.39  -13.68 
t14 10905.94  131.48    2.65  -10.79 
t13  3411.26   27.66   -8.30  -15.14 
t9   1494.58   19.46  -13.78  -15.86 
t11   914.42  -19.65  -19.71  -19.61 
t10   259.68  -14.79  -17.27  -16.76 
t12   178.79  -19.19  -19.61  -19.30 
}

  The AU p-values are shown by the \code{summary} method.
\preformatted{
> summary(mam15.trees)

Corrected P-values (percent):
    raw          k.1          k.2          k.3          model  aic     
t1  57.58 (0.16) 56.16 (0.04) 74.55 (0.05) 74.55 (0.05) poly.2  964.33 
t3  31.86 (0.15) 30.26 (0.05) 46.41 (0.09) 45.33 (0.13) poly.3 1306.50 
t2   3.68 (0.06)  3.68 (0.03) 12.97 (0.20) 16.12 (0.45) sing.3   -6.21 
t5   1.34 (0.04)  1.33 (0.02)  7.92 (0.25) 10.56 (0.56) sing.3  -14.11 
t6   3.18 (0.06)  3.15 (0.02) 13.15 (0.21) 15.86 (0.44) sing.3   -2.49 
t7   0.49 (0.02)  0.52 (0.01)  3.66 (0.21)  4.75 (0.42) sing.3  -12.04 
t4   1.55 (0.04)  1.53 (0.02) 10.54 (0.27) 14.84 (0.66) sing.3   -7.57 
t15  0.08 (0.01)  0.07 (0.00)  1.11 (0.19)  1.85 (0.48) sing.3  -17.08 
t8   0.00 (0.00)  0.00 (0.00)  0.04 (0.03)  0.07 (0.07) sing.3  -13.68 
t14  0.22 (0.01)  0.23 (0.01)  2.76 (0.26)  4.59 (0.71) sing.3  -10.79 
t13  0.02 (0.00)  0.01 (0.00)  0.50 (0.20)  1.30 (0.83) sing.3  -15.14 
t9   0.00 (0.00)  0.00 (0.00)  0.23 (0.05)  1.41 (0.29) sing.3  -15.86 
t11  0.00 (0.00)  0.00 (0.00)  0.00 (0.00)  0.00 (0.00) poly.3  -19.71 
t10  0.00 (0.00)  0.00 (0.00)  0.00 (0.00)  0.00 (0.00) poly.3  -17.27 
t12  0.00 (0.00)  0.00 (0.00)  0.00 (0.00)  0.00 (0.00) poly.3  -19.61 
}

  The p-values for 15 trees are shown above.  "raw" is
  the ordinary bootstrap probability, "k.1" is equivalent to "raw" but
  calculated from the multiscale bootstrap, "k.2" is equivalent to the
  third-order AU p-value of CONSEL, and finally "k.3" is an improved
  version of AU p-value.

  The details for each tree are shown by extracting the corresponding element. For
  example, details for the seventh largest tree in the
  log-likelihood value ("t4") is obtained by
\preformatted{
> mam15.trees[[7]] # same as mam15.trees$t4

Multiscale Bootstrap Probabilities (percent):
1    2    3    4    5    6    7    8    9    10   11   12   13   
0.00 0.00 0.01 0.08 0.27 0.80 1.55 2.55 3.58 4.42 5.22 6.00 6.38 

Numbers of Bootstrap Replicates:
1     2     3     4     5     6     7     8     9     10    11    12    13    
1e+05 1e+05 1e+05 1e+05 1e+05 1e+05 1e+05 1e+05 1e+05 1e+05 1e+05 1e+05 1e+05 

Scales (Sigma Squared):
1      2      3      4      5      6      7 8     9     10 11    12    13    
0.1111 0.1603 0.2311 0.3333 0.4808 0.6933 1 1.442 2.080 3  4.327 6.241 9.008 

Coefficients:
       beta0           beta1           beta2            
poly.1 2.8388 (0.0048)                                  
poly.2 1.8556 (0.0061) 0.3259 (0.0019)                  
poly.3 1.7157 (0.0085) 0.4508 (0.0061) -0.0152 (0.0007) 
sing.3 1.6178 (0.0153) 0.5435 (0.0143)  0.3261 (0.0201) 

Model Fitting:
       rss      df pfit   aic      
poly.1 29814.60 12 0.0000 29790.60 
poly.2   475.95 11 0.0000   453.95 
poly.3    25.22 10 0.0050     5.22 
sing.3    12.43 10 0.2571    -7.57 

Best Model:  sing.3 
> summary(mam15.trees[[7]])

Raw Bootstrap Probability:  1.55 (0.04) 

Corrected P-values (percent):
       k.1         k.2          k.3          aic      
poly.1 0.23 (0.00)  0.23 (0.00)  0.23 (0.00) 29790.60 
poly.2 1.46 (0.02)  6.30 (0.09)  6.30 (0.09)   453.95 
poly.3 1.57 (0.02)  9.50 (0.21) 10.57 (0.27)     5.22 
sing.3 1.53 (0.02) 10.54 (0.27) 14.84 (0.66)    -7.57 

Best Model:  sing.3 

> plot(mam15.trees[[7]],legend="topleft")
}  

  The plot diagnostics found in the bottom line are especially useful
  for confirming which model is fitting best.

  See other examples below.
}
\note{
  Dataset files for phylogenetic inference are found at
  \url{http://github.com/shimo-lab/scaleboot}.
  Look at the subdirectory \file{dataset/mam15-files}.
  This dataset
  was originally used in Shimodaira and Hasegawa (1999).
  }
\source{
  H. Shimodaira and M. Hasegawa (1999). Multiple comparisons of
  log-likelihoods with applications to phylogenetic inference, \emph{Molecular
  Biology and Evolution}, 16, 1114-1116.
}
\references{
  Yang, Z. (1997). PAML: a program package for phylogenetic analysis by
  maximum likelihood, \emph{Computer Applications in BioSciences},
  13:555-556 (software is available from
  \url{http://abacus.gene.ucl.ac.uk/software/paml.html}).

  Shimodaira, H. and Hasegawa, M. (2001). CONSEL: for assessing the
  confidence of phylogenetic tree selection, \emph{Bioinformatics}, 17,
  1246-1247 (software is available from
  \url{http://stat.sys.i.kyoto-u.ac.jp/prog/consel/}).
}
\seealso{\code{\link{mam105}}, \code{\link{relltest}}, 
  \code{\link{summary.scalebootv}},
  \code{\link{read.mt}}, \code{\link{read.ass}}.}
\examples{
data(mam15)

## show the results for trees and edges
mam15.relltest # print stat, shtest, bootstrap probabilities, and AIC
summary(mam15.relltest) # print AU p-values

\dontrun{
## simpler script to create mam15.trees
mam15.mt <- read.mt("mam15.mt")
mam15.ass <- read.ass("mam15.ass")
mam15.trees <- relltest(mam15.mt,nb=100000)
}

\dontrun{
## script to create mam15.relltest
mam15.mt <- read.mt("mam15.mt")
mam15.ass <- read.ass("mam15.ass")
mam15.relltest <- relltest(mam15.mt,nb=100000,ass=mam15.ass)
}

\dontrun{
## Parallel version of the above script (but different in random seed)
## It took 13 mins (40 cpu's of Athlon MP 2000+)
mam15.mt <- read.mt("mam15.mt")
mam15.ass <- read.ass("mam15.ass")
library(parallel)
cl <- makeCluster(40)
mam15.relltest <- relltest(mam15.mt,nb=100000,ass=mam15.ass,cluster=cl)
}


}
\keyword{datasets}
