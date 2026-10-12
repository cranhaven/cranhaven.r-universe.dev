# functions2.R 
# Part of the GMCPIC package.
#
# Copyright (C) 2013-2014  Samuel Soubeyrand <samuel.soubeyrand@inra.fr>
#                          Vincent Garreta
#                          Jean-Francois Rey <jean-francois.rey@inra.fr>
#                          INRA - BioSP Site Agroparc - 84914 Avignon Cedex 9 - France
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
#


#' @encoding UTF-8
#' @title Package implementing the Generalized Monte Carlo plug-in test with calibration (GMCPIC test)
#' @description Computer intensive procedure to test differences between pathogen compositions with small samples and sparse data
#' @aliases GMCPIC-package GMCPIC
#' @author Samuel Soubeyrand \email{Samuel.Soubeyrand@@avignon.inra.fr}
#' @author Vincent Garreta
#' @author Jean-Francois Rey \email{jean-francois.rey@@paca.inra.fr}
#' @docType package
#' @name GMCPIC-package
#' @details \tabular{ll}{
#'          Package: \tab GMCPIC\cr
#'          Type: \tab Package\cr
#'          Version: \tab 1.0\cr
#'          Date: \tab 2014-03-25\cr
#'          License: \tab GPL (>=2)\cr
#'          }
#' The GMCPIC package implements the Generalized Monte Carlo plug-in test with calibration proposed in Soubeyrand et al. (2014). This procedure can be used to test the equality of the vectors of probabilities of two multinomial draws. This test was developed to test the similarity of two pathogen compositions based on small samples and sparse data. Data sets analyzed in Soubeyrand et al. (2014) are provided in this package.
#'          
#' @references Soubeyrand S, Garreta V, Monteil C, Suffert F, Goyeau H, Berder J, Berge O, Moinard J, Fournier E, Tharreau D, Morris C, Sache I (2014). Testing differences between pathogen compositions with small samples and sparse data. Research Report, INRA, BioSP, Avignon, France.
#' @keywords package
#' @seealso \code{\link{gmcpic.test}}, \code{\link[stats:chisq.test]{stats:chisq.test}}
#' @examples
#' ## Load Pathogen Compositions of M. oryzae collected in Madagascar
#' data(PathogenCompositionMoryzaeMadagascar)
#' x=t(PathogenCompositionMoryzaeMadagascar)
#'
#' ## Apply the GMCPIC test (use B=10^3, M=10^4 to get a robust result)
#' testMada=gmcpic.test(x, B=10^1, M=10^1, weights=seq(0.5,0.99,by=0.01),threshold=0.05)
#' testMada
#'
#' ## Apply the Chi-squared test
#' chisq.test(x, simulate.p.value = TRUE, B = 10000)
NULL




############################################################

.test_p_value = function(p, N1, N2, B){
## Function providing the p-value of the test based on the
##   multinomial-density statistic
## p: vector of probabilities
## N1: vector of observed frequencies for population 1
## N2: vector of observed frequencies for population 2
## B: number of Monte Carlo simulations
## OUTPUT: p-value of the test
    
  if ((length(p)!=length(N1))|(length(p)!=length(N2))|(length(p)>1000))
    stop("[test_p_value] : Vectors p, N1 and N2 not consistent or too large")
  res = 0
  tempo = .C("C_test_p_value", d=as.integer(length(N1)), p=as.double(p),
    N1=as.integer(N1), N2=as.integer(N2), B=as.integer(B),
    resres=as.double(res),PACKAGE="StrainRanking")
  
  return(tempo$resres)
}


##########################################################

.calibration = function(N1, N2, seq.weights, B, M, threshold){
## Function to calibrate the distribution of test p-values
## N1: vector of observed frequencies for population 1
## N2: vector of observed frequencies for population 2
## seq.weights: vector of weights in [0,1] that are tried 
## B: number of Monte Carlo simulations
## M: number of repetitions for the calibration
## threshold: targeted risk level of the test
## OUTPUT: vector of actual risk levels for all the weights that are tried

  ## Repetitions are simulated under the maximum likelihood estimate of the
  ## vector of probabilities p based on all data (i.e. N1 and N2)
  weights0 = sum(N1)/sum(N1+N2)
  phat0 = (N1/sum(N1))*weights0 + (N2/sum(N2))*(1-weights0)
  
  out = NULL
  
  for(i in 1:length(seq.weights)){
    
    weights = seq.weights[i]

    print(paste("Current value of the tested weight:",weights))
    
    tempo = NULL
    
    ## M repetitions for a given value of weights
    for(m in 1:M){
      N1m = as.numeric(rmultinom(1, sum(N1), phat0))
      N2m = as.numeric(rmultinom(1, sum(N2), phat0))
      
      ## Estimation of p based on simulated data
      phat = (N1m/sum(N1m))*weights + (N2m/sum(N2m))*(1-weights)
      
      tempo = c(tempo, .test_p_value(phat, N1m, N2m, B))
    }
    out = c(out, mean(tempo<=threshold))
  }
  
  return(out)
}


######################################################
#' @title Function implementing the Generalized Monte Carlo plug-in test with calibration (GMCPIC test)
#' @description The GMCPIC test is a procedure to test the equality of the vectors of probabilities of two multinomial draws. The test statistics that is used is the multinomial-density statistic.
#' @author Samuel Soubeyrand <Samuel Soubeyrand <Samuel.Soubeyrand@@avignon.inra.fr>
#' @author Vincent Garreta
#' @aliases gmcpic.test
#' @param x [2-column matrix] Column 1 (resp. 2) contains the vector of observed frequencies in population 1 (resp. 2).
#' @param B [Integer] Number of Monte Carlo simulations.
#' @param M [Integer] Number of repetitions for the calibration.
#' @param weights [Numeric] Vector of weights in [0,1] that are tried for the calibration.
#' @param threshold [Numeric] Targeted risk level of the test; value in [0,1].
#' @return list with INPUT arguments (\code{x}, \code{B}, \code{M}, \code{weights} and \code{threshold}) and the following items:
#'  \item{calibrated.weight}{Weight selected by the calibration procedure.}
#'  \item{p.value}{Test p-value.}
#'  \item{reject.null.hypothesis}{Logical indicating whether the null hypothesis is rejected or not at the risk level specified by \code{threshold}.}
#'  \item{Message}{Details about the p-value interpretation.}
#' @details The GMCPIC test was developed to test the similarity of two pathogen compositions based on small samples and sparse data.
#' @references Soubeyrand S, Garreta V, Monteil C, Suffert F, Goyeau H, Berder J, Berge O, Moinard J, Fournier E, Tharreau D, Morris C, Sache I (2014). Testing differences between pathogen compositions with small samples and sparse data. Research Report, INRA, BioSP, Avignon, France.
#' @examples
#' ## Load Pathogen Compositions of M. oryzae collected in Madagascar
#' data(PathogenCompositionMoryzaeMadagascar)
#' x=t(PathogenCompositionMoryzaeMadagascar)
#'
#' ## Apply the GMCPIC test (use B=10^3, M=10^4 to get a robust result)
#' \donttest{testMada=gmcpic.test(x, B=10^2, M=10^3, weights=seq(0.5,0.99,by=0.01),threshold=0.05)}
#' \donttest{testMada}
#'
#' ## Apply the Chi-squared test
#' chisq.test(x, simulate.p.value = TRUE, B = 10000)
#' @keywords misc
#' @export
gmcpic.test=function(x,B,M,weights,threshold){
  ## GMCPIC test based on the multinomial-density statistic
  ## x: matrix with 2 columns; column 1 (resp. 2) contains the vector of observed frequencies in population 1 (resp. 2)
  ## B: number of Monte Carlo simulations
  ## M: number of repetitions for the calibration
  ## weights: vector of weights in [0,1] that are tried for the calibration
  ## threshold: targeted risk level of the test
  ## OUTPUT: list with INPUT arguments (B, M, threshold, weights) and the following items
  ##   calibrated.weight: weight selected by the calibration
  ##   p.value: test p-value
  ##   reject.null.hypothesis: logical indicating whether the null hypothesis is rejected or not
  ##   Message: details about the p-value interpretation
  N1 = as.numeric(x[,1])
  N2 = as.numeric(x[,2])
  calib = .calibration(N1, N2, weights, B, M, threshold)
  weightsM = weights[order(abs(calib-threshold))[1]]
  phatM = N1/sum(N1)*weightsM + N2/sum(N2)*(1-weightsM)
  out=NULL
  out$x=x
  out$B=B
  out$M=M
  out$threshold=threshold
  out$weights=weights
  out$calibrated.weight=weightsM
  out$p.value=.test_p_value(phatM, N1, N2, B)
  if(out$p.value<threshold){
    out$reject.null.hypothesis=TRUE
  } else {
    out$reject.null.hypothesis=FALSE
  }
  out$Message="The null hypothesis is rejected if the p-value is lower than the significance level (i.e. threshold) targeted in the calibration"
  return(out)
}


###################################################################
###  Documentation for data sets

#' @docType data
#' @name PathogenCompositionMoryzaeChina
#' @aliases PathogenCompositionMoryzaeChina MoryzaeChina China 
#' @title Compositions of Magnaporthe oryzae collected in China
#' @description Compositions of Magnaporthe oryzae formed from samples collected in Youle, Yunnan Province, China, in August 2008 and September 2009 (Saleh et al., 2014).
#' @usage data(PathogenCompositionMoryzaeChina)
#' @format A data frame with two rows, each row providing the pathogen composition (PC) at a given date (1st row: PC collected in August 2008; 2nd row: PC collected in September 2008).
#' @references Saleh D, Milazzo J, Adreit H, Fournier E, Tharreau D (2014). South-East Asia is the center of origin, diversity and dispersion of the rice blast fungus, Magnaporthe oryzae. New Phytologist 201: 1440-1456.
#' @references Soubeyrand S, Garreta V, Monteil C, Suffert F, Goyeau H, Berder J, Berge O, Moinard J, Fournier E, Tharreau D, Morris C, Sache I (2014). Testing differences between pathogen compositions with small samples and sparse data. Research Report, INRA, BioSP, Avignon, France.
#' @keywords datasets
#' @seealso \code{\link{PathogenCompositionMoryzaeMadagascar}}
#' @examples
#' ## Load Pathogen Compositions of M. oryzae collected in China
#' data(PathogenCompositionMoryzaeChina)
#'
#' ## Size of the first sample
#' sum(PathogenCompositionMoryzaeChina[1,])
#'
#' ## Size of the second sample
#' sum(PathogenCompositionMoryzaeChina[2,])
#'
#' ## Total number of different variants
#' ncol(PathogenCompositionMoryzaeChina)
#' 
#' ## Display pathogen compositions
#' x=PathogenCompositionMoryzaeChina
#' barplot(t(x), col=rainbow(ncol(x)), main="M. oryzae - China")
NULL


#' @docType data
#' @name PathogenCompositionMoryzaeMadagascar
#' @aliases PathogenCompositionMoryzaeMadagascar MoryzaeMadagascar Madagascar 
#' @title Compositions of Magnaporthe oryzae collected in Madagascar
#' @description Compositions of Magnaporthe oryzae formed from samples collected in Andranomanelatra, Madagascar, in February and April 2005 (Saleh et al., 2014).
#' @usage data(PathogenCompositionMoryzaeMadagascar)
#' @format A data frame with two rows, each row providing the pathogen composition (PC) at a given date (1st row: PC collected in February 2005; 2nd row: PC collected in April 2005).
#' @references Saleh D, Milazzo J, Adreit H, Fournier E, Tharreau D (2014). South-East Asia is the center of origin, diversity and dispersion of the rice blast fungus, Magnaporthe oryzae. New Phytologist 201: 1440-1456.
#' @references Soubeyrand S, Garreta V, Monteil C, Suffert F, Goyeau H, Berder J, Berge O, Moinard J, Fournier E, Tharreau D, Morris C, Sache I (2014). Testing differences between pathogen compositions with small samples and sparse data. Research Report, INRA, BioSP, Avignon, France.
#' @keywords datasets
#' @seealso \code{\link{PathogenCompositionMoryzaeChina}}
#' @examples
#' ## Load Pathogen Compositions of M. oryzae collected in Madagascar
#' data(PathogenCompositionMoryzaeMadagascar)
#'
#' ## Size of the first sample
#' sum(PathogenCompositionMoryzaeMadagascar[1,])
#'
#' ## Size of the second sample
#' sum(PathogenCompositionMoryzaeMadagascar[2,])
#'
#' ## Total number of different variants
#' ncol(PathogenCompositionMoryzaeMadagascar)
#' 
#' ## Display pathogen compositions
#' x=PathogenCompositionMoryzaeMadagascar
#' barplot(t(x), col=rainbow(ncol(x)), main="M. oryzae - Madagascar")
NULL


#' @docType data
#' @name PathogenCompositionPsyringaeClades
#' @aliases PathogenCompositionPsyringaeClades PsyringaeClades Clades 
#' @title Compositions of Pseudomonas syringae at the clade resolution
#' @description Compositions of Pseudomonas syringae formed from samples collected in South-East France, in Lower Durance River valley and in Upper Durance River valley (Monteil et al., 2014).
#' @usage data(PathogenCompositionPsyringaeClades)
#' @format A data frame with two rows, each row providing the pathogen composition (PC) at a given date (1st row: PC collected in Lower Durance River valley; 2nd row: PC collected in Upper Durance River valley).
#' @references Monteil C, ... (2014) ...
#' @references Soubeyrand S, Garreta V, Monteil C, Suffert F, Goyeau H, Berder J, Berge O, Moinard J, Fournier E, Tharreau D, Morris C, Sache I (2014). Testing differences between pathogen compositions with small samples and sparse data. Research Report, INRA, BioSP, Avignon, France.
#' @keywords datasets
#' @seealso \code{\link{PathogenCompositionPsyringaeHaplotypes}}, \code{\link{PathogenCompositionPsyringaePhylogroups}}
#' @examples
#' ## Load Pathogen Compositions of P. syringae at the clade resolution
#' data(PathogenCompositionPsyringaeClades)
#'
#' ## Size of the first sample
#' sum(PathogenCompositionPsyringaeClades[1,])
#'
#' ## Size of the second sample
#' sum(PathogenCompositionPsyringaeClades[2,])
#'
#' ## Total number of different variants
#' ncol(PathogenCompositionPsyringaeClades)
#' 
#' ## Display pathogen compositions
#' x=PathogenCompositionPsyringaeClades
#' barplot(t(x), col=rainbow(ncol(x)), main="P. syringae - Clades")
NULL



#' @docType data
#' @name PathogenCompositionPsyringaeHaplotypes
#' @aliases PathogenCompositionPsyringaeHaplotypes PsyringaeHaplotypes Haplotypes 
#' @title Compositions of Pseudomonas syringae at the haplotype resolution
#' @description Compositions of Pseudomonas syringae formed from samples collected in South-East France, in Lower Durance River valley and in Upper Durance River valley (Monteil et al., 2014).
#' @usage data(PathogenCompositionPsyringaeHaplotypes)
#' @format A data frame with two rows, each row providing the pathogen composition (PC) at a given date (1st row: PC collected in Lower Durance River valley; 2nd row: PC collected in Upper Durance River valley).
#' @references Monteil C, ... (2014) ...
#' @references Soubeyrand S, Garreta V, Monteil C, Suffert F, Goyeau H, Berder J, Berge O, Moinard J, Fournier E, Tharreau D, Morris C, Sache I (2014). Testing differences between pathogen compositions with small samples and sparse data. Research Report, INRA, BioSP, Avignon, France.
#' @keywords datasets
#' @seealso \code{\link{PathogenCompositionPsyringaeClades}}, \code{\link{PathogenCompositionPsyringaePhylogroups}}
#' @examples
#' ## Load Pathogen Compositions of P. syringae at the haplotype resolution
#' data(PathogenCompositionPsyringaeHaplotypes)
#'
#' ## Size of the first sample
#' sum(PathogenCompositionPsyringaeHaplotypes[1,])
#'
#' ## Size of the second sample
#' sum(PathogenCompositionPsyringaeHaplotypes[2,])
#'
#' ## Total number of different variants
#' ncol(PathogenCompositionPsyringaeHaplotypes)
#' 
#' ## Display pathogen compositions
#' x=PathogenCompositionPsyringaeHaplotypes
#' barplot(t(x), col=rainbow(ncol(x)), main="P. syringae - Haplotypes")
NULL



#' @docType data
#' @name PathogenCompositionPsyringaePhylogroups
#' @aliases PathogenCompositionPsyringaePhylogroups PsyringaePhylogroups Phylogroups 
#' @title Compositions of Pseudomonas syringae at the phylogroup resolution
#' @description Compositions of Pseudomonas syringae formed from samples collected in South-East France, in Lower Durance River valley and in Upper Durance River valley (Monteil et al., 2014).
#' @usage data(PathogenCompositionPsyringaePhylogroups)
#' @format A data frame with two rows, each row providing the pathogen composition (PC) at a given date (1st row: PC collected in Lower Durance River valley; 2nd row: PC collected in Upper Durance River valley).
#' @references Monteil C, ... (2014) ...
#' @references Soubeyrand S, Garreta V, Monteil C, Suffert F, Goyeau H, Berder J, Berge O, Moinard J, Fournier E, Tharreau D, Morris C, Sache I (2014). Testing differences between pathogen compositions with small samples and sparse data. Research Report, INRA, BioSP, Avignon, France.
#' @keywords datasets
#' @seealso \code{\link{PathogenCompositionPsyringaeClades}}, \code{\link{PathogenCompositionPsyringaeHaplotypes}}
#' @examples
#' ## Load Pathogen Compositions of P. syringae at the phylogroup resolution
#' data(PathogenCompositionPsyringaePhylogroups)
#'
#' ## Size of the first sample
#' sum(PathogenCompositionPsyringaePhylogroups[1,])
#'
#' ## Size of the second sample
#' sum(PathogenCompositionPsyringaePhylogroups[2,])
#'
#' ## Total number of different variants
#' ncol(PathogenCompositionPsyringaePhylogroups)
#' 
#' ## Display pathogen compositions
#' x=PathogenCompositionPsyringaePhylogroups
#' barplot(t(x), col=rainbow(ncol(x)), main="P. syringae - Phylogroups")
NULL



#' @docType data
#' @name PathogenCompositionPtriticinaGalibier
#' @aliases PathogenCompositionPtriticinaGalibier PtriticinaGalibier Galibier 
#' @title Compositions of Puccinia triticina in Galibier crops
#' @description Compositions of Puccinia triticina formed from samples collected in Lomagne, South-West France, from 2007 to 2013 (Soubeyrand et al., 2014).
#' @usage data(PathogenCompositionPtriticinaGalibier)
#' @format A data frame with 28 rows, each row providing the pathogen composition (PC) at a given date in years 2007-2013. The dates are provided in Soubeyrand et al. (2014).
#' @references Soubeyrand S, Garreta V, Monteil C, Suffert F, Goyeau H, Berder J, Berge O, Moinard J, Fournier E, Tharreau D, Morris C, Sache I (2014). Testing differences between pathogen compositions with small samples and sparse data. Research Report, INRA, BioSP, Avignon, France.
#' @keywords datasets
#' @seealso \code{\link{PathogenCompositionPtriticinaKalango}}
#' @examples
#' ## Load Pathogen Compositions of P. triticina in Galibier crops
#' data(PathogenCompositionPtriticinaGalibier)
#'
#' ## Size of the first sample
#' sum(PathogenCompositionPtriticinaGalibier[1,])
#'
#' ## Total number of different variants
#' ncol(PathogenCompositionPtriticinaGalibier)
#' 
#' ## Display pathogen compositions
#' x=PathogenCompositionPtriticinaGalibier
#' barplot(t(x), col=rainbow(ncol(x)), las=2, main="P. triticina - Galibier")
NULL



#' @docType data
#' @name PathogenCompositionPtriticinaKalango
#' @aliases PathogenCompositionPtriticinaKalango PtriticinaKalango Kalango 
#' @title Compositions of Puccinia triticina in Kalango crops
#' @description Compositions of Puccinia triticina formed from samples collected in Lomagne, South-West France, from 2007 to 2013 (Soubeyrand et al., 2014).
#' @usage data(PathogenCompositionPtriticinaKalango)
#' @format A data frame with 28 rows, each row providing the pathogen composition (PC) at a given date in years 2007-2013. The dates are provided in Soubeyrand et al. (2014).
#' @references Soubeyrand S, Garreta V, Monteil C, Suffert F, Goyeau H, Berder J, Berge O, Moinard J, Fournier E, Tharreau D, Morris C, Sache I (2014). Testing differences between pathogen compositions with small samples and sparse data. Research Report, INRA, BioSP, Avignon, France.
#' @keywords datasets
#' @seealso \code{\link{PathogenCompositionPtriticinaGalibier}}
#' @examples
#' ## Load Pathogen Compositions of P. triticina in Kalango crops
#' data(PathogenCompositionPtriticinaKalango)
#'
#' ## Size of the first sample
#' sum(PathogenCompositionPtriticinaKalango[1,])
#'
#' ## Total number of different variants
#' ncol(PathogenCompositionPtriticinaKalango)
#' 
#' ## Display pathogen compositions
#' x=PathogenCompositionPtriticinaKalango
#' barplot(t(x), col=rainbow(ncol(x)), las=2, main="P. triticina - Kalango")
NULL


