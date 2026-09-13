##' Data on automatic analysis of cardiotocograms
##' 
##' This data set contains measurements from 2126 fetal cardiotocograms (CTGs). 
##' The CTGs were automatically processed and the respective diagnostic features measured. 
##' The CTGs were also classified by three expert obstetricians and a consensus classification label 
##' assigned to each of them. This description is taken from the UC Irvine Machine 
##' Learning Repository, where this data set was downloaded from. The outcome \code{CLASS} 
##' is categorical with ten classes that correspond to different fetal heart rate patterns.
##' See the 'Details' section below for further information.
##'
##' The variables are as follows:
##' \itemize{
##'   \item \code{b}. numeric. Start instant
##'   \item \code{e}. numeric. End instant
##'   \item \code{LBE}. numeric. Fetal heart rate (FHR) baseline value assessed by medical expert (beats per minute)
##'   \item \code{LB}. numeric. FHR baseline value assessed by SisPorto (beats per minute)
##'   \item \code{AC}. numeric. Number of accelerations per second
##'   \item \code{FM}. numeric. Number of fetal movements per second
##'   \item \code{UC}. numeric. Number of uterine contractions per second
##'   \item \code{DL}. numeric. Number of light decelerations per second
##'   \item \code{DS}. numeric. Number of severe decelerations per second
##'   \item \code{DP}. numeric. Number of prolonged decelerations per second
##'   \item \code{ASTV}. numeric. Percentage of time with abnormal short term variability
##'   \item \code{MSTV}. numeric. Mean value of short term variability
##'   \item \code{ALTV}. numeric. Percentage of time with abnormal long term variability
##'   \item \code{MLTV}. numeric. Mean value of long term variability
##'   \item \code{Width}. numeric. Width of FHR histogram
##'   \item \code{Min}. numeric. Minimum value of FHR histogram
##'   \item \code{Max}. numeric. Maximum value of FHR histogram
##'   \item \code{Nmax}. numeric. Number of histogram peaks
##'   \item \code{Nzeros}. numeric. Number of histogram zeros
##'   \item \code{Mode}. numeric. Mode of the histogram
##'   \item \code{Mean}. numeric. Mean of the histogram
##'   \item \code{Median}. numeric. Median of the histogram
##'   \item \code{Variance}. numeric. Variance of the histogram
##'   \item \code{Tendency}. factor. Histogram tendency (-1 for left asymmetric; 0 for symmetric; 1 for right asymmetric)
##'   \item \code{CLASS}. factor. FHR pattern class
##' }
##' \verb{ }\cr
##' The classes of the outcome \code{CLASS} are as follows:
##' \itemize{
##'   \item \code{A}. Calm sleep	
##'   \item \code{B}. REM sleep	
##'   \item \code{C}. Calm vigilance	
##'   \item \code{D}. Active vigilance	
##'   \item \code{SH}. Shift pattern (A or SUSP with shifts)	
##'   \item \code{AD}. Accelerative/decelerative pattern (stress situation)			
##'   \item \code{DE}. Decelerative pattern (vagal stimulation)		
##'   \item \code{LD}. Largely decelerative pattern	
##'   \item \code{FS}. Flat-sinusoidal pattern (pathological state)		
##'   \item \code{SUSP}. Suspect pattern
##' }
##' This is a pre-processed version of the "Cardiotocography" data set published
##' in the UC Irvine Machine Learning Repository. The raw data contained the four
##' additional variables \code{Date}, \code{FileName}, \code{SegFile}, and \code{NSP},
##' which were removed in this version of the data. Moreover, the variable \code{DR}, representing 
##' the number of repetitive decelerations per second was removed as well because
##' it was 0 for all observations.
##' 
##' @format A data frame with 2126 observations, 24 covariates and one 10-class outcome variable
##' @source UC Irvine Machine Learning Repository, link: \url{https://archive.ics.uci.edu/dataset/193/cardiotocography/} (Accessed: 29/08/2024)
##'
##' @examples
##' 
##' # Load data:
##' data(ctg)
##' 
##' # Numbers of observations per outcome class:
##' table(ctg$CLASS)
##' 
##' # Dimension of data:
##' dim(ctg)
##'
##' # First rows of data:
##' head(ctg) 
##' 
##' @references
##' \itemize{
##'   \item Ayres-de Campos, D., Bernardes, J., Garrido, A., Marques-de-Sá, J., Pereira-Leite, L. (2000). SisPorto 2.0: a program for automated analysis of cardiotocograms. J Matern Fetal Med. 9(5):311-318, <\doi{10.1002/1520-6661(200009/10)9:5<311::AID-MFM12>3.0.CO;2-9}>.
##'   \item Dua, D., Graff, C. (2019). UCI Machine Learning Repository. Irvine, CA: University of California, School of Information and Computer Science. \url{https://archive.ics.uci.edu/ml/}.
##' }
##'
##' @name ctg
NULL