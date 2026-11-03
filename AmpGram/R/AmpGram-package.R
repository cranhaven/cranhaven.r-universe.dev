#' Prediction of antimicrobial peptides
#'
#' Antimicrobial peptides (AMPs) are ancient and evolutionarily conserved 
#' molecules widespread in all living organisms that participate in host 
#' defence and/or microbial competition. Due to their positive charge, 
#' hydrophobicity and amphipathicity, they preferentially disrupt 
#' negatively-charged bacterial membranes. AMPs are considered an important 
#' alternative to traditional antibiotics, especially in times when the latter 
#' are drastically losing their effectiveness. Therefore, efficient computational 
#' tools for AMP prediction are essential to identify the best AMP candidates without 
#' undertaking expensive experimental studies. AmpGram is our novel tool for predicting 
#' AMPs based on the stacked random forests and n-gram analysis, able to
#' successfully predict antimicrobial peptides in proteomes.
#' 
#' AmpGram is available as R function (\code{\link{predict.ampgram_model}}) or
#' shiny GUI (\code{\link{AmpGram_gui}}).
#' 
#' AmpGram requires the external package, AmpGramModel, which 
#' contains models necessary to perform the prediction. The model 
#' can be installed using \code{\link{install_AmpGramModel}}
#'
#' @name AmpGram-package
#' @aliases AmpGram-package AmpGram
#' @docType package
#' @importFrom utils menu
#' @author
#' Maintainer: Michal Burdukiewicz <michalburdukiewicz@@gmail.com>
#' @references Burdukiewicz M, Sidorczuk K, Rafacz D, Pietluch F, Chilimoniuk J,
#' Roediger S, Gagat P. (2020) \emph{AmpGram: a proteome screening tool for 
#' prediction and design of antimicrobial peptides}. (submitted)
#' @keywords package
NULL
