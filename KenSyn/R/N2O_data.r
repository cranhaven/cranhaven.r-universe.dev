#' @name N2O
#' @title Meta-analysis dataset to estimate the response of N2O emissions to the applied N fertilizer rate.
#' @description
#' The data set consists of the data collected from litterature for a meta-analysis to estimate
#' the response of N2O emissions (a greenhouse gas produced largely by agricultural activities,
#' including nitrogen fertilization) to the applied N fertilizer rate.
#' This dataset contains measurements of N2O emissions collected from 203 experimental studies
#' (corresponding to different sites-years). In each study (Ref_num), several fertilizer doses (N_rate) were applied
#' to different experimental plots and N2O emissions were measured (N2O, in kg ha-1 yr-1) on each.
#' A total of 985 N2O emission data are available.
#'
#' @docType data
#' @usage N2O
#' @format a \code{RangedData} instance, 1 row per measurement.
#' Type: , Ref_num: , Climate: , Texture_class: , Tex_act: , Carbon_content: , Nitrogen_content: ,
#' pH: , CEC: , Bulk_density: , Drainage: , Annual_Precipitation: , Mean_annual_temp: ,
#' crop_type: , Fertilizer_type: , Mode_of_application: , Timing_of_application: ,
#' N_rate: , N2O: , log_N2O: , duree_expe: , Method_N2O: , Freq_N2O
#' @source Philibert et al (2012), Gerber et al (2016), real data extracted from published papers.
#'  Philibert A., Loyce C., Makowski D. 2012. Quantifying uncertainties in N2O emission due to N fertilizer application
#'  in cultivated areas. Plos One 7(11): e50950. doi:10.1371/journal.pone.0050950.
#'  Gerber J.S., Kimberly M. C., Makowski D, Inaki Garcia de Cortazar-Atauri, Petr Havlik,
#'  Mario Herrero, Marie Launay, Nathaniel D. Mueller,  Christine S. O'Connell, Pete Smith,
#'  Paul C. West. 2016. Spatially explicit estimates of N2O emissions from croplands suggest climate mitigation opportunities from improved fertilizer management. Global Change Biology 22 3383-3394.
#' @examples
#' summary(N2O)
#' # Examples of N2O emission measurements obtained on 9 experiments
#' # for different doses of N fertilizer applied.
#' par(mfrow=c(3,3), mar=c(4.1,4.1,1,1))
#' ListNum<-c(12,363,312,232,158,226,87,21,17)
#' null<-sapply(ListNum, function(Num){plot(N2O$N_rate[N2O$Ref_num==Num],N2O$N2O[N2O$Ref_num==Num],
#' xlab="Dose engrais N (kg/ha)", ylab="Emission de N2O (kg/ha/an)", pch=19, xlim=c(0,350))})
NULL
