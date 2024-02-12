#' Aortic Stenosis RCT data
#'
#' Data from 5 randomized controlled trials of transcatheter aortic valve
#' replacement vs surgery in patients with Aortic Stenosis. The outcome is time
#' until death from any cause. For each RCT, 
#' we reconstructed the individual patient data for each randomization group.
#' We first extracted the time and survival probability coordinates 
#' from the Kaplan-Meier curves using the 
#' DigitizeIt software (\url{http://www.digitizeit.de/}). We used 
#' these coordinates, the total numbers of events, and the 
#' numbers of participants at risk to determine individual
#' event times and event indicators. (Guyot, BMC Med Res Method 2012)
#' @docType data
#' @usage data(AorticStenosisTrials)
#' @note 
#' \tabular{ccc}{
#'   Trial ID \tab Trial Name \tab Last observed time (months)*\cr
#'   ----------------- \tab ------------------- \tab ---------------------\cr
#'   1 \tab NOTION \tab 24.0\cr
#'   2 \tab PARTNER \tab 63.3\cr
#'   3 \tab SURTAVI \tab 24.1\cr
#'   4 \tab PARTNER2 \tab 36.1\cr
#'   5 \tab USCoreValve \tab 24.1}
#' * minimum of the last observed times across the two randomization groups.
#' 
#' @references
#' Sondergaard, L, Steinbruchel, DA, Ihlemann, N, Nissen, H, Kjeldsen, BJ, Peturs-son, P, Ngo, AT, Olsen, NT,
#'  Chang, Y, Franzen, OW and  others. (2016).  Two-year outcomes in patients with severe aortic valve stenosis
#'  randomized to transcatheter versus surgical aortic valve replacement. Circ Cardiovasc Interv 9(6)
#'
#' Mack, MJ, Leon, MB, Smith, CR, Miller, DC, Moses, JW, Tuzcu, EM, Webb, JG, Douglas, PS,
#'  Anderson, WN, Blackstone, EH and others. (2015).5-year outcomes of transcatheter aortic valve replacement or
#'  surgical aortic valve replacement for high surgical risk patients with aortic stenosis (PARTNER 1):
#'  a randomised controlled trial. Lancet 385, 2477-2484.
#'
#' Reardon, MJ, Van Mieghem, NM, Popma, JJ, Kleiman, NS, Søndergaard, L, Mum-taz,  M,  Adams,  DH,  Deeb,  GM,
#'  Maini,  B,  Gada,  H and  others.  (2017).   Surgical  or transcatheter aortic-valve replacement in
#'  intermediate-risk patients. N  Engl  J  Med 376(14), 1321-1331.
#'
#' Leon, MB, Smith, CR, Mack, MJ, Makkar, RR, Svensson, LG, Kodali, SK, Thourani,VH,  Tuzcu,  EM,  Miller,
#'  DC,  Herrmann,  HC and  others.  (2016). Transcatheter  orsurgical  aortic-valve  replacement  in
#'  intermediate-risk patients. N  Engl J Med 2016(374), 1609-1620.
#'
#' Deeb,  GM,  Reardon,  MJ,  Chetcuti,  S,  Patel,  HJ,  Grossman,  PM,  Yakubov,  SJ,Kleiman, NS, Coselli,
#'  JS, Gleason, TG, Lee, JS and  others. (2016).  3-year outcomes in high-risk patients who underwent surgical
#'  or transcatheter aortic valve replacement. J Am Coll Cardiol 67(22), 2565-2574.
#'  
#' Guyot P, Ades AE, Ouwens MJ, et al. Enhanced secondary 
#'  analysis of survival data: reconstructing the data from 
#'  published Kaplan-Meier survival curves. BMC Med Res Methodol 2012; 12:9.
"AorticStenosisTrials"
