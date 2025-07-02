#' @name tonguelength
#' 
#' @title Converts ITD (cm) to tongue length for bees.
#' 
#' @description Calculates tongue length (mm) from Cariveau et al. (2015) using intertegular distance (ITD) values (cm)..  
#' 
#' @param x A dataframe with the following two columns: bee intertegular distance (IT) measurments in mm and Family, a vector of bee families. Only implemented 5 out of the extant 7 families: 
#' "Andrenidae", "Apidae", "Colletidae", "Halictidae", "Megachilidae".
#' 
#' @param mouthpart The mouth part you are interested in. Options are "all", glossa", "prementum" and "tongue" (i.e. gloss + prementum)
#' 
#' @return A dataframe with bee tongue length (mm) and 95% confidence intervals are returned for each bees species along with your original dataframe.
#' 
#' @examples
#' example=cbind.data.frame(IT=c(1.3,2.3),
#'                          Family=c("Andrenidae","Apidae"))
#' tonguelength(example,mouthpart="all")
#' @references Kendall et al. (2018)  Pollinator size and its consequences: Predictive allometry for pollinating insects. <doi:10.1101/397604>
#' 
#' Cariveau et al. (2016) The allometry of bee tongue length an its uses in ecology and evolution. PLoS ONE 11(3): e0151482 <doi:10.1371/journal.pone.0151482>
#' 
#' @importFrom stats lm
#'
#' @export
tonguelength <- function(x, mouthpart = "all"){
  check_family <- x$family %in% c("Andrenidae", "Apidae", 
                                  "Colletidae", "Halictidae", "Megachilidae")
  if(any(check_family == FALSE)){
    stop("family should be one of: 'Andrenidae', 'Apidae', 
         'Colletidae', 'Halictidae', 'Megachilidae'")
  }
  check_mouthpart <- x$mouthpart %in% c("all", "glossa", "prementum", "tongue")
  if(any(check_mouthpart == FALSE)){
    stop("mouthpart should be one of: 'all', glossa', 'prementum', 'tongue'")
  }
  
  repmis::source_data("https://github.com/ibartomeus/traitbase/raw/master/raw_data/Cariveau_2016.rda", envir = environment())
  colnames(tongues)[7]="IT"
  proboscis=lm(log(mean_tongue_length_mm)~log(IT)+Family,tongues)
  glossa=lm(log(mean_glossa_length_mm)~log(IT)+Family,tongues)
  prementum=lm(log(mean_prementum_length_mm)~log(IT)*Family,tongues)
  
  if(mouthpart=="all"){
    out<-exp(predict(proboscis,x,interval = c( "confidence"),
                     level = 0.95))
    out2<-exp(predict(glossa,x,interval = c( "confidence"),
                      level = 0.95))
    out3<-exp(predict(prementum,x,interval = c( "confidence"),
                      level = 0.95))
    out=cbind(out,out2,out3)
    colnames(out)=c("Proboscis","P.lwr.CI","P.upr.CI","Glossa","G.lwr.CI","G.upr.CI","Prementum","Pr.lwr.CI","Pr.upr.CI")
    cbind(x,out)
    out
  }else{
    if(mouthpart=="glossa"){
      out<-exp(predict(glossa,x,interval = c( "confidence"),
                       level = 0.95))
      colnames(out)=c("Prementum","lwr.CI","upr.CI")
    }
    if(mouthpart=="prementum"){
      out<-exp(predict(prementum,x,interval = c( "confidence"),
                       level = 0.95))
      colnames(out)=c("Prementum","lwr.CI","upr.CI")
    }
    if(mouthpart=="tongue"){
      out<-exp(predict(proboscis,x,interval = c( "confidence"),
                       level = 0.95))
      colnames(out)=c("Proboscis","lwr.CI","upr.CI")
    }
    out=cbind(x,out)
    out
  }
  }


