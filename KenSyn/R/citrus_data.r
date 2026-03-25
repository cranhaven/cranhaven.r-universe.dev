#' @name citrus
#' @title Meta-analysis dataset: estimation the effectiveness of a fungicidal treatment to control Phyllosticta citricarpa, a citrus fungus.
#' @description
#' The data set consists of the data collected from litterature for a meta-analysis.
#' It contains the results of 16 trials conducted in different orchards located in different
#' regions around the world. Each trial has two treatments: an untreated part (the control, X = Fung_Gp = 0)
#' and a part treated with a fungicide (X = Fung_Gp = 1). In each treatment, 300 to 2000 fruits were observed, and the number of diseased fruits was counted.
#' @docType data
#' @usage organic
#' @format a \code{RangedData} instance, 1 row per measurement. NbFruits : number of fruits observed, NbDiseasedF : number of diseased fruits, Code : code of the experiment, Fung_Gp : treatment (1: fungicid, 0: control)
#' @source Makowski D., Vicent A., Pautasso M., Stancanelli G., Rafoss T. 2014. Comparison of
#' statistical models in a meta-analysis of fungicide treatments for the control of citrus
#' black spot caused by Phyllosticta citricarpa. European journal of plant pathology 139, 79-94
#' @examples
#' summary(citrus)
#' citrus$P_C<-citrus$NbDiseasedF[citrus$Fung_Gp==0]/citrus$NbFruits[citrus$Fung_Gp==0]
#' citrus$P_E<-citrus$NbDiseasedF[citrus$Fung_Gp==1]/citrus$NbFruits[citrus$Fung_Gp==1]
#' citrus=citrus[order(citrus$P_E),]
#' dotchart(citrus$P_E, xlab="Proportion of disease fruits",xlim=c(0,1),pch=19, ylab="experiment")
#' points(citrus$P_C,1:nrow(citrus))
#' legend("topleft",legend = c("treated", "control"),pch=c(19,1))
NULL
