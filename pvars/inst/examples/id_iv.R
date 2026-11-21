### reproduce Jentsch,Lunsford 2019:2668, Ch.III ###
data("PCIT")
names_k = c("APITR", "ACITR", "PITB", "CITB", "GOV", "RGDP", "DEBT")
names_l = c("m_PI", "m_CI")  # proxy names
names_s = paste0("epsilon[ ", c("PI", "CI"), " ]")  # shock names
dim_p   = 4  # lag-order

# estimate and identify under ordering "BLUE" of Fig.1 and 2 #
R.vars = vars::VAR(PCIT[ , names_k], p=dim_p, type="const")
R.idBL = id.iv(R.vars, iv=PCIT[-(1:dim_p), names_l], S2="MR", cov_u="OMEGA")
colnames(R.idBL$B) = names_s  # labeling

# estimate and identify under ordering "RED" of Fig.1 and 2 #
R.vars = vars::VAR(PCIT[ , names_k[c(2:1, 3:7)]], p=dim_p, type="const")
R.idRD = id.iv(R.vars, iv=PCIT[-(1:dim_p),names_l[2:1]], S2="MR", cov_u="OMEGA")
colnames(R.idRD$B) = names_s[2:1]  # labeling

\donttest{
# select minimal or full example #
is_min = TRUE
n.boot = ifelse(is_min, 5, 10000)

# bootstrap both under 1%-response normalization #
set.seed(2389)
R.norm = function(B) B / matrix(-diag(B), nrow(B), ncol(B), byrow=TRUE)
R.sbBL = sboot.mb(R.idBL, b.length=19, n.boot=n.boot, normf=R.norm)
R.sbRD = sboot.mb(R.idRD, b.length=19, n.boot=n.boot, normf=R.norm)

# plot IRF of Fig.1 and 2 with 68%-confidence levels #
library("ggplot2")
L.idx = list(BLUE1=c(1, 11, 5, 7, 3,  9)+0.1,
             RED1 =c(4, 12, 6, 8, 2, 10)+0.1, 
             RED2 =c(1, 11, 7, 5, 3,  9)+0.1, 
             BLUE2=c(4, 12, 8, 6, 2, 10)+0.1)
# Indexes to subset and reorder sub-plots in plot.sboot(), where 
# the 14 IRF-subplots in the 2D-panel are numbered as a 1D-sequence 
# vectorized by row. '+0.1' makes sub-setting robust against 
# truncation errors from as.integer(). In a given figure, the plots
# RED and BLUE display the same selection of IRF-subplots. 

R.fig1 = as.pplot(
 BLUE=plot(R.sbBL, lowerq=0.16, upperq=0.84, selection=list(1, L.idx[[1]])),
 RED =plot(R.sbRD, lowerq=0.16, upperq=0.84, selection=list(1, L.idx[[2]])),
 names_g=c("APITR first", "ACITR first"), color_g=c("blue", "red"), n.rows=3)

R.fig2 = as.pplot(
 RED =plot(R.sbRD, lowerq=0.16, upperq=0.84, selection=list(1, L.idx[[3]])), 
 BLUE=plot(R.sbBL, lowerq=0.16, upperq=0.84, selection=list(1, L.idx[[4]])),
 names_g=c("ACITR first", "APITR first"), color_g=c("red", "blue"), n.rows=3)

R.fig1$F.plot + labs(x="Quarters", color="Ordering", fill="Ordering")
R.fig2$F.plot + labs(x="Quarters", color="Ordering", fill="Ordering")
}

