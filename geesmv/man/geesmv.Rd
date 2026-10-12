\name{geesmv-package}
\alias{geesmv-package}
\alias{geesmv}
\docType{package}
\title{
Modified Variance Estimators for Generalized Estimating Equations
}
\description{
Generalized estimating equations with the original sandwich variance estimator proposed by Liang and Zeger (1986), and eight types of more recent modified variance estimators for improving the finite small-sample performance.
}
\details{
Generalized estimating equations with the original sandwich variance estimator proposed by Liang and Zeger (1986), and eight types of more recent modified variance estimators for improving the finite small-sample performance.

GEE.var.pan(), GEE.var.gst() and GEE.var.wl() are only for the balanced data, while the others can be used for both balanced and unbalanced data.
}
\author{
Ming Wang <mwang@phs.psu.edu>

Maintainer: Zheng Li <zheng.li@outlook.com>
}
\references{
De Backer M, De Vroey C, Lesaffre E, Scheys I, De Keyser P. Twelve weeks of continuous oral therapy for toenail onychomycosis caused by dermatophytes: a double-blind comparative trial of terbinafine 250 mg/day versus itraconazole 200 mg/day. Journal of the American Academy of Dermatology 1998; 38: 57-63.

Fay MP and Graubard BI. Small-sample adjustments for Wald-type tests using sandwich estimators. Biometrics 2001;57: 1198-1206. 

Gosho M, Sato Y and Takeuchi H. Robust covariance estimator for small-sample adjustment in the generalized estimating equations: A simulation study. Science Journal of Applied Mathematics and Statistics 2014;2(1):20-25.

Kauermann G and Carroll RJ. A note on the efficiency of sandwich covariance matrix estimation. Journal of the American Statistical Association 2001;96: 1387-1398. 

Morel JG, Bokossa MC, and Neerchal NK. Small sample correction for the variance of GEE estimators. Biometrical Journal 2003;45(4): 395-409. 

MacKinnon JG. Some heteroskedasticity-consistent covariance matrix estimators with improved finite sample properties. Journal of Econometrics 1985;29: 305-325. 

Mancl LA and DeRouen TA. A covariance estimator for GEE with improved small-sample properties. Biometrics 2001;57: 126-134. 

Pan W. On the robust variance estimator in Generalized Estimating Equations. Biometrika 2001;88: 901-906. 

Pottho R. F. and Roy, S. W. A generalized multivariate analysis of variance model
useful especially for growth curve problems. Biometrika 1964;51:313-326

Thall PF, and Vail SC. Some covariance models for longitudinal count data with overdispersion. Biometrics 1990; 46: 657-671.

Wang M and Long Q. Modified robust variance estimator for generalized estimating equations with improved small-sample performance. Statistics in Medicine 2011;30(11): 1278-1291. 

Zeger SL and Liang KY. Longitudinal data analysis for discrete and continuous outcomes. Biometrics 1986; 121-130.
}
\keyword{ package }
\seealso{
GEE.var.lz()

GEE.var.mk()

GEE.var.kc()

GEE.var.pan()

GEE.var.fg()

GEE.var.gst()

GEE.var.md()

GEE.var.mbn()

GEE.var.wl()

}
\examples{

### Run the data analysis using the package using seizure dataset (Poisson);
data(seizure)
seizure$subject <- 1:length(seizure[,1])
data_alt <- reshape(seizure, direction="long", idvar="subject", timevar="Time", 
                   varying=names(seizure)[1:4], v.names="response", times=1:4) 
data_alt <- data_alt[order(data_alt$subject),]
data_alt <- data_alt[,c(4,1:3,5,6)]

### independence working correlation structure;
formula <- response~base+trt+Time
lz.ind <- GEE.var.lz(formula,id="subject",family=poisson,
               data_alt,corstr="independence")
               
mk.ind <- GEE.var.mk(formula,id="subject",family=poisson,
                data_alt,corstr="independence")
                
pan.ind <- GEE.var.pan(formula,id="subject",family=poisson,
         data_alt,corstr="independence")
         
gst.ind <- GEE.var.gst(formula,id="subject",family=poisson,
        data_alt,corstr="independence")
        
kc.ind <- GEE.var.kc(formula,id="subject",family=poisson,
         data_alt,corstr="independence") 
         
md.ind <- GEE.var.md(formula,id="subject",family=poisson,
        data_alt,corstr="independence")
        
fg.ind <- GEE.var.fg(formula,id="subject",family=poisson,data_alt,
                      corstr="independence",b=0.75) 
mbn.ind <- GEE.var.mbn(formula,id="subject",family=poisson,data_alt,
                      corstr="independence",d=2,r=1)                
wl.ind <- GEE.var.wl(formula,id="subject",family=poisson,
             data_alt,corstr="independence")
}
