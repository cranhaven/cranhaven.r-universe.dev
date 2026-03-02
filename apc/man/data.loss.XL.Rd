\name{data.loss.XL}
\alias{data.loss.XL}
\title{US Casualty data, XL Group}
\description{
Function that organises US Casualty data from XL Group in \code{\link{apc.data.list}} format.

The data set is taken from table 1.1 Kuang and Nielsen (2020).
Data are for US Casualty data from the XL Group.
They are gross paid and reported loss and allocated loss adjustment expense in 1000 USD.

The data set is in "CL"-format.

}
\usage{data.loss.XL}
\value{The value is a list in \code{\link{apc.data.list}} format.
	\item{response}{matrix of paid amounts, incremental}
	\item{dose}{NULL.}
	\item{data.format}{logical. Equal to "CL".}
	\item{age1}{numeric. Equal to 1.}
	\item{per1}{NULL. Not needed when data.format="CL"}
	\item{coh1}{numeric. Equal to 1997.}
	\item{unit}{numeric. Equal to 1997.}
	\item{per.zero}{NULL.  Not needed when data.format="CL"} 
	\item{per.max}{NULL.  Not needed when data.format="CL"} 
	\item{time.adjust}{-1996.  Thus age=1 in cohort=1997 corresponds to period=1997+1997-1+(-1996)=1997.}	
	\item{label}{character. "loss, US casualty, XL Group".}						
}
\source{Table 1.1 of Kuang and Nielsen (2020) and in turn from XL Group Ltd.}
\references{
Kuang, D. and Nielsen B. (2020) Generalized log-normal chain-ladder. \emph{Scandinavian Actuarial Journal} 2020, 553-576. \emph{Download}: \href{https://www.tandfonline.com/doi/full/10.1080/03461238.2019.1696885}{Open access}. Earlier version: \href{https://www.nuffield.ox.ac.uk/economics/Papers/2018/2018W02_KuangNielsen2018GLNCL.pdf}{Nuffield DP}.
}
\author{Bent Nielsen <bent.nielsen@nuffield.ox.ac.uk> 26 August 2020 (10 Mar 2018)}
\seealso{General description of \code{\link{apc.data.list}} format.

	For explanation for Chain Ladder forecast, see \code{\link{apc.forecast.ac}}. 

	The analysis in Kuang and Nielsen (2020) is reproduced in the vignette
	\href{../doc/ReproducingKN2020.pdf}{\code{ReproducingKN2020.pdf}},
	\href{../doc/ReproducingKN2020.R}{\code{ReproducingKN2020.R}}
	on
	\href{../doc/index.html}{\code{Vignettes}}.
}	
\examples{
#########################
##	It is convenient to construct a data variable for paid data

data	<- data.loss.XL()
##	To see the content of the data
data

#########################
#	Get deviance table.
#	reproduce Table 4.1 in Kuang and Nielsen (2018).

apc.fit.table(data,"log.normal.response")
apc.fit.table(data,"log.normal.response",model.design.reference="AC")

#########################
#	> apc.fit.table(data,"log.normal.response")
#	     -2logL df.residual LR vs.APC df vs.APC prob(>chi_sq) F vs.APC prob(>F)     aic
#	APC 170.003         153       NaN       NaN           NaN      NaN      NaN 286.003
#	AP  243.531         171    73.527        18         0.000    3.564    0.000 323.531
#	AC  179.873         171     9.869        18         0.936    0.409    0.984 259.873
#	PC  633.432         171   463.428        18         0.000   68.736    0.000 713.432
#	Ad  258.570         189    88.567        36         0.000    2.230    0.000 302.570
#	Pd  643.892         189   473.888        36         0.000   36.340    0.000 687.892
#	Cd  649.142         189   479.139        36         0.000   37.368    0.000 693.142
#	A   357.359         190   187.355        37         0.000    5.956    0.000 399.359
#	P   644.176         190   474.172        37         0.000   35.412    0.000 686.176
#	C   672.392         190   502.388        37         0.000   41.099    0.000 714.392
#	t   664.488         207   494.484        54         0.000   27.015    0.000 672.488
#	tA  681.993         208   511.989        55         0.000   29.072    0.000 687.993
#	tP  664.746         208   494.742        55         0.000   26.560    0.000 670.746
#	tC  686.181         208   516.178        55         0.000   29.713    0.000 692.181
#	1   690.399         209   520.396        56         0.000   29.830    0.000 694.399
#
#	> apc.fit.table(data,"log.normal.response",model.design.reference="AC")
#	    -2logL df.residual LR vs.AC df vs.AC prob(>chi_sq) F vs.AC prob(>F)     aic
#	AC 179.873         171      NaN      NaN           NaN     NaN      NaN 259.873
#	Ad 258.570         189   78.698       18             0   4.319        0 302.570
#	Cd 649.142         189  469.269       18             0  79.257        0 693.142
#	A  357.359         190  177.486       19             0  11.955        0 399.359
#	C  672.392         190  492.519       19             0  84.930        0 714.392
#	t  664.488         207  484.615       36             0  42.993        0 672.488
#	tA 681.993         208  502.120       37             0  45.869        0 687.993
#	tC 686.181         208  506.308       37             0  46.886        0 692.181
#	1  690.399         209  510.526       38             0  46.670        0 694.399
 	
 
#########################
#	Fit log normal chain-ladder model
#	reproduce Table 4.2 in Kuang and Nielsen (2018).

fit.ac	<- apc.fit.model(data,"log.normal.response","AC")
id.ac	<- apc.identify(fit.ac)
id.ac$coefficients.dif
fit.ac$s2		
fit.ac$RSS

#########################
#	> id.ac$coefficients.dif
#	                  Estimate Std. Error     t value     Pr(>|t|)
#	level          7.660055032  0.1377951 55.59016605 0.000000e+00
#	D_age_1998     2.272100342  0.1335080 17.01846386 5.992216e-65
#	D_age_1999     0.932530550  0.1362610  6.84370899 7.716860e-12
#	D_age_2000     0.235606356  0.1398301  1.68494782 9.199864e-02
#	D_age_2001     0.088886609  0.1438733  0.61781154 5.366996e-01
#	D_age_2002    -0.176044303  0.1483681 -1.18653717 2.354102e-01
#	D_age_2003    -0.144445459  0.1533567 -0.94189218 3.462478e-01
#	D_age_2004    -0.427608601  0.1589136 -2.69082462 7.127565e-03
#	D_age_2005    -0.300527594  0.1651428 -1.81980421 6.878883e-02
#	D_age_2006    -0.399729999  0.1721838 -2.32153023 2.025824e-02
#	D_age_2007    -0.189656058  0.1802245 -1.05233225 2.926471e-01
#	D_age_2008    -0.242063670  0.1895226 -1.27722853 2.015216e-01
#	D_age_2009    -0.260459607  0.2004421 -1.29942545 1.937980e-01
#	D_age_2010    -0.555317528  0.2135164 -2.60081872 9.300158e-03
#	D_age_2011    -0.303234088  0.2295651 -1.32090683 1.865324e-01
#	D_age_2012     0.405830766  0.2499291  1.62378389 1.044219e-01
#	D_age_2013    -0.895278068  0.2769988 -3.23206421 1.228994e-03
#	D_age_2014     0.116668873  0.3156054  0.36966685 7.116307e-01
#	D_age_2015    -0.383048241  0.3777268 -1.01408813 3.105407e-01
#	D_age_2016    -0.273419402  0.5083832 -0.53782152 5.907003e-01
#	D_cohort_1998  0.288755900  0.1335080  2.16283663 3.055375e-02
#	D_cohort_1999  0.163424236  0.1362610  1.19934721 2.303930e-01
#	D_cohort_2000 -0.264981486  0.1398301 -1.89502518 5.808907e-02
#	D_cohort_2001  0.149829430  0.1438733  1.04139815 2.976908e-01
#	D_cohort_2002 -0.374386828  0.1483681 -2.52336417 1.162380e-02
#	D_cohort_2003 -0.198735893  0.1533567 -1.29590632 1.950078e-01
#	D_cohort_2004 -0.008807130  0.1589136 -0.05542087 9.558032e-01
#	D_cohort_2005 -0.005337953  0.1651428 -0.03232325 9.742143e-01
#	D_cohort_2006 -0.132272851  0.1721838 -0.76820710 4.423642e-01
#	D_cohort_2007 -0.021862643  0.1802245 -0.12130783 9.034472e-01
#	D_cohort_2008 -0.472602270  0.1895226 -2.49364600 1.264386e-02
#	D_cohort_2009 -0.437572798  0.2004421 -2.18303804 2.903301e-02
#	D_cohort_2010  0.295511564  0.2135164  1.38402260 1.663515e-01
#	D_cohort_2011  0.310545832  0.2295651  1.35275725 1.761332e-01
#	D_cohort_2012 -0.268692406  0.2499291 -1.07507473 2.823413e-01
#	D_cohort_2013  0.142131410  0.2769988  0.51311192 6.078730e-01
#	D_cohort_2014  0.201777590  0.3156054  0.63933494 5.226051e-01
#	D_cohort_2015 -0.092672697  0.3777268 -0.24534320 8.061907e-01
#	D_cohort_2016  0.872997251  0.5083832  1.71720334 8.594203e-02	
#	> fit.ac$s2	
#	[1] 0.1693316
#	> fit.ac$RSS
#	[1] 28.9557
#	> fit.ac$RSS

forecast <- apc.forecast.ac(fit.ac,quantiles=c(0.995))
forecast$response.forecast.coh

#########################
#	> forecast$response.forecast.coh
#			 forecast         se    se.proc      se.est     t-0.995
#	coh_2    1871.073   1026.463   707.4405    743.7428    4544.891
#	coh_3    5099.330   1874.681  1375.8435   1273.3744    9982.659
#	coh_4    7171.317   2123.128  1622.5220   1369.3412   12701.822
#	coh_5   11699.350   2984.949  2274.8292   1932.6338   19474.801
#	coh_6   13717.388   3345.138  2654.4080   2035.6984   22431.090
#	coh_7   14343.522   3188.410  2471.3130   2014.5886   22648.964
#	coh_8   18377.001   3834.057  2910.9751   2495.2390   28364.281
#	coh_9   25488.052   5241.618  3976.5389   3414.9225   39141.867
#	coh_10  30524.942   6213.652  4662.3320   4107.5694   46710.794
#	coh_11  40078.245   8115.990  5976.5789   5490.8835   61219.471
#	coh_12  32680.319   6603.511  4727.4210   4610.6241   49881.712
#	coh_13  28509.077   5895.265  4143.1332   4193.8760   43865.568
#	coh_14  51760.526  11013.030  7540.3989   8026.7807   80448.208
#	coh_15  98747.731  22063.641 14798.3216  16365.0210  156220.991
#	coh_16 100330.677  23254.845 14704.7084  18015.5316  160906.889
#	coh_17 149813.314  36629.836 21310.2885  29792.8931  245229.846
#	coh_18 221549.649  58610.037 29815.3239  50459.7158  374222.093
#	coh_19 229480.904  69931.745 29102.9866  63588.2473  411645.102
#	coh_20 575343.178 235016.967 70362.1087 224236.8135 1187535.497

}
