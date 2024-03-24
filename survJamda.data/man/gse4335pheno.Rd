\name{gse4335pheno}
\alias{gse4335pheno}
\docType{data}
\title{
Phenotype data of GSE4335.

}
\description{
Data such as patients' information, tumor characteristics, samples information related to GSE4335.

}
\usage{data(gse4335pheno)}
\format{
  A data frame with 115 observations on the following 17 variables.
  \describe{
    \item{\code{Array_ID}}{a factor with levels \code{shac091} \code{shac092} \code{shac093} \code{shac100} \code{shac107} \code{shac110} \code{shac112} \code{shac113} \code{shaz104} \code{shaz110} \code{shaz112} \code{shaz114} \code{shaz116} \code{shaz121} \code{shaz124} \code{shaz125} \code{shaz126} \code{shaz129} \code{shaz131} \code{shaz132} \code{shaz133} \code{shaz134} \code{shaz135} \code{shaz140} \code{shbg109} \code{shbg110} \code{shbg128} \code{shby020} \code{shby021} \code{shby022} \code{shby028} \code{shby033} \code{shby035} \code{shby037} \code{shby038} \code{shby039} \code{shby040} \code{shby041} \code{shby042} \code{shby043} \code{shby046} \code{shby049} \code{shby050} \code{shby051} \code{shby236} \code{shby245} \code{shby249} \code{svcc100} \code{svcc101} \code{svcc104} \code{svcc105} \code{svcc106} \code{svcc107} \code{svcc1077} \code{svcc108} \code{svcc111} \code{svcc1114} \code{svcc115} \code{svcc118} \code{svcc119} \code{svcc120} \code{svcc122} \code{svcc124} \code{svcc130} \code{svcc131} \code{svcc132} \code{svcc134} \code{svcc137} \code{svcc51} \code{svcc53} \code{svcc55} \code{svcc61} \code{svcc68} \code{svcc76} \code{svcc78} \code{svcc81} \code{svcc83} \code{svcc84} \code{svcc87} \code{svcc88} \code{svcc89} \code{svcc92} \code{svcc93} \code{svcc96} \code{svcc98} \code{svcc99} \code{svj104} \code{svl002} \code{svl003} \code{svl006} \code{svl007} \code{svl012} \code{svl015} \code{svl016} \code{svl018} \code{svl020} \code{svl022} \code{svl026} \code{svl027} \code{svl029} \code{svl031} \code{svl033} \code{svl034} \code{svl035} \code{svl036} \code{svl037} \code{svl039} \code{svl041} \code{svl103} \code{svl106} \code{svl108} \code{svl109} \code{svl110} \code{svn007} \code{svn015}}
    \item{\code{Patient_ID}}{a factor with levels \code{new_york_1} \code{New_York_2} \code{New_York_3} \code{Norway_06FU} \code{Norway_10} \code{Norway_100} \code{Norway_101} \code{Norway_102} \code{Norway_104} \code{Norway_109} \code{Norway_11} \code{Norway_111} \code{Norway_112} \code{Norway_12} \code{Norway_14} \code{Norway_15} \code{Norway_16} \code{Norway_17} \code{Norway_18} \code{Norway_19} \code{Norway_2} \code{Norway_21} \code{Norway_22} \code{Norway_24} \code{Norway_26} \code{Norway_27} \code{Norway_29} \code{Norway_32} \code{Norway_37} \code{Norway_39} \code{Norway_4} \code{Norway_41} \code{Norway_43} \code{Norway_47} \code{Norway_48} \code{Norway_5} \code{Norway_51} \code{Norway_53} \code{Norway_55} \code{Norway_56} \code{Norway_57} \code{Norway_6} \code{Norway_61} \code{Norway_63} \code{Norway_64} \code{Norway_65} \code{Norway_7} \code{Norway_74} \code{Norway_75} \code{Norway_8} \code{Norway_80} \code{Norway_81} \code{Norway_83} \code{Norway_85} \code{Norway_90} \code{Norway_92} \code{Norway_95} \code{Norway_96} \code{Norway_98} \code{Norway_FU01} \code{Norway_FU02} \code{Norway_FU04} \code{Norway_FU05} \code{Norway_FU07} \code{Norway_FU08} \code{Norway_FU09} \code{Norway_FU10} \code{Norway_FU11} \code{Norway_FU12} \code{Norway_FU14} \code{Norway_FU15} \code{Norway_FU16} \code{Norway_FU17} \code{Norway_FU18} \code{Norway_FU19} \code{Norway_FU20} \code{Norway_FU22} \code{Norway_FU23} \code{Norway_FU24} \code{Norway_FU25} \code{Norway_FU26} \code{Norway_FU27} \code{Norway_FU29} \code{Norway_FU30} \code{Norway_FU35} \code{Norway_FU37} \code{Norway_FU39} \code{Norway_FU40} \code{Norway_FU41} \code{Norway_FU43} \code{Norway_FU44} \code{Norway_FU45} \code{Norway_H2} \code{Norway_H3} \code{Norway_H4} \code{Norway_H5} \code{Norway_H6} \code{Stanford_14} \code{Stanford_16} \code{Stanford_17} \code{Stanford_18} \code{Stanford_2} \code{Stanford_23} \code{Stanford_24} \code{Stanford_31} \code{Stanford_35} \code{Stanford_38} \code{Stanford_4} \code{Stanford_40} \code{Stanford_44} \code{Stanford_45} \code{Stanford_46} \code{Stanford_48} \code{Stanford_6} \code{Stanford_A}}
    \item{\code{Sample_ID}}{a factor with levels \code{BC102B-BE} \code{BC104A-BE} \code{BC105A-BE} \code{BC106B-BE} \code{BC107B-BE} \code{BC108A-BE} \code{BC110B-BE} \code{BC111A-BE} \code{BC111B-BE} \code{BC112B-BE} \code{BC114A-BE} \code{BC115B-BE} \code{BC116A-BE} \code{BC117A-BE} \code{BC118B-BE} \code{BC119A-BE} \code{BC120A-BE} \code{BC121B-BE} \code{BC123B-BE} \code{BC124A-BE} \code{BC1257} \code{BC125A-BE} \code{BC1369} \code{BC14} \code{BC16} \code{BC17} \code{BC18} \code{BC2} \code{BC201B-BE} \code{BC205A-BE} \code{BC206A-BE} \code{BC208A-BE} \code{BC210B-AF} \code{BC213B-BE} \code{BC214B-BE} \code{BC23} \code{BC24} \code{BC303B-BE} \code{BC305A-BE} \code{BC307B-BE} \code{BC308B-BE} \code{BC309A-BE} \code{BC31-0} \code{BC35-0} \code{BC38} \code{BC40} \code{BC402B-BE} \code{BC404B-BE} \code{BC405A-BE} \code{BC406A-2ndTUMOR} \code{BC44} \code{BC45} \code{BC46-LN46} \code{BC48-0} \code{BC4-LN4} \code{BC503B-BE} \code{BC6} \code{BC601A-BE} \code{BC605B-BE} \code{BC606B-AF} \code{BC608B-BE} \code{BC610A-BE} \code{BC702B-BE} \code{BC703B-BE} \code{BC704B-AF} \code{BC706A-BE} \code{BC708B-BE} \code{BC709B-BE} \code{BC710A-BE} \code{BC711B-BE} \code{BC713A-BE} \code{BC790} \code{BC805A-BE} \code{BC807A-BE} \code{BC808A-BE} \code{BC-A} \code{BC-HBC2} \code{BC-HBC3} \code{BC-HBC4-T1} \code{BC-HBC5} \code{BC-HBC6} \code{FU_01-BE} \code{FU_02-BE} \code{FU_04-BE} \code{FU_05-BE} \code{FU_06-BE} \code{FU_07-BE} \code{FU_08-BE} \code{FU_09-BE} \code{FU_10-BE} \code{FU_11-BE} \code{FU_12-BE} \code{FU_14-BE} \code{FU_15-BE} \code{FU_16-BE} \code{FU_17-AF} \code{FU_18-BE} \code{FU_19-BE} \code{FU_20-BE} \code{FU_22-BE} \code{FU_23-BE} \code{FU_24-BE} \code{FU_25-BE} \code{FU_26-BE} \code{FU_27-BE} \code{FU_29-BE} \code{FU_30-AF} \code{FU_35-BE} \code{FU_37-BE} \code{FU_39-BE} \code{FU_40-BE} \code{FU_41-BE} \code{FU_43-BE} \code{FU_44-BE} \code{FU_45-BE}}
    \item{\code{Age_at_diagnosis}}{a numeric vector}
    \item{\code{X.Status_0.A._1.AWD._2.DOD._3.DOC.}}{a numeric vector}
    \item{\code{Overall_survival_.months.}}{a numeric vector}
    \item{\code{Relapse.free_survival_.months.}}{a numeric vector}
    \item{\code{X._ER_status_.0.neg._1.pos..}}{a factor with levels \code{0} \code{1} \code{na}}
    \item{\code{T_.tumor_size.}}{a factor with levels \code{1} \code{2} \code{3} \code{4} \code{na}}
    \item{\code{N_.node_status.}}{a factor with levels \code{0} \code{1} \code{2} \code{na} \code{x}}
    \item{\code{M_.metastasis.}}{a numeric vector}
    \item{\code{Grade}}{a factor with levels \code{1} \code{2} \code{3} \code{na}}
    \item{\code{Histology}}{a factor with levels \code{DCIS} \code{Ductal} \code{Lobular} \code{Mucinous} \code{Papillary} \code{Pleomorph} \code{Undifferentiated}}
    \item{\code{reference_sample_batch_ID_}}{a factor with levels \code{CRA} \code{CRB} \code{CRD} \code{CRF} \code{CRG}}
    \item{\code{Microarray_batch..genes}}{a factor with levels \code{shac-23k} \code{shaz-49k} \code{shbg-49k} \code{shby-43k} \code{svcc-8k} \code{svj-8k} \code{svl-8k} \code{svn-8k}}
    \item{\code{Comments}}{a factor}
    
\item{\code{Previously_published}}{a factor with levels \code{no} \code{yes}}
  }
}

\source{
\url{http://genome-www.stanford.edu/breast_cancer/robustness/data/SupplTable2.xls}
}
\references{
Yasrebi H,  Sperisen P,  Praz V,  Bucher P, 2009 Can Survival Prediction Be Imp
roved By Merging Gene Expression Data Sets?. PLoS ONE 4(10): e7431. doi:10.1371/journal.pone.0007431 

}
\seealso{
\code{\link{gse4335}} 
}

\examples{
data(gse4335pheno)
}
\keyword{datasets}
