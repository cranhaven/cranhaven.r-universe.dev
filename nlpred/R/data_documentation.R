
#' adult
#' 
#' The "Adult" data set from UCI machine learning repository. Raw data have been processed
#' and an \code{outcome} column added. 
#' 
#' Description (copied from UCI):
#' 
#' Extraction was done by Barry Becker from the 1994 Census database. A set of reasonably clean records was extracted using the following conditions: ((AAGE>16) && (AGI>100) && (AFNLWGT>1)&& (HRSWK>0)) 
#' 
#' Prediction task is to determine whether a person makes over 50K a year (column \code{outcome}). 
#'
#' Listing of attributes: 
#' 
#' >50K, <=50K 
#' 
#' age: continuous. 
#' 
#' workclass: Private, Self-emp-not-inc, Self-emp-inc, Federal-gov, Local-gov, State-gov, Without-pay, Never-worked. 
#' 
#' fnlwgt: continuous. 
#' 
#' education: Bachelors, Some-college, 11th, HS-grad, Prof-school, Assoc-acdm, Assoc-voc, 9th, 7th-8th, 12th, Masters, 1st-4th, 10th, Doctorate, 5th-6th, Preschool. 
#' 
#' education-num: continuous. 
#' 
#' marital-status: Married-civ-spouse, Divorced, Never-married, Separated, Widowed, Married-spouse-absent, Married-AF-spouse. 
#' 
#' occupation: Tech-support, Craft-repair, Other-service, Sales, Exec-managerial, Prof-specialty, Handlers-cleaners, Machine-op-inspct, Adm-clerical, Farming-fishing, Transport-moving, Priv-house-serv, Protective-serv, Armed-Forces. 
#' 
#' relationship: Wife, Own-child, Husband, Not-in-family, Other-relative, Unmarried. 
#' 
#' race: White, Asian-Pac-Islander, Amer-Indian-Eskimo, Other, Black. 
#' 
#' sex: Female, Male. 
#' 
#' capital-gain: continuous. 
#' 
#' capital-loss: continuous. 
#' 
#' hours-per-week: continuous. 
#' 
#' native-country: United-States, Cambodia, England, Puerto-Rico, Canada, Germany, Outlying-US(Guam-USVI-etc), India, Japan, Greece, South, China, Cuba, Iran, Honduras, Philippines, Italy, Poland, Jamaica, Vietnam, Mexico, Portugal, Ireland, France, Dominican-Republic, Laos, Ecuador, Taiwan, Haiti, Columbia, Hungary, Guatemala, Nicaragua, Scotland, Thailand, Yugoslavia, El-Salvador, Trinadad&Tobago, Peru, Hong, Holand-Netherlands.
#' 
#' @name adult
#' @docType data
#' @source \url{https://archive.ics.uci.edu/ml/datasets/Adult}
#' @references \url{http://robotics.stanford.edu/~ronnyk/nbtree.pdf}
#' @keywords data
NULL


#' bank 
#' 
#' Bank data from UCI Machine Learning Repository. The raw bank data have been processed
#' and an \code{outcome} column added. 
#' 
#' Description (copied from UCI):
#' 
#' The data is related with direct marketing campaigns of a Portuguese banking institution. The marketing campaigns were based on phone calls. Often, more than one contact to the same client was required, in order to access if the product (bank term deposit) would be ('yes') or not ('no') subscribed. 
#' There are four datasets: 
#' 
#' 1) (included in \code{predtmle}) bank-additional-full.csv with all examples (41188) and 20 inputs, ordered by date (from May 2008 to November 2010), very close to the data analyzed in [Moro et al., 2014]
#' 
#' 2) bank-additional.csv with 10\% of the examples (4119), randomly selected from 1), and 20 inputs.
#' 
#' 3) bank-full.csv with all examples and 17 inputs, ordered by date (older version of this dataset with less inputs). 
#' 
#' 4) bank.csv with 10\% of the examples and 17 inputs, randomly selected from 3 (older version of this dataset with less inputs). 
#' 
#' The smallest datasets are provided to test more computationally demanding machine learning algorithms (e.g., SVM). 
#' The classification goal is to predict if the client will subscribe (yes/no) a term deposit (variable y).
#' 
#' Attribute Information:
#' 
#' Input variables:
#' 
#' # bank client data:
#' 
#' 1 - age (numeric)
#' 
#' 2 - job : type of job (categorical: 'admin.','blue-collar','entrepreneur','housemaid','management','retired','self-employed','services','student','technician','unemployed','unknown')
#' 
#' 3 - marital : marital status (categorical: 'divorced','married','single','unknown'; note: 'divorced' means divorced or widowed)
#' 
#' 4 - education (categorical: 'basic.4y','basic.6y','basic.9y','high.school','illiterate','professional.course','university.degree','unknown')
#' 
#' 5 - default: has credit in default? (categorical: 'no','yes','unknown')
#' 6 - housing: has housing loan? (categorical: 'no','yes','unknown')
#' 
#' 7 - loan: has personal loan? (categorical: 'no','yes','unknown')
#' 
#' # related with the last contact of the current campaign:
#' 
#' 8 - contact: contact communication type (categorical: 'cellular','telephone') 
#' 
#' 9 - month: last contact month of year (categorical: 'jan', 'feb', 'mar', ..., 'nov', 'dec')
#' 
#' 10 - day_of_week: last contact day of the week (categorical: 'mon','tue','wed','thu','fri')
#' 
#' 11 - duration: last contact duration, in seconds (numeric). Important note: this attribute highly affects the output target (e.g., if duration=0 then y='no'). Yet, the duration is not known before a call is performed. Also, after the end of the call y is obviously known. Thus, this input should only be included for benchmark purposes and should be discarded if the intention is to have a realistic predictive model.
#' 
#' # other attributes:
#' 
#' 12 - campaign: number of contacts performed during this campaign and for this client (numeric, includes last contact)
#' 
#' 13 - pdays: number of days that passed by after the client was last contacted from a previous campaign (numeric; 999 means client was not previously contacted)
#' 
#' 14 - previous: number of contacts performed before this campaign and for this client (numeric)
#' 
#' 15 - poutcome: outcome of the previous marketing campaign (categorical: 'failure','nonexistent','success')
#' 
#' # social and economic context attributes
#' 
#' 16 - emp.var.rate: employment variation rate - quarterly indicator (numeric)
#' 
#' 17 - cons.price.idx: consumer price index - monthly indicator (numeric) 
#' 
#' 18 - cons.conf.idx: consumer confidence index - monthly indicator (numeric) 
#' 
#' 19 - euribor3m: euribor 3 month rate - daily indicator (numeric)
#' 
#' 20 - nr.employed: number of employees - quarterly indicator (numeric)
#' 
#' Output variable (desired target):
#' 
#' 21 - y - has the client subscribed a term deposit? (binary: 'yes','no')
#' 
#' @name bank
#' @docType data
#' @source \url{https://archive.ics.uci.edu/ml/datasets/Bank+Marketing}
#' @references S. Moro, P. Cortez and P. Rita. A Data-Driven Approach to Predict the Success of Bank Telemarketing. Decision Support Systems, Elsevier, 62:22-31, June 2014
#' @keywords data
NULL

#' Cardiotocography 
#' 
#' Cardiotocography data from UCI machine learning repository. Raw data have been 
#' cleaned and an \code{outcome} column added that is a binary variable of predicting
#' NSP (described below) = 2. 
#' 
#' Data Set Information:
#' 2126 fetal cardiotocograms (CTGs) were automatically processed and the respective diagnostic features measured. 
#' The CTGs were also classified by three expert obstetricians and a consensus classification label assigned to each of them. Classification was both with respect to a morphologic pattern (A, B, C. ...) and to a fetal state (N, S, P). Therefore the dataset can be used either for 10-class or 3-class experiments.
#' 
#' Attribute Information:
#' 
#' LB - FHR baseline (beats per minute) 
#' 
#' AC - # of accelerations per second 
#' 
#' FM - # of fetal movements per second 
#' 
#' UC - # of uterine contractions per second 
#' 
#' DL - # of light decelerations per second 
#' 
#' DS - # of severe decelerations per second 
#' 
#' DP - # of prolongued decelerations per second 
#' 
#' ASTV - percentage of time with abnormal short term variability 
#' 
#' MSTV - mean value of short term variability 
#' 
#' ALTV - percentage of time with abnormal long term variability 
#' 
#' MLTV - mean value of long term variability 
#' 
#' Width - width of FHR histogram 
#' 
#' Min - minimum of FHR histogram 
#' 
#' Max - Maximum of FHR histogram 
#' 
#' Nmax - # of histogram peaks 
#' 
#' Nzeros - # of histogram zeros 
#' 
#' Mode - histogram mode 
#' 
#' Mean - histogram mean 
#' 
#' Median - histogram median 
#' 
#' Variance - histogram variance 
#' 
#' Tendency - histogram tendency 
#' 
#' CLASS - FHR pattern class code (1 to 10) 
#' 
#' NSP - fetal state class code (N=normal; S=suspect; P=pathologic)
#' 
#' @name cardio
#' @docType data
#' @keywords data
#' @references Ayres de Campos et al. (2000) SisPorto 2.0 A Program for Automated Analysis of Cardiotocograms. J Matern Fetal Med 5:311-318 
#' @source \url{https://archive.ics.uci.edu/ml/datasets/Cardiotocography}
NULL

#' drugs
#' 
#' "Drug consumption (quantified) Data Set" from UCI Machine Learning Repository. 
#' Raw data have been processed and an \code{outcome} (heroin use) column added.
#' 
#' Data Set Information (copied from UCI library):
#' 
#' Database contains records for 1885 respondents. For each respondent 12 attributes are known: Personality measurements which include NEO-FFI-R (neuroticism, extraversion, openness to experience, agreeableness, and conscientiousness), BIS-11 (impulsivity), and ImpSS (sensation seeking), level of education, age, gender, country of residence and ethnicity. All input attributes are originally categorical and are quantified. After quantification values of all input features can be considered as real-valued. In addition, participants were questioned concerning their use of 18 legal and illegal drugs (alcohol, amphetamines, amyl nitrite, benzodiazepine, cannabis, chocolate, cocaine, caffeine, crack, ecstasy, heroin, ketamine, legal highs, LSD, methadone, mushrooms, nicotine and volatile substance abuse and one fictitious drug (Semeron) which was introduced to identify over-claimers. For each drug they have to select one of the answers: never used the drug, used it over a decade ago, or in the last decade, year, month, week, or day.
#' 
#' Database contains 18 classification problems. Each of independent label variables contains seven classes: "Never Used", "Used over a Decade Ago", "Used in Last Decade", "Used in Last Year", "Used in Last Month", "Used in Last Week", and "Used in Last Day".
#' 
#' Problem which can be solved:
#' 
#' * Seven class classifications for each drug separately.
#' 
#' * Problem can be transformed to binary classification by union of part of classes into one new class. For example, "Never Used", "Used over a Decade Ago" form class "Non-user" and all other classes form class "User".
#' 
#' * The best binarization of classes for each attribute.
#' 
#' * Evaluation of risk to be drug consumer for each drug.
#' 
#' Detailed description of database and process of data quantification are presented in E. Fehrman, A. K. Muhammad, E. M. Mirkes, V. Egan and A. N. Gorban, "The Five Factor Model of personality and evaluation of drug consumption risk.," arXiv [Web Link], 2015
#' 
#' Paper above solve binary classification problem for all drugs. For most of drugs sensitivity and specificity are greater than 75\%.
#' 
#' @name drugs
#' @docType data
#' @keywords data
#' @source \url{https://archive.ics.uci.edu/ml/datasets/Drug+consumption+\%28quantified\%29}
#' @references \url{https://arxiv.org/abs/1506.06297}
NULL

#' wine
#' 
#' "Wine Quality" data set from UCI Machine Learning Repository. The red and white wine data sets
#' have been combined with an added attribute for red vs. white. 
#' 
#' Data Set Information (copied from UCI):
#' 
#' The two datasets are related to red and white variants of the Portuguese "Vinho Verde" wine. For more details, consult: [Web Link] or the reference [Cortez et al., 2009]. Due to privacy and logistic issues, only physicochemical (inputs) and sensory (the output) variables are available (e.g. there is no data about grape types, wine brand, wine selling price, etc.). 
#' 
#' These datasets can be viewed as classification or regression tasks. The classes are ordered and not balanced (e.g. there are munch more normal wines than excellent or poor ones). Outlier detection algorithms could be used to detect the few excellent or poor wines. Also, we are not sure if all input variables are relevant. So it could be interesting to test feature selection methods. 
#' 
#' Attribute Information:
#' 
#' For more information, read [Cortez et al., 2009]. 
#' 
#' Input variables (based on physicochemical tests): 
#' 
#' 1 - fixed acidity 
#' 
#' 2 - volatile acidity 
#' 
#' 3 - citric acid 
#' 
#' 4 - residual sugar 
#' 
#' 5 - chlorides 
#' 
#' 6 - free sulfur dioxide 
#' 
#' 7 - total sulfur dioxide 
#' 
#' 8 - density 
#' 
#' 9 - pH 
#' 
#' 10 - sulphates 
#' 
#' 11 - alcohol 
#' 
#' Output variable (based on sensory data): 
#' 
#' 12 - quality (score between 0 and 10)
#' 
#' @source \url{https://archive.ics.uci.edu/ml/datasets/Wine+Quality}
#' @references P. Cortez, A. Cerdeira, F. Almeida, T. Matos and J. Reis. Modeling wine preferences by data mining from physicochemical properties. In Decision Support Systems, Elsevier, 47(4):547-553, 2009. 
#' @references \url{https://doi.org/10.1016/j.dss.2009.05.016}
#' @name wine
#' @keywords data
#' @docType data
NULL