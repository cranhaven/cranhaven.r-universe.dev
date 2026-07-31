#' Alcohol data
#'
#' Lemmens , Knibbe and Tan(1988) described a study of self reported alcohol frequencies.
#' The no of alcohol consumption data in two reference weeks is separately self reported
#' by a randomly selected sample of 399 respondents in the Netherlands in 1983.
#' Number of days a  given individual consumes alcohol out of 7 days a week can be treated as a binomial
#' variable.
#' The collection of all such variables from all respondents would be defined as "Binomial Outcome Data".
#'
#' @format A data frame with 3 columns and 8 rows.
#' \describe{
#' \item{\code{Days}}{No of Days Drunk}
#' \item{\code{week1}}{Observed frequencies for week1}
#' \item{\code{week2}}{Observed frequencies for week2}
#' }
#'
#' @examples
#' Alcohol_data$Days          # extracting the binomial random variables
#' sum(Alcohol_data$week2)       # summing all the frequencies in week2
#'
#' @source
#' Extracted from
#'
#' Manoj, C., Wijekoon, P. & Yapa, R.D., 2013. The McDonald Generalized Beta-Binomial Distribution: A New
#' Binomial Mixture Distribution and Simulation Based Comparison with Its Nested Distributions in Handling
#' Overdispersion. International Journal of Statistics and Probability, 2(2), pp.24-41.
#'
#' Available at: \doi{10.5539/ijsp.v2n2p24}
#'
 "Alcohol_data"

#' Male children data
#'
#' The number of male children among the first 12 children of family size 13 in 6115 families taken
#' from the hospital records in the nineteenth century Saxony (Sokal & Rohlf(1994), Lindsey (1995),
#' p. 59). The thirteenth child is ignored to assuage the effect of families non-randomly stopping
#' when a desired gender is reached.
#'
#' @format A data frame with 2 columns and 13 rows.
#' \describe{
#' \item{\code{No_of_Males}}{No of Male children among first 12 children of family size 13}
#' \item{\code{freq}}{Observed frequencies for corresponding male children}
#' }
#'
#' @examples
#' Male_Children$No_of_Males   # extracting the binomial random variables
#' sum(Male_Children$freq)     # summing all the frequencies
#'
#' @source
#' Extracted from
#'
#' Borges, P., Rodrigues, J., Balakrishnan, N. and Bazan, J., 2014. A COM-Poisson type
#' generalization of the binomial distribution and its properties and applications.
#' Statistics & Probability Letters, 87, pp.158-166.
#'
#' Available at: \doi{10.1016/j.spl.2014.01.019}
#'
 "Male_Children"


#' Plant Disease Incidence data
#'
#' Cochran(1936) provided a data that comprise the number of tomato spotted wilt virus(TSWV) infected
#' tomato plants in the field trials in Australia. The field map was divided into 160 'quadrats'.
#' 9 tomato plants in each quadrat. then the numbers of TSWV infected tomato plants were counted in each
#' quadrat.
#' Number of infected plants out of 9 plants per quadrat can be treated as a binomial variable.
#' the collection of all such responses from all 160 quadrats would form "binomial outcome data"
#' below provided is a data set similar to Cochran plant disease incidence data.
#' Marcus R(1984). orange trees infected with citrus tristeza virus (CTV) in an orchard in central
#' Israel.
#' We divided the field map into 84 "quadrats" of 4 rows x 3 columns and counted the total number
#' (1981 + 1982) of infected trees out of a maximum of n = 12 in each quadrat
#'
#' @format A data frame with 2 columns and 10 rows
#' \describe{
#' \item{\code{Dis.plant}}{Diseased Plants}
#' \item{\code{fre}}{Observed frequencies}
#' }
#'
#' @examples
#' Plant_DiseaseData$Dis.plant      # extracting the binomial random variables
#' sum(Plant_DiseaseData$fre)       # summing all the frequencies
#'
#' @source
#' Extracted from
#'
#' Hughes, G., 1993. Using the Beta-Binomial Distribution to Describe Aggregated Patterns of Disease Incidence.
#' Phytopathology, 83(9), p.759.
#'
#' Available at: \doi{10.1094/Phyto-83-759}.
#'
"Plant_DiseaseData"


#' Course Data
#'
#' The data refer to the numbers of courses taken by a class of 65 students from the first year of the
#' Department of Statistics of Athens University of Economics. The students enrolled in this class
#' attended 8 courses during the first year of their study. The total numbers of successful
#' examinations (including resits) were recorded.
#'
#' @format A data frame with 2 columns and 9 rows
#' \describe{
#' \item{\code{sub.pass}}{subjects passed}
#' \item{\code{fre}}{Observed frequencies}
#' }
#'
#' @examples
#' Course_data$sub.pass             # extracting the binomial random variables
#' sum(Course_data$fre)             # summing all the frequencies
#'
#' @source
#' Extracted from
#'
#' Karlis, D. & Xekalaki, E., 2008. The Polygonal Distribution. In Advances in Mathematical and Statistical
#' Modeling. Boston: Birkhuser Boston, pp. 21-33.
#'
#' Available at: \doi{10.1007/978-0-8176-4626-4_2}.
#'
"Course_data"


#' Chromosome Data
#'
#' Data in this example refer to 337 observations on the secondary
#' association of chromosomes in Brassika; n , which is now the number of
#' chromosomes, equals 3 and X is the number of pairs of bivalents showing association.
#'
#' @format A data frame with 2 columns and 4 rows
#' \describe{
#' \item{\code{No.of.Asso}}{No of Associations}
#' \item{\code{fre}}{Observed frequencies}
#' }
#'
#' @examples
#' Chromosome_data$No.of.Asso          #extracting the binomial random variables
#' sum(Chromosome_data$fre)            #summing all the frequencies
#'
#' @source
#' Extracted from
#'
#' Paul, S.R., 1985. A three-parameter generalization of the binomial distribution. Communications in
#' Statistics - Theory and Methods, 14(6), pp.1497-1506.
#'
#' Available at: \doi{10.1080/03610928508828990}
"Chromosome_data"


#' Exam Data
#'
#' In an examination, there were 9 questions set on a particular
#' topic. Each question is marked out of a total of 20 and in assessing the
#' final class of a candidate, particular attention is paid to the total
#' number of questions for which he has an "alpha", i.e., at least 15 out of 20,
#' as well as his total number of marks. His number of alpha's is a rough
#' indication of the "quality" of his exam performance. Thus, the distribution
#' of alpha's over the candidates is of interest. There were 209 candidates
#' attempting questions from this section of 9 questions and a total of 326 alpha's
#' was awarded. So we treat 9 as the "litter size", and the dichotomous response
#' is whether or not he got an alpha on the question.
#'
#' @format A data frame with 2 columns and 10 rows
#' \describe{
#' \item{\code{No.of.alpha}}{No of Alphas}
#' \item{\code{fre}}{Observed frequencies}
#' }
#'
#' @examples
#' Exam_data$No.of.alpha              #extracting the binomial random variables
#' sum(Exam_data$fre)                 #summing all the frequencies
#'
#' @source
#' Extracted from
#'
#' Paul, S.R., 1985. A three-parameter generalization of the binomial distribution. Communications in
#' Statistics - Theory and Methods, 14(6), pp.1497-1506.
#'
#' Available at: \doi{10.1080/03610928508828990}
#'
"Exam_data"


#' Terror Data USA
#'
#' Jenkins and Johnson (1975) compiled a chronology of incidents of international terrorism from 1/1968
#' through 04/1974. During this period 507 incidents are recorded in the world, where 64 incidents occurred
#' in the United States and 65 ones in Argentina.
#'
#' @format A data frame with 2 columns and 9 rows
#' \describe{
#' \item{\code{Incidents}}{No of Incidents Occurred}
#' \item{\code{fre}}{Observed frequencies}
#' }
#'
#' @examples
#' Terror_data_USA$Incidents         #extracting the binomial random variables
#' sum(Terror_data_USA$fre)              #summing all the frequencies
#'
#' @source
#' Extracted from
#'
#' Li, X. H., Huang, Y. Y., & Zhao, X. Y. (2011). The Kumaraswamy Binomial Distribution. Chinese Journal
#' of Applied Probability and Statistics, 27(5), 511-521.
#'
"Terror_data_USA"


#' Terror Data ARG
#'
#' Jenkins and Johnson (1975) compiled a chronology of incidents of international terrorism from 1/1968
#' through 04/1974. During this period 507 incidents are recorded in the world, where 64 incidents occurred
#' in the United States and 65 ones in Argentina.
#'
#' @format A data frame with 2 columns and 9 rows
#' \describe{
#' \item{\code{Incidents}}{No of Incidents Occurred}
#' \item{\code{fre}}{Observed frequencies}
#' }
#'
#' @examples
#' Terror_data_ARG$Incidents        #extracting the binomial random variables
#' sum(Terror_data_ARG$fre)              #summing all the frequencies
#'
#' @source
#' Extracted from
#'
#' Li, X. H., Huang, Y. Y., & Zhao, X. Y. (2011). The Kumaraswamy Binomial Distribution. Chinese Journal
#' of Applied Probability and Statistics, 27(5), 511-521.
#'
"Terror_data_ARG"

#' Family Epidemics
#'
#' In this investigation, families of the same size, two parents and three children, living in different
#' circumstances of domestic overcrowding were visited at fortnightly intervals. The date of onset and the clinical
#' nature of upper respiratory infectious experienced by each member of the family were charted on a time scale
#' marked off in days. Family epidemics of acute coryza-or common colds-were thus available for analysis.
#'
#' By inspection of the epidemic time charts, it was possible to identify new or primary introductions of illness
#' into the household by the onset of a cold after a lapse of 10 days since the last such case in the same home.
#' Two such cases occurring on the same or succeeding days were classified as multiple primaries. Thereafter, the
#' links in the epidemic chain of spread were defined by an interval of one day or more between successive cases
#' in the same family. These family epidemics could then be described thus 1-2-1, 1-1-1-0, 2-1-0, etc. It must be
#' emphasized that although this method of classification is somewhat arbitrary, it was completed before the
#' corresponding theoretical distributions were worked out and the interval chosen agrees with the distribution
#' of presumptive incubation periods of the common cold seen in field surveys (e.g. Badger, Dingle, Feller,
#' Hodges, Jordan, and Rammelkamp, 1953).
#'
#' @format A data frame with 6 columns and 5 rows
#' \describe{
#' \item{\code{Cases}}{No of Further Cases}
#' \item{\code{Families}}{No of Families}
#' \item{\code{Father}}{Father with Status of Introducing Cases}
#' \item{\code{Mother}}{Mother with Status of Introducing Cases}
#' \item{\code{SChild}}{School Child with Status of Introducing Cases}
#' \item{\code{PSChild}}{Pre-School Child with Status of Introducing Cases}
#' }
#'
#' @examples
#'
#' Epidemic_Cold$Cases
#' sum(Epidemic_Cold$SChild)
#'
#' @source
#' Extracted from
#'
#' Heasman, M. A. and Reid, D. D. (1961). "Theory and observation in family epidemics of the common cold."
#' Br. J. pleu. SOC. Med., 15, 12-16.
#'
"Epidemic_Cold"

#' @export
.onAttach<-function(libname,pkgname)
{
  packageStartupMessage("Hello, This is Amalan. For more details refer --> https://amalan-constat.github.io/fitODBOD/index.html")
}

#' Binomial Data Extraction from Raw data
#'
#' The below function has the ability to extract from the raw data to Binomial Outcome Data. This
#' function simplifies the data into more presentable way to the user.
#'
#' @usage
#' BODextract(data)
#'
#' @param data                vector of observations
#'
#' @details
#' \strong{NOTE} : If input parameters are not in given domain conditions
#' necessary error messages will be provided to go further
#'
#' @return
#' The output of \code{BODextract} gives a list format consisting
#'
#' \code{RV} binomial random variables in vector form
#'
#' \code{Freq}  corresponding frequencies in vector form
#'
#' @examples
#' datapoints <- sample(0:10,340,replace=TRUE) #creating a sample set of observations
#' BODextract(datapoints)                   #extracting binomial outcome data from observations
#' Random.variable <- BODextract(datapoints)$RV #extracting the binomial random variables
#'
#' @export
BODextract<-function(data)
{
  if(is.null(data)){
    stop("data is empty")
  }

  #checking if inputs consist NA(not assigned)values, infinite values or NAN(not a number)values
  #if so cleaning the data set and removing all NA,infinite and NAN values
  if(any(is.na(data)) | any(is.infinite(data)) |any(is.nan(data)) )
  {
    warning("NA, NAN or infinite values are removed")

    data1<-data[!is.nan(data)]
    data2<-data1[!is.na(data1)]
    data3<-data2[!is.infinite(data2)]
    #assigning the cleaned data into value variable
    value<-table(data3)
    #converting value variable into a matrix
    mat.tab<-as.matrix(value)
    #assigning the binomial random variables from the row names of the matrix
    BOD.Random.variable<-as.integer(row.names(mat.tab))
    #removing the dimension names
    mat.tab<-unname(mat.tab)
    #assigning the frequencies for the corresponding binomial random variables
    BOD.Frequency<-mat.tab[,1]
    # generating an output in list format consisting Random variable and corresponding frequency
    return(list("RV"=BOD.Random.variable,"Freq"=BOD.Frequency))
  }
  else
  {
    #assigning the data into value variable
    value<-table(data)
    #converting value variable into a matrix
    mat.tab<-as.matrix(value)
    #assigning the binomial random variables from the row names of the matrix
    BOD.Random.variable<-as.integer(row.names(mat.tab))
    #removing the dimension names
    mat.tab<-unname(mat.tab)
    #assigning the frequencies for the corresponding binomial random variables
    BOD.Frequency<-mat.tab[,1]
    #generating an output in list format consisting Random variable and corresponding frequency
    return(list("RV"=BOD.Random.variable,"Freq"=BOD.Frequency))
  }
}

#' Fitting the Binomial Distribution when binomial random variable, frequency and probability
#' value are given
#'
#' The function will fit the Binomial distribution when random variables, corresponding
#' frequencies and probability value are given. It will provide the expected frequencies, chi-squared
#' test statistics value, p value and degree of freedom  so that it can be
#' seen if this distribution fits the data.
#'
#' @usage fitBin(x,obs.freq,p=0)
#'
#' @param x                  vector of binomial random variables.
#' @param obs.freq           vector of frequencies.
#' @param p                  single value for probability or zero to estimate p.
#'
#' @details
#' \deqn{x = 0,1,2,...}
#' \deqn{0 < p <= 1}
#' \deqn{obs.freq \ge 0}
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions
#' necessary error messages will be provided to go further.
#'
#' @return
#' The output of \code{fitBin} gives the class format \code{fitB} and \code{fit} consisting a list
#'
#' \code{bin.ran.var} binomial random variables.
#'
#' \code{obs.freq} corresponding observed frequencies.
#'
#' \code{exp.freq} corresponding expected frequencies.
#'
#' \code{statistic} chi-squared test statistics value.
#'
#' \code{df} degree of freedom.
#'
#' \code{p.value} probability value by chi-squared test statistic.
#'
#' \code{fitB} fitted probability values of \code{dbinom}.
#'
#' \code{phat} estimated probability value.
#'
#' \code{call} the inputs of the function.
#'
#' @examples
#' No.D.D <- 0:7      #assigning the random variables
#' Obs.fre.1 <- c(47,54,43,40,40,41,39,95)  #assigning the corresponding frequencies
#'
#' #fitting when the random variable,frequencies are given.
#' fitBin(No.D.D,Obs.fre.1)
#'
#' @export
fitBin<-function(x,obs.freq,p=0)
{
  #checking if inputs consist NA(not assigned)values, infinite values or NAN(not a number)values if so
  #creating an error message as well as stopping the function progress.
  if(any(is.na(c(x,obs.freq,p))) | any(is.infinite(c(x,obs.freq,p))) |
     any(is.nan(c(x,obs.freq,p))) ){
    stop("NA or Infinite or NAN values in the Input")
  }
  #checking if the probability value is less than or equal to zero and greater than one,
  #if so creating an error message as well as stopping the function progress.
  if(p<0 | p>=1)
  {
    stop("Probability value cannot be less than or equal to zero or greater than or equal to one")
  }

  if(p==0)
  {
    i<-1:length(x)
    #estimating the probability value when it is not given
    p.hat<-(sum(x[i]*obs.freq[i]))/(max(x)*sum(obs.freq))
    #estimating the probability values for the given binomial random variables
    est.prob<-stats::dbinom(x,max(x),p.hat)
    #calculating the expected frequencies
    exp.freq<-round((sum(obs.freq)*est.prob),2)
    #chi-squared test statistics is calculated with observed frequency and expected frequency
    statistic<-sum(((obs.freq-exp.freq)^2)/exp.freq)
    #degree of freedom is calculated
    df<-length(x)-2
    #p value of chi-squared test statistic is calculated
    p.value<-1-stats::pchisq(statistic,df)

    #checking if df is less than or equal to zero
    if(df<0 | df==0)
    {
      stop("Degrees of freedom cannot be less than or equal to zero")
    }
    #checking if any of the expected frequencies are less than five and greater than zero, if so
    #a warning message is provided in interpreting the results
    if(min(exp.freq)<5 && min(exp.freq) > 0)
    {
      message("Chi-squared approximation may be doubtful because expected frequency is less than 5")
    }
    #checking if expected frequency is zero, if so providing a warning message in interpreting
    #the results
    if(min(exp.freq)==0)
    {
      message("Chi-squared approximation is not suitable because expected frequency approximates to zero")
    }
    #the final output is in a list format containing the calculated values
    final<-list("bin.ran.var"=x,"obs.freq"=obs.freq,"exp.freq"=exp.freq,"statistic"=round(statistic,4),
                "df"=df,"p.value"=round(p.value,4),"fitB"=est.prob,"phat"=p.hat,"call"=match.call())
  }
  else
  {
    #estimating the probability values for the given binomial random variables
    est.prob<-stats::dbinom(x,max(x),p)
    #calculating the expected frequencies
    exp.freq<-round((sum(obs.freq)*est.prob),2)
    #applying the chi squared test
    ans<-stats::chisq.test(x=obs.freq,p=est.prob)

    #checking if any of the expected frequencies are less than five and greater than zero, if so
    #a warning message is provided in interpreting the results
    if(min(exp.freq)<5 && min(exp.freq) > 0)
    {
      message("Chi-squared approximation may be doubtful because expected frequency is less than 5")
    }
    #checking if expected frequency is zero, if so providing a warning message in interpreting
    #the results
    if(min(exp.freq)==0)
    {
      message("Chi-squared approximation is not suitable because expected frequency approximates to zero")
    }
    #the final output is in a list format containing the calculated values
    final<-list("bin.ran.var"=x,"obs.freq"=obs.freq,"exp.freq"=exp.freq,
                "statistic"=round(ans$statistic,4),"df"=ans$parameter,
                "p.value"=round(ans$p.value,4),"fitB"=est.prob,"phat"=p,
                "call"=match.call())
  }
  class(final)<-c("fitB","fit")
  return(final)
}

#' @method fitBin default
#' @export
fitBin.default<-function(x,obs.freq,p=0)
{
  return(fitBin(x,obs.freq,p=0))
}

#' @method print fitB
#' @export
print.fitB<-function(x,...)
{
  cat("Call: \n")
  print(x$call)
  cat("\nChi-squared test for Binomial Distribution \n\t
      Observed Frequency : ",x$obs.freq,"\n\t
      expected Frequency : ",x$exp.freq,"\n\t
      estimated probability value :",x$phat,"\n\t
      X-squared :",x$statistic,"  ,df :",x$df,"  ,p-value :",x$p.value,"\n")
}

#' @method summary fitB
#' @export
summary.fitB<-function(object,...)
{
  cat("Call: \n")
  print(object$call)
  cat("\nChi-squared test for Binomial Distribution \n\t
      Observed Frequency : ",object$obs.freq,"\n\t
      expected Frequency : ",object$exp.freq,"\n\t
      estimated probability value :",object$phat," \n\t
      X-squared :",object$statistic,"  ,df :",object$df,"  ,p-value :",object$p.value,"\n")
}


#' @importFrom stats dbinom
#' @importFrom stats chisq.test
#' @importFrom stats pchisq
