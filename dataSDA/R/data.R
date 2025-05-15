#' @name lackinfo.int
#' @title Lack of information questionnaire interval dataset.
#' @description
#' Lack of information questionnaire interval dataset generated from lackinfo dataset.
#' A dataset containing some biographical data and the responses
#' to 5 items measuring the perception of lack of information in
#' a questionnaire.
#'
#' @details
#' An educational innovation project was carried out for improving
#' teaching-learning processes at the University of Oviedo (Spain)
#' for the 2020/2021 academic year. A total of 50 students have been
#' requested to answer an online questionnaire about some biographical
#' data (sex and age) and their perception of lack of information by selecting the
#' interval that best represents their level of agreement to the
#' statements proposed in a interval-valued scale bounded between 1 and 7,
#' where 1 represents the option 'strongly disagree' and 7 represents the option
#' 'strongly agree'.
#'
#' These are the 5 items used to measure the perception of lack of information:
#' \itemize{
#'     \item I1: I receive too little information from my classmates.
#'     \item I2: It is difficult to receive relevant information from my classmates.
#'     \item I3: It is difficult to receive relevant information from the teacher.
#'     \item I4: The amount of information I receive from my classmates is very low.
#'     \item I5: The amount of information I receive from the teacher is very low.
#' }
#'
#' @usage data(lackinfo.int)
#' @format
#' A data frame with 50 observations of the following 8 variables:
#' \itemize{
#'     \item \code{id}: identification number.
#'     \item \code{sex}: sex of the respondent (\code{male} or \code{female}).
#'     \item \code{age}: respondent's age (in years).
#'     \item \code{item1}: respondent's interval-valued answer to item 1.
#'     \item \code{item2}: respondent's interval-valued answer to item 2.
#'     \item \code{item3}: respondent's interval-valued answer to item 3.
#'     \item \code{item4}: respondent's interval-valued answer to item 4.
#'     \item \code{item5}: respondent's interval-valued answer to item 5.
#' }
#'
#' @examples
#' data(lackinfo.int)
#' @keywords datasets
#' @source \url{https://CRAN.R-project.org/package=IntervalQuestionStat}
"lackinfo.int"

#' @name ohtemp.int
#' @title 30 year trimmed mean daily temperatures interval dataset for the Ohio river basin.
#' @description
#' 30 year trimmed mean daily temperatures interval dataset for the Ohio river basin generated from ohtemp dataset.
#' Intervals are defined by the mean daily maximum and minimum temperatures for
#' the Ohio river basin from January 1, 1988 - December 31, 2018. The 116
#' observations in this dataset all had at least 300 daily observations of
#' temperature in at least 30 of the 31 considered years. The mean was
#' calculated after trimming 10% of the data in the tails to remove the
#' influence of potential outliers.
#' @usage data(ohtemp.int)
#' @format A data frame with 161 rows and 7 variables:
#' \itemize{
#'     \item \code{ID}: The global historical climatological network (GHCN) station identifier
#'     \item \code{NAME}: The GHCN station name
#'     \item \code{STATE}: The two-digit designation for the state in which each station resides
#'     \item \code{LATITUDE}: Latitude coordinate position
#'     \item \code{LONGITUDE}: Longitude coordinate position
#'     \item \code{ELEVATION}: Elevation of the measurement location (meters)
#'     \item \code{TEMPERATURE}: The 30 year mean daily temperature (tenths of degrees Celsius)
#' }
#' @examples
#' data(ohtemp.int)
#' @keywords datasets
#' @source \url{https://CRAN.R-project.org/package=intkrige}
"ohtemp.int"

#' @name soccer.bivar.int
#' @title Soccer bivar Interval Data Set
#' @description
#' Soccer bivar interval dataset generated from soccer.bivar dataset.
#' A real interval-valued data set.
#'
#' @format A data frame with 20 rows and 3 variables:
#' \itemize{
#'     \item \code{y}: The response variable Y (weight)
#'     \item \code{t1}: The explanatory variable T1 (height)
#'     \item \code{t2}: The explanatory variable T2 (age)
#' }
#'
#' @details This data set concerns the record of the Weight (Y),
#' Height (T1) and Age (T2) from 20 soccer teams of the premiere French championship.
#'
#' @references
#' Lima Neto, E. A., Cordeiro, G. and De Carvalho, F.A.T. (2011).
#' Bivariate symbolic regression models for interval-valued variables.
#' Journal of Statistical Computation and Simulation (Print), 81, 1727–1744.
#' @examples
#' data(soccer.bivar.int)
#' @keywords datasets
#' @source \url{https://CRAN.R-project.org/package=iRegression}
"soccer.bivar.int"

#' @name Cars.int
#' @title Cars Interval Dataset
#' @description
#' Cars Interval Dataset generated from Cars dataset.
#' This data set consist of the intervals for four characteristics
#' (Price, EngineCapacity, TopSpeed and Acceleration) of 27 cars models partitioned into
#' four different classes (Utilitarian, Berlina, Sportive and Luxury).
#' @format
#' A data frame containing 27 observations on 5 variables, the first five with the interval
#' characteristics for 27 car models, the last one a factor indicating the model class.
#' @usage data(Cars.int)
#' @examples
#' data(Cars.int)
#' @keywords datasets
#' @source \url{https://CRAN.R-project.org/package=MAINT.Data}
"Cars.int"

#' @name ChinaTemp.int
#' @title China Temperatures Interval Dataset
#' @description
#' China Temperatures Interval Dataset generated from ChinaTemp dataset.
#' This data set consist of the intervals of observed temperatures (Celsius scale) in
#' each of the four quarters, Q_1 to Q_4, of the years 1974 to 1988 in 60 chinese
#' meteorologic stations; one outlier observation (YinChuan_1982) has been discarded.
#' The 60 stations belong to different regions in China,
#' which therefore define a partition of the 899 stations-year combinations.
#' @format
#' A data frame containing 899 observations on 5 variables, the first four with the temperatures by quarter in the 899 stations-year
#' combinations, the last one a factor indicating the geographic region of each station.
#' @usage data(ChinaTemp.int)
#' @examples
#' data(ChinaTemp.int)
#' @keywords datasets
#' @source \url{https://CRAN.R-project.org/package=MAINT.Data}
"ChinaTemp.int"

#' @name LoansbyPurpose.int
#' @title Loans by purpose: Interval Dataset
#' @description
#' Loans by purpose interval dataset generated from LoansbyPurpose dataset.
#' This data set consist of the lower and upper bounds of the intervals for four
#' interval characteristics of the loans aggregated by their purpose.
#' The original microdata is available at the Kaggle Data Science platform and consists
#' of 887 383 loan records characterized by 75 descriptors.
#' Among the large set of variables available, we focus on borrowers'
#' income and account and loan information aggregated by the 14 loan purposes, wich are
#' considered as the units of interest.
#'
#' @format A data frame containing 14 observations on the following 4 variables:
#' \itemize{
#'     \item \code{ln-inc}: The current loan purpose of natural logarithm of the self-reported annual income provided by the borrower during registration
#'     \item \code{ln-revolbal}: The current loan purpose of natural logarithm of the total credit revolving balance
#'     \item \code{open-acc}: The current loan purpose of the number of open credit lines in the borrower's credit file
#'     \item \code{total-acc}: The current loan purpose, of the total number of credit lines currently in the borrower's credit file
#' }
#'
#' @usage data(LoansbyPurpose.int)
#' @examples
#' data(LoansbyPurpose.int)
#' @keywords datasets
#' @source
#'     \url{https://CRAN.R-project.org/package=MAINT.Data}
"LoansbyPurpose.int"

#' @name nycflights.int
#' @title New York City flights Interval Dataset
#' @description
#' New York City flights interval dataset generated from nycflights dataset.
#' A interval-valued data set containing 142 units and four interval-valued variables
#' (dep_delay, arr_delay, air_time and distance), created from from the flights data set in the R package
#' nycflights13 (on-time data for all flights that departed the JFK, LGA or EWR airports in 2013), after
#' removing all rows with missing observations, and aggregating by month and carrier.
#' @format
#'
#'  \describe{
#'  \item{FlightsDF}{A data frame containing the original 327346 valid (i.e. with non missing values) flights from the nycflights13 package, described by the 4 variables: dep_delay, arr_delay, air_time and distance.}
#'  \item{FlightsUnits}{A factor with 327346 observations and 142 levels, indicating the month by carrier combination to which each orginal flight belongs to.}
#'  \item{FlightsIdt}{An IData object with 142 observations and 4 interval-valued variables, describing the intervals formed by agregating the FlightsDF microdata by the 0.05 and 0.95 quantiles of the subsamples formed by FlightsUnits factor.}
#'
#' }
#' @usage data(nycflights.int)
#' @references Duarte Silva, A. P., Brito, P., Filzmoser, P., & Dias, J. G. (2021). MAINT. Data: Modelling
#' and Analysing Interval Data in R. R Journal, 13(2).
#' @examples
#' data(nycflights.int)
#' @keywords datasets
#' @source
#'     \url{https://CRAN.R-project.org/package=MAINT.Data}
"nycflights.int"

#' @name mushroom
#' @title Mushroom Data Set
#' @description The mushroom data set consists of a set of 23 species described by 3 interval variables.
#' These mushroom species are members of the genus Agaricies.
#' The specific variables and their values are extracted from the Fungi of California Species.
#'
#' @format A data frame with 23 observations and 5 variables named Species, Pileus Cap
#' Width, Stipe Length, Stipe Thickness, and Edibility.
#'
#' \itemize{
#'     \item \code{Species}: The class of mushroom.
#'     \item \code{Pileus Cap Width}: The pileus cap width of the mushroom.
#'     \item \code{Stipe Length}: The stipe length of the mushroom.
#'     \item \code{Stipe Thickness}: The stipe thickness of the mushroom.
#'     \item \code{Edibility}: The edibility of mushroom (U: unknown, Y:  Yes, N: No, T: Toxic).
#' }
#'
#' @usage data(mushroom)
#' @references Billard L. and  Diday E. (2006).Symbolic data analysis:
#' Conceptual statistics and data mining. Wiley, Chichester.
#' @examples
#' data(mushroom)
#' @keywords datasets
#' @source
#'     Billard, L. and Diday, E. (2006) Symbolic Data Analysis: Conceptual Statistics and Data Mining John Wiley & Sons, Ltd.
"mushroom"

#' @name mushroom.int
#' @title Mushroom Interval Dataset
#' @description
#' Mushroom interval dataset generated from mushroom dataset.
#' The mushroom data set consists of a set of 23 species described by 3 interval variables.
#' These mushroom species are members of the genus Agaricies.
#' The specific variables and their values are extracted from the Fungi of California Species.
#'
#' @format A data frame with 23 observations and 5 variables named Species, Pileus Cap
#' Width, Stipe Length, Stipe Thickness, and Edibility.
#'
#' \itemize{
#'     \item \code{Species}: The class of mushroom.
#'     \item \code{Pileus Cap Width}: The pileus cap width of the mushroom.
#'     \item \code{Stipe Length}: The stipe length of the mushroom.
#'     \item \code{Stipe Thickness}: The stipe thickness of the mushroom.
#'     \item \code{Edibility}: The edibility of mushroom (U: unknown, Y:  Yes, N: No, T: Toxic).
#' }
#'
#' @usage data(mushroom.int)
#' @references Billard L. and  Diday E. (2006).Symbolic data analysis:
#' Conceptual statistics and data mining. Wiley, Chichester.
#' @examples
#' data(mushroom.int)
#' @keywords datasets
#' @source
#'     Billard, L. and Diday, E. (2006) Symbolic Data Analysis: Conceptual Statistics and Data Mining John Wiley & Sons, Ltd.
"mushroom.int"

#' @name age_cholesterol_weight.int
#' @title Age-cholesterol-weight Interval-Valued Dataset
#' @description Age-cholesterol-weight Interval-Valued Dataset.
#' @usage data(age_cholesterol_weight.int)
#' @references Billard L. and  Diday E. (2006).Symbolic data analysis:
#' Conceptual statistics and data mining. Wiley, Chichester.
#' @examples
#' data(age_cholesterol_weight.int)
#' @keywords datasets
"age_cholesterol_weight.int"

#' @name airline_flights
#' @title Airline Flights Dataset
#' @description Airline Flights Dataset.
#' @usage data(airline_flights)
#' @references Billard L. and  Diday E. (2006).Symbolic data analysis:
#' Conceptual statistics and data mining. Wiley, Chichester.
#' @examples
#' data(airline_flights)
#' @keywords datasets
"airline_flights"

#' @name airline_flights2
#' @title Airline Flights Modal-Valued Dataset
#' @description Airline Flights Modal-Valued Dataset.
#' @usage data(airline_flights2)
#' @references Billard L. and  Diday E. (2006).Symbolic data analysis:
#' Conceptual statistics and data mining. Wiley, Chichester.
#' @examples
#' data(airline_flights2)
#' @keywords datasets
"airline_flights2"

#' @name baseball.int
#' @title Baseball Interval-Valued Dataset
#' @description Baseball Interval-Valued Dataset.
#' @usage data(baseball.int)
#' @references Billard L. and  Diday E. (2006).Symbolic data analysis:
#' Conceptual statistics and data mining. Wiley, Chichester.
#' @examples
#' data(baseball.int)
#' @keywords datasets
"baseball.int"

#' @name bird.int
#' @title Bird Interval-Valued Dataset
#' @description Bird Interval-Valued Dataset.
#' @usage data(bird.int)
#' @references Billard L. and  Diday E. (2006).Symbolic data analysis:
#' Conceptual statistics and data mining. Wiley, Chichester.
#' @examples
#' data(bird.int)
#' @keywords datasets
"bird.int"

#' @name blood_pressure.int
#' @title Blood Pressure Interval-Valued Dataset
#' @description blood pressure Interval-Valued Dataset.
#' @usage data(blood_pressure.int)
#' @references Billard L. and  Diday E. (2006).Symbolic data analysis:
#' Conceptual statistics and data mining. Wiley, Chichester.
#' @examples
#' data(blood_pressure.int)
#' @keywords datasets
"blood_pressure.int"

#' @name car.int
#' @title Car Interval-Valued Dataset
#' @description Car Interval-Valued Dataset.
#' @usage data(car.int)
#' @references Billard L. and  Diday E. (2006).Symbolic data analysis:
#' Conceptual statistics and data mining. Wiley, Chichester.
#' @examples
#' data(car.int)
#' @keywords datasets
"car.int"

#' @name crime
#' @title Crime demographics Dataset
#' @description Crime demographics Dataset.
#' @usage data(crime)
#' @references Billard L. and  Diday E. (2006).Symbolic data analysis:
#' Conceptual statistics and data mining. Wiley, Chichester.
#' @examples
#' data(crime)
#' @keywords datasets
"crime"

#' @name crime2
#' @title Crime demographics Modal-Valued Dataset
#' @description Crime demographics Modal-Valued Dataset.
#' @usage data(crime2)
#' @references Billard L. and  Diday E. (2006).Symbolic data analysis:
#' Conceptual statistics and data mining. Wiley, Chichester.
#' @examples
#' data(crime2)
#' @keywords datasets
"crime2"

#' @name finance.int
#' @title Finance Interval-Valued Dataset
#' @description Finance Interval-Valued Dataset.
#' @usage data(finance.int)
#' @references Billard L. and  Diday E. (2006).Symbolic data analysis:
#' Conceptual statistics and data mining. Wiley, Chichester.
#' @examples
#' data(finance.int)
#' @keywords datasets
"finance.int"

#' @name fuel_consumption
#' @title Fuel Consumption Dataset
#' @description Fuel Consumption Dataset.
#' @usage data(fuel_consumption)
#' @references Billard L. and  Diday E. (2006).Symbolic data analysis:
#' Conceptual statistics and data mining. Wiley, Chichester.
#' @examples
#' data(fuel_consumption)
#' @keywords datasets
"fuel_consumption"

#' @name health_insurance
#' @title Health Insurance Dataset
#' @description Health Insurance Dataset.
#' @usage data(health_insurance)
#' @references Billard L. and  Diday E. (2006).Symbolic data analysis:
#' Conceptual statistics and data mining. Wiley, Chichester.
#' @examples
#' data(health_insurance)
#' @keywords datasets
"health_insurance"

#' @name health_insurance2
#' @title Health Insurance Modal-Valued Dataset
#' @description Health Insurance Modal-Valued Dataset.
#' @usage data(health_insurance2)
#' @references Billard L. and  Diday E. (2006).Symbolic data analysis:
#' Conceptual statistics and data mining. Wiley, Chichester.
#' @examples
#' data(health_insurance2)
#' @keywords datasets
"health_insurance2"

#' @name hierarchy
#' @title Hierarchy Dataset
#' @description Hierarchy Dataset.
#' @usage data(hierarchy)
#' @references Billard L. and  Diday E. (2006).Symbolic data analysis:
#' Conceptual statistics and data mining. Wiley, Chichester.
#' @examples
#' data(hierarchy)
#' @keywords datasets
"hierarchy"

#' @name hierarchy.int
#' @title Hierarchy Interval-Valued Dataset
#' @description Hierarchy Interval-Valued Dataset.
#' @usage data(hierarchy.int)
#' @references Billard L. and  Diday E. (2006).Symbolic data analysis:
#' Conceptual statistics and data mining. Wiley, Chichester.
#' @examples
#' data(hierarchy.int)
#' @keywords datasets
"hierarchy.int"

#' @name horses.int
#' @title Horses Interval-Valued Dataset
#' @description Horses Interval-Valued Dataset.
#' @usage data(horses.int)
#' @references Billard L. and  Diday E. (2006).Symbolic data analysis:
#' Conceptual statistics and data mining. Wiley, Chichester.
#' @examples
#' data(horses.int)
#' @keywords datasets
"horses.int"

#' @name occupations
#' @title Occupation Salaries Dataset
#' @description Occupation Salaries Dataset.
#' @usage data(occupations)
#' @references Billard L. and  Diday E. (2006).Symbolic data analysis:
#' Conceptual statistics and data mining. Wiley, Chichester.
#' @examples
#' data(occupations)
#' @keywords datasets
"occupations"

#' @name occupations2
#' @title Occupation Salaries Modal-Valued Dataset
#' @description Occupation Salaries Modal-Valued Dataset.
#' @usage data(occupations)
#' @references Billard L. and  Diday E. (2006).Symbolic data analysis:
#' Conceptual statistics and data mining. Wiley, Chichester.
#' @examples
#' data(occupations2)
#' @keywords datasets
"occupations2"

#' @name profession.int
#' @title Profession Work Salary Time Interval-Valued Dataset
#' @description Profession Work Salary Time Interval-Valued Dataset.
#' @usage data(profession.int)
#' @references Billard L. and  Diday E. (2006).Symbolic data analysis:
#' Conceptual statistics and data mining. Wiley, Chichester.
#' @examples
#' data(profession.int)
#' @keywords datasets
"profession.int"

#' @name veterinary.int
#' @title Veterinary Interval-Valued Dataset
#' @description Veterinary Interval-Valued Dataset.
#' @usage data(veterinary.int)
#' @references Billard L. and  Diday E. (2006).Symbolic data analysis:
#' Conceptual statistics and data mining. Wiley, Chichester.
#' @examples
#' data(veterinary.int)
#' @keywords datasets
"veterinary.int"

#' @name Abalone.iGAP
#' @title Abalone iGAP format Dataset
#' @description A interval-valued data set containing 24 units,
#' created from from the Abalone dataset (UCI Machine Learning Repository), after aggregating by sex and age.
#' @usage data(Abalone.iGAP)
#' @references Billard L. and  Diday E. (2006).Symbolic data analysis:
#' Conceptual statistics and data mining. Wiley, Chichester.
#' @examples
#' data(Abalone.iGAP)
#' @keywords datasets
"Abalone.iGAP"

#' @name Abalone
#' @title Abalone Dataset
#' @description A interval-valued data set containing 24 units,
#' created from from the Abalone dataset (UCI Machine Learning Repository), after aggregating by sex and age.
#' @usage data(Abalone)
#' @references Billard L. and  Diday E. (2006).Symbolic data analysis:
#' Conceptual statistics and data mining. Wiley, Chichester.
#' @examples
#' data(Abalone)
#' @keywords datasets
"Abalone"

#' @name Face.iGAP
#' @title Face iGAP format Dataset
#' @description Symbolic data matrix with all the variables of interval type.
#' @usage data(Face.iGAP)
#' @references Billard L. and  Diday E. (2006).Symbolic data analysis:
#' Conceptual statistics and data mining. Wiley, Chichester.
#' @examples
#' data(Face.iGAP)
#' @keywords datasets
"Face.iGAP"
