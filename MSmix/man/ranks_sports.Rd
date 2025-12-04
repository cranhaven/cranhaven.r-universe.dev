\name{ranks_sports}
\alias{ranks_sports}
\docType{data}
\title{Sports Data (complete rankings with covariates)}
\description{The Sports dataset was collected through an on-line questionnaire on sport preferences and habits administered within the 2016 Big Data Analystics in Sports (BDsports) project, developed by the Big and Open Data Innovation Laboratory (BODaI-Lab) of the University of Brescia. A sample of \eqn{N=647} respondents provided their complete rankings of \eqn{n=8} popular sports according to their personal preferences. The sports are:
 1 = Soccer, 2 = Swimming, 3 = Volleyball, 4 = Cycling, 5 = Basket, 6 = Boxe and martial arts, 7 = Tennis and 8 = Jogging. The dataset also includes several covariates concerning respondents' socio-demographics characteristics and other sport-related information.}
\usage{data(ranks_sports)}
\format{
A data frame gathering \eqn{N=647} complete rankings of the sports in the first \eqn{n=8} columns (rank 1 = most preferred item) and individual covariates in the remaining columns. The variables are detailed below:
\describe{
\item{Soccer}{Rank assigned to Soccer.}
\item{Swimming}{Rank assigned to Swimming.}
\item{Volleyball}{Rank assigned to Volleyball.}
\item{Cycling}{Rank assigned to Cycling.}
\item{Basket}{Rank assigned to Basket.}
\item{Boxe_and_martial_arts}{Rank assigned to Boxe and Martial Arts.}
\item{Tennis}{Rank assigned to Tennis.}
\item{Jogging}{Rank assigned to Jogging.}
\item{Gender}{Gender.}
\item{Birth_month}{Month of birth.}
\item{Birth_year}{Year of birth.}
\item{Education}{Education level.}
\item{Residence}{Geographical area of residence.}
\item{Work}{Type of work.}
\item{Smoking}{Smoking status.}
\item{Sport_frequency}{Number of times per week that the respondent plays sports.}
\item{Sport_hours}{Number of hours per week that the respondent watches sports.}
\item{Sport_played}{Sport played by the respondent.}
\item{Personality}{Main aspect of respondent's personality.}
\item{Sport_motivation}{Main reason why the respondent plays sport.}
\item{Sport_type}{Favorite sport type.}
\item{Sport_relationships}{Do you think that sport, especially in team games, allows you to make new friends?}
\item{Water}{Quantity of water consumed per day.}
\item{Alcohol}{Frequency of alcohol consumption.}
\item{Fastfood}{Frequency of fast food consumption.}
\item{Food_supplements}{Opinion about the use of food supplements in sports.}
\item{Massmedia}{Prevalent mass media used to inquire about sport.}
\item{TV_space}{Do you think that sport currently occupies the space it deserves on TV? Rating from 1=definitely not to 7=definitely yes with 4=indifferent.}
\item{Magazine_space}{Do you think that sport currently occupies the space it deserves on the magazines? Rating from 1=definitely not to 7=definitely yes with 4=indifferent.}
\item{Radio_space}{Do you think that sport currently occupies the space it deserves on the radio? Rating from 1=definitely not to 7=definitely yes with 4=indifferent.}
\item{Internet_space}{Do you think that sport currently occupies the space it deserves on internet? Rating from 1=definitely not to 7=definitely yes with 4=indifferent.}
\item{Paid_channels}{Do you think it is right that some sports are only accessible on paid channels? Rating from 1=definitely not to 7=definitely yes with 4=indifferent.}
\item{Subscriptions}{Any past or current subscription to a sport magazine/channel.}
\item{Psycol_well_being}{Do you think that practicing sports affects psychological well-being? Rating from 1=definitely not to 7=definitely yes with 4=indifferent.}
\item{Physical_well_being}{Do you think that practicing sports affects physical well-being? Rating from 1=definitely not to 7=definitely yes with 4=indifferent.}
\item{Sport_nutrition}{Do you think nutrition affects sport? Rating from 1=definitely not to 7=definitely yes with 4=indifferent.}
\item{Overall_health}{Do you think that practicing sports affects health? Rating from 1=definitely not to 7=definitely yes with 4=indifferent.}
\item{Stress}{Self-reported stress level on a scale between 0 and 100.}
\item{Economic_status}{Level of satisfaction for one's own economic status: 0=not at all, 1=a little bit, 2=enough, 3=satisfied, 4=a lot. It is the only covariate with some \code{NA}'s.}
}
}
\references{

Simone, R., Cappelli, C. and Di Iorio, F., (2019). Modelling marginal ranking distributions: the uncertainty tree. \emph{Pattern Recognition Letters}, \bold{125}, pages 278--288, DOI: 10.1016/j.patrec.2019.04.026.

Simone, R. and Iannario, M., (2018). Analysing sport data with clusters of opposite preferences. \emph{Statistical Modelling}, \bold{18}(5-6), pages 505--524, DOI: 10.1177/1471082X18798455.

}
\examples{
str(ranks_sports)
head(ranks_sports)
}
\keyword{datasets}
