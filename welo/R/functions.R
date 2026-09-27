#' Cleaning function
#'
#' Cleans the dataset in order to create a suitable data.frame ready to be used in the \code{\link{welofit}} function. 
#' @details
#' The cleaning operations are:
#' 1. Remove all the uncompleted matches;
#' 2. Remove all the NAs from B365 odds;
#' 3. Remove all the NAs from the variable "ranking";
#' 4. Remove all the NAs from the variable "games"; 
#' 5. Remove all the NAs from the variable "sets";
#' 6. Remove all the matches where the B365 odds are equal;
#' 7. Define players \eqn{i} and \eqn{j} and their outcomes (\eqn{Y_i} and \eqn{Y_j});
#' 8. Remove all the matches of players who played less than MNM matches;
#' 9. Remove all the matches of players with rank greater than MRANK;
#' 10. Sort the matches by date.
#' @param x Data to be cleaned. It must be a data.frame coming from \href{http://www.tennis-data.co.uk/}{http://www.tennis-data.co.uk/}.
#' @return Data.frame cleaned
#' @importFrom Rdpack reprompt
#' @param MNM **optional** Minimum number of matches played by each player to include in the cleaned dataset. Default to 10. This means that
#' each player has to play at least 10 matches
#' @param MRANK **optional** Maximum rank of the players to consider. Default to 500. This means that all the matches with players
#' with ranks greater than 500 are dropped
#' @examples
#' \donttest{
#' data(atp_2019) 
#' db_clean<-clean(atp_2019)
#' str(db_clean)
#' }
#' @export

## Cleaning function

clean<-function(x,MNM=10,MRANK=500){

############# checks 

if(!inherits(x,"data.frame")) { stop("x must be a data.frame. Please provide it in the correct form")}

############# first dataset

players_1<-table(c(x$Winner,x$Loser))

players_tab_1<-data.frame(player=names(players_1),matches=as.numeric(players_1))

x_1 <- x

############# repeat

repeat{

############# cancel all the uncompleted matches 

x_old	<-	x
x		<-	subset(x,x$Comment=="Completed")


############# cancel NAs from B365 

if(any(is.na(x$B365W)|is.na(x$B365L))){

todrop<-which(is.na(x$B365W)|is.na(x$B365L))

x<-x[-todrop,]

}

############ data transformation 

x$WRank<-suppressWarnings(as.numeric(x$WRank))
x$LRank<-suppressWarnings(as.numeric(x$LRank))

x$WPts<-suppressWarnings(as.numeric(x$WPts))
x$LPts<-suppressWarnings(as.numeric(x$LPts))

x$Wsets<-as.numeric(x$Wsets)
x$Lsets<-as.numeric(x$Lsets)

############# cancel NAs from WRank e LRank

if(any(is.na(x$WRank)|is.na(x$LRank))){

todrop<-which(is.na(x$WRank)|is.na(x$LRank))

x<-x[-todrop,]

}

############# cancel NAs from W1/L1 

if(any(is.na(x$W1)|is.na(x$L1))){

todrop<-which(is.na(x$W1)|is.na(x$L1))

x<-x[-todrop,]

}

############# cancel NAs from Wsets/Lsets

if(any(is.na(x$Wsets)|is.na(x$Lsets))){

todrop<-which(is.na(x$Wsets)|is.na(x$Lsets))

x<-x[-todrop,]

}

############# cancel all the matches where the B365 odds are equal

if(any(x$B365W==x$B365L)){

x		<-	subset(x,x$B365W!=x$B365L)

}

############# define player i and player j. Player i is the player with the shortest B365 odds

x$P_i	<-	ifelse(x$B365W<x$B365L,as.character(x$Winner),as.character(x$Loser))
x$P_j	<-	ifelse(x$B365W>x$B365L,as.character(x$Winner),as.character(x$Loser))

############# define the outcomes for players i and j

x$Y_i	<-	ifelse(x$P_i==x$Winner,1,0)
x$Y_j	<-	ifelse(x$P_j==x$Winner,1,0)

############# consider only the matches of players 
############# who played at least MNM matches

players<-table(c(x$P_i,x$P_j))

players_tab<-data.frame(player=names(players),matches=as.numeric(players))

players_tab$GT_10<-ifelse(players_tab$matches>=MNM,1,0)

players_to_cons<-subset(players_tab,players_tab$GT_10==1)

x$player_to_cons_d<-ifelse(x$P_i %in% players_to_cons$player&
x$P_j %in% players_to_cons$player,1,0)

x<-subset(x,x$player_to_cons_d==1)

col_to_drop<-which(colnames(x)=="player_to_cons_d")

x<-x[,-col_to_drop]

############# cancel all the matches played by players with
############# rank greater than MRANK

L<-length(which(as.numeric(x$WRank)>MRANK|as.numeric(x$LRank)>MRANK))

if(L>=1){
todrop<-which(as.numeric(x$WRank)>MRANK|as.numeric(x$LRank)>MRANK)
x<-x[-todrop,]
}

############# calculate the number of games for players i and j

if(any(colnames(x)=="W4")){ ### if we consider male matches:

x$NG_i  	<- 	ifelse(x$Winner==x$P_i,
rowSums(x[,c("W1","W2","W3","W4","W5")],na.rm=T),
rowSums(x[,c("L1","L2","L3","L4","L5")],na.rm=T))

x$NG_j  	<- 	ifelse(x$Winner==x$P_j,
rowSums(x[,c("W1","W2","W3","W4","W5")],na.rm=T),
rowSums(x[,c("L1","L2","L3","L4","L5")],na.rm=T))

} else {

x$NG_i  	<- 	ifelse(x$Winner==x$P_i,
rowSums(x[,c("W1","W2","W3")],na.rm=T),
rowSums(x[,c("L1","L2","L3")],na.rm=T))

x$NG_j  	<- 	ifelse(x$Winner==x$P_j,
rowSums(x[,c("W1","W2","W3")],na.rm=T),
rowSums(x[,c("L1","L2","L3")],na.rm=T))

}

############# calculate the number of sets for players i and j

x$NS_i	<-	(ifelse(x$Winner==x$P_i,x$Wsets,x$Lsets))
x$NS_j	<-	(ifelse(x$Winner==x$P_j,x$Wsets,x$Lsets))

############# calculate the f(g) (in terms of games)

x$f_g_i	<-	ifelse(x$Y_i==1,
x$NG_i/(x$NG_i+x$NG_j),
x$NG_j/(x$NG_i+x$NG_j)
)

x$f_g_j<-ifelse(x$Y_j==1,
x$NG_j/(x$NG_i+x$NG_j),
x$NG_i/(x$NG_i+x$NG_j)
)

############# calculate f(g) (in terms of set)

x$f_s_i	<-	ifelse(x$Y_i==1,
x$NS_i/(x$NS_i+x$NS_j),
x$NS_j/(x$NS_i+x$NS_j)
)

x$f_s_j<-ifelse(x$Y_j==1,
x$NS_j/(x$NS_i+x$NS_j),
x$NS_i/(x$NS_i+x$NS_j)
)

############# add id column

x$id<-1:nrow(x)

############# order the matches

if(substr(x[1,"Date"],5,5)=="-"){
type_of_data<-"%Y-%m-%d"
} else {
type_of_data<-"%d/%m/%Y"
}


x_data<-strptime(x[,"Date"], type_of_data,tz="GMT")

if(any(is.na(x_data))){
todrop<-which(is.na(x_data))
x<-x[-todrop,]
x_data<-x_data[-todrop]
}

x_i<-xts::as.xts(x[,"id"],x_data)

x_ord<-x[as.numeric(x_i),]
x_ord$id<-1:nrow(x_ord)

x<-x_new<-x_ord

############# end repeat

if (nrow(x_new) == nrow(x_old)){
  break
 }

}

############# end

cat('Number of matches (before cleaning)', nrow(x_1),'\n')
cat('Number of matches (after cleaning)', nrow(x),'\n')
cat('Number of players (before cleaning)', nrow(players_tab_1),'\n')
cat('Number of players (after cleaning)', nrow(cbind(table(c(x$P_i,x$P_j)))),'\n')

return(x)

}

#' Probability of winning 
#'
#' Calculates the probability that player \eqn{i} wins over player \eqn{j} for match at time \eqn{t+1} using the WElo or Elo rates at time \eqn{t}. Formally:
#' \deqn{\hat{p}_{i,j}(t+1) = \frac{1}{1+10^{\left(E_j(t)-E_i(t)\right)/400}},}
#' where \eqn{E_{i}(t)} and \eqn{E_j(t)} are the WElo or Elo rates at time \eqn{t}.
#' @param i WElo or Elo rates for player \eqn{i}
#' @param j WElo or Elo rates for player \eqn{j}
#' @return Probability that player \eqn{i} wins the match against player \eqn{j}
#' @importFrom Rdpack reprompt
#' @examples
#' tennis_prob(2000,2000) 
#' tennis_prob(2500,2000)
#' @export

############# Probability of winning

tennis_prob<-function(i,j){

expon<-(j-i)/400

den<- 1+10^expon

return(1/den)

}


#' Download tennis data
#'
#' Imports ATP or WTA data from the website
#' \url{http://www.tennis-data.co.uk/}.
#'
#' @param YEAR Year to consider, in `"YYYY"` format.
#' @param Circuit Tennis circuit. Valid choices are `"ATP"` and `"WTA"`.
#'
#' @details
#' The structure of the datasets provided by
#' \url{http://www.tennis-data.co.uk/} may vary across years.
#' Therefore, before combining data from multiple seasons, users should
#' verify that the variables are consistent across datasets and, if
#' necessary, retain only the common columns.
#'
#' @return A data frame containing the data for the selected year and circuit.
#'
#' @importFrom rio import
#'
#' @examples
#' \dontrun{
#' db <- tennis_data("2022", "ATP")
#' head(db)
#' }
#'
#' @export

tennis_data <- function(YEAR, Circuit) {

  YEAR <- as.character(YEAR)

  year_num <- suppressWarnings(as.numeric(YEAR))

  if (
    length(YEAR) != 1L ||
    is.na(year_num) ||
    !grepl("^[0-9]{4}$", YEAR)
  ) {
    stop(
      "YEAR must be a single year in 'YYYY' format.",
      call. = FALSE
    )
  }

  Circuit <- match.arg(
    toupper(Circuit),
    choices = c("ATP", "WTA")
  )

 if (year_num >= 2013) {
  extension <- ".xlsx"
} else {
  extension <- ".xls"
}

if (Circuit == "WTA") {
  year_to_cons <- paste0(YEAR, "w/", YEAR)
} else {
  year_to_cons <- paste0(YEAR, "/", YEAR)
}

myurl <- paste0(
  "http://www.tennis-data.co.uk/",
  year_to_cons,
  extension
)

tmp_file <- tempfile(fileext = extension)

  on.exit(unlink(tmp_file), add = TRUE)

  download_ok <- tryCatch(
    {
      utils::download.file(
        url = myurl,
        destfile = tmp_file,
        mode = "wb",
        quiet = TRUE,
        method = "libcurl"
      )
      TRUE
    },
    warning = function(w) {
      FALSE
    },
    error = function(e) {
      FALSE
    }
  )

  if (
    !download_ok ||
    !file.exists(tmp_file) ||
    file.size(tmp_file) == 0L
  ) {
    stop(
      "The tennis data could not be downloaded from ",
      myurl,
      ". Please check your Internet connection, verify that the ",
      "requested year is available, or try again later.",
      call. = FALSE
    )
  }

  db <- tryCatch(
    rio::import(
      tmp_file,
      na = c("", "N/A"),
      sheet = YEAR
    ),
    error = function(e) {
      stop(
        "The downloaded file could not be imported. ",
        "The file format or worksheet name may have changed.",
        call. = FALSE
      )
    }
  )

  if (!is.data.frame(db)) {
    stop(
      "The downloaded object is not a data frame.",
      call. = FALSE
    )
  }

  if (!"Date" %in% names(db)) {
    stop(
      "The downloaded file does not contain the expected 'Date' column.",
      call. = FALSE
    )
  }

  db$Date <- substr(as.character(db$Date), 1L, 10L)

  db
}


#' Brier score
#'
#' Calculates the Brier score.
#' @return Vector of the errors.
#' @importFrom Rdpack reprompt
#' @keywords internal

BS<-function(y,y_hat){

return((y-y_hat)^2)

}

#' Log-loss score
#'
#' Calculates the Log-loss score.
#' @return Vector of the errors.
#' @importFrom Rdpack reprompt
#' @keywords internal

LL<-function(y,y_hat){

ll<-ifelse(y==1,-log(y_hat),-log(1-y_hat))

return(ll)

}

#' Accuracy 
#'
#' Calculates the accuracy rate score.
#' @return Percentage of matches correctly predicted.
#' @importFrom Rdpack reprompt
#' @keywords internal

ACC<-function(y,y_hat,quant){

N<-length(which(y_hat>stats::quantile(y_hat,quant)))
acc<-ifelse(y==1&y_hat>stats::quantile(y_hat,quant),1,0)

return((sum(acc)/N)*100)

}

#' Calculates the WElo and Elo rates
#'
#' Calculates the WElo and Elo rates according to \insertCite{angelini2021weighted;textual}{welo}. In particular, the Elo updating system 
#' defines the rates (for player \eqn{i}) as:
#' \deqn{E_{i}(t+1) = E_{i}(t) + K_i(t) \left[W_{i}(t)- \hat{p}_{i,j}(t) \right],}
#' where \eqn{E_{i}(t)} is the Elo rate at time \eqn{t}, \eqn{W_{i}(t)} is the outcome (1 or 0) for player \eqn{i} in the match at time \eqn{t},
#' \eqn{K_i(t)} is a scale factor, and \eqn{\hat{p}_{i,j}(t)} is the probability of winning for match at time \eqn{t}, calculated using \code{\link{tennis_prob}}.
#' The scale factor \eqn{K_i(t)} determines how much the rates change over time. By default, according to \insertCite{kovalchik_2016;textual}{welo}, it is defined as
#' \deqn{K_i(t)=250/\left(N_i(t)+5\right)^{0.4},} 
#' where \eqn{N_i(t)} is the number of matches disputed by player \eqn{i} up to time \eqn{t}. Alternately, \eqn{K_i(t)} can be multiplied by 1.1 if 
#' the match at time \eqn{t} is a Grand Slam match or is played on a given surface. Finally, it can be fixed to a constant value. 
#' The WElo rating system is defined as:
#' \deqn{E_{i}^\ast(t+1) = E_{i}^\ast(t) + K_i(t) \left[W_{i}(t)- \hat{p}_{i,j}^\ast(t) \right] f(W_{i,j}(t)),}
#' where \eqn{E_{i}^\ast(t+1)} denotes the WElo rate for player \eqn{i}, \eqn{\hat{p}_{i,j}^\ast(t)} the probability of winning using \code{\link{tennis_prob}} and
#' the WElo rates, and \eqn{f(W_{i,j}(t))} represents a function whose values depend on the games (by default) or sets won in the previous match. 
#' In particular, when parameter 'W' is set to "GAMES", \eqn{f(W_{i,j}(t))} is defined as:
#' \deqn{   f(W_{i,j}(t)) \equiv f(G_{i,j}(t))=  
#' \left\{
#'                \begin{array}{ll}
#'                 \frac{NG_i(t)}{NG_i(t)+NG_j(t)} \quad if~player~i~has~won~match~t;\\
#'                  \frac{NG_j(t)}{NG_i(t)+NG_j(t)} \quad if~player~i~has~lost~match~t,
#'                \end{array}
#'             \right. 
#'}
#' where \eqn{NG_i(t)} and \eqn{NG_j(t)} represent the number of games won by player \eqn{i} and player \eqn{j} in match \eqn{t}, respectively.
#' When parameter 'W' is set to "SET", \eqn{f(W_{i,j}(t))} is:
#' \deqn{   f(W_{i,j}(t)) \equiv f(S_{i,j}(t))=  
#' \left\{
#'                \begin{array}{ll}
#'                 \frac{NS_i(t)}{NS_i(t)+NS_j(t)} \quad if~player~i~has~won~match~t;\\
#'                  \frac{NS_j(t)}{NS_i(t)+NS_j(t)} \quad if~player~i~has~lost~match~t,
#'                \end{array}
#'             \right. 
#'}
#' where \eqn{NS_i(t)} and \eqn{NS_j(t)} represent the number of  sets won by player \eqn{i} and player \eqn{j} in match \eqn{t}, respectively.
#' The scale factor \eqn{K_i(t)} is the same as the Elo model.
#' @param x Data cleaned through the function \code{\link{clean}} or, if the parameter 'new_data' is present, 
#' a former estimated list coming from the \code{\link{welofit}} function
#' @param W **optional** Weights to use for the WElo rating system. Valid choices are: "GAMES" (by default) and "SETS"
#' @param SP **optional** Starting points for calculating the rates. 1500 by default
#' @param K **optional** Scale factor determining how much the WElo and Elo rates change over time. Valid choices are:
#' "Kovalchik" (by default), "Grand_Slam", "Surface_Hard", "Surface_Grass", "Surface_Clay" and, finally, a constant value \eqn{K}.
#' The first option ("Kovalchik") is equal to what was suggested by \insertCite{kovalchik_2016;textual}{welo}, 
#' Putting \eqn{K} to "Grand_Slam" lets the Kovalchik scale factor multiplied by 1.1, if the match is a Grand Slam match. 
#' Similarly, the choices "Surface_Hard", "Surface_Grass" and "Surface_Clay" make the Kovalchik scale factor
#' increased by 1.1 if, respectively, the match is played on hard, grass or clay. Finally, \eqn{K} can be any scalar value, indipendently of the number of matches played before the match \eqn{t}
#' @param CI **optional** Confidence intervals for the WElo and Elo rates. Default to FALSE. If 'CI' is set to "TRUE", then the 
#' confidence intervals are calculated, according to the procedure explained by \insertCite{angelini2021weighted;textual}{welo}
#' @param alpha **optional** Significance level of the confidence interval. Default to 0.05
#' @param B **optional** Number of bootstrap samples used to calculate the confidence intervals. Default to 1000
#' @param new_data **optional** New data, cleaned through the function \code{\link{clean}}, to append to an already estimated set of matches (included
#' in the parameter 'x')
#' @return \code{welofit} returns an object of class 'welo', which is a list containing the following components:
#' \itemize{
#' 	\item results: The data.frame including a variety of variables, among which there are the estimated WElo and Elo rates, before and 
#' after the match \eqn{t}, for players \eqn{i} and \eqn{j},
#'  the lower and upper confidence intervals (if CI=TRUE) for the WElo and Elo rates, labelled as '_lb' and '_ub', respectively, and the probability of winning the match for player \eqn{i} (labelled as 'WElo_pi_hat' and 
#' 'Elo_pi_hat', respectively, for the WElo and Elo models). 
#'   \item matches: The number of matches analyzed.
#'   \item period: The sample period considered.
#'	\item loss: The Brier score \insertCite{brier1950}{welo} and log-loss (used by  \insertCite{kovalchik_2016;textual}{welo}, among others) 
#' averages, calculated considering the distance with respect to the outcome of the match.
#'	\item highest_welo: The player with the highest WElo rate and the relative date.
#'   \item highest_elo: The player with the highest Elo rate and the relative date.
#'   \item dataset: The dataset used for the estimation of the WElo and Elo rates.
#' }
#' @references
#' \insertAllCited{} 
#' @importFrom Rdpack reprompt
#' @examples
#' \donttest{
#' data(atp_2019) 
#' db_clean<-clean(atp_2019)
#' res<-welofit(db_clean)
#' # append new data
#' db_clean_1<-db_clean[1:500,]
#' db_clean_2<-db_clean[501:1200,]
#' res_1<-welofit(db_clean_1)
#' res_2<-welofit(res_1,new_data=db_clean_2)
#' }
#' @export

welofit<-function(x,W="GAMES",SP=1500,K="Kovalchik",CI=FALSE,alpha=0.05,B=1000,new_data=NULL){

############################## checks

if(!any(
colnames(x)=="P_i"|
colnames(x)=="P_j"|
colnames(x)=="Y_i"|
colnames(x)=="Y_j"|
colnames(x)=="NG_i"|
colnames(x)=="NG_j"|
colnames(x)=="NS_i"|
colnames(x)=="NS_j"|
colnames(x)=="f_g_i"|
colnames(x)=="f_g_i"
)&is.null(new_data)) { stop(cat("#Warning:\n It seems that some variables are missing. Use first the function 'clean' on the dataset downloaded from tennis-data.co.uk \n"))}

if((W != "GAMES")&(W != "SETS")) { stop(cat("#Warning:\n Valid choices for the parameter 'W' are currently 'GAMES' and 'SETS' \n"))}

if(any(colnames(x)=="Tier")) {
colnames(x)[which(colnames(x)=="Tier")]<-"Series"
}

################################# begin

if(is.null(new_data)){

TT<-nrow(x)

################################# Standard Elo

x$Elo_i_before_match<-NA
x$Elo_j_before_match<-NA
x$Elo_i_after_match<-NA
x$Elo_j_after_match<-NA
x$Elo_pi_hat<-NA

################################# Weighted Elo

x$WElo_i_before_match<-NA
x$WElo_j_before_match<-NA
x$WElo_i_after_match<-NA
x$WElo_j_after_match<-NA
x$WElo_pi_hat<-NA

################################# Standard errors for WElo

if (CI==TRUE){

x$WElo_i_lb<-NA
x$WElo_i_ub<-NA
x$WElo_j_lb<-NA
x$WElo_j_ub<-NA

################################# Standard errors for Elo

x$Elo_i_lb<-NA
x$Elo_i_ub<-NA
x$Elo_j_lb<-NA
x$Elo_j_ub<-NA

}

} else {

TT_old<-nrow(x$dataset)
TT_new<-nrow(new_data) 

x_old<-x$dataset
x_res<-x$results

new_data$Elo_i_before_match<-NA
new_data$Elo_j_before_match<-NA
new_data$Elo_i_after_match<-NA
new_data$Elo_j_after_match<-NA
new_data$Elo_pi_hat<-NA   

new_data$WElo_i_before_match<-NA
new_data$WElo_j_before_match<-NA
new_data$WElo_i_after_match<-NA
new_data$WElo_j_after_match<-NA
new_data$WElo_pi_hat<-NA 

x<-rbind(x_old,new_data)

x$Elo_i_before_match[1:TT_old]<-x_res$Elo_i_before_match
x$Elo_j_before_match[1:TT_old]<-x_res$Elo_j_before_match
x$Elo_i_after_match[1:TT_old]<-x_res$Elo_i_after_match
x$Elo_j_after_match[1:TT_old]<-x_res$Elo_j_after_match
x$Elo_pi_hat[1:TT_old]<-x_res$Elo_pi_hat

x$WElo_i_before_match[1:TT_old]<-x_res$WElo_i_before_match
x$WElo_j_before_match[1:TT_old]<-x_res$WElo_j_before_match
x$WElo_i_after_match[1:TT_old]<-x_res$WElo_i_after_match
x$WElo_j_after_match[1:TT_old]<-x_res$WElo_j_after_match
x$WElo_pi_hat[1:TT_old]<-x_res$WElo_pi_hat
}

################################# begin cycle

if (is.null(new_data)){
begin_c<-1
end_c<-TT 
} else {
begin_c<-(TT_old+1)
end_c<-nrow(x)
}

#################################

for(tt in begin_c:end_c){

player_i<-x$P_i[tt]
player_j<-x$P_j[tt]

#################################### check if player i has disputed a match before time tt
if(match(player_i,x$P_i[1:tt])==tt & 
is.na(match(player_i,x$P_j[1:tt]))
   ) {
x$Elo_i_before_match[tt]<-SP
x$WElo_i_before_match[tt]<-SP
N_i<-1
} else {
match_i_tt_1<-max(which(player_i==x$P_i[1:(tt-1)]),
which(player_i==x$P_j[1:(tt-1)]))

## number of matches played by i before match at time tt
N_i<-length(c(which(player_i==x$P_i[1:(tt-1)]),
which(player_i==x$P_j[1:(tt-1)])))

x$Elo_i_before_match[tt]<-ifelse(
player_i==x$P_i[match_i_tt_1],
x$Elo_i_after_match[match_i_tt_1],
x$Elo_j_after_match[match_i_tt_1])

x$WElo_i_before_match[tt]<-ifelse(
player_i==x$P_i[match_i_tt_1],
x$WElo_i_after_match[match_i_tt_1],
x$WElo_j_after_match[match_i_tt_1])

}


#################################### check if player j has disputed a match before time tt
if(match(player_j,x$P_j[1:tt])==tt & 
is.na(match(player_j,x$P_i[1:tt]))
   ) {
x$Elo_j_before_match[tt]<-SP
x$WElo_j_before_match[tt]<-SP
N_j<-1
} else {
match_j_tt_1<-max(which(player_j==x$P_i[1:(tt-1)]),
which(player_j==x$P_j[1:(tt-1)]))

## number of matches played by j before match at time tt
N_j<-length(c(which(player_j==x$P_i[1:(tt-1)]),
which(player_j==x$P_j[1:(tt-1)])))

x$Elo_j_before_match[tt]<-ifelse(
player_j==x$P_i[match_j_tt_1],
x$Elo_i_after_match[match_j_tt_1],
x$Elo_j_after_match[match_j_tt_1])

x$WElo_j_before_match[tt]<-ifelse(
player_j==x$P_i[match_j_tt_1],
x$WElo_i_after_match[match_j_tt_1],
x$WElo_j_after_match[match_j_tt_1])

}

## if the match at time tt is a Grand Slam match

gs<-ifelse(x$Series[tt]=="Grand Slam",1.1,1)

#### surface hard

if(K=="Surface_Hard"){
surf<-ifelse(x$Surface[tt]=="Hard",1.1,1)
}

#### surface clay

if(K=="Surface_Clay"){
surf<-ifelse(x$Surface[tt]=="Clay",1.1,1)
}

#### surface hard

if(K=="Surface_Grass"){
surf<-ifelse(x$Surface[tt]=="Grass",1.1,1)
}

if(K=="Kovalchik"){

K_i<-250/(N_i+5)^0.4
K_j<-250/(N_j+5)^0.4

} else if (K=="Grand_Slam"){

K_i<-250/(N_i+5)^0.4*gs
K_j<-250/(N_j+5)^0.4*gs

} else if (K=="Surface_Grass"|K=="Surface_Clay"|K=="Surface_Hard"){

K_i<-250/(N_i+5)^0.4*surf
K_j<-250/(N_j+5)^0.4*surf

} else {

K_i<-K
K_j<-K

}

############################# standard ELO

x$Elo_pi_hat[tt]<-tennis_prob(x$Elo_i_before_match[tt],
x$Elo_j_before_match[tt])

x$Elo_i_after_match[tt]<-x$Elo_i_before_match[tt]+K_i*(
x$Y_i[tt]-x$Elo_pi_hat[tt])

x$Elo_j_after_match[tt]<-x$Elo_j_before_match[tt]+K_j*(
x$Y_j[tt]-(1-x$Elo_pi_hat[tt]))

############################# Weighted ELO

x$WElo_pi_hat[tt]<-tennis_prob(x$WElo_i_before_match[tt],
x$WElo_j_before_match[tt])

if(W=="GAMES"){

x$WElo_i_after_match[tt]<-x$WElo_i_before_match[tt]+K_i*(
x$Y_i[tt]-x$WElo_pi_hat[tt])*x$f_g_i[tt]

x$WElo_j_after_match[tt]<-x$WElo_j_before_match[tt]+K_j*(
x$Y_j[tt]-(1-x$WElo_pi_hat[tt]))*x$f_g_j[tt]

} else {
x$WElo_i_after_match[tt]<-x$WElo_i_before_match[tt]+K_i*(
x$Y_i[tt]-x$WElo_pi_hat[tt])*x$f_s_i[tt]

x$WElo_j_after_match[tt]<-x$WElo_j_before_match[tt]+K_j*(
x$Y_j[tt]-(1-x$WElo_pi_hat[tt]))*x$f_s_j[tt]
}

###################################################################

################################################################### Elo: Standard errors 

if (CI==TRUE){

############################# Elo

p<-x$Elo_pi_hat[tt]
q<-1-p

### Standard errors for i
sim_i<-sample(0:1,B,replace=T,prob=c(q,p))
x$Elo_i_lb[tt]<-stats::quantile(x$Elo_i_before_match[tt]+K_i*(sim_i-x$Elo_pi_hat[tt]),alpha/2)
x$Elo_i_ub[tt]<-stats::quantile(x$Elo_i_before_match[tt]+K_i*(sim_i-x$Elo_pi_hat[tt]),1-alpha/2)

### Standard errors for j
sim_j<-sample(0:1,B,replace=T,prob=c(p,q))
x$Elo_j_lb[tt]<-stats::quantile(x$Elo_j_before_match[tt]+K_j*(sim_j-(1-x$Elo_pi_hat[tt])),alpha/2)
x$Elo_j_ub[tt]<-stats::quantile(x$Elo_j_before_match[tt]+K_j*(sim_j-(1-x$Elo_pi_hat[tt])),1-alpha/2)

############################# WElo

p_w<-x$WElo_pi_hat[tt]
q_w<-1-p_w

### Standard errors for i
sim_i<-sample(0:1,B,replace=T,prob=c(q_w,p_w))
x$WElo_i_lb[tt]<-stats::quantile(x$WElo_i_before_match[tt]+K_i*(sim_i-x$WElo_pi_hat[tt]),alpha/2)
x$WElo_i_ub[tt]<-stats::quantile(x$WElo_i_before_match[tt]+K_i*(sim_i-x$WElo_pi_hat[tt]),1-alpha/2)


### Standard errors for j
sim_j<-sample(0:1,B,replace=T,prob=c(p_w,q_w))
x$WElo_j_lb[tt]<-stats::quantile(x$WElo_j_before_match[tt]+K_j*(sim_j-(1-x$WElo_pi_hat[tt])),alpha/2)
x$WElo_j_ub[tt]<-stats::quantile(x$WElo_j_before_match[tt]+K_j*(sim_j-(1-x$WElo_pi_hat[tt])),1-alpha/2)

}

} #end cycle

cat('-----------------------------','\n') 


odds_i<-ifelse(x$P_i==x$Winner,x$B365W,x$B365L) 
odds_j<-ifelse(x$P_j==x$Winner,x$B365W,x$B365L)
best_odds_i<-ifelse(x$P_i==x$Winner,x$MaxW,x$MaxL)
best_odds_j<-ifelse(x$P_j==x$Winner,x$MaxW,x$MaxL)
avg_odds_i<-ifelse(x$P_i==x$Winner,x$AvgW,x$AvgL)
avg_odds_j<-ifelse(x$P_j==x$Winner,x$AvgW,x$AvgL)

if(CI==FALSE){
x_sub<-data.frame("Date"=x$Date,
"Series"=x$Series,
"Surface"=x$Surface,
"P_i"=x$P_i,
"P_j"=x$P_j,
"Outcome_P_i"=x$Y_i,
"Outcome_P_j"=x$Y_j,
"Elo_i_before_match"=x$Elo_i_before_match,    
"Elo_j_before_match"=x$Elo_j_before_match, 
"Elo_pi_hat"=x$Elo_pi_hat,
"Elo_i_after_match"=x$Elo_i_after_match,    
"Elo_j_after_match"=x$Elo_j_after_match, 
"WElo_i_before_match"=x$WElo_i_before_match,
"WElo_j_before_match"=x$WElo_j_before_match,
"WElo_pi_hat"=x$WElo_pi_hat,
"WElo_i_after_match"=x$WElo_i_after_match,
"WElo_j_after_match"=x$WElo_j_after_match,
"Odds_i"=odds_i,
"Odds_j"=odds_j,
"Best_odds_i"=best_odds_i,
"Best_odds_j"=best_odds_j,
"Avg_odds_i"=avg_odds_i,
"Avg_odds_j"=avg_odds_j)
} else {
x_sub<-data.frame("Date"=x$Date,
"Series"=x$Series,
"Surface"=x$Surface,
"P_i"=x$P_i,
"P_j"=x$P_j,
"Outcome_P_i"=x$Y_i,
"Outcome_P_j"=x$Y_j,
"Elo_i_before_match_lb"=x$Elo_i_lb, 
"Elo_i_before_match"=x$Elo_i_before_match,
"Elo_i_before_match_ub"=x$Elo_i_ub, 
"Elo_j_before_match_lb"=x$Elo_j_lb,
"Elo_j_before_match"=x$Elo_j_before_match, 
"Elo_j_before_match_ub"=x$Elo_j_ub, 
"Elo_pi_hat"=x$Elo_pi_hat,
"Elo_i_after_match"=x$Elo_i_after_match,    
"Elo_j_after_match"=x$Elo_j_after_match,
"WElo_i_before_match_lb"=x$WElo_i_lb,
"WElo_i_before_match"=x$WElo_i_before_match,
"WElo_i_before_match_ub"=x$WElo_i_ub,  
"WElo_j_before_match_lb"=x$WElo_j_lb,
"WElo_j_before_match"=x$WElo_j_before_match,
"WElo_j_before_match_ub"=x$WElo_j_ub,  
"WElo_pi_hat"=x$WElo_pi_hat,
"WElo_i_after_match"=x$WElo_i_after_match,
"WElo_j_after_match"=x$WElo_j_after_match,
"Odds_i"=odds_i,
"Odds_j"=odds_j,
"Best_odds_i"=best_odds_i,
"Best_odds_j"=best_odds_j,
"Avg_odds_i"=avg_odds_i,
"Avg_odds_j"=avg_odds_j)
}

loss_w<-cbind(
mean(c(x_sub$WElo_pi_hat- x_sub$Outcome_P_i)^2),
mean(ifelse(x_sub$Outcome_P_i==1,-log(x_sub$WElo_pi_hat),-log(1-x_sub$WElo_pi_hat)))
)

loss_e<-cbind(
mean(c(x_sub$Elo_pi_hat - x_sub$Outcome_P_i)^2),
mean(ifelse(x_sub$Outcome_P_i==1,-log(x_sub$Elo_pi_hat),-log(1-x_sub$Elo_pi_hat)))
)

loss<-rbind(loss_w,loss_e)

colnames(loss)<-c("Brier","Log-Loss")
rownames(loss)<-c("WElo","Elo")

loss<-round(loss,4)

res<-list(
results=x_sub,
matches=paste("Number of matches:",nrow(x),sep=" "),
period=paste("From",x_sub$Date[1],"to",x_sub$Date[nrow(x_sub)],sep=" "),
loss=loss,
highest_welo=paste("The player with the highest WElo rate, reached on", x_sub$Date[which.max(x_sub$WElo_i_after_match)], "is:",
x_sub$P_i[which.max(x_sub$WElo_i_after_match)],sep=" "),
highest_elo=paste("The player with the highest Elo rate, reached on", x_sub$Date[which.max(x_sub$Elo_i_after_match)], "is:",
x_sub$P_i[which.max(x_sub$Elo_i_after_match)],sep=" "),
dataset=x
)

cat(utils::capture.output(res$loss),  sep = '\n')
class(res)<-c("welo")
return(res)
}

#' Plot for WElo and Elo rates
#'
#' Plots WElo and Elo rates.
#' @param x An object of class 'welo', obtained after running the \code{\link{welofit}} function
#' @param players A character vector including the players whose rates will be plotted. 
#' The indication of the player has to be: 'Surname N.'. For instance, 'Roger Federer' will be
#' included in the 'players' vector as 'Federer R.'
#' @param rates **optional** Rates to be plotted. Valid choices are 'WElo' (by default) and 'Elo'
#' @param SP **optional**  Starting points from which the rates originate. By default, SP is 1500
#' @param line_width **optional** Line width, by default it is 1.5
#' @return A ggplot2 plot
#' @importFrom Rdpack reprompt
#' @importFrom xts as.xts
#' @importFrom xts merge.xts
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 scale_x_date
#' @importFrom ggplot2 ylab
#' @importFrom reshape2 melt
#' @examples
#' db<-tennis_data("2022","ATP") 
#' db_clean<-clean(db,MNM=5)
#' res_welo<-welofit(db_clean)
#' players<-c("Nadal R.","Djokovic N.","Berrettini M.","Sinner J.")
#' welo_plot(res_welo,players,rates="WElo",SP=1500,line_width=1.5)
#' @export

welo_plot<-function(x,players,rates="WElo",SP=1500,line_width=1.5){

db_p<-x$results

### checks

if(!inherits(x,"welo")) { stop("x must be a 'welo' object. Please run first the function 'welofit' and then the function 'welo_plot'")}

full_list_players<-unique(c(db_p$P_i,db_p$P_j))

if(!all(players %in% full_list_players)) { stop("At least one of the players within the vector 'players' is not included in the
full sample. Please provide a correct list of players")}

####

N<-length(players)

TT<-nrow(db_p)

seq_date<-seq(
as.Date(db_p$Date[1]), 
as.Date(db_p$Date[TT]), 
by = "1 day")

db_i<-list()

for (i in 1:N){

################################ SUBSET

db_sub<-subset(db_p,db_p$P_i==players[i]|db_p$P_j==players[i])

################################ standard elo

db_sub$ELO<-ifelse(db_sub$P_i==players[i],db_sub$Elo_i_after_match,db_sub$Elo_j_after_match)

################################ weighted elo

db_sub$WELO<-ifelse(db_sub$P_i==players[i],db_sub$WElo_i_after_match,db_sub$WElo_j_after_match)

## First: create a data.frame with all the days
full_date <- data.frame(date = seq_date)
full_date <-  xts::as.xts(1:nrow(full_date),
strptime(full_date[,1], "%Y-%m-%d", tz="GMT"))

colnames(full_date)<-c("FD")

inc_data<-strptime(unlist(lapply(db_sub$Date, as.character)), "%Y-%m-%d", tz="GMT")

db_sub_i<-as.xts(cbind(db_sub$WELO,db_sub$ELO),inc_data)

colnames(db_sub_i)<-c("WELO","ELO")

############### merge the two datasets

merged_db<-xts::merge.xts(full_date,db_sub_i,join='left')

### let the first observation be equal to SP

merged_db[1,2:3]<-SP

### fill the missing values for each week

for(tt in 2:nrow(merged_db)){
merged_db$ELO[tt]<-ifelse(is.na(merged_db$ELO[tt]),merged_db$ELO[tt-1],merged_db$ELO[tt])
merged_db$WELO[tt]<-ifelse(is.na(merged_db$WELO[tt]),merged_db$WELO[tt-1],merged_db$WELO[tt])
}

db_i[[i]]<-merged_db

}

if(rates=="WElo"){

plot_data<-matrix(NA,ncol=(N),nrow=nrow(db_i[[1]]))

colnames(plot_data)<-players

for(i in 1:N){
plot_data[,i]<-db_i[[i]][,2]
}

plot_data_f<-data.frame(Time=as.Date(stats::time(db_i[[1]])),plot_data)
colnames(plot_data_f)[2:ncol(plot_data_f)]<-players

data_long <- reshape2::melt(plot_data_f, 'Time')

colnames(data_long)[c(2:3)]<-c("Players","WElo")

Time<-WElo<-Players<-NULL

final_plot<-ggplot2::ggplot(data_long,            
               ggplot2::aes(x = Time,
                   y = WElo,
                   color = Players,
					size = line_width)) +  ggplot2::geom_line(size = line_width) + ggplot2::scale_x_date(date_labels ="%m/%Y") +
				  ggplot2::ylab("WElo rates") 

} else {

plot_data<-matrix(NA,ncol=(N),nrow=nrow(db_i[[1]]))

colnames(plot_data)<-players

for(i in 1:N){
plot_data[,i]<-db_i[[i]][,3]
}

plot_data_f<-data.frame(Time=as.Date(stats::time(db_i[[1]])),plot_data)
colnames(plot_data_f)[2:ncol(plot_data_f)]<-players

data_long <- reshape2::melt(plot_data_f, 'Time')

colnames(data_long)[c(2:3)]<-c("Players","Elo")

Time<-Elo<-Players<-NULL

final_plot<-ggplot2::ggplot(data_long,            
               ggplot2::aes(x = Time,
                   y = Elo,
                   color = Players,
				size = line_width)) +  ggplot2::geom_line(size = line_width) + ggplot2::scale_x_date(date_labels ="%m/%Y") +
				ggplot2::ylab("Elo rates") 

}

return(final_plot)

}

#' Plot for official (ATP or WTA) rates
#'
#' Plots the official (ATP or WTA) rates.
#' @param x An object of class 'welo', obtained after running the \code{\link{welofit}} function
#' @param players A character vector including the players whose rates will be plotted. 
#' The indication of the player has to be: 'Surname N.'. For instance, 'Roger Federer' will be
#' included in the 'players' vector as 'Federer R.'
#' @param line_width **optional** Line width, by default it is 1.5
#' @param nbreaks **optional** Number of breaks for y-axis, by default it is 1
#' @return A ggplot2 plot
#' @importFrom Rdpack reprompt
#' @importFrom xts as.xts
#' @importFrom xts merge.xts
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 scale_x_date
#' @importFrom ggplot2 ylab
#' @importFrom reshape2 melt
#' @examples
#' db<-tennis_data("2022","ATP") 
#' db_clean<-clean(db,MNM=5)
#' res_welo<-welofit(db_clean)
#' players<-c("Nadal R.","Djokovic N.","Berrettini M.","Sinner J.")
#' rank_plot(res_welo,players,line_width=1.5)
#' @export


rank_plot<-function(x,players,line_width=1.5,nbreaks=1){

db_p<-x$dataset

### checks

if(!inherits(x,"welo")) { stop("x must be a 'welo' object. Please run first the function 'welofit' and then the function 'welo_plot'")}

full_list_players<-unique(c(db_p$P_i,db_p$P_j))

if(!all(players %in% full_list_players)) { stop("At least one of the players within the vector 'players' is not included in the
full sample. Please provide a correct list of players")}

####

N<-length(players)

TT<-nrow(db_p)

seq_date<-seq(
as.Date(db_p$Date[1]), 
as.Date(db_p$Date[TT]), 
by = "1 day")

db_i<-list()

for (i in 1:N){

################################ SUBSET

db_sub<-subset(db_p,db_p$P_i==players[i]|db_p$P_j==players[i])

db_sub$RANK<-ifelse(db_sub$P_i==players[i]&db_sub$Y_i==1|
db_sub$P_j==players[i]&db_sub$Y_j==1,db_sub$WRank,db_sub$LRank)

## First: create a data.frame with all the days
full_date <- data.frame(date = seq_date)
full_date <-  xts::as.xts(1:nrow(full_date),
strptime(full_date[,1], "%Y-%m-%d", tz="GMT"))

colnames(full_date)<-c("FD")

inc_data<-strptime(unlist(lapply(db_sub$Date, as.character)), "%Y-%m-%d", tz="GMT")

db_sub_i<-xts::as.xts(cbind(db_sub$RANK),inc_data)

colnames(db_sub_i)<-c("RANK")

############### merge the two datasets

merged_db<-xts::merge.xts(full_date,db_sub_i,join='left')

### fill the missing values for each week

merged_db[1,2]<-db_sub$RANK[1]

for(tt in 2:nrow(merged_db)){
merged_db$RANK[tt]<-ifelse(is.na(merged_db$RANK[tt]),merged_db$RANK[tt-1],merged_db$RANK[tt])
}

db_i[[i]]<-merged_db

}


plot_data<-matrix(NA,ncol=(N),nrow=nrow(db_i[[1]]))

colnames(plot_data)<-players

for(i in 1:N){
plot_data[,i]<-db_i[[i]][,2]
}

plot_data_f<-data.frame(Time=as.Date(stats::time(db_i[[1]])),plot_data)
colnames(plot_data_f)[2:ncol(plot_data_f)]<-players

data_long <- reshape2::melt(plot_data_f, 'Time')

colnames(data_long)[c(2:3)]<-c("Players","Rank")

Time<-Rank<-Players<-NULL

final_plot<-ggplot2::ggplot(data_long,            
               ggplot2::aes(x = Time,
                   y = Rank,
                   color = Players,
					size = line_width)) +  ggplot2::geom_line(size = line_width) + ggplot2::scale_x_date(date_labels ="%m/%Y") +
				  	ggplot2::ylab("Rank")  +
					ggplot2::scale_y_continuous(trans = "reverse", breaks=seq(min(data_long$Rank),max(data_long$Rank),nbreaks))
					
return(final_plot)

}




#' Betting function
#'
#' Places bets using the WElo and Elo probabilities, on the basis of two thresholds \eqn{r} and \eqn{q}, according to \insertCite{angelini2021weighted;textual}{welo}. 
#' By default, the amount of $1 is placed on the best odds (that is, the highest odds available) for player \eqn{i} for all 
#' the matches where it holds that
#' \deqn{\frac{\hat{p}_{i,j}(t)}{q_{i,j}(t)} > r,}
#' where \eqn{\hat{p}_{i,j}(t)} is the estimated probability (coming from the WElo or Elo model) that player \eqn{i} wins the match \eqn{t} against player \eqn{j} 
#' and \eqn{q_{i,j}(t)} is its implied probability obtained as the reciprical of the Bet365 odds. The implied
#' probability \eqn{q_{i,j}(t)} is assumed to be greater than \eqn{q}. If \eqn{q=0}, all the players are considered. If \eqn{q} increases,
#' heavy longshot players are excluded.
#' In general, higher thresholds \eqn{r} and \eqn{q} imply less betting opportunities.
#' @return A matrix including the number of bets placed, the Return-on-Investiment (ROI), expressed in percentage, and its boostrap confidence interval, 
#' calculated using \eqn{R} replicates and the significance level \eqn{\alpha}.
#' @importFrom Rdpack reprompt
#' @param x an object of class 'welo', obtained from the \code{\link{welofit}} function
#' @param r Vector or scalar identifying the threshold of the ratio between the estimated and the implied probability (see above)
#' @param q Scalar parameter used to exclude the heavy underdogs signalled by Bet365 bookmaker. 
#' No bets will be placed on those matches where players have implied probabilities smaller than \eqn{q}
#' @param model Valid choices are: "WELO" and "ELO"
#' @param bets **optional** Parameter identifying on which type of odds the bet is placed. Default to "Best_odds". Valid choices are:
#' "Best_odds", "Avg_odds" and "B365_odds". "Best_odds" are the highest odds available. "Avg_odds" are the average odds for that match and 
#' "B365_odds" are the Bet365 odds
#' @param R **optional** Number of bootstrap replicates to calculate the confidence intervals. Default to 2000
#' @param alpha **optional** Significance level for the boostrap confidence intervals. Default to 0.1
#' @param start_oos **optional** Character parameter denoting the starting year for the bets.
#' If included (default to NULL), then the bets will be placed on matches starting in that year. It has to be formatted as "YYYY"
#' @param end_oos **optional** Character parameter denoting the ending year for the bets. 
#' If included (default to NULL), then the bets will be placed on matches included in the period "start_oos/end_oos". 
#' It has to be formatted as "YYYY"
#' @examples
#' \donttest{
#' data(atp_2019) 
#' db_clean<-clean(atp_2019)
#' db_est<-welofit(db_clean)
#' bets<-betting(db_est,r=c(1.1,1.2,1.3),q=0.3,model="WELO")
#' bets
#' }
#' @export

## Betting function

betting<-function(x,r,q,model,bets="Best_odds",R=2000,alpha=0.10,start_oos=NULL,end_oos=NULL){

### checks

if(!inherits(x,"welo")) { stop("x must be a 'welo' object. Please run first the function 'welofit' and then the function 'betting'")}

#### define the dataset

x_res<-x$results

x<-data.frame(x_res,impl_prob_i=1/x_res$Odds_i,impl_prob_j=1/x_res$Odds_j)

#### choice the type of bets

if(bets=="Best_odds"){

x$bet_odds_i<-x$Best_odds_i
x$bet_odds_j<-x$Best_odds_j

} else if (bets=="Avg_odds"){

x$bet_odds_i<-x$Avg_odds_i
x$bet_odds_j<-x$Avg_odds_j

} else {

x$bet_odds_i<-x$Odds_i
x$bet_odds_j<-x$Odds_j

}

#### verify if there are the specifications for the out-of-sample periods

if(!missing(start_oos)){
x$year<-as.numeric(substring(x$Date,1,4))
first_match<-xts::first(which(x$year==start_oos))
last_match<-xts::last(which(x$year==end_oos))
x<-x[first_match:last_match,]
}


if(length(r)==1){
tab<-matrix(rep(NA,length(r)*4),ncol=4)
} else {
tab<-matrix(rep(NA,length(r)*5),ncol=5)
}

#################################

if(model=="WELO"&length(r)>1){

for(i in 1:length(r)){

############################### rule to bet

x$player_bet<-ifelse(
(x$WElo_pi_hat/x$impl_prob_i)>r[i],"P_i",
ifelse(
((1-x$WElo_pi_hat)/x$impl_prob_j)>r[i]&(x$impl_prob_j>q),"P_j",
"no bet")
)

x_sub<-subset(x,x$player_bet!="no bet")

x_sub$net_inc<-ifelse(
x_sub$player_bet=="P_i",
c(x_sub$bet_odds_i*x_sub$Outcome_P_i - 1),
c(x_sub$bet_odds_j*x_sub$Outcome_P_j - 1))

tab[i,1]<-r[i]
tab[i,3]<-100*mean(x_sub$net_inc)
tab[i,2]<-nrow(x_sub)

b <- boot::boot(x_sub$net_inc, function(u,i) mean(u[i]), R = R)
conf_int<-as.numeric(boot::boot.ci(b, type = c("basic"),conf = (1-alpha))$basic[1,4:5])

tab[i,4:5]<-100*c(conf_int)

cat('parameter r', i, 'of', length(r),'\n')
cat('-----------','\n')
}
} else if(model=="ELO"&length(r)>1) {

for(i in 1:length(r)){

############################### rule to bet

x$player_bet<-ifelse(
(x$Elo_pi_hat/x$impl_prob_i)>r[i],"P_i",
ifelse(
((1-x$Elo_pi_hat)/x$impl_prob_j)>r[i]&(x$impl_prob_j>q),"P_j",
"no bet")
)

x_sub<-subset(x,x$player_bet!="no bet")

x_sub$net_inc<-ifelse(
x_sub$player_bet=="P_i",
c(x_sub$bet_odds_i*x_sub$Outcome_P_i - 1),
c(x_sub$bet_odds_j*x_sub$Outcome_P_j - 1))

tab[i,1]<-r[i]
tab[i,3]<-100*mean(x_sub$net_inc)
tab[i,2]<-nrow(x_sub)

b <- boot::boot(x_sub$net_inc, function(u,i) mean(u[i]), R = R)
conf_int<-as.numeric(boot::boot.ci(b, type = c("basic"),conf = c(1-alpha))$basic[1,4:5])

tab[i,4:5]<-100*c(conf_int)

cat('parameter r', i, 'of', length(r),'\n')
cat('-----------','\n')

} 
} else if(model=="WELO"&length(r)==1){

############################### rule to bet

x$player_bet<-ifelse(
(x$WElo_pi_hat/x$impl_prob_i)>r,"P_i",
ifelse(
((1-x$WElo_pi_hat)/x$impl_prob_j)>r&(x$impl_prob_j>q),"P_j",
"no bet")
)

x_sub<-subset(x,x$player_bet!="no bet")

x_sub$net_inc<-ifelse(
x_sub$player_bet=="P_i",
c(x_sub$bet_odds_i*x_sub$Outcome_P_i - 1),
c(x_sub$bet_odds_j*x_sub$Outcome_P_j - 1))

tab[1,2]<-100*mean(x_sub$net_inc)
tab[1,1]<-nrow(x_sub)

b <- boot::boot(x_sub$net_inc, function(u,i) mean(u[i]), R = R)
conf_int<-as.numeric(boot::boot.ci(b, type = c("basic"),conf = (1-alpha))$basic[1,4:5])

tab[1,3:4]<-100*c(conf_int)

} else if(model=="ELO"&length(r)==1){

############################### rule to bet

x$player_bet<-ifelse(
(x$Elo_pi_hat/x$impl_prob_i)>r,"P_i",
ifelse(
((1-x$Elo_pi_hat)/x$impl_prob_j)>r&(x$impl_prob_j>q),"P_j",
"no bet")
)

x_sub<-subset(x,x$player_bet!="no bet")

x_sub$net_inc<-ifelse(
x_sub$player_bet=="P_i",
c(x_sub$bet_odds_i*x_sub$Outcome_P_i - 1),
c(x_sub$bet_odds_j*x_sub$Outcome_P_j - 1))

tab[1,2]<-100*mean(x_sub$net_inc)
tab[1,1]<-nrow(x_sub)

b <- boot::boot(x_sub$net_inc, function(u,i) mean(u[i]), R = R)
conf_int<-as.numeric(boot::boot.ci(b, type = c("basic"),conf = (1-alpha))$basic[1,4:5])

tab[1,3:4]<-100*c(conf_int)

}
if(length(r)==1){
colnames(tab)<-c("# Bets","ROI(%)","LCI","UCI")
} else {
colnames(tab)<-c("r","# Bets","ROI(%)","LCI","UCI")
}
cat(utils::capture.output(tab),  sep = '\n')
return(tab)
}


#' Random betting function
#'
#' Places bets on players \eqn{i} and \eqn{j} randomly chosen, among all the matches selected by
#' the following strategy:
#' by default, the amount of $1 is placed on the best odds (that is, the highest odds available) for player \eqn{i} for all 
#' the matches where it holds that
#' \deqn{\frac{\hat{p}_{i,j}(t)}{q_{i,j}(t)} > r,}
#' where \eqn{\hat{p}_{i,j}(t)} is the estimated probability (coming from the WElo or Elo model) that player \eqn{i} wins the match \eqn{t} against player \eqn{j} 
#' and \eqn{q_{i,j}(t)} is its implied probability obtained as the reciprical of the Bet365 odds. The implied
#' probability \eqn{q_{i,j}(t)} is assumed to be greater than \eqn{q}. If \eqn{q=0}, all the players are considered. If \eqn{q} increases,
#' heavy longshot players are excluded.
#' Once got the number of matches satisfying the previously described strategy, each player (\eqn{i} and \eqn{j}) on which
#' place a bet is randomly selected. Then the Return-on-Investiment (ROI) of this strategy is stored. Finally, the mean of the ROI 
#' obtained from repeating this operation \eqn{B} times is reported.
#' @return A matrix reporting the number of bets and the mean of the ROI (in percentage) across the \eqn{B} values for every 
#' threshold r used 
#' @importFrom Rdpack reprompt
#' @param x an object of class 'welo', obtained from the \code{\link{welofit}} function
#' @param r Vector or scalar identifying the threshold of the ratio between the estimated and the implied probability (see above)
#' @param q Scalar parameter used to exclude the heavy underdogs signalled by B365 bookmaker.
#' No bets will be placed on those matches where players have odds smaller than \eqn{q}
#' @param model Valid choices are: "WELO" and "ELO"
#' @param bets **optional** Parameter identifying on which type of odds the bet is placed. Default to "Best_odds". Valid choices are:
#' "Best_odds", "Avg_odds" and "B365_odds". "Best_odds" are the highest odds available. "Avg_odds" are the average odds and 
#' "B365_odds" are the Bet365 odds
#' @param B **optional** Number of replicates to calculate the overall mean ROI. Default to 10000
#' @param start_oos **optional** Character parameter denoting the starting year for the bets. 
#' If included (default to NULL), then the bets will be placed on matches starting in that year. It has to be formatted as "YYYY"
#' @param end_oos **optional** Character parameter denoting the ending year for the bets. 
#' If included (default to NULL), then the bets will be placed on matches included in the period "start_oos/end_oos". 
#' It has to be formatted as "YYYY"
#' @examples
#' \donttest{
#' data(atp_2019) 
#' db_clean<-clean(atp_2019)
#' db_est<-welofit(db_clean)
#' rand_bets<-random_betting(db_est,r=c(1.1,1.2,1.3),q=0.3,model="WELO",B=1000)
#' rand_bets
#' }
#' @export

## Random betting function

random_betting<-function(x,r,q,model,bets="Best_odds",B=10000,start_oos=NULL,end_oos=NULL){

### checks

if(!inherits(x,"welo")) { stop("x must be a 'welo' object. Please run first the function 'welofit' and then the function 'random_betting'")}

#### define the dataset

x_res<-x$results

x<-data.frame(x_res,impl_prob_i=1/x_res$Odds_i,impl_prob_j=1/x_res$Odds_j)


#### choice the type of bets

if(bets=="Best_odds"){

x$bet_odds_i<-x$Best_odds_i
x$bet_odds_j<-x$Best_odds_j

} else if (bets=="Avg_odds"){

x$bet_odds_i<-x$Avg_odds_i
x$bet_odds_j<-x$Avg_odds_j

} else {

x$bet_odds_i<-x$Odds_i
x$bet_odds_j<-x$Odds_j

}


#### verify if there are the specifications for the out-of-sample periods

if(!missing(start_oos)){
x$year<-as.numeric(substring(x$Date,7,10))
first_match<-xts::first(which(x$year==start_oos))
last_match<-xts::last(which(x$year==end_oos))
x<-x[first_match:last_match,]
}

if(length(r)>1){
tab<-matrix(rep(NA,length(r)*3),ncol=3)
} else {
tab<-matrix(rep(NA,2),ncol=2)
}

################

if(model=="WELO"&length(r)>1){

for(i in 1:length(r)){

############################### rule to bet

x$player_bet<-ifelse(
(x$WElo_pi_hat/x$impl_prob_i)>r[i],"P_i",
ifelse(
((1-x$WElo_pi_hat)/x$impl_prob_j)>r[i]&(x$impl_prob_j>q),"P_j",
"no bet")
)

x_sub<-subset(x,x$player_bet!="no bet")

ROI<-rep(NA,B)

for(b in 1:B){

player_rand<-sample(c("P_i","P_j"),nrow(x_sub),replace=T)

ROI[b]<-mean(ifelse(
player_rand=="P_i",
c(x_sub$bet_odds_i*x_sub$Outcome_P_i - 1),
c(x_sub$bet_odds_j*x_sub$Outcome_P_j - 1)))*100

} #end B

tab[i,1]<-r[i]
tab[i,2]<-nrow(x_sub)
tab[i,3]<-mean(ROI)

} #end length(r)

} else if(model=="ELO"&length(r)>1) {

for(i in 1:length(r)){

############################### rule to bet

x$player_bet<-ifelse(
(x$Elo_pi_hat/x$impl_prob_i)>r[i],"P_i",
ifelse(
((1-x$Elo_pi_hat)/x$impl_prob_j)>r[i]&(x$impl_prob_j>q),"P_j",
"no bet")
)

x_sub<-subset(x,x$player_bet!="no bet")

ROI<-rep(NA,B)

for(b in 1:B){

player_rand<-sample(c("P_i","P_j"),nrow(x_sub),replace=T)

ROI[b]<-mean(ifelse(
player_rand=="P_i",
c(x_sub$bet_odds_i*x_sub$Outcome_P_i - 1),
c(x_sub$bet_odds_j*x_sub$Outcome_P_j - 1)))*100

} #end B

tab[i,1]<-r[i]
tab[i,2]<-nrow(x_sub)
tab[i,3]<-mean(ROI)
} #end length(r)

} else if(model=="WELO"&length(r)==1){

############################### rule to bet

x$player_bet<-ifelse(
(x$WElo_pi_hat/x$impl_prob_i)>r,"P_i",
ifelse(
((1-x$WElo_pi_hat)/x$impl_prob_j)>r&(x$impl_prob_j>q),"P_j",
"no bet")
)

x_sub<-subset(x,x$player_bet!="no bet")

ROI<-rep(NA,B)

for(b in 1:B){

player_rand<-sample(c("P_i","P_j"),nrow(x_sub),replace=T)

ROI[b]<-mean(ifelse(
player_rand=="P_i",
c(x_sub$bet_odds_i*x_sub$Outcome_P_i - 1),
c(x_sub$bet_odds_j*x_sub$Outcome_P_j - 1)))*100

} #end B

tab[,1]<-nrow(x_sub)
tab[,2]<-mean(ROI)

} #end if
else if(model=="ELO"&length(r)==1){

############################### rule to bet

x$player_bet<-ifelse(
(x$Elo_pi_hat/x$impl_prob_i)>r,"P_i",
ifelse(
((1-x$Elo_pi_hat)/x$impl_prob_j)>r&(x$impl_prob_j>q),"P_j",
"no bet")
)

x_sub<-subset(x,x$player_bet!="no bet")

tab[,1]<-nrow(x_sub)

ROI<-rep(NA,B)

for(b in 1:B){

player_rand<-sample(c("P_i","P_j"),nrow(x_sub),replace=T)

ROI[b]<-mean(ifelse(
player_rand=="P_i",
c(x_sub$bet_odds_i*x_sub$Outcome_P_i - 1),
c(x_sub$bet_odds_j*x_sub$Outcome_P_j - 1)))*100

} #end B

tab[,1]<-nrow(x_sub)
tab[,2]<-mean(ROI)

} #end if

###########################

if(length(r)==1){
colnames(tab)<-c("# Bets","ROI(%)")
} else {
colnames(tab)<-c("r","# Bets","ROI(%)")
}
cat(utils::capture.output(tab),  sep = '\n')

return(tab)

}

