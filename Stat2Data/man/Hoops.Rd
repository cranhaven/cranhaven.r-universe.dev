\name{Hoops}
\alias{Hoops}
\docType{data}
\title{Grinnell College Basketball Games}
\description{
Data from games played by the Grinnell College men's basketball team between 1997 and 2006
}
\format{
  A data frame with 147 observations on the following 22 variables.
  \tabular{rl}{
    \code{Game} \tab {An ID number assigned to each game}\cr
    \code{Opp} \tab {Name of the opponent school for the game}\cr
    \code{Home} \tab {Indicator variable where \code{1} = home game and \code{0} = away game}\cr
    \code{OppAtt} \tab {Number of field goal attempts by the opposing team}\cr
    \code{GrAtt} \tab {Number of field goal attempts by Grinnell}\cr
    \code{Gr3Att} \tab {Number of three-point field goal attempts by Grinnell}\cr
    \code{GrFT} \tab {Number of free throw attempts by Grinnell}\cr
    \code{OppFT} \tab {Number of free throw attempts by the opponent}\cr
    \code{GrRB} \tab {Total number of Grinnell rebounds}\cr
    \code{GrOR} \tab {Number of Grinnell offensive rebounds}\cr
    \code{OppDR} \tab {Number of defensive rebounds the opposing team had}\cr
    \code{OppPoint} \tab {Points scored in the game by the opponent}\cr
    \code{GrPoint} \tab {Points scored in the game by Grinnell}\cr
    \code{GrAss} \tab {Number of assists Grinnell had in the game}\cr
    \code{OppTO} \tab {Number of turnovers the opposing team gave up}\cr
    \code{GrTO} \tab {Number of turnovers Grinnell gave up}\cr
    \code{GrBlocks} \tab {Number of blocks Grinnell had in the game}\cr
    \code{GrSteal} \tab {Number of steals Grinnell had in the game}\cr
    \code{X40Point} \tab {Indicator variable that is 1 if some Grinnell player scored 40 or more points}\cr
    \code{X30Point} \tab {Indicator variable that is 1 if some Grinnell player scored 30 or more points}\cr
    \code{WinLoss} \tab {\code{1}=Grinnell win or \code{0}=Grinnell loss}\cr
    \code{PtDiff} \tab {Point differential for the game (Grinnell score minus Opponent's score)}\cr
  }
}
\details{
Since 1991, David Arseneault, men's basketball coach of Grinnell College,
has developed a unique, fast-paced style of basketball that he calls "the system."
This dataset comes from the 147 games the Grinnell team
played within its athletics conference between the 1997-98 season through the 2005-06 season.
}
\source{
These data were collected by Grinnell College students Eric Ohrn and Ben Johannsen.
}

\keyword{datasets}
