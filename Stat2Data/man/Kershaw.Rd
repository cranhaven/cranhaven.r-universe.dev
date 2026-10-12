\name{Kershaw}
\alias{Kershaw}
\docType{data}
\title{Kershaw Pitch Data}
\description{
Pitch-by-pitch data for baseball pitcher Clayton Kershaw in the 2013 season
}
\format{
  A data frame with 3402 observations on the following 24 variables.
  \describe{
    \item{\code{BatterNumber}}{Number of batters faced so far that game}
    \item{\code{Outcome}}{One of 14 possible results for a pitch (e.g. \code{Ball}, \code{Ball In Dirt}, \code{Called Strike}, ...,  \code{Swinging Strike (Blocked))}}
    \item{\code{Class}}{One of three classifications (\code{B}=ball, \code{S}=strike, or \code{X}=in play)}
    \item{\code{Result}}{From pitcher's perspective (\code{Neg}=ball or hit,  \code{Pos}=strike or out)}
    \item{\code{Swing}}{Did the batter swing at the pitch? (\code{No} or \code{Yes})}
    \item{\code{Time}}{Date and time of the pitch (format \code{yyyy-mm-ddThh:mm:ssZ}) }
    \item{\code{StartSpeed}}{Speed leaving the pitcher's hand (in mph)}
    \item{\code{EndSpeed}}{Speed crossing home plate (in mph)}
    \item{\code{HDev}}{Horizontal movement (inches)}
    \item{\code{VDev}}{Vertical movement (inches)}
    \item{\code{HPos}}{Horizontal position at home plate (inches from center, positive is catcher's right)}
    \item{\code{VPos}}{Vertical position at home plate (inches above the ground)}
    \item{\code{PitchType}}{Code for pitch type (\code{CH}=changeup, \code{CU}=curve, \code{FF}=fastball, or \code{SL}=slider)}
    \item{\code{Zone}}{1-9 in theoretical strike zone (upper left to lower right), 11-14 are out of strike zone}
    \item{\code{Nasty}}{A measure on a 0-100 scale of difficulty of the pitch to hit (100 is most difficult)}
    \item{\code{Count}}{Ball strike count (\code{0-0}, \code{0-1}, \code{0-2}, \code{1-1}, \code{1-2}, \code{2-1}, \code{2-2}, \code{3-1}, or \code{3-2})}
    \item{\code{BallCount}}{Number of balls before the pitch (0, 1, 2, or 3)}
    \item{\code{StrikeCount}}{Number of strikes before the pitch (0, 1, or 2)}
    \item{\code{Inning}}{Inning of the game}
    \item{\code{InningSide}}{Portion of the inning (\code{bottom}= pitcher at home or \code{top}=pitcher away)}
    \item{\code{Outs}}{Number of outs when the pitch is thrown}
    \item{\code{BatterHand}}{Batter's stance (\code{L}=left or \code{R}=right)}
    \item{\code{ABEvent}}{Result of the at bat (several possibilities) }
    \item{\code{Batter}}{Name of the batter faced}
  }
}
\details{
Dataset includes information for 3,402 individual pitches thrown by Los Angeles Dodger baseball pitcher Clayton Kershaw during the 2013 regular season when he won the Cy Young award as the best pitcher in the National League.  Many variables are measured using Major League Baseball's PITCHf/x system that uses camera systems in each ballpark to track characteristics of each pitch thrown.
}
\source{
Data scraped from the MLB GameDay website (http://gd2.mlb.com/components/game/mlb/) using pitchRx
}
\keyword{datasets}
