#' Plots the Sympathy, Antipathy, Preference or Impact table of a group
#' 
#' Shows the social status of each person and the persons who have contributed
#' to that status
#' 
#' Plots the Sociometric Status, as calculated by SSrat, of each individual in
#' a group. In the case of positive or negative ratings (Sympathy or
#' Antipathy), the arrows shows who has has provided these ratings. The left
#' legend shows the social status, identified by color. The right legend shows
#' the individuals, identified by number and optionally their names. See
#' \code{\link{calcgroup}} for more information on the four SSrat data tables
#' and alpha.
#' 
#' @aliases gplot.SSrat gplotSSrat
#' @param calcedgroup calcedgroup is the outcome of the function
#' \code{\link{calcgroup}}. See calcgroup for more information. When
#' \code{calcgroup$dataframe} is extended with the variable \code{resplabel},
#' this is used automatically as an extra indicator of each respondent.
#' @param maintitle The maintitle is either "Sympathy", "Antipathy"",
#' "Preference"" or "Impact". This also determinates the selection of the
#' correct table from calcedgroup.See calcgroup for more information. Default
#' is "Sympathy".
#' @param SS.alpha This can be any of the alpha's used for the calculation of
#' status in the calcedgroups. Reasonable values are "SS.01" , "SS.05" or 0.10.
#' Default is "SS.05". As a rule of thumb, alpha should be chosen so that the
#' group of Popular respondents is about 15%. N.B. SS.alpha indicates the
#' column from the dataframe to use, and should be equal to that column name.
#' @param thresh Number indicating the threshold for tie values. Only ties of
#' value > thresh are displayed. By default, thresh = 0. This allows for the
#' reduction of arrows (edges), which may make the plot more readable.
#' 
#' @return A two-column matrix containing the vertex positions as x,y
#' coordinates. See also gplot sna.
#' @author Hans Landsheer
#' @seealso \code{\link{calcgroup}} \code{\link{gplot}}
#' @references Wasserman, S. and Faust, K. (1994) Social Network Analysis:
#' Methods and Applications. Cambridge: Cambridge University Press.\cr Maassen,
#' G. H. and Landsheer, J. A. (1998). SSRAT: The processing of rating scales
#' for the determination of two-dimensional sociometric status. Behavior
#' Research Methods Instruments & Computers, 30(4), 674-679.
#' @keywords hplot & High-Level Plots
#' @examples
#' 
#' data(klas2.rat)
#' out =calcgroup (school=3, group=2, dataframe=klas2.rat, scalelength="3")
#' out$dataframe
#' 
#' gplot.SSrat(calcedgroup=out, maintitle="Sympathy", SS.alpha="SS.10")
#' gplot.SSrat(out, "Antipathy", SS.alpha="SS.10")
#' gplot.SSrat(out, "Preference", SS.alpha="SS.10")
#' x=gplot.SSrat(out, "Impact", SS.alpha="SS.10")
#' x
#' 
#' @importFrom graphics legend par plot
#' @importFrom sna gplot
#' 
#' @export
gplot.SSrat <- function(calcedgroup, maintitle=c('Sympathy','Antipathy','Preference','Impact'), 
                        SS.alpha="SS.05", thresh=0){
  maintitle=match.arg(maintitle)
  switch(maintitle,
         Sympathy = {plotdata=calcedgroup$S},
         Antipathy = {plotdata=calcedgroup$A},
         Preference = {plotdata=calcedgroup$P},
         Impact = {plotdata=calcedgroup$I}
  )
  if (thresh > 0) {maintitle=paste(maintitle,", threshold = ", thresh,sep='')}
  nrratees = nrow(calcedgroup$dataframe)
  status= as.factor(calcedgroup$dataframe[, SS.alpha])
  x=gplot(plotdata, usecurve = F, main = maintitle, label=calcedgroup$dataframe$respid, 
          vertex.col=as.numeric(status), vertex.sides=4, thresh=thresh)
  opar <- par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), 
              mar=c(0, 0, 0, 0), new=TRUE)
  on.exit(par(opar))
  plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
  legend("topright", legend=paste(calcedgroup$dataframe$respid, calcedgroup$dataframe$resplabel))
  legend("topleft", legend=levels(status), fill=1:length(levels(status)))
  invisible(x)
}


