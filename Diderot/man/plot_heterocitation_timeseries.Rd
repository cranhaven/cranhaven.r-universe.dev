\name{plot_heterocitation_timeseries}
\alias{plot_heterocitation_timeseries}
\alias{Heterocitation Timeseries}
\alias{Hetero-citation Timeseries}
\concept{Heterocitation Share}
\concept{Heterocitation Balance}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to plot the heterocitation share and heterocitation balance timeseries
}
\description{
This function plots and returns annual heterocitation share and heterocitation balance values.
}
\usage{
plot_heterocitation_timeseries(gr_arg, labels, mini = -1, maxi = -1, cesure = -1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gr_arg}{Citation graph}
  \item{labels}{Labels (i.e. names) of the two corpora featured in the graph.}
  \item{mini}{Start year of the time window}
  \item{maxi}{End year of the time window}
  \item{cesure}{Year before which values should be cumulated. Default value is -1, which indicates that each year in the time window should be plotted.}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
Returns a dataframe with year and annual values for heterocitation share (sx1, sx2 and sxall for corpus A and B and global resp.) and heterocitation balance (dx1, dx2 and dxall for corpus A and B and global resp.).
}
%%\references{
%% ~put references to the literature/web site here ~
%%}
\author{
Christian Vincenot (christian@vincenot.biz)
}
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{precompute_heterocitation}}, \code{\link{heterocitation}}
}
\examples{
\dontshow{
# Generate corpora
corp1<-data.frame(Authors=paste("Author",round(runif(45,1,20)),sep=""),Title=paste("Title",seq(1,45), sep=""), Year=round(runif(45,1990,2018)), References=NA,stringsAsFactors=FALSE)
corp2<-data.frame(Authors=paste("Author",round(runif(65,15,35)),sep=""),Title=paste("Title",seq(46,110), sep=""), Year=round(runif(65,1990,2018)), References=NA, stringsAsFactors=FALSE)

len1<-length(corp1$Authors)
len2<-length(corp2$Authors)
lnall<-len1+len2

for (i in seq(1:lnall)) {
  str<-""
  for (j in seq(1:round(runif(1,1,4)))) {
    rn<-round(runif(1,1,lnall))
    if (rn > len1) { 
      str<-sprintf("\%s \%s. \%d. \%s;", str, corp2$Authors[rn-len1], corp2$Year[rn-len1], corp2$Title[rn-len1])
    } else {
      str<-sprintf("\%s \%s. \%d. \%s;", str, corp1$Authors[rn], corp1$Year[rn], corp1$Title[rn])
    }
  }
  if (i>len1) {
    corp2$References[i-len1]<-str
  } else corp1$References[i]<-str
}

# Create joint references
for (i in seq(1:5)) {
  corp1<-rbind(corp1, corp2[runif(1,1,len2),])
  corp2<-rbind(corp2, corp1[runif(1,1,len1),])
}

# Add duplicate entry
corp1<-rbind(corp1, corp1[1,])

tempfi1<-file.path(tempdir(),"corpus1.csv")
tempfi2<-file.path(tempdir(),"corpus2.csv")
write.csv(corp1, tempfi1)
write.csv(corp2, tempfi2)
}

labels<-c("Corpus1","Corpus2")

# Build a bibliographical dataset from Scopus exports
db<-create_bibliography(corpora_files=c(tempfi1,tempfi2), 
                        labels=labels, keywords=NA)
\dontshow{unlink(c(tempfi1, tempfi2))}

# Build graph
gr<-build_graph(db=db,small.year.mismatch=TRUE, attrs=c("Corpus","Year","Authors"), nb.cores=1)

# Heterocitation timeseries
gr<-precompute_heterocitation(gr,labels, 1990, 2018)
plot_heterocitation_timeseries(gr, labels, 1990, 2018)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
