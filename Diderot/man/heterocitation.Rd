\name{heterocitation}
\alias{heterocitation}
\alias{Hetero-Citation}
\concept{Heterocitation Share}
\concept{Heterocitation Balance}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function to calculate the heterocitation between two corpora}
\description{
This function calculates the heterocitation share and heterocitation balance between two corpora A and B in the time window specified. The heterocitation share (Sx) of a publication belonging to corpus A is defined as the percentage of citations to publications belonging to corpus B (or A|B) in its reference list. The global heterocitation share for corpus A is calculated as the average heterocitation share of the publications that corpus A contains (e.g. a value of 0.2 for corpus A indicates that, on average, publications in corpus A cite only 20\% of papers from corpus B). The heterocitation balance metric (Dx), on the other hand, takes into consideration the respective sizes of corpus A and B to discern how much the heterocitation share deviates from values expected in the case of well-mixedness (i.e. if A and B originated from a unique community; e.g. a value of -50\% for corpus A indicates that, on average, publications in corpus A cite papers from corpus B half less frequently than expected, which suggests a lack of mutual awareness between the corpora and related communities).   
}
\usage{
heterocitation(gr, labels, infLimitYear, supLimitYear)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gr}{Citation graph priorly preprocessed with \code{\link{precompute_heterocitation}}}
  \item{labels}{Labels (i.e. names) of the two corpora featured in the graph.}
  \item{infLimitYear}{Start year of the time window considered (included)}
  \item{supLimitYear}{End year of the time window considered (*excluded*)}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{Returns a numerical vector containing, in this order, the heterocitation share (Sx) for corpus A, B and global, and the heterocitation balance (Dx) for A, B and global.}
%%\references{
%% ~put references to the literature/web site here ~
%%}
\author{
Christian Vincenot (christian@vincenot.biz)
}
\note{
\code{\link{precompute_heterocitation}} should be called before running this function. 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{precompute_heterocitation}}, \code{\link{plot_heterocitation_timeseries}}, \code{\link{heterocitation_authors}}, \code{\link{MC_baseline_distribution}}, \code{\link{significance_Dx}}
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

# Heterocitation
gr<-precompute_heterocitation(gr,labels, 1990, 2018)
heterocitation(gr,labels, 1990, 2018)
 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
