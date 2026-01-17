\name{compute_Ji}
\alias{compute_Ji}
\alias{Ji Metric}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to compute the evolution of the Ji metric for a term (e.g. publication title) present in the reference list of a bibliographic dataset.
}
\description{
This function computes the Ji metric for a given term from a bibliographic dataset and returns its annual evolution within the timeframe specified. This metric indicates how much the term (e.g. publication title, software name) is cited simulteaneously in the references of both corpora and is thus important for cross-fertilization between the two communities. This function is run on the bibliographic dataset (created with \code{\link{create_bibliography}}) and is thus useful before graph creation or when the term to be searched is not the title of a node in the resulting graph. For instance, if the user knows that a publication (or, e.g. software or scientific database referenced only through a URL or grey literature) is cited and may have an impact on cross-fertilization between the two communities (the literature of which is represented by the two corpora) but does not have its own entry in the bibliographic database and would therefore not be featured as a node in the graph created by \code{\link{build_graph}}, the \code{\link{compute_Ji}} function can be used to assess its importance.   
}
\usage{
compute_Ji(db, pubtitle, labels, from = -1, to = -1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{db}{Bibliographic database created with created_bibliography.}
  \item{pubtitle}{Publication title, or more generally term to be searched (e.g. software name).}
  \item{labels}{Labels (i.e. names) of the two corpora featured in the graph.}
  \item{from}{Start year of the time window considered (included)}
  \item{to}{End year of the time window considered (*excluded*)}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{Dataframe containing year and Ji metric value.}
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
\code{\link{create_bibliography}}
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

# Compute Ji
compute_Ji(db, "Title1", labels, from=1990, to=2018)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
