\name{heterocitation_authors}
\alias{heterocitation_authors}
\alias{Author Heterocitation}
\alias{Author Hetero-citation}
\concept{Author Heterocitation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
This function computes heterocitation metrics for authors
}
\description{
This function computes heterocitation metrics for authors. The heterocitation share (Sx) and heterocitation balance (Dx) of an author are calculated as the average of these metrics for papers published by this author within the given time window. See the man page of \code{\link{heterocitation}} for definitions of heterocitation metrics.
}
\usage{
heterocitation_authors(gr, infLimitYear, supLimitYear, pub_threshold = 0, 
                       remove_orphans = F, remove_citations_to_joint_papers = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gr}{Citation graph priorly preprocessed with \code{\link{precompute_heterocitation}}}
  \item{infLimitYear}{Start year of the time window considered (included)}
  \item{supLimitYear}{End year of the time window considered (*excluded*)}
  \item{pub_threshold}{Minimum number of publications for authors to be considered.}
  \item{remove_orphans}{Do not consider publications that do not cite any other paper in the dataset (i.e. orphan nodes in the citation network)}
  \item{remove_citations_to_joint_papers}{Do not consider publications belonging to both corpora in the authors' average corpus calculation.}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
Returns a data frame containing author name ("Authors"), number of publications ("NbPubs"), list of publication years ("Years"), list of publications corpora ("Corpus"), list of publication heterocitation share ("Sx"), list of publication heterocitation balance ("Dx"), average heterocitation share ("avgSx"), average heterocitation balance ("avgDx"), average corpus value of publications ("avgCorpus"), regression coefficient of the heterocitation share evolution ("coeffSx"), regression coefficient of the heterocitation balance evolution ("coeffDx"), regression coefficient of the evolution of the corpus value of publications ("coeffCorpus").   
}
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

# Heterocitation
gr<-precompute_heterocitation(gr,labels, 1990, 2018)

# Author heterocitation
heterocitation_authors(gr, 1990, 2018)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
