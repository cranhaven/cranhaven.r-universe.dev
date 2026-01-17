\name{build_graph}
\alias{build_graph}
\alias{Citation Graph Building}
\concept{Citation Graph}
\concept{Citation Network}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Builds a citation graph. 
}
\description{
Builds a citation graph based on a database of bibliographic records generated with create_bibliography. This process is automatically parallelized on multicore hardware. By default, matching between title and references is done based on the full title, publication year, and three first authors. Publication attributes present in the dataframe can be copied to graph nodes using the attrs argument.
}
\usage{
build_graph(db, title = "Cite Me As", year = "Year", authors = "Authors", 
            ref = "Cited References", set.title.as.name = F, attrs = NULL, 
            verbose = F, makeCluster.type = "PSOCK", nb.cores=NA, 
            fine.check.threshold = 1000, fine.check.nb.authors = 3, 
            small.year.mismatch = T, debug = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{db}{Bibliographic database created with created_bibliography.}
  \item{title}{Name of the data frame column in which publication titles are listed.}
  \item{year}{Name of the data frame column in which publication years are listed.}
  \item{authors}{Name of the data frame column in which publication authors are listed.}
  \item{ref}{Name of the data frame column in which publication references are listed.}
  \item{set.title.as.name}{Set graph vertex ID to publication title}
  \item{attrs}{Attributes of the bibliographic database (i.e. data frame column names, such as "Authors"", "Year") to be set as vertex attributes.}
  \item{verbose}{Verbosity flag triggering a more detailed output during graph building.}
  \item{makeCluster.type}{Type of cluster to be used to parallelize the graph building process. For more options, see \code{\link{makeCluster}} in the doParallel library.}
  \item{nb.cores}{Number of cores to be used for parallel computation.}
  \item{fine.check.threshold}{Title length under which citation matching is further confirmed based on publication year. This value can be reduced to increase performance on large bibliographic databases. By default, publication year check is always performed.}
  \item{fine.check.nb.authors}{Maximum number of authors to check against for citation matching. This value can be reduced to increase performance on large bibliographic databases. Default value is three authors.}
  \item{small.year.mismatch}{Flag indicating whether small year mismatches (+- 1 year) should be tolerated. It is recommended to keep this this flag to TRUE to accomodate usual inconsistencies in bibliographic databases.}
  \item{debug}{Debug flag allowing the user to browse function calls upon execution error. For more details, see \code{\link{recover}} in the utils library.}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}

\value{Returns a graph object.}

%%\references{
%% ~put references to the literature/web site here ~
%%}
\author{
Christian Vincenot (christian@vincenot.biz)
}

%\note{
%%  ~~further notes~~
%}

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
    } else str<-sprintf("\%s \%s. \%d. \%s;", str, corp1$Authors[rn], corp1$Year[rn], corp1$Title[rn])
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

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
