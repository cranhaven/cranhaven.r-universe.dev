\name{create_bibliography}
\alias{create_bibliography}
\alias{Bibliographic Network Creation}
\alias{Citation Graph Creation}
\concept{Citation Graph}
\concept{Citation Network}
\concept{Literature Review}
\concept{Bibliographic Network}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to create a bibliographic dataset
}
\description{
This function creates a bibliographic dataset based on two external corpus files, each representing the bibliography of a given domain.
}
\usage{
create_bibliography(corpora_files, labels, keywords, retrieve_pubdates = F, 
                    clean_refs = F, encoding = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{corpora_files}{Vector containing the pathes to two corpus files (e.g. Scopus exports). The CSV files should contain for each record at least Authors (comma separated), Publication Title, Publication Year, and References (semicolon separated). The inclusion of DOI (for date checking; see the retrieve_pubdates option) as well as Abstract, Author.Keywords, and Index.Keywords (for the in-depth identification of publications belonging to both corpora) are strongly recommended.}
  \item{labels}{Labels (i.e. names) given to the two corpora to be analyzed.}
  \item{keywords}{Keywords identifying the two corpora}
  \item{retrieve_pubdates}{Flag indicating whether to confirm publication dates by retrieving them (see \code{\link{get_date_from_doi}})}
  \item{clean_refs}{Attempt to clean references and keep titles only. NOT RECOMMENDED, especially if \code{\link{build_graph}} should be used subsequently.}
  \item{encoding}{Character encoding used in the input files.}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
Returns a dataframe containing a bibliographic dataset usable by Diderot and including all references from both corpora.
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
\code{\link{build_graph}}
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

labels<-c("Corpus1","Corpus2")

# Build a bibliographical dataset from Scopus exports
db<-create_bibliography(corpora_files=c(tempfi1,tempfi2), 
                        labels=labels, keywords=NA)
unlink(c(tempfi1, tempfi2))
}
\dontrun{
  # Two corpora on individual-based modelling (IBM) and agent-based modelling (ABM)
  # were downloaded from Scopus. The structure of each corpus is as follows:
  tt<-read.csv("IBMmerged.csv", stringsAsFactors=FALSE)
  str(tt,strict.width="cut")
  ### 'data.frame':  3184 obs. of  9 variables:
  ### $ Authors        : chr  "Chen J., Marathe A., Marathe M." "Van Dijk D., Sl"..
  ### $ Title          : chr  "Coevolution of epidemics, social networks, and in"..
  ### $ Year           : int  2010 2010 2010 2010 2010 2010 2010 2010 2010 2010 ...
  ### $ DOI            : chr  "10.1007/978-3-642-12079-4_28" "10.1016/j.procs.20"..
  ### $ Link           : chr  "http://www.scopus.com/inward/record.url?eid=2-s2."..
  ### $ Abstract       : chr  "This research shows how a limited supply of antiv"..
  ### $ Author.Keywords: chr  "Antiviral; Behavioral economics; Epidemic; Microe"..
  ### $ Index.Keywords : chr  "Antiviral; Behavioral economics; Epidemic; Microe"..
  ### $ References     : chr  "(2009) Centre Approves Restricted Retail Sale of "..
  
  # Define the name of corpora (labels) and specific keywords to identify relevant
  # publications (keys).
  labels<-c("IBM","ABM")
  keys<-c("individual-based model|individual based model", 
          "agent-based model|agent based model")
  
  # Build the IBM-ABM bibliographical dataset from Scopus exports
  db<-create_bibliography(corpora_files=c("IBMmerged.csv","ABMmerged.csv"), 
                          labels=labels, keywords=keys)
  ### [1] "File IBMmerged.csv contains 3184 records"
  ### [1] "File ABMmerged.csv contains 9641 records"
  
  # Processed output. Note the field name changes (for standardization with ISI Web 
  # of Knowledge format) and addition of the "Corpus" field (with identification of
  # joint "IBM | ABM" publications based on keywords).
  str(db, strict.width="cut")
  ### 'data.frame':  12504 obs. of  10 variables:
  ### $ Authors         : chr  "Chen J., Marathe A., Marathe M." "Van Dijk D., Sloot "..
  ### $ Cite Me As      : chr  "Coevolution of epidemics, social networks, and indivi"..
  ### $ Year            : int  2010 2010 2010 2010 2010 2010 2010 2010 2010 2010 ...
  ### $ DOI             : chr  "10.1007/978-3-642-12079-4_28" "10.1016/j.procs.2010.0"..
  ### $ Link            : chr  "http://www.scopus.com/inward/record.url?eid=2-s2.0-78"..
  ### $ Abstract        : chr  "This research shows how a limited supply of antiviral"..
  ### $ Author.Keywords : chr  "Antiviral; Behavioral economics; Epidemic; Microecono"..
  ### $ Index.Keywords  : chr  "Antiviral; Behavioral economics; Epidemic; Microecono"..
  ### $ Cited References: chr  "(2009) Centre Approves Restricted Retail Sale of Tami"..
  ### $ Corpus          : chr  "IBM" "IBM | ABM" "IBM | ABM" "IBM" ...
}
}
%\keyword{ ~kwd1 }
