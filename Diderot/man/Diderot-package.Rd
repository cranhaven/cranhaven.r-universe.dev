\name{Diderot-package}
\alias{Diderot-package}
\alias{Diderot}
\concept{Citation Graph}
\concept{Citation Network}
\concept{Literature Review}
\concept{Bibliographic Network}
\concept{Scopus}
\concept{ISI Web of Knowledge}
\concept{Google Scholar}
\concept{Bibliometrics}
\concept{Scientometrics}
\docType{package}
\title{
Bibliographic Network Analysis Package
}

\description{
\tabular{c}{
   \if{html}{\figure{Diderot.png}{options: width="30\%" alt="Figure: Diderot.png"}}
   \if{latex}{\figure{Diderot.pdf}{options: width=6cm}} \cr
   \emph{\bold{Denis Diderot} (1713-1784), French philosopher and co-founder of the modern encyclopedia.}
}
This package allows to detect and quantify the unification or separation of two bibliographic corpora through the creation of citation networks. This tool can be used to study the spread of concepts across scientific disciplines, or the fusion/fission of scientific communities.  
}

\details{
\tabular{ll}{
Package: \tab Diderot\cr
Type: \tab Package\cr
Version: \tab 0.13\cr
Date: \tab 2020-04-17\cr
License: \tab GPL (>=2)\cr
}
A typical flow of use of the package includes the following points.

First, literature metadata, including references, from the two fields of studies to analyze are downloaded from Scopus (or built manually). This data is imported to create a bibliographic dataset using \code{\link{create_bibliography}}.

Second, a graph is created with a call to \code{\link{build_graph}} to reproduce the citation network in the bibliographic dataset.

Finally, statistical analysis can be performed on the graph to assess the fusion/fission state of the two corpora/communities. Heterocitation indices (i.e. share and balance) show how much publications or authors cite papers from the other corpus (see \code{\link{heterocitation}} and \code{\link{heterocitation_authors}} respectively). Such analysis shall always be preceded by a call to \code{\link{precompute_heterocitation}} to perform initial calculations. These metrics are completed by traditional as well as custom modularity metrics (see \code{\link{compute_modularity}} and \code{\link{compute_custom_modularity}} respectively) that translate how much the communities are separated. Publications that foster mutual awareness and cross-fertilization between the corpora/communities can be identified using the usual betweeness centrality metric (see \code{\link{compute_BC_ranking}}) and the Ji index (see \code{\link{compute_Ji_ranking}}).
}
\author{
Christian Vincenot

Maintainer: Christian Vincenot (christian@vincenot.biz)
}
%%\references{
%%}

\keyword{ package }
\seealso{
\code{\link[igraph:igraph-package]{igraph}}
}
\examples{
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
  
  # Build and save citation graph
  gr<-build_graph(db=db,small.year.mismatch=T,fine.check.nb.authors=2,
                  attrs=c("Corpus","Year","Authors", "DOI"))
  ### [1] "Graph built! Execution time: 1200.22 seconds."
  save_graph(gr, "graph.graphml")
  
  # Compute and plot modularity
  compute_modularity(gr_sx, 1987, 2018)
  ###[1] 0.3164805
  plot_modularity_timeseries(gr_sx, 1987, 2018, window=1000)
  
  # Compute and plot publication heterocitation
  gr_sx<-precompute_heterocitation(gr,labels=labels,infLimitYear=1987, supLimitYear=2018)
  ###[1] "Summary of the nodes considered for computation (1987-2017)"
  ###[1] "-----------------------------------------------------------"
  ###[1] "IBM     ABM     IBM|ABM"
  ###[1] "1928     5378     153"
  ###[1]
  ###[1] "Edges summary"
  ###[1] "-------------"
  ###[1] "IBM->IBM/IBM->Other 5583/1086 => Prop 0.163"
  ###[1] "ABM->ABM/ABM->Other 16946/2665 => Prop 0.136"
  ###[1] "General Same/Diff 22529/3751 => Prop 0.143"
  ###[1]
  ###[1] "Heterocitation metrics"
  ###[1] "----------------------"
  ###[1] "Sx ALL /  IBM  /  ABM"
  ###[1] "0.127 / 0.137 / 0.124"
  ###[1] "Dx ALL /  IBM  /  ABM"
  ###[1] "-0.652 / -0.803 / -0.598"
  heterocitation(gr_sx, labels=labels, 1987, 2005)
  ###[1] "Sx ALL /  ABM  /  IBM"
  ###[1] "0.047 / 0.214 / 0.007"
  ###[1] "Dx ALL /  ABM  /  IBM"
  ###[1] "-0.927 / -0.690 / -0.982"
  plot_heterocitation_timeseries(gr_sx, labels=labels, mini=-1, maxi=-1, cesure=2005)

  # Compute author heterocitation
  hetA<-heterocitation_authors(gr_sx, 1987, 2018, pub_threshold=4)
  head(hetA[order(hetA$avgDx,decreasing=T),c(1)], n=10)
  ### [1] "Ashlock D." "Evora J." "Hernandez J.J." "Hernandez M." "Gooch K.J."          
  ### [6] "Reinhardt J.W." "Ng K." "Kazanci C." "Senior A.M." "Ariel G." 
  
  # Try to figure which publication are most impactful in terms of cross-fertilization
  jir<-compute_Ji_ranking(gr_sx, labels=labels, 1987, 2018)
  head(jir[,c(2,7)],n=3)
  ###         Title                                                                           Ji
  ### 758     A standard protocol for describing individual-based and agent-based models      200
  ### 4437    Pattern-oriented modeling of agent-based complex systems: Lessons from ecology  134
  ### 33      The ODD protocol: A review and first update                                     120
}
}
