\name{GetExampleData}
\alias{GetExampleData}
\title{
Get the environment data
}
\description{
Get variables in current environment.
}
\usage{
GetExampleData(exampleData)
}
\arguments{
  \item{exampleData}{
  A character string. It must be one of them, including "code", "compound", "g2", "gene", "gene2path", "gene2symbol", "GeneExp", "keggGene2gene", "lnc2Name", "lncBackground", "LncExp", "LncGenePairs", "mart", "nocode", "pp", "resultT" and "sub".
 }
}
\details{
 The parameter exampleData is "code", which represents to obtain all human coding gene collected from Genecode.
 
 The parameter K2riData is "compound", which represents to obtain all huamn compound.
 
 The parameter K2riData is "g2", which represents to obtain undirect KEGG metabolic pathway graphs with genes as nodes.
 
 The parameter K2riData is "gene", which represents to obtain interseted gene of example.
 
 The parameter K2riData is "gene2symbol", which represents to obtain gene-symbol data.
 
 The parameter K2riData is "gene2path", which represents to obtain gene-pathway data.
 
 The parameter K2riData is "GeneExp", which represents to obtain gene expression profile of example.
 
 The parameter K2riData is "keggGene2gene", which represents to obtain kegggene-gene data.
 
 The parameter K2riData is "lnc2Name", which represents to obtain lncRNA Ensemble-Name data.
 
 The parameter K2riData is "lncBackground", which represents to obtain background of lncRNA, gene or lncRNA-gene.
 
 The parameter K2riData is "LncExp", which represents to obtain lncRNA expression profile of example.
 
 The parameter K2riData is "LncGenePairs", which represents to obtain the cmepetitively regulated lncRNA-mRNA interactions.
 
 The parameter K2riData is "mart", which represents to obtain gene ensemble-name-entrez data.
 
 The parameter K2riData is "nocode", which represents to obtain all huaman lncRNA collected from Genecode.
 
 The parameter K2riData is "resultT", which represents to obtain result of example.
 
 The parameter K2riData is "sub", which represents to obtain subpahtway of example.
 
 The parameter K2riData is "geneLnc", which represents to obtain user-interested genes and lncRNAs of example.
}

\author{ Xinrui Shi, Chunquan Li and Xia Li}


\examples{
\dontrun{


# obtain lncRNA expression profile #
LncExp <- GetExampleData(exampleData="LncExp")


}
}
\keyword{ file }
