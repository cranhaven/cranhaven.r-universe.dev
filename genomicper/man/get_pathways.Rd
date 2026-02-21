\name{get_pathways}
\alias{get_pathways}
\title{
Pathways
}
\description{
Helper function to download pathways and their gene identifiers.
reactome.db used for pathway annotations.
}\usage{
get_pathways(source="reactome",all_paths=TRUE,envir = "")
}
\arguments{
  \item{source}{
"reactome"
}
  \item{all_paths}{
TRUE or FALSE. If FALSE a subset will be asked by the function
}
  \item{envir}{
R environment to save Pathways to
}
}
\value{
Returns "Pathways description" All downloaded pathways are saved in the workspace
User will be prompt to set a prefix.
}
\seealso{
\code{\link{read2_paths}}
}
\examples{
\dontrun{
# get pathways source = "reactome"
if (!require("reactome.db")) install.packages("reactome.db") 
library(reactome.db)

# Create new environment to save data:
gper.env <- new.env()

paths <- get_pathways(source="reactome",all_paths=FALSE,envir=gper.env)
# when prompted introduce species as listed
Homo sapiens
# when prompted introduce prefix. Avoid characters "-" and "_" (e.g mypath, or leave blank)
# if all_paths set to TRUE. All pathways are downloaded automatically
# IF all_paths set to FALSE, select a subset of pathway identifiers from 
# list. Separated by ","
R-HSA-8964572,R-HSA-9613354,R-HSA-8876384,R-HSA-446343,R-HSA-9620244
}
}
\keyword{ annotation }
\keyword{ pathways }