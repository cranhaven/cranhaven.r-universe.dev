\name{plot_results}
\alias{plot_results}
\title{
Plot Results Circular Permutation
}
\description{
QQ plots
}
\usage{
plot_results(results="",by="",plot_all=TRUE, var = "", save_plot=TRUE, plot_name="", 
bf= FALSE , save_qq = TRUE)
}
\arguments{
  \item{results}{
Results datarame from "get_results()"
}
  \item{by}{
Visualize results by "trait" OR by "set"  
}
  \item{plot_all}{
plot_all = TRUE plots all the variables in the results dataframe and saves a pdf file in the working directory.
Setting plot_all to FALSE plots a single variable(trait or set). The argument "var" must be declared.
}
  \item{var}{
Variable name to plot
}
  \item{save_plot}{
save_plot = TRUE saves the plots in the working directory. save_plot = FALSE the plot is visualized at the console. 
save_plot = FALSE can be used only when plot_all is set to FALSE. 
The plot displayed at the console is interactive, clicking on a point displays the points name.
}
  \item{plot_name}{
Argument used to save the file name for the plots. 
Default value = Results_genomicper_[set/trait]
}
  \item{bf}{
Displays the bonferroni correction 
}
 \item{save_qq}{
TRUE returns the qq plot values
}
} 
\value{
\item{qq}{
Data frame with qq plot values
}
}
\seealso{
\code{\link{get_results}}
}

\examples{

data(demo,SNPsAnnotation)
all_data <- read_pvals(data_name=demo,snps_ann=SNPsAnnotation)
genome_results <-genome_order(all_data=all_data)
	# Results from genome_order
	ordered_alldata <- genome_results$ordered_alldata
	gs_locs <- genome_results$gs_locs

# Create new environment to save the data:
gper.env <- new.env()

# Load Pathways
data(RHSA164843,RHSA446343,RHSA8876384,RHSA8964572,RHSA109582,RHSA1474244,envir=gper.env)


paths_res <- read2_paths(ordered_alldata=ordered_alldata,gs_locs=gs_locs,
sets_from="workspace",sets_prefix="RHSA",level="snp",envir=gper.env)
		pers_ids <- paths_res$per_ors
		pathways<- paths_res$pathways

snps_permutation(ordered_alldata=ordered_alldata,pers_ids=pers_ids,
ntraits=c(7,9),nper=10,saveto="workspace",threshold=0.05,
gs_locs=gs_locs,envir = gper.env)

results <- get_results(res_pattern="Permus",level="snp",
from="workspace",threshold=0.05,envir = gper.env)

#saves plots to working directory
\dontrun{
qq <- plot_results(results=results,by="set",plot_all=TRUE)
qq <- plot_results(results=results,by="trait",plot_all=FALSE,var="trait1")
qq <- plot_results(results=results,by="set",
	plot_all=FALSE,var="R-HSA-8964572",
	save_plot=FALSE) ## IMPORTANT: to EXIT interactive plot 
	## right click on the plot to stop
}
}
\keyword{results}

