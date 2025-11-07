library(stacomiR)

stacomi(database_expected=FALSE) 
## launches the application in the command line
## here an example of loading
## the following lines will only run if you have the program installed
## and the iav scheme available in the database
## this example generates the r_mig_mult dataset
# prompt for user and password but you can set appropriate options for host, port and dbname
\dontrun{
  stacomi(
	  database_expected=TRUE)	
if (interactive()){
	if (!exists("user")){
		user <- readline(prompt="Enter user: ")
		password <- readline(prompt="Enter password: ")	
	}	
}
options(					
		stacomiR.dbname = "bd_contmig_nat",
		stacomiR.host ="localhost",
		stacomiR.port = "5432",
		stacomiR.user = user,
		stacomiR.password = password						
)	
  r_mig_mult <- new("report_mig_mult")
  r_mig_mult <- choice_c(r_mig_mult,
	  dc=c(5,6,12),
	  taxa=c("Anguilla anguilla"),
	  stage=c("AGG","AGJ","CIV"),
    datedebut="2011-01-01",
    datefin="2011-12-31")
  r_mig_mult <- charge(r_mig_mult)
  # launching charge will also load classes associated with the report
  # e.g. report_ope, report_df, report_dc
  r_mig_mult <- connect(r_mig_mult)
  # calculations 
  r_mig_mult <- calcule(r_mig_mult,silent=TRUE)
}

# Use this as example if you don't have a connexion to the database
data("r_mig_mult")
# The following re-create the object at the time of loading
# All three classes were created by the charge and connect 
# method of report_mig_mult in the previous example
data("r_mig_mult_ope")
assign("report_ope",r_mig_mult_ope,envir=envir_stacomi)
data("r_mig_mult_df")
assign("report_df",r_mig_mult_df,envir=envir_stacomi)
data("r_mig_mult_dc")
assign("report_dc",r_mig_mult_dc,envir=envir_stacomi)
# use the following to get the raw data loaded by the connect method
# not shown there as the database and program might not be installed

#Individual plot for all DC, taxa and stage where data present



\dontrun{	
plot(r_mig_mult,plot.type="standard",silent=TRUE)
# colors in the following order (glass eel)
# working, stopped, 1...5 types of operation,numbers, weight, 2 unused colors
# for yellow eel and other taxa
# stopped, 1...5 types of operation, ponctuel, expert, calcule,mesure,working,
  plot(r_mig_mult,plot.type="standard",
	  color=c("#DEF76B","#B950B5","#9ABDDA","#781A74","#BF9D6E","#FFC26E",
		  "#A66F24","#012746","#6C3E00","#DC7ED8","#8AA123"),
	  color_ope=c("#5589B5","#FFDB6E","#FF996E","#1C4D76"),
	  silent=TRUE)
#For the following plot, beware, all stages and DC are grouped. This can make sense
# for instance if you want to display the cumulated migration for one species
# in several counting devices located on the same dam...
  plot(r_mig_mult,plot.type="step",silent=TRUE)
  
  
# Combined plot for ggplot2
  plot(r_mig_mult,plot.type="multiple",silent=TRUE)
# Data will be written in the data directory specified in 
# the datawd argument to stacomi, default "~"
  summary(r_mig_mult,silent=FALSE)
}
