stacomi(database_expected=FALSE) 
# If you have a working database
# the following line of code will create the r_mig dataset from the iav (default) 
# schema in the database
\dontrun{	
	stacomi(database_expected=TRUE)	
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
			stacomiR.user = password						
	)	
  stacomi(
	  database_expected=TRUE)	
  r_mig=new("report_mig")
  r_mig=choice_c(r_mig,
	  dc=5,
	  taxa=c("Chelon ramada"),
	  stage=c("IND"),
	  datedebut="2015-01-01",
	  datefin="2015-12-31")
  r_mig<-charge(r_mig)
  # launching charge will also load classes associated with the report
  # e.g. report_ope, report_df, report_dc
  r_mig<-connect(r_mig)
  ########################
# calculations
  ########################
  r_mig<-calcule(r_mig,silent=TRUE)
}
########################
# loading data
## use the following to get the raw data loaded by the connect method
# not shown there as the database and program might not be installed
# All three classes report... were created by the charge and connect method 
# of report_mig_mult
# in the previous example
################################
data("r_mig")
data("r_mig_ope")
assign("report_ope",r_mig_ope,envir=envir_stacomi)
data("r_mig_df")
assign("report_df",r_mig_df,envir=envir_stacomi)
data("r_mig_dc")
assign("report_dc",r_mig_dc,envir=envir_stacomi)


#Individual plot for all DC (standard), taxa and stage where data present
#silent argument to stop all messages
plot(r_mig,plot.type="standard",silent=TRUE)
#cumulated migration at the station (all stages and DC grouped)
plot(r_mig,plot.type="step")

# data will be written in the data directory specified in datawd argument to stacomi default "~"
#file

\dontrun{
  summary(r_mig,silent=TRUE)
}
# this will write the daily report for later in in the reportnMigrationInterannuelle-class
\dontrun{
  write_database(r_mig,silent=TRUE,dbname="bd_contmig_nat",host="localhost",port=5432)
}
