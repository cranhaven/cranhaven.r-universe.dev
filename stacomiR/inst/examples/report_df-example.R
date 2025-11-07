stacomi(
	database_expected=FALSE)
# An example that will work with the database installed only and schema iav in the database
# prompt for user and password but you can set appropriate options for host, port and dbname

\dontrun{
	stacomi(
			database_expected=TRUE, sch='iav')	
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
  r_df=new("report_df")
  r_df<-choice_c(r_df,
	  1,
	  horodatedebut="2015-01-01",
	  horodatefin="2015-12-31",
	  silent=TRUE)
  Sys.setenv(TZ='GMT')
  # the times at Arzal are recorded continuously
  # they are converted to date when a time appears while the hour is changing
  # hence the following
  r_df<-connect(r_df)
}

data("r_df")
plot(r_df,plot.type="4")
# the following examples work but take a while to compute
\dontrun{
  plot(r_df,plot.type="1")
  plot(r_df,plot.type="2",main="A nice title")
  plot(r_df,plot.type="3",main="A nice title")	
}





