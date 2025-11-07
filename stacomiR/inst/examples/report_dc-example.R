

# An example that will work only if the database is present 
# and the program installed and comprises the schema iav
# prompt for user and password but you can set appropriate options for host, port and dbname
\dontrun{
	if (interactive()){
		if (!exists("user")){
			user <- readline(prompt="Enter user: ")
			password <- readline(prompt="Enter password: ")	
		}	

	options(					
			stacomiR.dbname = "bd_contmig_nat",
			stacomiR.host ="localhost",
			stacomiR.port = "5432",
			stacomiR.user = user,
			stacomiR.password = password						
	)	
}
 stacomi(TRUE,sch="iav")
  r_dc=new("report_dc")
  r_dc<-choice_c(r_dc,
	  5,
	  horodatedebut="2000-01-01",
	  horodatefin="2015-12-31",
	  silent=TRUE)
  r_dc<-connect(r_dc)

##
	
  # this dataset has been loaded by the previous lines
  ###########################################################	
# Without connexion to the database (use dataset r_dc)
  ##########################################################
	# this option allows to launch the program without the interface to display 
# some of the program features.
	stacomi(database_expected=FALSE)
  data("r_dc")
  plot(r_dc,plot.type="1")
  plot(r_dc,plot.type="2")
  plot(r_dc,plot.type="3",main="trial title")
  plot(r_dc,plot.type="4",main="trial title")
# the following will write in the datawd folder
   summary(r_dc)
}




