stacomi(
	database_expected=FALSE)
\dontrun{
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
  r_env<-new("report_env")
  r_env<-choice_c(r_env,
	  stationMesure=c("temp_gabion","coef_maree"),
	  datedebut="2008-01-01",
	  datefin="2008-12-31",
	  silent=FALSE)	
  r_env<-connect(r_env)
  
}	

data("r_env")
plot(r_env,silent=TRUE)
