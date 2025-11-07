require(stacomiR)
stacomi(
	database_expected=FALSE)
# the following will load the data provided the user has access to the database
# with data in the iav example scheme.
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
			stacomiR.user = password						
	)	
  r_mig_env<-new("report_mig_env")
  r_mig_env<-choice_c(r_mig_env,
	  dc=c(5,6,12),
	  taxa=c("Anguilla anguilla"),
	  stage=c("AGJ","AGG","CIV"),
	  stationMesure=c("temp_gabion","coef_maree","phases_lune"),
	  datedebut="2008-01-01",
	  datefin="2008-12-31",
	  silent=FALSE)	
  r_mig_env<-charge(r_mig_env) # this is necessary to load operations, DF and DC
  r_mig_env<-connect(r_mig_env)
  r_mig_env<-calcule(r_mig_env,silent=TRUE)
}	

data("r_mig_env")
# An example of plot with custom colors.
plot(r_mig_env,
	color_station=c("temp_gabion"="red","coef_maree"="blue","phases_lune"="pink"),
 	color_dc=c("5"="yellow","6"="orange","12"="purple")
)
