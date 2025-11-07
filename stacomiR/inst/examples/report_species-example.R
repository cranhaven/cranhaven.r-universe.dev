# launching stacomi without selecting the scheme or interface
stacomi(	database_expected=FALSE)
# the following script will load data 
# from the two Anguillere monitored in the Somme
# If you have a working database
# the following line of code will create the bilesp dataset from the "iav." 
# schema in the database

\dontrun{
  bilesp<-new("report_species")
  # split is one of "none", "year", "week", "month
  bilesp<-choice_c(bilesp,
	  dc=c(5,6,12),
	  split="year", 
	  start_year="2008",
	  end_year="2012",
	  silent=FALSE)	
  bilesp <- connect(bilesp)
  bilesp <- calcule(bilesp)
  plot(bilesp, plot.type="pie", silent=FALSE)
  plot(bilesp, plot.type="barplot", silent=FALSE)
  bilesp <- choice_c(bilesp,
	  dc=c(5,6,12),
	  split="month",
	  start_year="2015",
	  end_year="2016",
	  silent=FALSE)
  bilesp <- charge(bilesp)
  bilesp <- connect(bilesp)
  plot(bilesp, plot.type="pie", silent=FALSE)
  plot(bilesp, plot.type="barplot", silent=FALSE)
  #length(unique(bilesp@calcdata$taxa_stage)) # 15
  # here creating a vector of length 15 with nice blending colours
	if (requireNamespace("grDevices", quietly = TRUE)) {
	mycolorrampblue <-
			grDevices::colorRampPalette(c("#395B74", "#010F19"))
	mycolorrampyellow <-
			grDevices::colorRampPalette(c("#B59C53", "#271D00"))
	mycolorrampred <-
			grDevices::colorRampPalette(c("#B56F53", "#270B00"))
  color<-c(mycolorrampblue(5),
	  mycolorrampyellow(5),
	  mycolorrampred(5))
  plot(bilesp,plot.type="barplot",color=color,silent=TRUE)
	}
  summary(bilesp)
}	

