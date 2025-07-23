bindate2main <- function(timestep,date1, date2=date1)
{
#   if(date1 == "20030229") date1 <- "20030228"
  a <- date1
  b <- date2
  
  if(timestep != "1s"){
    Title <- format(a,"%B")
  }else{
    months <-  c(format(b,"%B"),format(a,"%B"))
    if(format(a,"%m") == "02" & format(b,"%m") == "12") {
      s <- "season 1, "
      months <-  c(format(b,"%B"),format(a,"%B"))
    }
    if(format(a,"%m") == "12" & format(b,"%m") == "02") s <- "season 1, "
    if(format(a,"%m") == "03" & format(b,"%m") == "05") s <- "season 2, "
    if(format(a,"%m") == "06" & format(b,"%m") == "08") s <- "season 3, "
    if(format(a,"%m") == "09" & format(b,"%m") == "11") s <- "season 4, "
    Title <- eval(substitute(expression(bold(season~m1-m2)),
                             list(m1=as.character(months[1]),m2=as.character(months[2]),season=s)))
    
  }
  return(Title)
}
