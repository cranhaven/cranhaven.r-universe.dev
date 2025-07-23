bindate2ylab <- function(timestep,date1, date2=date1)
{
#   if(date1 == "20030229") date1 <- "20030228"
  a <- date1
  b <- date2
  
  if(as.numeric(substr(timestep,1,1)) == 1){
    if(timestep == "1m"){
      if(format(a,"%Y") == format(b,"%Y")){Title <- format(a,"%Y")
      }else{
        d1 <- format(a,"%Y")
        d2 <- format(b,"%Y")
        Title <- eval(substitute(expression(bold(d1-d2)),
                                 list(d1=as.character(d1),d2=as.character(d2))))  
#         Title <- paste(format(a,"%Y "),format(b,"- %Y"),sep="")}
      }
    }
    if(timestep == "1d"){Title <- format(a,"%d/%m/%Y")}
    if(timestep == "1s" ){
      years <- c(format(a,"%Y"),format(b,"%Y"))
      Title <- eval(substitute(expression(bold(y1-y2)),
                               list(y1=as.character(min(years)),y2=as.character(max(years)))))
#       if(format(a,"%m") == "02" & format(b,"%m") == "12") Title <- paste0("season 1, ", format(b,"%B"),format(a," - %B"),", ",years)
#       if(format(a,"%m") == "03" & format(b,"%m") == "05") Title <- paste0("season 2, ", format(a,"%B"),format(b," - %B"),", ",years)
#       if(format(a,"%m") == "06" & format(b,"%m") == "08") Title <- paste0("season 3, ", format(a,"%B"),format(b," - %B"),", ",years)
#       if(format(a,"%m") == "09" & format(b,"%m") == "11") Title <- paste0("season 4, ", format(a,"%B"),format(b," - %B"),", ",years)
    }
  }else{
  if(format(a,"%m/%Y") == format(b,"%m/%Y")){
#     Title <- paste(format(a,"%d"), '-',format(b,"%d/%m/%Y"),sep="")
    d1 <- format(a,"%d")
    d2 <- format(b,"%d/%m/%Y")
    Title <- eval(substitute(expression(bold(d1-d2)),
                             list(d1=as.character(d1),d2=as.character(d2))))  
  }
}
#   print(Title)
  return(Title)
}


