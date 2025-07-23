bindate2Title <- function(timestep,date1, date2=date1)
{
  Title <- c()
  if(is(date1, 'character')){
    #     if(date1 == "20030229") date1 <- "20030228"
    if(nchar(date1) > 8){
      fm <- "%Y%m%d%H"
    }else{
      fm <- "%Y%m%d"
    }
    a <- as.POSIXct(date1,format=fm)
    b <- as.POSIXct(date2,format=fm)
  }else{
    a <- date1
    b <- date2
  }
  
  if(grepl('h', timestep)){      
    y1 <- format(a,"%Y")
    m1 <- format(a,"%m")
    d <- format(a,"%d/%m/%Y %H:%M")
    Title <- eval(substitute(expression(bold(d)),
                             list(d=as.character(d))))
    #         dhM1 <- format(a,"%d %H:%M")
    #         Title <- eval(substitute(expression(bold(y1-m1-dhM1)),
    #                                  list(y1=as.character(y1),m1=as.character(m1),dhM1=as.character(dhM1))))
    #                              list(d=as.character(d))))
    #         Title <- substr(a,1,16) # format: 2006-05-04 14:00
  }else{  
    if(grepl(1, timestep)){
      
      if(timestep == "1m"){
        if(format(a,"%B%Y") == format(b,"%B%Y")){
          d <- format(a,"%B %Y")
          Title <- eval(substitute(expression(bold(d)),
                                   list(d=as.character(d))))
        }else{
          if(format(a,"%m") == format(b,"%m")){
            #             Title <- paste(format(a,"%B %Y "),format(b,"- %Y"),sep="")
            m1 <- format(a,"%B %Y")
            m2 <- format(b,"%Y")
            Title <- eval(substitute(expression(bold(m1-m2)),
                                     list(m1=as.character(m1),m2=as.character(m2))))
          }else{
            #             Title <- paste(format(a,"%B"),format(b," - %B"), ', ',format(a,"%Y "),format(b,"- %Y"),sep="")
            m1 <- format(a,"%B")
            m2 <- format(b,"%B")
            y1 <- format(a,"%Y")
            y2 <- format(b,"%Y")
            Title <- eval(substitute(expression(bold(m1-m2~y1-y2)),
                                     list(m1=as.character(m1),m2=paste0(as.character(m2), ', '),y1=as.character(y1),y2=as.character(y2))))
          }
        }
      }
      if(timestep == "1d" ){ 
        h <- as.numeric(format(a,"%H"))
        if(h == 0){
          d <- format(a,"%d/%m/%Y") # treat as timestep ?h
        }else{
          y1 <- format(a,"%Y")
          m1 <- format(a,"%m")
          d <- format(a,"%d/%m/%Y %H:%M")
        }
        Title <- eval(substitute(expression(bold(d)),
                                 list(d=as.character(d))))
      }
      if(timestep == "1w" ){
        year <- c(as.numeric(substr(a,1,4)),as.numeric(substr(b,1,4)))
        month <- c(as.numeric(substr(a,6,7)),as.numeric(substr(b,6,7)))
        d1 <- switch(length(unique(year)), switch(length(unique(month)), format(a,"%d"),format(a,"%d/%m")),format(a,"%d/%m/%Y"))
        d2 <- format(b,"%d/%m/%Y")
        Title <- eval(substitute(expression(bold(d1-d2)),list(d1=as.character(d1),d2=as.character(d2))))
      }
      if(timestep == "1y" ){
        if(format(a,"%Y") == format(b,"%Y")){Title <- paste(format(a,"%B"),format(b," - %B"), ' ',format(a,"%Y"),sep="")
        }else{
          #         Title <- paste(format(a,"%d/%m/%Y"),format(b," - %d/%m/%Y"),sep="")
          d1 <- format(a,"%d/%m/%Y")
          d2 <- format(b,"%d/%m/%Y")
          Title <- eval(substitute(expression(bold(d1-d2)),
                                   list(d1=as.character(d1),d2=as.character(d2))))
        }
      }
      if(timestep == "1s" ){
        #       years <- paste0(format(a,"%Y "),format(b,"- %Y"))
        #       if(format(a,"%m") == "02" & format(b,"%m") == "12") Title <- paste0("season 1, ", format(b,"%B"),format(a," - %B"),", ",years)
        #       if(format(a,"%m") == "12" & format(b,"%m") == "02") Title <- paste0("season 1, ", format(a,"%B"),format(b," - %B"),", ",years)
        #       if(format(a,"%m") == "03" & format(b,"%m") == "05") Title <- paste0("season 2, ", format(a,"%B"),format(b," - %B"),", ",years)
        #       if(format(a,"%m") == "06" & format(b,"%m") == "08") Title <- paste0("season 3, ", format(a,"%B"),format(b," - %B"),", ",years)
        #       if(format(a,"%m") == "09" & format(b,"%m") == "11") Title <- paste0("season 4, ", format(a,"%B"),format(b," - %B"),", ",years)
        #       
        years <- c(format(a,"%Y"),format(b,"%Y"))
        months <-  c(format(b,"%B"),format(a,"%B"))
        if(format(a,"%m") == "02" & format(b,"%m") == "12") {
          s <- "season 1, "
          months <-  c(format(b,"%B"),format(a,"%B"))
        }
        if(format(a,"%m") == "12" & format(b,"%m") == "02") s <- "season 1, "
        if(format(a,"%m") == "03" & format(b,"%m") == "05") s <- "season 2, "
        if(format(a,"%m") == "06" & format(b,"%m") == "08") s <- "season 3, "
        if(format(a,"%m") == "09" & format(b,"%m") == "11") s <- "season 4, "
        Title <- eval(substitute(expression(bold(season~m1-m2~y1-y2)),
                                 list(season=s, m1=as.character(months[1]),m2=paste0(as.character(months[2]), ', '),y1=as.character(min(years)),y2=as.character(max(years)))))
      }
    }
    if(length(Title) == 0){
      if(format(a,"%m/%Y") == format(b,"%m/%Y")){
        #       Title <- paste(format(a,"%d"), '-',format(b,"%d/%m/%Y"),sep="")
        d1 <- format(a,"%d")
        d2 <- format(b,"%d/%m/%Y")
        Title <- eval(substitute(expression(bold(d1-d2)),
                                 list(d1=as.character(d1),d2=as.character(d2))))  
      }else{
        #       Title <- paste(format(a,"%d/%m/%Y"), '-',format(b,"%d/%m/%Y"),sep="")
        d1 <- format(a,"%d/%m/%Y")
        d2 <- format(b,"%d/%m/%Y")
        Title <- eval(substitute(expression(bold(d1-d2)),
                                 list(d1=as.character(d1),d2=as.character(d2))))      
      }
    }
  }
  
  return(Title)
}

