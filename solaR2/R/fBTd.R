fBTd<-function(mode = 'prom',
               year =  as.POSIXlt(Sys.Date())$year+1900,
               start = paste('01-01-', year, sep = ''),
               end = paste('31-12-', year, sep = ''), 
               format = '%d-%m-%Y'){
    promDays<-c(17,14,15,15,15,10,18,18,18,19,18,13)
    BTd = switch(mode,
               serie = {
                   start.<-as.POSIXct(start, format = format, tz = 'UTC')
                   end.<-as.POSIXct(end, format = format, tz = 'UTC')
                   res<-seq(start., end., by = "1 day")
               },
               prom = as.POSIXct(paste(year, 1:12, promDays, sep = '-'), tz = 'UTC')
               )
    BTd
}
