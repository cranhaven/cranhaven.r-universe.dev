
# Helper functions for the mcmcOutput print method - not exported

# Generate the simsList attribute
secs2dhms <- function(secs, digits=3) {
  secs <- as.numeric(secs[1])
  if(secs <= 90)
    return(paste(round(secs, digits), "secs"))
  mins <- secs / 60
  if(mins <= 90)
    return(paste(round(mins, digits), "mins"))
  hrs <- mins / 60
  if(hrs <= 36)
    return(paste(round(hrs, digits), "hrs"))
  days <- hrs / 24
  return(paste(round(days, digits), "days"))
}
# .......................................................................

