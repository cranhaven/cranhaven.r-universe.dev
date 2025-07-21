## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval = FALSE-------------------------------------------------------------
#  library(wiesbaden)
#  
#  # Assuming credentials are stored via save_credentials()
#  test_login(genesis=c(db='regio'))
#  #> [1] "Sie wurden erfolgreich an- und abgemeldet."
#  
#  # ... or supply password/username
#  test_login(genesis=c(db='regio', user="your-username", password="your-password"))
#  #> [1] "Sie wurden erfolgreich an- und abgemeldet."

## ----eval = FALSE-------------------------------------------------------------
#  d <- retrieve_datalist(tableseries="141*", genesis=c(db='regio'))

## ----eval = FALSE-------------------------------------------------------------
#  subset(d, grepl("Kreise", description))
#  #>    tablename
#  #> 1 14111KJ001
#  #> 2 14111KJ002
#  #>                                                                                      description
#  #> 1 Wahlberechtigte, Wahlbeteiligung, Gültige Zweitstimmen, Kreise und kreisfreie Städte, Stichtag
#  #> 2                         Gültige Zweitstimmen, Kreise und kreisfreie Städte, Parteien, Stichtag

## ----eval = FALSE-------------------------------------------------------------
#  data <- retrieve_data(tablename="14111KJ002", genesis=c(db='regio'))

## ----eval = FALSE-------------------------------------------------------------
#  head(data)
#  #>   id14111 KREISE     PART04       STAG WAHL09_val WAHL09_qual WAHL09_lock
#  #> 1       D  01001        AFD 22.09.2013       1855           e          NA
#  #> 2       D  01001        AFD 24.09.2017       3702           e          NA
#  #> 3       D  01001 B90-GRUENE 16.10.1994       4651           e          NA
#  #> 4       D  01001 B90-GRUENE 27.09.1998       3815           e          NA
#  #> 5       D  01001 B90-GRUENE 22.09.2002       5556           e          NA
#  #> 6       D  01001 B90-GRUENE 18.09.2005       5028           e          NA
#  #>   WAHL09_err
#  #> 1          0
#  #> 2          0
#  #> 3          0
#  #> 4          0
#  #> 5          0
#  #> 6          0

## ----eval = FALSE-------------------------------------------------------------
#  retrieve_metadata(tablename="14111KJ002", genesis=c(db='regio'))
#  #>     name                  description   unit
#  #> 1 WAHL09         Gültige Zweitstimmen Anzahl
#  #> 2   STAG                     Stichtag
#  #> 3 PART04                     Parteien
#  #> 4 KREISE Kreise und kreisfreie Städte

## ----eval = FALSE-------------------------------------------------------------
#  retrieve_valuelabel("PART04", genesis=c(db='regio'))
#  #>       PART04       description
#  #> 1        AFD               AfD
#  #> 2 B90-GRUENE             GRÜNE
#  #> 3        CDU           CDU/CSU
#  #> 4   DIELINKE         DIE LINKE
#  #> 5        FDP               FDP
#  #> 6   SONSTIGE Sonstige Parteien
#  #> 7        SPD               SPD

