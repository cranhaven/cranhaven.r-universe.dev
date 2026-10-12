# README #

Provides two data sets from the Italian Survey on Household and Wealth, 2008 and 2010, a sample survey conducted by the Bank of Italy every two years. 

* `italy08`: The 2010 survey covered 13,702 individuals. 

* `italy10`: The 2010 survey covered 13,702 individuals, while the 2008 survey covered 13,734 individuals. 

Source: (https://www.bancaditalia.it/pubblicazioni/indagine-famiglie/bil-fam2012/index.html?com.dotmarketing.htmlpage.language=1)

Both data sets contain the following categorical variables that can be used to perform record linkage. The variables are given in Italian and we translate them in English. 

* ANASC (year of birth) 
* NASCREG (working status)
* CIT (employment status)
* ACOM4C (branch of activity)
* STUDIO (town size)
* IREG (geographical area of birth)
* SEX (sex)
* QUAL (whether or not Italian national)
* SETT (highest educational level obtained)
* Q (quality of life status)
* id (unique identifier based upon a type of social security identifier that is thought to be very reliable). 

Please see Steorts (2015)for more information about this data set or performing record linkage. 

## Installation

```R
# Install the released version from CRAN
install.packages(“italy”)

# Install the development version from GitHub
devtools::install_github(“resteorts/italy”)
```