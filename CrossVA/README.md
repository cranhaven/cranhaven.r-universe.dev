# CrossVA

**Description** 	

CrossVA is an R package for transforming verbal autopsy records into a format
accepted by the InSilicoVA and InterVA5 R packages.  Verbal autopsy records
are expected to be collected using the WHO VA 2016 instrument (Revision 1.4.1 
or 1.5.1) or the WHO VA 2014 instrument. This package is made available by WHO 
and the Bloomberg Data for Health Initiative.

**Input**		
- CSV file containing submissions of the 2016 WHO VA questionnaire (Revision
  1.4.1 or 1.5.1), exported from ODK Aggregate 
- CSV file containing submissions of the 2014 WHO VA questionnaire (2-15-10
  with form id: va-who_2014_final10), exported from ODK Aggregate

**Output**		
A CSV file intended for processing by a coding algorithm (i.e., InSilicoVA
or InterVA)

**Status**		

- `odk2openVA` is actively supported
- Future support for the WHO VA 2012 questionnaire and the PHMRC questionnaire 
will be available in a future release.

**Testing**

For testing purposes, install via
```
install.packages("devtools")
devtools::install_github("verbal-autopsy-software/CrossVA/CrossVA")
```
or download and install from [here https://github.com/verbal-autopsy-software/CrossVA/](https://github.com/verbal-autopsy-software/CrossVA)

Use your own VA records, or one of the sythetic sample data sets included in the package for testing:
(WHO VA 2016, version 1.5.1)[https://github.com/verbal-autopsy-software/CrossVA/blob/master/CrossVA/inst/sample/who151_odk_export.csv],
(WHO VA 2016, version 1.4.1)[https://github.com/verbal-autopsy-software/CrossVA/blob/master/CrossVA/inst/sample/who141_odk_export.csv], or
(WHO VA 2014)[https://github.com/verbal-autopsy-software/CrossVA/blob/master/CrossVA/inst/sample/who2014_odk_export.csv], or

**Examples**
```
library(CrossVA)
library(openVA)

# WHO VA Questionnaire 2016
## version 151
datafile_2016_151 <- system.file("sample", "who151_odk_export.csv", package = "CrossVA")
records_2016_151 <- read.csv(datafile_2016_151)
whoData2016_151 <- odk2openVA(records_2016_151, version = "1.5.1")

out1 <- insilico(whoData2016_151, data.type = "WHO2016")
summary(out1)
out2 <- InterVA5(whoData2016_151, HIV = "l", Malaria = "l", directory = getwd())
summary(out2)

## version 141
datafile_2016_141 <- system.file("sample", "who141_odk_export.csv", package = "CrossVA")
records_2016_141 <- read.csv(datafile_2016_141)
whoData2016_141 <- odk2openVA(records, version = "1.4.1")

out3 <- insilico(whoData2016_141, data.type = "WHO2016")
summary(out3)
out4 <- InterVA5(whoData2016_141, HIV = "l", Malaria = "l", directory = getwd())
summary(out4)

## WHO VA Questionnaire 2014
datafile_2014 <- system.file("sample", "who2014_odk_export.csv", package = "CrossVA")
records_2014 <- read.csv(datafile_2014)
whoData2014 <- odk2openVA(records, version = "2014")

out5 <- insilico(whoData2014, data.type = "WHO2016")
summary(out5)
out6 <- InterVA5(whoData2014, HIV = "l", Malaria = "l", directory = getwd())
summary(out6)
```
