library(nprcgenekeepr)
library(rmsutilityr)
library(stringi)
library(here)
## Reading in large ped file and transforming columns and values - delete from
## script
file_names <- c("2021-01-06_Deidentified_Pedigree.csv",
                "deidentified_jmac_ped_edited.csv",
                "2022-05-02_Deidentified_Pedigree.xlsx")
pedOneFile <- stri_c(here("inst", "extdata", file_names[3L]))
if (tools::file_ext(pedOneFile) == ".csv") {
  pedOne <- read.csv(file = pedOneFile, header = TRUE, sep = ",")
} else {
  pedOne <- nprcgenekeepr:::readExcelPOSIXToCharacter(pedOneFile)
}
minParentAge <- 2L # Min breeding age
pedOne <- qcStudbook(pedOne, minParentAge = minParentAge)
pedOne$fromCenter[is.na(pedOne$fromCenter)] <- TRUE

#calc list of births prior to loop
#pre allocate mem for containers - create matrix with NAs - make ids factors
potentialParents <-
  getPotentialParents(ped = pedOne, minParentAge = 2L,
                      maxGestationalPeriod = 210L)
##for (i in c(1390, 1508, 1629, 1644, 1813)) {
##  cat(paste0("#", i, " is counter: ", potentialParents[[i]]$counter,
##             "; id: ", potentialParents[[i]]$id, "; dams are ",
##             get_and_or_list(potentialParents[[i]]$dams), "; sires are ",
##             get_and_or_list(potentialParents[[i]]$sires), ".\n"))
##}
potential_sire_length <-
  sapply(potentialParents, function(x) {
    length(x$sires)
  })
plot(density(potential_sire_length))
hist(potential_sire_length)

potential_dam_length <-
  sapply(potentialParents, function(x) {
    length(x$dams)
  })
plot(density(potential_dam_length))
hist(potential_dam_length)

n <- 1L
counts <- NULL
for (i in 1L:5L) {
  simKinships <- createSimKinships(ped = pedOne,
                                   allSimParents = potentialParents,
                                   pop = pedOne$id, n = n, verbose = TRUE)
  kValues <- kinshipMatricesToKValues(simKinships)
  counts <- countKinshipValues(kValues, counts)
  stats <- summarizeKinshipValues(counts)
  filename <- get_dated_excel_name("counts")
  nprcgenekeepr::create_wkbk(file = filename, df_list = list(stats),
                             sheetnames = "stats", replace = TRUE)
}
