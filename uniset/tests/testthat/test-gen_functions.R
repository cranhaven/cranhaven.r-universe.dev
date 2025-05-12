 library(testthat)
# library(devtools)

 # delete all in tempdir !!
#  rm(list=ls(all.names = TRUE))
# detach(name="pkg_dogPack_envs")
# devtools::load_all(".")

#### prepare ####
 ptp <- path.package("uniset")
 if (dir.exists(paste0(ptp, "/inst"))) {
     ptpInst <- paste0(ptp, "/inst")
 } else {
     ptpInst <- ptp
 } # end else
 #



#
######## the easy part #######-----
# ptp <- "~/Documents/RPS/uniset_R/uniset"
###
test_that("checkCh1", {
    a <- "a"; b <- "a"
    expect_null(checkCh1(a,b))
    a <- c("a", "b"); b <- "a"
    expect_error(checkCh1(a,b))
    a <- 1; b <- "a"
    expect_error(checkCh1(a,b))
}) # EOT

test_that("checkGetTaPaSH", {
    a <- "a"; b <- "a"
    expect_type(checkGetTaPaSH(a,b), "character")
    a <- 1; b <- "a"
    expect_error(checkGetTaPaSH(a,b))
    a <- c("a", "b"); b <- "a"
    expect_error(checkGetTaPaSH(a,b))
}) # EOT

test_that("checkPath_Package_getName", {
    expect_type(checkPath_Package_getName(ptp), "character")
    expect_error(checkPath_Package_getName("a"))
}) # EOT

test_that("printFinalCodeMessage", {
    expect_output(printFinalCodeMessage("a", "b", "c"))
    expect_message(printFinalCodeMessage("a", "b", "c"), "getstn()")
}) # EOT

taPaName <- "dogPack"
taPaObj <- "settings"
taPaSH <- "dogPack_SH"
tmpl <- "_TEMPLATE"
setupFunc <- "hereNameOfSetupFunction"
test_that("readInReplaceTxtUnisFiles", {
    expect_type(readInReplaceTxtUnisFiles(taPaName, taPaSH, taPaObj, tmpl, setupFunc), "list")
}) # EOT

td <- tempdir()
na <- "name"
pa <- paste0(td, "/", na)
tx <- "some text"
test_that("createFilesWriteText", {
    expect_null(createFilesWriteText(na, pa, tx, na, pa, tx, na, pa, tx))
}) # EOT

where <- tempdir()
test_that("uniset_getFiles", {
    expect_type(uniset_getFiles(taPaName, setupFunc, where, taPaSH, taPaObj, tmpl), "character")
    expect_message(uniset_getFiles(taPaName, setupFunc, where, taPaSH, taPaObj, tmpl), "getstn()")
    expect_error(uniset_getFiles(taPaName, setupFunc, where="~/blabla", taPaSH, taPaObj, tmpl))
    expect_error(uniset_getFiles(taPaName=1, setupFunc, where, taPaSH, taPaObj, tmpl))
    expect_error(uniset_getFiles(taPaName, setupFunc=NULL, where, taPaSH, taPaObj, tmpl))
    expect_message(uniset_getFiles(taPaName, setupFunc, where, taPaSH="dogPack_settingsHomeDirectory"), "getstn()")
    expect_message(uniset_getFiles(taPaName, setupFunc, where), "getstn()")
}) # EOT


pso <- paste0(ptp, "/testHelpers/dopaem")
if (!file.exists(pso)) {
    pso <- paste0(ptp, "/inst/testHelpers/dopaem") # because the Cmd-check testing is using the **installed** package, while the cmd-shift-T is using the source structure for testing
}
file.copy(pso, td, recursive=TRUE)
ptp <- paste0(td, "/dopaem") # the path to package dopaem
# file.exists(ptp)
test_that("uniset_copyFilesToPackage", {
    expect_message(uniset_copyFilesToPackage(ptp, setupFunc, taPaSH, taPaObj, tmpl), "getstn()")
    expect_null(uniset_copyFilesToPackage(ptp, setupFunc, taPaSH, taPaObj, tmpl))
    expect_error(uniset_copyFilesToPackage(ptp, setupFunc=NULL, taPaSH, taPaObj, tmpl))
    expect_error(uniset_copyFilesToPackage("~/blabla", setupFunc, taPaSH, taPaObj, tmpl))
    expect_null(uniset_copyFilesToPackage(ptp, setupFunc))
}) # EOT




# here have an other template settings file (in td/dopaem/inst finally)  for better testing
a <- file.copy(paste0(pso, "/R/dogPack_settings.R"), paste0(ptp, "/inst"), overwrite = TRUE)
pathSettings_test <- paste0(ptp, "/inst")
pathSettings_fn_test <- paste0(ptp, "/inst/dogPack_settings.R")
a <- file.copy(paste0(pso, "/R/Renviron_template_change"), paste0(ptp, "/inst"), overwrite = TRUE)
a <- file.copy(paste0(pso, "/R/Renviron_template_missing"), paste0(ptp, "/inst"), overwrite = TRUE)
a <- file.copy(paste0(pso, "/R/Renviron_template_multiple"), paste0(ptp, "/inst"), overwrite = TRUE)


setupFunc <- setupFuncName <- "dogPack_demo_setup" # the name of the function of the target package holding the setup function
test_that("checkSetupFuncName", {
    expect_error(checkSetupFuncName(setupFunc=NULL, taPaName))
    expect_true(checkSetupFuncName(setupFunc, taPaName))
}) # EOT


########### the tricky part ######## -----
uniset_handover <- list(pkgname = "dogPack", funcname = "dogPack_handover_to_uniset")
uhb1 <- ".someString"
uhb2 <- list(a=1, b=2, c=3)
uhb3 <- list(pkgnameX = "dogPack", funcname = "dogPack_handover_to_uniset")

test_that("check_target_package_handover", {
    expect_true(check_target_package_handover(uniset_handover))
    expect_error(check_target_package_handover(uhb1))
    expect_error(check_target_package_handover(uhb2))
    expect_error(check_target_package_handover(uhb3))
}) # EOT

# test_that("getUnisHandoverVariables", {
#     expect_error(getUnisHandoverVariables(uniset_handover)) # does not exist yet
# }) # EOT

# try and build the real dogPack
ptdopa_source <- paste0(ptpInst, "/examples/dogPack")
file.copy(ptdopa_source, td, recursive = TRUE)
ptdopa <- paste0(td, "/dogPack")
uniset_copyFilesToPackage(ptdopa, setupFunc = "dogPack_demo_setup")
devtools::document(ptdopa, roclets = c('rd', 'collate', 'namespace'), quiet = TRUE)
devtools::install(ptdopa)

test_that("getUnisHandoverVariables", {
    expect_type(getUnisHandoverVariables(uniset_handover), "list")
}) # EOT
# getUnisHandoverVariables(uniset_handover)


test_that("uniset_test", {
    expect_type(uniset_test(uniset_handover), "list")
    expect_output(uniset_test(uniset_handover))
    expect_error(uniset_test(uniset_handover = "blabla"))
}) # EOT
test_that("getUnisHandoverVariables", {
    expect_type(getUnisHandoverVariables(uniset_handover), "list")
    expect_error(getUnisHandoverVariables(uniset_handover = "blabla"))
}) # EOT

taPaList <- getUnisHandoverVariables(uniset_handover)

test_that("sourceSettingsToObject", {
    expect_type(sourceSettingsToObject(taPaList, pathSettings_fn_test), "list")
}) # EOT


#### test the setup functions ####
test_that("suStop", {
    expect_error(suStop())
}) # EOT

test_that("checkPathToFolder", {
    expect_true(checkPathToFolder(where=td, what="where"))
    expect_error(checkPathToFolder(where=paste0(td, "/blabla"), what="where"))
}) # EOT

pex <- path.expand("~")
test_that("getCheckPathToFolder", {
    expect_identical(getCheckPathToFolder(where=td, what="where"), td)
    expect_identical(getCheckPathToFolder(where="~", what="where"), pex)
#    expect_error(getCheckPathToFolder(where=NULL, what="where"), td) # is interactive, does not work in test
}) # EOT

td <- tempdir()
tempSysHome <- "tempSysHome"
taPaName <- "dogPack"
fn_taPaSH <- "dogPack_SH"
fullPath <- paste0(td, "/", tempSysHome)
# dogSH <- paste0(fullPath, "/", fn_taPaSH)
dir.create(fullPath, showWarnings=FALSE) # we create the tempSystemHome
userLoc <- paste0(td, "/userLoc") # this is the folder where the user wants the settings-home to be
dir.create(userLoc, showWarnings=FALSE)
#
systemHome_R <- fullPath
fullRenvPath <- paste0(systemHome_R, "/.Renviron")
fn_taPaSH <- taPaSH <- "dogPack_SH"

taPaSH_system_test <- ""

# a <- performSetup_create_taPaSH(userLoc, taPaList)
test_that("performSetup_create_taPaSH", {
    expect_message(performSetup_create_taPaSH(userLoc, taPaList), "has been created")
    expect_message(performSetup_create_taPaSH(userLoc, taPaList), "already existed")
}) # EOT

# a <- performSetup_copySettingsFile(userLoc, taPaList, pathSettings_test)
test_that("performSetup_copySettingsFile", {
    expect_message(performSetup_copySettingsFile(userLoc=paste0(td, "/blabla"), taPaList, pathSettings_test), "not possible to create")
    expect_message(performSetup_copySettingsFile(userLoc, taPaList, paste0(td, "/blabla")), "not possible to copy")
    expect_message(performSetup_copySettingsFile(userLoc, taPaList, pathSettings_test), "has been copied")
    expect_message(performSetup_copySettingsFile(userLoc, taPaList, pathSettings_test), "already existed")
}) # EOT

# checkSetup - CASE 1 - no Renv
test_that("checkSetup", {
    expect_message(checkSetup(taPaList, setupFuncName, systemHome_R, taPaSH_system_test), "does not exist")
}) # EOT

#
### Check .Renviron   #
# a <- performSetup_checkCreateModRenv(userLoc, taPaList, systemHome_R) # for looking at the fu ll message
# case 1: no Renviron existing
test_that("performSetup_checkCreateModRenv", {
    expect_message(performSetup_checkCreateModRenv(userLoc, taPaList, paste0(td, "/blabla")), "failed")
    expect_message(performSetup_checkCreateModRenv(userLoc, taPaList, systemHome_R), "has been created")
}) # EOT

# case 2: Renv does exist, but the variable is not present
a <- file.copy(paste0(ptp, "/inst/Renviron_template_missing"), paste0(systemHome_R, "/.Renviron"), overwrite=TRUE)
# checkSetup - CASE 3 -  Renv yes, but no variable
test_that("checkSetup", {
    expect_message(checkSetup(taPaList, setupFuncName, systemHome_R, taPaSH_system_test), "does not exist")
}) # EOT
test_that("performSetup_checkCreateModRenv", {
    expect_message(performSetup_checkCreateModRenv(userLoc, taPaList, systemHome_R), "has been appended")
}) # EOT

# case 3+4: Renv does exist, but the variable has to be changed; then all is good
a <- file.copy(paste0(ptp, "/inst/Renviron_template_change"), paste0(systemHome_R, "/.Renviron"), overwrite=TRUE)
test_that("performSetup_checkCreateModRenv", {
    expect_message(performSetup_checkCreateModRenv(userLoc, taPaList, systemHome_R), "was corrected to")
    expect_message(performSetup_checkCreateModRenv(userLoc, taPaList, systemHome_R), "was correctly set to")
}) # EOT

# case 5+4: Renv does exist, more than one occurrence of variable; then all is good
a <- file.copy(paste0(ptp, "/inst/Renviron_template_multiple"), paste0(systemHome_R, "/.Renviron"), overwrite=TRUE)
test_that("performSetup_checkCreateModRenv", {
    expect_message(performSetup_checkCreateModRenv(userLoc, taPaList, systemHome_R), "occurrences of the variable")
    expect_message(performSetup_checkCreateModRenv(userLoc, taPaList, systemHome_R), "was correctly set to")
}) # EOT

###
# case 5+4 from above is required !!
bupDir <- paste0(td, "/backup")
test_that("checkRenvContent_maybeCopyBackup", {
    expect_true(checkRenvContent_maybeCopyBackup(systemHome_R, bupDir))
#    expect_false(checkRenvContent_maybeCopyBackup(systemHome_R, bupDir))
}) # EOT
a <- file.copy(paste0(ptp, "/inst/Renviron_template_change"), paste0(td,  "/backup/.Renviron"), overwrite=TRUE)
test_that("checkRenvContent_maybeCopyBackup", {
    expect_false(checkRenvContent_maybeCopyBackup(systemHome_R, bupDir)) # is written back from backup
    expect_true(checkRenvContent_maybeCopyBackup(systemHome_R, bupDir)) # no difference
}) # EOT

###
# now all together
unlink(paste0(td, "/backup"), TRUE)
unlink(paste0(td, "/", tempSysHome, "/.Renviron"), TRUE)
unlink(paste0(td, "/userLoc/", taPaSH), TRUE)
test_that("performSetup_sys", {
    expect_true(performSetup_sys(userLoc, taPaList, pathSettings_test, systemHome_R))
    expect_true(performSetup_sys(userLoc, taPaList, pathSettings_test, systemHome_R))
}) # EOT

userLoc2 <- paste0(td, "/userLoc2")
a <- dir.create(userLoc2, showWarnings = FALSE)
test_that("performSetup_sys", {
    expect_true(performSetup_sys(userLoc2, taPaList, pathSettings_test, systemHome_R))
}) # EOT

a <- file.copy(paste0(ptp, "/inst/Renviron_template_missing"), paste0(systemHome_R, "/.Renviron"), overwrite=TRUE)
test_that("performSetup_sys", {
    expect_true(performSetup_sys(userLoc2, taPaList, pathSettings_test, systemHome_R))
}) # EOT

a <- file.copy(paste0(ptp, "/inst/Renviron_template_multiple"), paste0(systemHome_R, "/.Renviron"), overwrite=TRUE)
test_that("performSetup_sys", {
    expect_true(performSetup_sys(userLoc2, taPaList, pathSettings_test, systemHome_R))
    expect_true(performSetup_sys(userLoc2, taPaList, pathSettings_test, systemHome_R))
}) # EOT

test_that("performSetup_sys", {
    expect_error(performSetup_sys(userLoc2, taPaList, pathSettings_test, paste0(td, "/blabla")))
    expect_true(performSetup_sys(userLoc2, taPaList, pathSettings_test, systemHome_R))
}) # EOT

test_that("performSetup_sys", {
   expect_error(performSetup_sys(paste0(td, "/blabla"), taPaList, pathSettings_test, systemHome_R))
}) # EOT

###

taPaSH_system <- paste0(td, "/userLoc/", taPaSH)
taPaSH_system_wrong <- aa <-  paste0(td, "/userLoc/", "blablabla")
setFiName <- "dogPack_settings.R"
taPaSettingsPath <- paste0(td, "/dopaem/inst/", setFiName)

test_that("pleaaseCopyAsTemplate", {
    expect_true(pleaaseCopyAsTemplate(taPaSettingsPath, taPaSH_system, setFiName))
}) # EOT


##### test setup check #####

# checkSetup(taPaList, setupFuncName, systemHome_R, taPaSH_system_test="")

# checkSetup - CASE 2 - Renviron exists, but multiple vars
a <- file.copy(paste0(ptp, "/inst/Renviron_template_multiple"), paste0(systemHome_R, "/.Renviron"), overwrite=TRUE)
test_that("checkSetup", {
    expect_message(checkSetup(taPaList, setupFuncName, systemHome_R, taPaSH_system_test), "occurrences of the variable")
}) # EOT

a <- file.copy(paste0(ptp, "/inst/Renviron_template_missing"), paste0(systemHome_R, "/.Renviron"), overwrite=TRUE)
a <- performSetup_sys(userLoc, taPaList, pathSettings_test, systemHome_R)
# checkSetup - CASE 6 - Renviron exists, System var is empty
test_that("checkSetup", {
    expect_message(checkSetup(taPaList, setupFuncName, systemHome_R, taPaSH_system_test=""), "update the systems environemnt variables")
}) # EOT

# checkSetup - CASE 8 - all is good
test_that("checkSetup", {
    expect_true(checkSetup(taPaList, setupFuncName, systemHome_R, taPaSH_system))
}) # EOT

# checkSetup - CASE 7 - different values in system and Renviron
test_that("checkSetup", {
    expect_message(checkSetup(taPaList, setupFuncName, systemHome_R, taPaSH_system_wrong), "to become effective")
}) # EOT



print("------------------------------------------------------------")
print("------------------------------------------------------------")
print("------------------------------------------------------------")
print("------------------------------------------------------------")

#### go for checkFileVersionPossiblyModify #####


pathToPack <- paste0(td, "/dopaem/inst/", setFiName)
folderLocal <- paste0(td,  "/userLoc/", fn_taPaSH) # same as taPaSH_system above
nameLocal <- setFiName
tmpl <- "_TEMPLATE"

pt1 <-  paste0(td, "/dopaem/R/", "s_mod_plus.R")
msg1 <- "varAdd1, varAdd2, varAdd3"

pt11 <-  paste0(td, "/dopaem/R/", "s_mod_plus_my.R")
msg11 <- "myAdd01, myAdd02, myAdd1, myAddTNH, myAdd2, myAdd3, myAdd4"

pt12 <-  paste0(td, "/dopaem/R/", "s_mod_plus_my2.R")
msg12 <- "peter, paul, mary, newHeaderKey"

pt13 <-  paste0(td, "/dopaem/R/", "s_mod_plus_my3.R")
msg13 <- "changed, John"

pt10 <-  paste0(td, "/dopaem/R/", "s_mod_plus2.R")
msg10 <- "varAdd_0, varAdd1, varAdd2, varAdd3, varAdd4, varAdd5, varAdd6"

pt2 <-  paste0(td, "/dopaem/R/", "s_mod_minus.R")
msg2 <- "willBeDeleted1, willBeDeleted2, willBeDeleted3"

pt3 <-  paste0(td, "/dopaem/R/", "s_mod_plus_minus.R")
msg3 <- "comboAdd1, comboAdd2, comboAdd3"

pt4 <-  paste0(td, "/dopaem/R/", "s_block_new.R")
msg4 <- "newBlock1, newBlock2, newBlock3, below_newBlock4"

pt5 <-  paste0(td, "/dopaem/R/", "s_block_delete.R")
msg5 <- "block2, block2_2, comboDelete1, block2_oneMoreVariable"

pt6 <-  paste0(td, "/dopaem/R/", "s_block_delete2.R")
msg6 <- "block2, block2_2, comboDelete1, block2_oneMoreVariable, block3_giveMeaningfulNames, favouriteColor, willBeDeleted2, comboDelete2, comboDelete3"

pt7 <-  paste0(td, "/dopaem/R/", "s_block_delete3.R")
msg7 <- "block2, block2_2, comboDelete1, block2_oneMoreVariable, block4, blabla, andSoOn, petterson, findus, mouse"

cleanUp <- function(onTest=TRUE){
    a <- file.copy(taPaSettingsPath, paste0(td, "/userLoc/", taPaSH), overwrite = TRUE)
    message("clean")
} # EOF
cleanUp()



potoloc <- paste0(folderLocal, "/", nameLocal) # path to local
ptpDouble <- paste0(td, "/dopaem/R/settings_doubleKey.R")
# getCheckForDoubleNames(pathToLocal=potoloc, pathToPack=ptpDouble, pmu="$settings")
test_that("getCheckForDoubleNames", {
    expect_type(getCheckForDoubleNames(pathToLocal=potoloc, pathToPack, pmu="$settings"), "list")
    expect_error(getCheckForDoubleNames(pathToLocal=potoloc, pathToPack=ptpDouble, pmu="$settings"))
    expect_error(getCheckForDoubleNames(pathToLocal=ptpDouble, pathToPack=potoloc, pmu="$settings"))
}) # EOT



test_that("checkFileVersionPossiblyModify - original", {
    expect_true(checkFileVersionPossiblyModify(pathToPack, folderLocal, nameLocal, pm=taPaObj, tmpl, taPaName)) ####
    cleanUp()
}) # EOT

test_that("checkFileVersionPossiblyModify - 1", {
    expect_message(checkFileVersionPossiblyModify(pt1, folderLocal, nameLocal, pm=taPaObj, tmpl, taPaName), msg1) # change it
   	cleanUp()
}) # EOT

test_that("checkFileVersionPossiblyModify - 11, 12, 13", {
	# 11
    expect_message(checkFileVersionPossiblyModify(pt11, folderLocal, nameLocal, pm=taPaObj, tmpl, taPaName), msg11) # change it
    expect_true(checkFileVersionPossiblyModify(pt11, folderLocal, nameLocal, pm=taPaObj, tmpl, taPaName)) # no difference
	# 12
    expect_message(checkFileVersionPossiblyModify(pt12, folderLocal, nameLocal, pm=taPaObj, tmpl, taPaName), msg12) # change it
    expect_true(checkFileVersionPossiblyModify(pt12, folderLocal, nameLocal, pm=taPaObj, tmpl, taPaName)) # no difference
	# 13
    expect_message(checkFileVersionPossiblyModify(pt13, folderLocal, nameLocal, pm=taPaObj, tmpl, taPaName), msg13) # change it
    expect_true(checkFileVersionPossiblyModify(pt13, folderLocal, nameLocal, pm=taPaObj, tmpl, taPaName)) # no difference
	cleanUp()
}) # EOT

test_that("checkFileVersionPossiblyModify - 10, 2, 3", {
	#10
    expect_message(checkFileVersionPossiblyModify(pt10, folderLocal, nameLocal, pm=taPaObj, tmpl, taPaName), msg10) # change it
    expect_true(checkFileVersionPossiblyModify(pt10, folderLocal, nameLocal, pm=taPaObj, tmpl, taPaName)) # no difference
	cleanUp()
    #2
    expect_message(checkFileVersionPossiblyModify(pt2, folderLocal, nameLocal, pm=taPaObj, tmpl, taPaName), msg2) # change it
    expect_true(checkFileVersionPossiblyModify(pt2, folderLocal, nameLocal, pm=taPaObj, tmpl, taPaName)) # no difference
	cleanUp()
	#3
    expect_message(checkFileVersionPossiblyModify(pt3, folderLocal, nameLocal, pm=taPaObj, tmpl, taPaName), msg3) # change it
    expect_true(checkFileVersionPossiblyModify(pt3, folderLocal, nameLocal, pm=taPaObj, tmpl, taPaName)) # no difference
	cleanUp()
}) # EOT


test_that("checkFileVersionPossiblyModify - 4, 5, 6, 7", {
	#4
    expect_message(checkFileVersionPossiblyModify(pt4, folderLocal, nameLocal, pm=taPaObj, tmpl, taPaName), msg4) # change it
    expect_true(checkFileVersionPossiblyModify(pt4, folderLocal, nameLocal, pm=taPaObj, tmpl, taPaName)) # no difference
	cleanUp()
    #5
    expect_message(checkFileVersionPossiblyModify(pt5, folderLocal, nameLocal, pm=taPaObj, tmpl, taPaName), msg5) # change it
    expect_true(checkFileVersionPossiblyModify(pt5, folderLocal, nameLocal, pm=taPaObj, tmpl, taPaName)) # no difference
	cleanUp()
    #6
    expect_message(checkFileVersionPossiblyModify(pt6, folderLocal, nameLocal, pm=taPaObj, tmpl, taPaName), msg6) # change it
    expect_true(checkFileVersionPossiblyModify(pt6, folderLocal, nameLocal, pm=taPaObj, tmpl, taPaName)) # no difference
	cleanUp()
    #7
    expect_message(checkFileVersionPossiblyModify(pt7, folderLocal, nameLocal, pm=taPaObj, tmpl, taPaName), msg7) # change it
    expect_true(checkFileVersionPossiblyModify(pt7, folderLocal, nameLocal, pm=taPaObj, tmpl, taPaName)) # no difference
	cleanUp()
}) # EOT


######################################
taPaSH_system <- paste0(td, "/userLoc/", taPaSH) # had that already above
cleanUp()
test_that("checkSettings", {
    expect_true(checkSettings(taPaList, setupFuncName, systemHome_R, taPaSH_system, taPaSettingsPath))
    expect_message(checkSettings(taPaList, setupFuncName, systemHome_R, taPaSH_system, pt4), msg4)
}) # EOT


# we still have 2 cases in checkSetup to test
# checkSetup(taPaList, setupFuncName, systemHome_R, taPaSH_system_test="")
unlink(paste0(userLoc, "/", taPaSH, "/", setFiName), TRUE)
# checkSetup - CASE 5 - no settings file
test_that("checkSetup", {
    expect_message(checkSetup(taPaList, setupFuncName, systemHome_R, ""), "does not seem to exist")
}) # EOT

unlink(paste0(userLoc, "/", taPaSH), TRUE)
# checkSetup - CASE 4 - no taPaSH folder
test_that("checkSetup", {
    expect_message(checkSetup(taPaList, setupFuncName, systemHome_R, ""), "not pointing to an existing directory")
}) # EOT

test_that("100", {
    expect_equal(1,1)
}) # EOT

# now clean up
unlink(paste0(td, "/dopaem"), TRUE)
unlink(paste0(td, "/backup"), TRUE)
unlink(paste0(td, "/userLoc"), TRUE)
unlink(paste0(td, "/userLoc2"), TRUE)
unlink(paste0(td, "/", tempSysHome), TRUE)
unlink(paste0(td, "/", "UnisetFiles_R-pkg_'dogPack'"), TRUE)
unlink(paste0(td, "/name"), TRUE)
unlink(paste0(td, "/dogPack"), TRUE)
unlink(paste0(td, "/dogPack.Rcheck"), TRUE)
remove.packages("dogPack")
#
rm(list=ls(all.names = TRUE))





