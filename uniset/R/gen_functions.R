checkCh1 <- function(char, argName) {
    if (!all(is.character(char)) | length(char) != 1) {
        stop(paste0("Please provide a character length one to the argument '", argName, "'."), call.=FALSE)
    }
    return(invisible(NULL))
} # EOF

checkPathToFolder <- function(where, what) {
    # we did checkCh1 one upstairs
    if (!dir.exists(where)) {
        stop(paste0("Sorry, the provided path '", where, "' does not seem to lead to an existing folder.\nPlease check your input at the parameter '", what, "'.\n"), call.=FALSE)
    } # end if
    return(TRUE) # for testing
} # EOF

getCheckPathToFolder <- function(where, what) {
    if (is.null(where)) {
        where <- try(easycsv::choose_dir(), silent=TRUE)
        if (is(where, "try-error")) {
            stop(paste0("Sorry, choosing the path to '", what, "' does not seem to work interactively.\nPlease provide a valid path to an existing folder in the argument '", what, "' manually.\n"), call.=FALSE)
        } # end if class(where) == "try-error"
        where <- substr(where, 1, nchar(where)-1) # cut away the "/" at the end
    } # end if is.null(where)
    checkCh1(where, what)
    oldPath <- where
    where <- try(path.expand(where), silent=TRUE)
    if (is(where, "try-error")) {
        where <- oldPath
    } # end if
    checkPathToFolder(where, what)  
    return(where)
} # EOF

checkGetTaPaSH <- function(taPaSH, taPaName) {
    shSuff <- glob_settingsHomeSuffix # "_SH"
    #
    checkCh1(taPaSH, "taPaSH")
    checkCh1(taPaName, "taPaName")
    #
    if (taPaSH == "def") {
        return(paste0(taPaName, shSuff))
    } else {
        return(taPaSH)
    }
} # EOF

checkPath_Package_getName <- function(pathToPackage) {
    stdiFn_descr <- "DESCRIPTION"
    stdFin_ns <- "NAMESPACE"
    stdFon_R <- "R"
    stdVec <- c(stdiFn_descr, stdFin_ns, stdFon_R)
    packSkellName <- "anRpackage" # the name given when using the function package.skelleton() without providing a package name
    ptp <- pathToPackage
    #
    if (dir.exists(ptp)) {
        fls <- list.files(ptp)
        if (!all(stdVec %in% fls)) {
            stop(paste0("Sorry, the provided path '", ptp, "' does not seem to lead to a valid R-package structure."), call.=FALSE)
        } # end if
        descrFile <- paste0(ptp, "/",  stdiFn_descr)
        fcon <- file(descrFile, open="r")
        descrTxt <- readLines(fcon) # the full settings.r text from the package
        close(fcon)
        taPaName <- trimws(unlist(strsplit(descrTxt[1], ":"))[2]) # gives back the package name
        if (taPaName == packSkellName) {
            stop(paste0("Please provide your package with an other name than '", packSkellName, "'"), call.=FALSE)
        } # end if
    } else {
        stop(paste0("Sorry, the provided path '", ptp, "' does not lead to a folder."), call.=FALSE)
    } # end else
    return(taPaName)
} # EOF

printFinalCodeMessage <- function(taPaObj, expSettingsName, taPaName) {
    altMsg1 <- paste0("\n\nUse the function \n") 
    getStnMsg <- "'autoUpS()' or 'getstn()'"
    altMsg2 <-  paste0("inside a function defined in the package '", taPaName, "' to get the list holding the key=value pairs.\n\n")
    #
    cat(altMsg1)
    message(getStnMsg)
    cat(altMsg2)
    #
    return(invisible(NULL))
} # EOF

###############
readInReplaceTxtUnisFiles <- function(taPaName, taPaSH, taPaObj, tmpl,
    setupFunc) {
    #
    taPackName <- glob_targetPackageName                        # XXX_packageName
    taPackSHname <- glob_targetPackageSettingsHomeVarName       # XXX_SH
    taPackStnName <- glob_targetPackgae_settingsObjectName      # XXX_obj
    taPackTmplSuff <- glob_targetPackage_templateSuffix         # XXX_template
    taPackActualSett <- glob_actualSettingsName                 # XXX_actualSettingsName
    setupFuncName <- glob_setupFuncName                         # XXX_setupFuncName

    #
    filnameSettings <- glob_filnameSettings
    templGlobals <- glob_templGlobals       # "uniset_globals.R"
    templSettinsg <- glob_templSettinsg     # "settings.R"
    tmplFuncs <- glob_filenameFunctions         # "uniset_functions.R"
    fileAdd <- "/templates/"                # the above three files will reside in the folder 'templates' in the installation of the uniset-package

    ####### define paths ######
    aa <- path.package("uniset")
    if (dir.exists(paste0(aa, "/inst"))) { # so we are in a local dev mode and the templates are residing in the 'inst' folder of package root
        fileAdd <- "/inst/templates/"
    } # end if
    path_unis_globals <- paste0(aa, fileAdd, templGlobals)
    path_unis_settingsTemplate <- paste0(aa, fileAdd, templSettinsg)
    path_unis_functions <- paste0(aa, fileAdd, tmplFuncs)
    #
    ###### read in the three template files #######
    fcon <- file(path_unis_globals, open="r")
    globalsTxt <- readLines(fcon) 
    close(fcon)
    fcon <- file(path_unis_settingsTemplate, open="r")
    settingsTxt <- readLines(fcon) 
    close(fcon)
    fcon <- file(path_unis_functions, open="r")
    functionsTxt <- readLines(fcon) 
    close(fcon)
    #

    ####### replace text #########
    expSettingsName <- paste0(taPaName, "_", filnameSettings) # put the package name and 'settings.R' together
    #
    globalsTxt <- gsub(taPackName, taPaName, globalsTxt) # the package name
    #
    settingsTxt <- gsub(taPackName, taPaName, settingsTxt) # the name of the target package
    settingsTxt <- gsub(taPackStnName, taPaObj, settingsTxt) # the name of the object holding the settings-list
    settingsTxt <- gsub(taPackActualSett, expSettingsName, settingsTxt) # the name of settings file expanded with the name of the package
    #
    functionsTxt <- gsub(taPackStnName, taPaObj, functionsTxt)
    functionsTxt <- gsub(setupFuncName, setupFunc, functionsTxt)
    functionsTxt <- gsub(taPackName, taPaName, functionsTxt)
    functionsTxt <- gsub(taPackSHname, taPaSH, functionsTxt) # settings home
    functionsTxt <- gsub(taPackTmplSuff, tmpl, functionsTxt)
    functionsTxt <- gsub(taPackActualSett, expSettingsName, functionsTxt)
    #
    return(list(globalsTxt=globalsTxt, settingsTxt=settingsTxt, functionsTxt=functionsTxt))
} # EOF

createFilesWriteText <- function(globalsName, globalsPath, globalsTxt,
    settingsName, settingsPath, settingsTxt,
    filenameFunctions, functionsPath, functionsTxt) {
    #
    ok <- file.create(globalsPath)
    if (!ok) {
        stop(paste0("Sorry, the file '", globalsName, "' could not be created."), call.=FALSE)
    } # end if
    ok <- file.create(settingsPath)
    if (!ok) {
        stop(paste0("Sorry, the file '", settingsName, "' could not be created."), call.=FALSE)
    } # end if
    #
    ok <- file.create(functionsPath)
    if (!ok) {
        stop(paste0("Sorry, the file '", filenameFunctions, "' could not be created."), call.=FALSE)
    } # end if

    # write into the files
    fcon <- file(globalsPath, open="w")
    writeLines(globalsTxt, fcon) # write globals file
    close(fcon)
    #
    fcon <- file(settingsPath, open="w")
    writeLines(settingsTxt, fcon) # write the settings file
    close(fcon)
    #
    fcon <- file(functionsPath, open="w")
    writeLines(functionsTxt, fcon) # write the settings file
    close(fcon)
    #
    return(invisible(NULL))
} # EOF

#' @title Get Uniset Files
#' @description Function to generate the four files required in the target
#' package (i.e. the package that should be enabled to use the package 'uniset').
#' @details Look at the content of the four generated files for information on 
#' where they should be moved.
#' @param taPaName Character length one. The name of the target package. 
#' @param setupFunc Character length one. The name of the function 
#' \strong{in the target package} that is containing the setup-function 
#' \code{\link{uniset_setup}}. 
#' @param where Character length one. The location where the folder with the
#' resulting four files should be copied to. Defaults to 'NULL'. If left at the 
#' default 'NULL', the location should be selectable interactively. Provide a 
#' character length one holding a valid path to an existing folder to copy the 
#' folder containing the four required files there. 
#' @param taPaSH Character length one. The name of the variable to be defined in
#' the '.Renviron' file, leading to the place where the settings.R file for the
#' target package will be stored. If left at the default 'def', \code{'taPaName_SH'}
#' will be used, with 'taPaName' being the value provided at the argument
#' 'taPaName'.
#' @param taPaObj Character length one. The name of the object holding the list
#' with the key-value pairs that can be defined to be used in the target package. 
#' Can be left at the default 'settings'.
#' @param tmpl Character length one. the Character string that will be appended
#' to the fresh settings file that is copied (by the target package) to the users 
#' settings home directory if updating the key=value pairs was not successful. 
#' Can be left at the default '_TEMPLATE'.
#' @return Creates a folder at the location specified at argument 'where' with the
#' four files to be moved into the target package in it. Returns an (invisible)
#' character holding the path of the folder where the three files were written
#' into.
#' @seealso \code{\link{uniset_copyFilesToPackage}}
#' @section Note: Please refer to \code{\link{uniset}} for a link to examples 
#' and a real-world demo.
#' @examples{
#' library(uniset)
#' # first copy the target package example into tempdir
#' to <- tempdir()
#' from <- paste0(path.package("uniset"), "/examples/dogPack")
#' file.copy(from, to, recursive = TRUE) 
#' # now copy the four required files
#' uniset_getFiles("dogPack", setupFunc="nameOfSetupFunc", where=to)
#' # Now manually move the four files according to the instructions contained 
#' # in them.
#' }
#' @export
uniset_getFiles <- function(taPaName=NULL, setupFunc=NULL, where=NULL,
    taPaSH="def", taPaObj="settings", tmpl= "_TEMPLATE") {
    #
    folderPrev <- glob_folderPrev
    filenameGlobals <- glob_filenameGlobals
    filnameSettings <- glob_filnameSettings  # the '.r' gets appended below
    filenameFunctions <- glob_filenameFunctions    # "uniset_functions.R"
    #

    where <- getCheckPathToFolder(where, what="where") ## ***
    checkCh1(taPaName, "taPaName")
    checkCh1(setupFunc, "setupFunc")
    checkCh1(taPaObj, "taPaObj")
    checkCh1(tmpl, "tmpl")
    taPaSH <- checkGetTaPaSH(taPaSH, taPaName) ## ***
    checkCh1(tmpl, "tmpl")
    #

    ##### read in and replace text #######
    aaa <- readInReplaceTxtUnisFiles(taPaName, taPaSH, taPaObj, tmpl, setupFunc)
        globalsTxt <- aaa$globalsTxt
        settingsTxt <- aaa$settingsTxt
        functionsTxt <- aaa$functionsTxt
    #
    expSettingsName <- paste0(taPaName, "_", filnameSettings) # put the package name and 'settings.r' together
    #

    ####### write to file ##########
    # check create folder
    folderName <- paste0(folderPrev, taPaName, "'")
    folderPath <- paste0(where, "/", folderName)
    if (dir.exists(folderPath)) {
        fls <- list.files(folderPath)
        if (length(fls) != 0) {
            ok <- file.remove(paste0(folderPath, "/", fls))
            if (!all(ok)) {
                stop(paste0("Sorry, there was a problem when trying to remove files in the folder '", folderPath, "'."), call.=FALSE)
            } # end if !ok
        } # end if
    } else { # so the folder path is not existing
        ok <- dir.create(folderPath, showWarnings=FALSE)
        if (!ok) {
            stop("Sorry, the folder could not be created", call.=FALSE)
        } # end if !ok
    } # end else
    # now we can be sure to have an empty folder
    #

    #### create files and write text into them #######
    globalsPath <- paste0(folderPath, "/", filenameGlobals)
    settingsPath <- paste0(folderPath, "/", expSettingsName)
    functionsPath <- paste0(folderPath, "/", filenameFunctions)
    #
    createFilesWriteText(filenameGlobals, globalsPath, globalsTxt,
        expSettingsName, settingsPath, settingsTxt,
        filenameFunctions, functionsPath, functionsTxt)
    ##

    allfns <- c(filenameGlobals, expSettingsName, filenameFunctions)
    cat(paste0("Three files called \n'", paste0(allfns, collapse="'\n'"), "'\nhave been written to the folder \n'", folderPath, "'"))
    cat("\n")
    cat("Please move these three files into their resp. target folders \n(see ?uniset, or have a look at the content of the three generated files)")
    printFinalCodeMessage(taPaObj, expSettingsName, taPaName)
    return(invisible(folderPath))
} # EOF

#' @title Copy Uniset Files into Target Package
#' @description Generate the four files required in the target
#' package (i.e. the package that should be enabled to use the package 'uniset').
#' The generated files will be copied directly into their required destination
#' folders in the target package. The name of the target package will be extracted
#' from the description file.
#' @param pathToPackage Character length one. The path to the root of the
#' target package.
#' @inheritParams uniset_getFiles
#' @return Writes the four required files directly into a valid R-package folder
#' structure. Returns (invisible) NULL.
#' @seealso \code{\link{uniset_getFiles}}
#' @section Note: Please refer to \code{\link{uniset}} for a link to examples 
#' and a real-world demo.
#' @examples{
#' library(uniset)
#' # first copy the target package example into tempdir
#' to <- tempdir()
#' from <- paste0(path.package("uniset"), "/examples/dogPack")
#' file.copy(from, to, recursive = TRUE) 
#' # now copy the four required files directly into the package 'dogPack'
#' path <- paste0(to, "/dogPack")
#' uniset_copyFilesToPackage(path, "nameOfSetupFunc")
#' }
#' @export
uniset_copyFilesToPackage <- function(pathToPackage, setupFunc=NULL,
    taPaSH="def", taPaObj="settings", tmpl= "_TEMPLATE") {
    #
    filenameGlobals <- glob_filenameGlobals
    filnameSettings <- glob_filnameSettings # the '.r' gets appended below
    filenameFunctions <- glob_filenameFunctions
    #

    #### check and get names ######
    taPaName <- checkPath_Package_getName(pathToPackage) # stops if path is not good
    checkCh1(setupFunc, "setupFunc")
    checkCh1(taPaObj, "taPaObj")
    taPaSH <- checkGetTaPaSH(taPaSH, taPaName)
    checkCh1(tmpl, "tmpl")
    #

    ##### read in and replace text #######
    aaa <- readInReplaceTxtUnisFiles(taPaName, taPaSH, taPaObj, tmpl, setupFunc)
        globalsTxt <- aaa$globalsTxt
        settingsTxt <- aaa$settingsTxt
        functionsTxt <- aaa$functionsTxt
    #
    expSettingsName <- paste0(taPaName, "_", filnameSettings) # put the package name and 'settings.R' together
    #

    ########## check / remove files #############
    folderName <- basename(pathToPackage)
    folderPath <- pathToPackage
    pathToR <- paste0(folderPath, "/R")
    paToInst <- paste0(folderPath, "/inst")
    if (dir.exists(paToInst)) {
        fls <- list.files(paToInst)
        if (expSettingsName %in% fls) {
            ok <- file.remove(paste0(paToInst, "/", expSettingsName))
            if (!ok) {
                stop(paste0("Sorry, there was a problem when trying to remove the previously generated file '", expSettingsName, "' in the folder 'inst'."), call.=FALSE)
            } # end if
        } # end if
    } else { # so the folder "inst" is not existing
        ok <- dir.create(paToInst, showWarnings=FALSE)
        if (!ok) {
            stop("Sorry, the folder 'inst' could not be created", call.=FALSE)
        } # end if !ok
    } # end else
    # check / remove globals.r
    fls <- list.files(pathToR)
    if (length(fls) != 0) { # check if already there
        if (filenameGlobals %in% fls) { # remove it
            ok <- file.remove(paste0(pathToR, "/", filenameGlobals))
            if (!ok) {
                stop(paste0("Sorry, there was a problem when trying to remove the previously generated file '", filenameGlobals, "' in the folder 'R'."), call.=FALSE)
            } # end if
        } # end if filename globals already there
        if (filenameFunctions %in% fls) { # remove it
            ok <- file.remove(paste0(pathToR, "/", filenameFunctions))
            if (!ok) {
                stop(paste0("Sorry, there was a problem when trying to remove the previously generated file '", filenameFunctions, "' in the folder 'R'."), call.=FALSE)
            } # end if
        } # end if filename functions already there
    } # end if length(fls) != 0
    # now we are sure to have a) the inst folder present, and b) none of our three files present
    #

    #### create files and write text into them #######
    globalsPath <- paste0(pathToR, "/", filenameGlobals)
    settingsPath <- paste0(paToInst, "/", expSettingsName)
    functionsPath <- paste0(pathToR, "/", filenameFunctions)
    #
    createFilesWriteText(filenameGlobals, globalsPath, globalsTxt,
        expSettingsName, settingsPath, settingsTxt,
        filenameFunctions, functionsPath, functionsTxt)
    ##

    cat(paste0("A file called '", expSettingsName, "' has been written into the 'inst' folder, \ntwo files called '", filenameGlobals, "' and '", filenameFunctions, "'\nhave been written into the 'R' folder of the package '", taPaName, "' at\n'", folderPath, "'."))
    printFinalCodeMessage(taPaObj, expSettingsName, taPaName)
    return(invisible(NULL))
} # EOF

check_target_package_handover <- function(taPaInput) {
    #
    liNames <- c("pkgname", "funcname")
    #
    msg <- "\nPackage uniset has been updated.\nPlease see \nhttps://bpollner.github.io/uniset/news/index.html \nfor more information on what was changed. \n\nA package tried to hand over values to package 'uniset' in an outdated manner."
    #
    if (!is(taPaInput, "list")) {
        stop(msg)
    } # end if
    if (length(taPaInput) != 2) {
        stop(msg)
    } # end if
    if (! all(names(taPaInput) %in% liNames)) {
        stop(msg)
    } # end if
    return(TRUE)
} # EOF

#' @title Simple Test
#' @description Test if the input package name etc. was correct / successful. 
#' This function is meant to be called from inside the  target package.
#' @param uniset_handover List length two, containing two elements:
#' 1. `pkgname`: The name of the target package.
#' 1. `funcname`: The name of the function handing over the required values.
#' @return Is printing the parameters defined by the target package, and is 
#' returning those parameters in an (invisible) list.
#' @section Important: This function is intended to be called from within the 
#' target package.
#' @section Note: Please refer to \code{\link{uniset}} for a link to examples 
#' and a real-world demo.
#' @examples{
#' \dontrun{
#' # to be called from within the target package
#' uniset_test(uniset_handover_pkgname, uniset_handover_funcname)
#' }
#' }
#' @export
uniset_test <- function(uniset_handover) {
    check_target_package_handover(uniset_handover)
    txt <- paste0(uniset_handover$pkgname, "::", uniset_handover$funcname, "()")
    val_list <- eval(parse(text=txt))
    print(utils::str(val_list))
    return(invisible(val_list))
} # EOF
####################################################################
getCheckForDoubleNames <- function(pathToLocal, pathToPack, pmu) {
    #
    checkIt <- function(charVec, what) {
        aa <- length(charVec)
        bb <- length(unique(charVec))
        if (bb < aa) {
            rl <- rle(sort(charVec)) # but they already come in sorted *
            ind <- which(rl$lengths > 1)
            vals <- rl$values[ind]
            stop(paste0("Sorry, the keys '", paste0(vals, collapse="', '"), "' ", "seem to appear more than once in the ", what, "settings.R file."), call.=FALSE)
        } # end if
    } # EOIF
    ##
    lenv <- new.env()
    sys.source(pathToLocal, envir=lenv)
    txt <- paste0("sort(names(lenv", pmu, "))") # *
    locNames <- eval(parse(text=txt))
    penv <- new.env()
    sys.source(pathToPack, envir=penv)
    txt <- paste0("sort(names(penv", pmu, "))") # *
    pacNames <- eval(parse(text=txt))
    #
    checkIt(locNames, "local ")
    checkIt(pacNames, "package ")
    #
    return(list(locNames=locNames, pacNames=pacNames))
} # EOF

getStnCenter <- function(pac, taPaObj) {
        fconPack <- file(pac, open="r")
        ftPack <- readLines(fconPack)   # read in the pac file
        close(fconPack)
        indPm <- which(startsWith(trimws(ftPack), taPaObj)) # on which line number is the "stn" object
        indBracket <- which(startsWith(trimws(ftPack), ")"))
        return(ftPack[indPm:indBracket])
} # EOF

getKeysOnlyFromText <- function(ftXX, splitChar, taPaObj) {
    ftXXr <- unlist(lapply(strsplit(trimws(ftXX), splitChar), function(x) trimws(x[1])))
    ftXXr[is.na(ftXXr)] <- "" # replace NAs with empty character. NAs were those that did not have a key = value pair. (--> an "=")
    ftXXr[1] <- taPaObj
    return(ftXXr)
} # EOF

getMissingKVPs <- function(ftPack, ftPackR, missingKeys) {
    out <- character(length(missingKeys))
    for (i in 1: length(missingKeys)) {
        out[i] <- ftPack[which(ftPackR == missingKeys[i])]
    } # end for i
    return(out)
} # EOF

getTxtsBetweenLocal <- function(ftLocal, ftLocalR, indLocHook, indLocNext, ftPack, ftPackR, indKey, maxS) {
#   indKey is in ftPack/R dimension
#   txtBetweenLocal: tbl
#   txtBetweenLocalUpper: tbl_U
#   txtBetweenLocalLower: tbl_L
    #
    if (indLocHook+1 == indLocNext) { # so there is no between text
        return(list(tbl=NULL, tbl_U=NULL, tbl_L=NULL)) # direct insertion between two adjacent lines
    } # end if
    #
    tbl <- ftLocal[(indLocHook+1):(indLocNext-1)]
    ######
    indPacHook <- which(ftPackR == ftLocalR[indLocHook])
    if (indPacHook+1 == indKey) { # there is no space above; the new key is directly below the hook
        return(list(tbl=tbl, tbl_U=NULL, tbl_L=tbl))
    } # end if
    #
    indPacNext <- which(ftPackR == ftLocalR[indLocNext])
    if (indPacNext-1 == indKey) { # there is no space below, the new key is directly above the next
        return(list(tbl=tbl, tbl_U=tbl, tbl_L=NULL))
    } # end if
    #######
    if (all(trimws(tbl) == "")) {
        return(list(tbl=tbl, tbl_U=tbl, tbl_L=NULL)) # classic case for new block
    } # end if
    if (all(trimws(tbl) != "")) {
        return(list(tbl=tbl, tbl_U=rep("", maxS), tbl_L=tbl)) # can be either or (top or bottom, no way to know). But we are "somewhere in the middle", so insert empty lines above
    } # end if
    # by now tbl has to be longer than 1
    # and by now the new key HAS to be "somewhere in the middle"
    indFirstSpace <- which(trimws(tbl) == "")[1] # so the text could belong more to below
    if (indFirstSpace == 1) {
        return(list(tbl=tbl, tbl_U=rep("", maxS), tbl_L=tbl))
    } # end if
    #
    tbl_U <- tbl[1:(indFirstSpace-1)]
    tbl_L <- tbl[indFirstSpace: length(tbl)]
    return(list(tbl=tbl, tbl_U=c(tbl_U, rep("", maxS)), tbl_L=tbl_L)) # because we have to be "in the middle" somewhere
} # EOF

getTextBetweenPac <- function(pacNames, singleMissingKey, ftPackR, ftPack, indKey, ftLocal) {
    pacHookAbove <- pacNames[which(pacNames == singleMissingKey)-1]
    indPacHook <- which(ftPackR == pacHookAbove)
    keyIndFT <- which(ftPackR == singleMissingKey)
    if (indPacHook == (keyIndFT-1)) { # so there is no space between the two keys
        return(NULL)
    } # end if
    #
    txtBetweenPac <- ftPack[(indPacHook+1):(indKey-1)]
    # cut away everything above that is above an empty line
    txtT <- trimws(txtBetweenPac)
    if (all(txtT == "")) {
        return(NULL)
    } # end if
    aa <- max(which(txtT == ""))    # get the index of the last empty line
    if (aa == length(txtBetweenPac)) { # so if the last line is empty, that means we do not have text directly above the key
        return(NULL)
    } # end if
    out <- txtBetweenPac[(aa+1):(length(txtBetweenPac))]
    ind <- which(out %in% ftLocal)
    if (length(ind) != 0) {
        out <- out[-ind] # make sure that we do not copy anything that is already in the local file
    } # end if
    if (length(out) == 0) {
        out <- NULL # just to be sure
    } # end if
    return(out)
} # EOF

getTxtEmptyBelowPacKey <- function(indKey, pacNames, ftPack, ftPackR, ftLocal) { # indKey is in ftPackR-dimension
    aa <- which(pacNames == ftPackR[indKey])+1
    indPacNext <- which(ftPackR == pacNames[aa])
    if (indKey+1 == indPacNext) { # so there is no space between
        return(NULL)
    } # end if
    txtPackBelowKey <- ftPackR[(indKey+1):(indPacNext-1)]
    txtPackBelowKey_Full <- ftPack[(indKey+1):(indPacNext-1)]

    if (all(txtPackBelowKey == "")) {
        return(txtPackBelowKey)
    } # end if
    if (all(txtPackBelowKey != "")) {
        return(NULL)
    } # end if
    rl <- rle(txtPackBelowKey == "")
    if (rl$values[1]) { # we have some empty lines as first block
        return(rep("", rl$lengths[1]))
    } else {
        txt <- txtPackBelowKey_Full[1:rl$lengths[1]] # first get the characters
        aa <- which(txt %in% ftLocal)
        txt <- txt[-aa]
        empty <- rep("", rl$lengths[2]) # then get the empty lines
        return(c(txt, empty))
    } # end else
    return("\t# something went wrong...") # we should never get here
} # EOF

getMaxSpace <- function(ftLocal) {
    aa <- trimws(ftLocal)
    rl <- rle(aa=="")
    out <- max(rl$lengths[rl$values])
    return(out)
} # EOF

reduceEmptyLines <- function(ftLocal, maxS, maxSD) { # maxSD is after deleting (is the higher one)
    # find the indices where the too big space maxSD is
    if (maxSD > maxS) { # so we have to cut down on big empty spaces
        ftT <- trimws(ftLocal)
        rl <- rle(ftT == "")
        aboveSIndRL <- which(rl$lengths > maxS & rl$values)
        indDelOut <- NULL
        for (i in 1: length(aboveSIndRL)) {
            ind <- aboveSIndRL[i]
            thisLeng <- rl$lengths[ind]
            cutAwayLeng <- thisLeng - maxS
            maxIndTxt <- sum(rl$lengths[1:ind])
            minIndTxt <- maxIndTxt - cutAwayLeng +1
            indDelOut <- c(indDelOut, (minIndTxt : maxIndTxt)) # cut down delete to the max empty lines of before deletion
        } # end for i
        return(ftLocal[-indDelOut])
    } else {
    return(ftLocal) # nothing to cut away, return the original
    }
} # EOF

tellKeyAddDelete <- function(keys, folderLocal, nameLocal, what="add") {
    if (what == "add") {
        whatTxt <- " added to"
    } else {
        whatTxt <- " deleted from"
    }
    if (length(keys) > 1) {
        plS <- "s"; plC <- " were"
    } else {
        plS <- ""; plC <- " was"
    }
#   cat(paste0("The following ", length(keys), " key", plS, plC, whatTxt, " the settings-file '", nameLocal, "' in \n'", folderLocal, "':\n\t", paste0(keys, collapse=", "), "\n\n"))
    cat(paste0("The following ", length(keys), " key", plS, plC, whatTxt, " the settings-file '", nameLocal, "' in \n'", folderLocal, "':\n\t"))
    message(paste0(keys, collapse=", "), "\n")
} # EOF

addMissingKeys <- function(ftLocal, splitChar, taPaObj, pathToPack, folderLocal, nameLocal, pacNames, locNames, maxS) {
    missingKeys <- pacNames[which(!pacNames %in% locNames)]
    if (length(missingKeys != 0)) { # so we do have to add something
        ftLocalR <- getKeysOnlyFromText(ftLocal, splitChar, taPaObj)
        ftPack <- getStnCenter(pathToPack, taPaObj)
        ftPackR <- getKeysOnlyFromText(ftPack, splitChar, taPaObj)
        missingKVPs <- getMissingKVPs(ftPack, ftPackR, missingKeys) # all the missing key-values pairs in one object
        for (i in 1: length(missingKeys)) {
            indKey <- which(ftPackR == missingKeys[i])  # the pac index of a key missing in loc
            if (length(indKey) > 1) {stop("Sorry, it seems that a key name appears twice.", call.=FALSE)}
            newKVP <- missingKVPs[i]

            # now look up for the next hook
            pacHook <- pacNames[which(pacNames == missingKeys[i])-1]
            locHookKeyInd <- which(locNames == pacHook)
            locHook <- locNames[locHookKeyInd] # the name of the key in loc one higher than the missing one

            # get the comments above and empty spaces below the pacHook (if there are any)
            txtBetweenPack <- getTextBetweenPac(pacNames, missingKeys[i],ftPackR, ftPack, indKey, ftLocal)
            txtEmptyBelowPacKey <- getTxtEmptyBelowPacKey(indKey, pacNames, ftPack, ftPackR, ftLocal)

            # get all local lines between hook and next
            indLocHook <- which(ftLocalR == locHook) # next higher hook in local file !!
            locNext <- locNames[which(locNames == locHook)+1] # the name of the next key present in the local file
            indLocNext <- which(ftLocalR == locNext)
            aa <- getTxtsBetweenLocal(ftLocal, ftLocalR, indLocHook, indLocNext, ftPack, ftPackR, indKey, maxS) # here it is decided where in the between text the new KVP is put
                txtBetweenLocal <- aa$tbl
                txtBetweenLocalUpper <- aa$tbl_U
                txtBetweenLocalLower <- aa$tbl_L
            txtUpper <- ftLocal[(1):(indLocHook)] # !!!!! changes this if pacHook == taPaObj.
            txtLower <- ftLocal[(indLocNext):length(ftLocal)]
            #
            # put together the text, add to locNames etc. as well.
            ftLocal <- c(txtUpper, txtBetweenLocalUpper, txtBetweenPack, newKVP, txtEmptyBelowPacKey, txtBetweenLocalLower, txtLower) ## CORE ##
            ftLocalR <- getKeysOnlyFromText(ftLocal, splitChar, taPaObj) # could be removed (is done at the top of the loop already)
            locNames <- c(locNames[1:locHookKeyInd], missingKeys[i], locNames[(locHookKeyInd+1):length(locNames)])
            if (FALSE) {
        # #     print(txtUpper)
#               print(locHook)
#               print(txtBetweenLocalUpper)
#               print(txtBetweenPack)
#               print(newKVP)
#       #       print(txtEmptyBelowPacKey)
#       #       print(txtBetweenLocalLower)
#               print(locNext)
#       #       print(txtLower)
#       #       #
#       #       print(ftLocal)
#               print("---------------------------")
#       #       wait()
            } # end if TRUE  # dev helpers, is printing things.
        } # end for i (going through missing keys)
        maxSD <- getMaxSpace(ftLocal) # the max space after adding keys
        ftLocal <- reduceEmptyLines(ftLocal, maxS, maxSD)
        #
        tellKeyAddDelete(missingKeys, folderLocal, nameLocal, what="add")
    } ########### end if length(missingKeys) != 0 # until here, things were added. OR not.
    return(ftLocal)
} # EOF

deleteSurplusKeys <- function(folderLocal, nameLocal, ftLocal, splitChar, taPaObj, locNames, pacNames, maxS) {
    surplusKeys <- locNames[which(!locNames %in% pacNames)]
    if (length(surplusKeys != 0)) { # so we do have to delete something
        ftLocalR <- getKeysOnlyFromText(ftLocal, splitChar, taPaObj) # the incoming ftLocal is "stn" only, and was possibly modified above in the additions
        for (i in 1: length(surplusKeys)) { # we are collecting possible single line comments above a block
            indKey <- which(ftLocalR == surplusKeys[i])
            aa <- trimws(ftLocal) # so that tabs etc go to ""
            ikm <- NULL
            if ( (aa[indKey-1] != "")  & (aa[indKey-2] == "") ) { # that means we have a single line of comment above a key
                ikm <- indKey-1 # ikm: index key minus
                if (ftLocalR[ikm] %in% locNames) { # now this above could be a key, check....
                    ikm <- NULL
                } # end if
                if (ftLocalR[indKey+1] %in% locNames) { # means we have an other key directly below the one to be deleted, so we will *not* delete the one comment line above
                    ikm <- NULL
                } # end if
            } # end if
            indDel <- c(indKey, ikm)
            ftLocal <- ftLocal[-indDel] # delete here #### ******************
            ftLocalR <- getKeysOnlyFromText(ftLocal, splitChar, taPaObj)
        } # end for i going through surplusKeys
        #
        maxSD <- getMaxSpace(ftLocal) # the max space after deleting keys
        ftLocal <- reduceEmptyLines(ftLocal, maxS, maxSD)
        #
        tellKeyAddDelete(surplusKeys, folderLocal, nameLocal, what="delete")
        ind <- which(locNames %in% surplusKeys)
        locNames <- locNames[-ind]
    } # end if (length(surplusKeys != 0))
    return(list(ftLocal=ftLocal, locNames=locNames))
} # EOF

checkFileVersionPossiblyModify <- function(pathToPack, folderLocal, nameLocal, pm=NULL, tmpl, taPaName=NULL){
    pv_suffixForTemplates <- tmpl
    taPaObj <- pm
    splitChar <- "="
    #
    loc <- pathToLocal <- paste0(folderLocal, "/", nameLocal)
    pac <- pathToPack
    if (is.null(pm)) {
        pmu <- ""
    } else {
        pmu <- paste0("$", taPaObj)
    }
    aa <- getCheckForDoubleNames(pathToLocal, pathToPack, pmu) # is checking for non-unique keys
    if (identical(aa$locNames, aa$pacNames)) {
        return(invisible(TRUE))
    } # end if identical

    # we only continue, if the locNames and pacNames are NOT identical

    ######## first we will ADD any possible keys
    # get the name of the keys that are missing in local / added in pathToPack
    lenv <- new.env()
    sys.source(pathToLocal, envir=lenv)
    txt <- paste0("names(lenv", pmu, ")") # NOT sorted
    locNames <- c(taPaObj, eval(parse(text=txt))) # add taPaObj as first in case of a first key is introduced. Need a hook then.
    penv <- new.env()
    sys.source(pathToPack, envir=penv)
    txt <- paste0("names(penv", pmu, ")") # NOT sorted
    pacNames <- c(taPaObj, eval(parse(text=txt)))
    #
    #get parts before and after the list (TaPaPbj)
    fconPack <- file(pathToPack, open="r")
    ftPack <- readLines(fconPack)   # read in the pac file
    close(fconPack)
    fconLocal <- file(pathToLocal, open="r")
    ftLocal <- ftLocalBackup <- readLines(fconLocal)   # read in the local file
    close(fconLocal)
    indPm <- which(startsWith(trimws(ftLocal), taPaObj)) # on which line number is the "stn" object
    indBracket <- which(startsWith(trimws(ftLocal), ")"))
    txtAbove <- ftLocal[1:(indPm-1)] # get the txtAbove and txtBelow from the local file. The user could have written something in there that should stay.
    txtBelow <- ftLocal[(indBracket+1):length(ftLocal)]
    #
    ftLocal <- getStnCenter(pathToLocal, taPaObj) # might need that in the deletions. !!! gets possibly modified in the additions below
    maxS <- getMaxSpace(ftLocal) # get the maximum number of continuous empty lines
    #######
    aa <- deleteSurplusKeys(folderLocal, nameLocal, ftLocal, splitChar, taPaObj, locNames, pacNames, maxS) # ***************
        ftLocal <- aa$ftLocal
        locNames <- aa$locNames # locNames have to updated due a possible deletion of keys
    ftLocal <- addMissingKeys(ftLocal, splitChar, taPaObj, pathToPack, folderLocal, nameLocal, pacNames, locNames, maxS) # **************
    #######
    # now write into local settings file
    fconLocal <- file(loc, open="w")
    writeLines(c(txtAbove, ftLocal, txtBelow), fconLocal) # write the new file to settings.r in pathSH
    close(fconLocal)
    #
    # just to be sure, check again
    aa <- getCheckForDoubleNames(pathToLocal, pathToPack, pmu)
    if (!identical(aa$locNames, aa$pacNames)) { # this should never happen
        message("Sorry, for unknown reasons the keys in the settings file could not be updated.")
        fconLocal <- file(loc, open="w")
        writeLines(ftLocalBackup, fconLocal) # write the backup file to settings.r in pathSH
        close(fconLocal)
        cat(paste0("A template containing the required key-value pairs will be made available.\n"))
        pleaaseCopyAsTemplate(pathToPack, folderLocal, nameLocal)
        return(invisible(FALSE))
    } # end if identical
    #
    return(invisible(TRUE))
} # EOF
####################################################################

# this is the big one checking everything. Telling re-run setup or to restart.
checkSetup <- function(taPaList, setupFuncName, sysHome_R_test=NULL, taPaSH_system_test=NULL) {
    ######
    aaa <- taPaList
        taPaName <- aaa$taPaName
        taPaSH <- aaa$taPaSH
        taPaObj <- aaa$taPaObj
        tmplName <- aaa$tmplName
        setFiName <- aaa$setFiName
    ######
    systemHome <- Sys.getenv("HOME")
    systemHome_R <- gsub("\\\\", "/", systemHome)
    pat <- paste0("Sys.getenv(\"", taPaSH, "\")")
    taPaSH_system <- eval(parse(text=pat))  #  the value for taPaSH from the environment variables
    onTest <- FALSE
    ###
    if (!is.null(sysHome_R_test)) { # so we are in a test
        onTest <- TRUE
        systemHome_R <- sysHome_R_test
        taPaSH_system <- taPaSH_system_test
    } # end if
    fullRenvPath <- paste0(systemHome_R, "/.Renviron")
    fn_taPaSH <- taPaSH # makes the name of the variable and the name of the final folder identical
    #
    noRenvMsg <- paste0("The required .Renviron file at \n'", systemHome_R, "'\n does not exist. \n")
    varMissMsg <- paste0("The required variable '", taPaSH, "' does not exist in the .Renviron file at \n'", fullRenvPath, "'\n")
    SHdirNotValid <- paste0("The variable '", taPaSH, "' in the .Renviron file at \n'", fullRenvPath, "'\n is not pointing to an existing directory: \n")   
    SHdirNotValid_add <- paste0("Please: \n    a) Modify the value of '", taPaSH, "', or \n    b) Create the appropriate folder, or \n    c) Re-Run the setup function '", setupFuncName, "'.\n")
    noSetFiMsg <- paste0("The required file '", setFiName, "' does not seem to exist at \n")
    pleaseRunMsg <- paste0("Please call the setup function '", setupFuncName, "'.\n")
    shSystemEmpty <- paste0("Please restart R to update the systems environemnt variables.")
    shSystemDiff <- paste0("Please restart R for the changes in the .Renviron file at the variable '", taPaSH, "' to become effective.\n")
    #
    if (!file.exists(fullRenvPath)) {                            ### CASE 1 ### ok
        message(paste0(noRenvMsg, pleaseRunMsg))
        return(FALSE)
    } # EOF
    #
    ###### so .Renviron does exist: check if we have the variable
    fcon <- file(fullRenvPath, open="rt")
    content <- readLines(fcon)
    close(fcon)
    ind <- which(grepl(taPaSH, content))
    
    # we have more than one variable of taPaSH
    if (length(ind) > 1) {                                  ### CASE 2 ### ok
        multiVarMsg <- paste0("There are ", length(ind), " occurrences of the variable '", taPaSH, "' in the .Renviron file.\n")
        message(paste0(multiVarMsg, pleaseRunMsg))
        return(FALSE)
    } # end if (length(ind) > 1)

    # we do NOT have taPaSH
    if (length(ind) == 0) {                                 ### CASE 3 ### ok
        message(paste0(varMissMsg, pleaseRunMsg))
        return(FALSE)
    } # end if

    # by now the variable must be there exactly once, check its value
    taPaSH_file <- content[ind] # get only the one string that is the taPaSH
    fileValue <- trimws(strsplit(taPaSH_file, "=")[[1]][[2]]) # the [[1]] to get out of the list. naja. 
    if (!dir.exists(fileValue)) {                           ### CASE 4 ###  ok
        message(paste0(SHdirNotValid, "'" ,fileValue, "'\n", SHdirNotValid_add))
        return(FALSE)
    } # end if
    
    # check if we have a settings.R file at this destination
    if (! file.exists(paste0(fileValue, "/", setFiName))) {  ### CASE 5 ###  ok
        message(paste0(noSetFiMsg, "'", fileValue, "'\n", pleaseRunMsg))
        return(FALSE)
    } # end if
        
        
    # by now we must have one variable pointing to a valid directory containing a settings.r file. 
    # Now check if also in the system, i.e. if we have to restart or not.
    if (!onTest) {
        pat <- paste0("Sys.getenv(\"", taPaSH, "\")")
        taPaSH_system <- eval(parse(text=pat))  # returns `""` if not existing in Sys.getenv()
    } # end if !onTest
    if (taPaSH_system == "") {                              ### CASE 6 ### ok
        message(shSystemEmpty)
        return(FALSE)
    } # end if
    # check if the value is different on file and on system --> the user could have changed the value etc. we need to restart
    if (taPaSH_system != fileValue) {                        ### CASE 7 ###
        message(shSystemDiff)
        return(FALSE)
    } # end if
    #
    # if we made it to here, everything should be nice and well. Ha.
    return(TRUE)                                            ### CASE 8 ###  ok
} # EOF

# this is called by uniset_updateSettings
checkSettings <- function(taPaList, setupFuncName, sysHome_R_test=NULL, taPaSH_system_test=NULL, taPaSettingsPath_test=NULL) {
    #
    setupOk <- checkSetup(taPaList, setupFuncName, sysHome_R_test, taPaSH_system_test) # returns TRUE or FALSE ### CORE ###
    #
    if (!setupOk) {
        return(FALSE)
    } # end if
    #
    setFiName <- taPaList$setFiName
    taPaObj <- taPaList$taPaObj
    taPaName <- taPaList$taPaName
    tmplName <- taPaList$tmplName
    taPaSH <- taPaList$taPaSH
    #
    if (is.null(sysHome_R_test)) { # so we are in real life
        taPaSettingsPath <- paste0(path.package(taPaName), "/",  setFiName) # the full path to the settings in the *installed* target package (there, it is in root !)
        pat <- paste0("Sys.getenv(\"", taPaSH, "\")")
        taPaSH_system <- eval(parse(text=pat))  #  the value for taPaSH from the environment variables
    } else {
        taPaSettingsPath <- taPaSettingsPath_test
        taPaSH_system <- taPaSH_system_test
    } # end else
    return(checkFileVersionPossiblyModify(pathToPack=taPaSettingsPath,
        folderLocal=taPaSH_system, nameLocal=setFiName, pm=taPaObj,
        taPaName, tmpl=tmplName))  # returns TRUE or FALSE
} # EOF
####################################################################
checkSetupFuncName <- function(setupFunc, taPaName) {
    if (is.null(setupFunc)) {
        stop(paste0("Please provide the name of the function in the package '", taPaName, "' that is containing the function 'uniset::uniset_setup'.\n"), call.=FALSE)
    } # end if
    checkCh1(setupFunc, "setupFunc")
    # no additional check if it really exists or not
    return(TRUE) # for the test
} # EOF

pleaaseCopyAsTemplate <- function(taPaSettingsPath, taPaSH_system, setFiName) {
    tmpl <- "_TEMPLATE.R"
    #
    td <- tempdir()
    ok <- file.copy(taPaSettingsPath, td, overwrite = TRUE)
    nfn <- paste0(td, "/", setFiName)
    nfn_T <- paste0(nfn, tmpl)
    ok <- file.rename(nfn, nfn_T)
    ok <- file.copy(nfn_T, taPaSH_system, overwrite = TRUE)
    unlink(nfn_T)
    if (!ok) {
        message("Sorry, for unknown reasons the required template file could not be copied.\n")
        return(invisible(FALSE))
    } else {
    message(paste0("The file '", paste0(setFiName, tmpl), "' has been copied into \n'", taPaSH_system, "'\n"))
    return(invisible(TRUE))
    } # end else
} # EOF

getUnisHandoverVariables <- function(uniset_handover) {
    #
    seFiName <- glob_filnameSettings # "settings.R"
    #
    txt <- paste0(uniset_handover$pkgname, "::", uniset_handover$funcname, "()")
    val_list <- eval(parse(text=txt))
    #
    taPaName <- val_list$pkgUniset_UserPackageName
    taPaSH <- val_list$pkgUniset_RenvironSettingsHomeName
    taPaObj <- val_list$pkgUniset_SettingsObjectName
    tmplName <- val_list$pkgUniset_SuffixForTemplate
    setFiName <- paste0(taPaName, "_", seFiName)
    return(list(taPaName=taPaName, taPaSH=taPaSH, taPaObj=taPaObj, tmplName=tmplName, setFiName=setFiName))
} # EOF

sourceSettingsToObject <- function(taPaList, pathSettings_test=NULL) {
    taPaName <- taPaList$taPaName
    taPaSH <- taPaList$taPaSH
    taPaObj <- taPaList$taPaObj
    setFiName <- taPaList$setFiName
    #
    if (is.null(pathSettings_test)) {
        pathSettings <- paste0(Sys.getenv(taPaSH), "/", setFiName) # the path to the local settings file as defined in the .Renviron file
    } else {
        pathSettings <- pathSettings_test
    } # end else
    #
    options(warn=-1)
    stn_obj <- try(source(pathSettings, verbose = FALSE)$value, silent=TRUE)
    options(warn=0)
    if (is(stn_obj, "try-error")) {
    	return(NULL)
    } else {
        return(invisible(stn_obj))
    } # end else
} # EOF

suStop <- function() {
    stop("Setup process has been stopped", call.=FALSE)
} # EOF

performSetup_create_taPaSH <- function(userLoc, taPaList) { #1
    fn_taPaSH <- taPaList$taPaSH
    #
    if (!dir.exists(paste0(userLoc, "/", fn_taPaSH))) { # it should not exist, unless the user is calling the setup function twice.
        dirCreaOk <- dir.create(paste0(userLoc, "/", fn_taPaSH), showWarnings=FALSE) # to the CRAN-reviewer: this is NOT a default writing into user space. We are only creating the directory here when the user called the setup function 'uniset_setup' and provided a path where to create the directory and copy the file to. 
        if (!dirCreaOk) {
            msg <- paste0("Sorry, the required settings-home directory `", fn_taPaSH, "` could not be created in \n'", userLoc, "'\n")
            message(msg)
            return(FALSE)
        } else { # so all is good, the folder could be created
            msg <- paste0("The folder `", fn_taPaSH, "` as settings-home directory has been created in \n`", userLoc, "`\n")
            message(msg)
            return(TRUE)
        } # end else    
    } # end if !dir.exists
    message(paste0("The folder \n'", paste0(userLoc, "/", fn_taPaSH), "' \nalready existed.\n"))
    return(TRUE)    # so the dir does already exist
} # EOF

performSetup_copySettingsFile <- function(userLoc, taPaList, taPaSettingsPath_test=NULL) { #2
    taPaName <- taPaList$taPaName
    taPaSH <- taPaList$taPaSH
    setFiName <- taPaList$setFiName
    ##
    if (is.null(taPaSettingsPath_test)) { # so we are for real, we are NOT in a test
        onTest <- FALSE
        taPaSettingsPath <- path.package(taPaName)
        taPaSettingsPath_fn <- paste0(taPaSettingsPath, "/",  setFiName)
    } else { # so we are in a test
        onTest <- TRUE
        taPaSettingsPath <- taPaSettingsPath_test
        taPaSettingsPath_fn <- paste0(taPaSettingsPath_test, "/", setFiName)
    } # end else
    ##
    ##
    userLoc_SH <- paste0(userLoc, "/", taPaSH)
    userLoc_fn <- paste0(userLoc_SH, "/", setFiName)
    successsMsg <- paste0("The file '", setFiName, "' has been copied into \n'", userLoc_SH, "'\n")
    # check if already there, and if not please simply copy the settings
    if (!dir.exists(userLoc_SH)) { # if the settings-home folder is not existing, create one
        ok <- dir.create(userLoc_SH, showWarnings=FALSE)
        if (!ok) {
            message(paste0("Sorry, it was not possible to create the folder `", taPaSH, "` in the directory `", userLoc, "`."))
            return(FALSE)       
        } # end if
    } # end if
    if (!file.exists(userLoc_fn)) {
        ok <- file.copy(taPaSettingsPath_fn, userLoc_SH)
        if (!ok) {
            message(paste0("Sorry, it was not possible to copy the `", setFiName, "` file from \n`", taPaSettingsPath_fn, "` to \n`", userLoc_SH, "`."))
            return(FALSE)
        } else { # so we could copy the settings.r file
            message(successsMsg)
            return(TRUE)
        } # end else
    } else { # so the settings.R file does exist
        message(paste0("The file '", setFiName, "' already existed in \n'", userLoc_SH, "'\n"))
        return(TRUE)
    } # end else    
} # EOF

checkRenvContent_maybeCopyBackup <- function(systemHome_R, bupDir) { # in #3
    # this is to ensure that in the writing / appending attempts we do not destroy / damage the previous, already present .Renviron file
    #
    fullRenvPath_actual <- paste0(systemHome_R, "/.Renviron")
    fullRenvPath_backup <- paste0(bupDir, "/.Renviron")
    
    # the Renviron at the moment
    fcon <- file(fullRenvPath_actual, open="rt")
    contentActual <- readLines(fcon)
    close(fcon)
    
    # the backup Renviron
    fcon <- file(fullRenvPath_backup, open="rt")
    contentBackup <- readLines(fcon)
    close(fcon) 
    
    # compare content, possibly copy
    if (! identical(contentActual, contentBackup)) { # already very unlikely
        ok <- file.copy(fullRenvPath_backup, systemHome_R, overwrite=TRUE) # this restores the backup version
        if (!ok) { # this should never ever happen
            stop(paste0("An error while trying to append / modify the existing .Renviron file has occurred. \nWe are terribly sorry, but for unknown reasons it was not possible to restore your .Renviron file to its original version.\nPlease go to \n'", fullRenvPath_actual, "',\ncheck the content of the file and possibly restore it from one of your personal backups."), call.=FALSE)
        } # end if !ok
        return(FALSE) # only needed for testing
    } else { # so they are identical
        return(TRUE) # only needed for testing
    } # end else    
} # EOF

performSetup_checkCreateModRenv <- function(userLoc, taPaList, sysHome_R_test=NULL) { #3
    taPaSH <- fn_taPaSH <- taPaList$taPaSH
    taPaName <- taPaList$taPaName
    ##
    # do we have one ? if no, create, if yes, add.
    systemHome <- Sys.getenv("HOME")
    systemHome_R <- gsub("\\\\", "/", systemHome)
    if (!is.null(sysHome_R_test)) { # so we are in a test
            systemHome_R <- sysHome_R_test
    } # end if  
    fullRenvPath <- paste0(systemHome_R, "/.Renviron")
    intendedValue <- paste0(userLoc, "/", fn_taPaSH) # the value that the variable taPaSH in the Renviron file should have
    ##
    creationMsg <- paste0("The required '.Renviron' file in \n'", systemHome_R, "' \nhas been created.\n\n")
    pathMsg <- paste0("The path of '", taPaSH, "' in the .Renviron file \n'", fullRenvPath, "' \nhas been set to \n'", userLoc, "/", fn_taPaSH, "'.\n(This is the path to the folder containing the settings.R file.)\n\n")
    appendMsg <- paste0("The existing .Renviron file has been appended with the new variable '", taPaSH, "'.\n\n")
    addInfoRestartMsg <- "Restart R for the changes to become effective.\n\n"
    restartMsg <- "Please restart R for the changes in the .Renviron file to become effective.\n\n"
    varExistMsg <- paste0("The variable '", taPaSH, "' already existed in the .Renviron file \n'", fullRenvPath, "'\n\n")
    varValueGood <- paste0("The value of '", taPaSH, "' was correctly set to \n'", intendedValue, "'.\n")
    varValueBad <- paste0("The value of '", taPaSH, "' was corrected to \n'", intendedValue, "'.\n\n")  
    multiVarMsg <- NULL         
    ##
    #######
    renvExists <- file.exists(fullRenvPath)

    if (!renvExists) { ####### we have to create one and fill with content ##########################################
        createOK <- file.create(fullRenvPath, showWarnings=FALSE) # to the CRAN-reviewer: this is NOT a default writing into user space. We are only creating the file here when the user called the setup function 'uniset_setup'
        if (!createOK) {  #  if .Renviron could not be created
            msg <- paste0("Sorry, the creation of the .Renviron file in `", systemHome_R, "` failed.\n")
            message(msg)
            return(FALSE)
        } # end if !createOK
        # now we have to fill the newly created .Renviron file and point taPaSH to the newly created folder
        aa <- try({
            defaultFillForRenviron <- c(paste0("\n\n## ", taPaName, ":"), paste0(taPaSH, " = ", userLoc, "/", fn_taPaSH))
            fcon <- file(fullRenvPath, open="w")
            writeLines(defaultFillForRenviron, fcon)
            close(fcon)
        }, silent=TRUE)
        if (is(aa, "try-error")) { # how to test for that ?
    #       unlink(fullRenvPath)
            message(paste0("Sorry, it was not possible to write the new variable '", taPaSH, "' to the created .Renviron file."))
            return(FALSE)
        } # end if
        message(paste0(creationMsg, pathMsg, addInfoRestartMsg))
        return(TRUE)
    } # end if (!renvExists)
    
    
    ###### so Renv does exist, we are appending to / modifying the existing .Renviron file 
    
    # make backup and read in content of existing Renviron file
    bupDir <- paste0(tempdir(), "/backup")
    if (!dir.exists(bupDir)) {dir.create(bupDir)} # # create the folder "backup" in tempdir.
    file.copy(fullRenvPath, bupDir, overwrite=TRUE) # copy the existing .Renviron to tempdir/backup as backup
    ##
    fcon <- file(fullRenvPath, open="rt")
    content <- readLines(fcon) # read in the content from the existing Renviron file
    close(fcon)
    
    
    #### now check if taPaSH is already in the Renviron file
    ind <- which(grepl(taPaSH, content)) # *** check here 
    
    # taPaSH is present more than once
    if (length(ind) > 1) { # so we have more than one variable of taPaSH
        content <- content[-ind] # delete all of them
        multiVarMsg <- paste0("There were ", length(ind), " occurrences of the variable '", taPaSH, "' in the .Renviron file. They were all deleted.\n")
        ind <- integer(0)
    } # end if (length(ind) > 1)
    
    # taPaSH is NOT present
    if (length(ind) == 0) { # so we do NOT have taPaSH in the Renviron file, we have to append it 
        newContent <- c(paste0("\n\n## ", taPaName, ":"), paste0(taPaSH, " = ", userLoc, "/", fn_taPaSH), "\n") 
        aa <- try({
            fcon <- file(fullRenvPath, open="wt")
            writeLines(c(content, newContent), fcon) # append the existing content with the new variable and path to local settings home
            close(fcon)
        }, silent=TRUE)
        if (is(aa, "try-error")) { # how to test for that?
            checkRenvContent_maybeCopyBackup(systemHome_R, bupDir)
            message(paste0("Sorry, it was not possible to append the existing .Renviron file with the new variable '", taPaSH, "'.\n\n"))
            return(FALSE)
        } # end if
        # so by now we are sure that we were able to append the .Renviron file
        message(paste0(multiVarMsg, appendMsg, pathMsg, addInfoRestartMsg))
        return(TRUE)        
    } # end if (length(ind) == 0)

    # taPaSH is present one time
    if (length(ind) == 1) { # Check if its value is correct. If not, replace it.  
        taPaSH_file <- content[ind] # get only the one string that is the taPaSH
        fileValue <- trimws(strsplit(taPaSH_file, "=")[[1]][[2]]) # the [[1]] to get out of the list. naja.
        if (fileValue != intendedValue) { # we have to replace it  
            newLine <- paste0(taPaSH, " = ", userLoc, "/", fn_taPaSH)
            content[ind] <- newLine  # replace the previous old value with the new one
            aa <- try({
                fcon <- file(fullRenvPath, open="wt")
                writeLines(content, fcon) # write back the old content, but with the one line containing the taPaSH variable changed to the new value
                close(fcon)
            }, silent=TRUE)
            if (is(aa, "try-error")) { # how to test for that?
                checkRenvContent_maybeCopyBackup(systemHome_R, bupDir)
                message(paste0("Sorry, it was not possible to change the value of the variable '", taPaSH, "' in the existing .Renviron file \n'", fullRenvPath, "' from \n'", fileValue, "' to \n'", intendedValue, "'.\n\n"))
                return(FALSE)
            } # end if  
            message(paste0(varExistMsg, varValueBad, addInfoRestartMsg))
            return(TRUE)
        } else { # so fileValue and intendedValue are the same, all is good, nothing to do 
            # We still have the possible restart-issue.  # but we check this at the update functions
            message(paste0(varExistMsg, varValueGood))
            return(TRUE)    
        } # end else if (fileValue != intendedValue)
    } # endif (length(ind) == 1)
    stop("We should never get here.")
} # EOF

performSetup_sys <- function(userLoc, taPaList, taPaSettingsPath_test=NULL, sysHome_R_test=NULL) {
#   aa <- taPaList
#       taPaName <- aa$taPaName
#       taPaSH <- aa$taPaSH
#       taPaObj <- aa$taPaObj
#       tmplName <- aa$tmplName
#       setFiName <- aa$setFiName
    ###
    ok <- performSetup_create_taPaSH(userLoc, taPaList)
    if (!ok){suStop()}
    ok <- performSetup_copySettingsFile(userLoc, taPaList, taPaSettingsPath_test)
    if (!ok){suStop()}  
    ok <- performSetup_checkCreateModRenv(userLoc, taPaList, sysHome_R_test)
    if (!ok) {suStop()}
    # if we came up to here, everything was ok. 
    return(TRUE)
} # EOF

#' @title Perform Setup
#' @description Perform the required setup to enable the target package to make 
#' use of the functionality of package 'uniset'. Only has to be called once 
#' by the user of the target package.
#' @details This function is intended to be called from \strong{within} the 
#' target package by the user of the target package. Only has to be called once 
#' to initiate the system, i.e. to
#' \itemize{
#' \item Define the folder where the settings.R file will be located,
#' \item Copy the settings.R file into this folder, and
#' \item Create a corresponding entry in the .Renviron file (or create the 
#' .Renviron file if does not exist).
#' }
#' This setup has to be done manually (but only once!) by the user of the target 
#' package. However, if called repeatedly, it enables the user of the target 
#' package to conveniently change the settings-home directory and its 
#' corresponding variable in the .Renviron file. In that case, a factory-fresh 
#' version of the settings.R file will be copied into the new settings-home 
#' directory. For the user-defined values in the 'old' settings.R file not 
#' to be lost, the user then has to manually move / copy the settings from 
#' the old location to the new one.
#' @param where Character length one, holding the path to the location where the 
#' folder containing the settings.R file should be located. Defaults to 'NULL'. 
#' If left at the default 'NULL', the location should be selectable interactively.
#' @inheritParams uniset_updateSettings
#' @section Important: This function is meant to be called from within the 
#' target package.
#' @return Called for its side effects, i.e. to initiate the dynamic settings file 
#' system (see Details.) Returns an (invisible) character length one holding the 
#' path to the settings-home directory.
#' @examples{
#' \dontrun{
#' # to be called from within the target package
#' uniset_setup(where, uniset_handover)
#' }
#' }
#' @export
uniset_setup <- function(where=NULL, uniset_handover) {
    check_target_package_handover(uniset_handover)
    where <- getCheckPathToFolder(where, what="where")
    taPaList <- getUnisHandoverVariables(uniset_handover)
    ok <- performSetup_sys(where, taPaList)
    if (ok) {
        message(paste("\nSetup successful\n"))
    } # end if
    return(invisible(where))
} # EOF

#' @title Update Settings of Target Package
#' @description Manually read in the settings-file in the target package settings
#' home directory as specified in the .Renviron file.
#' @param uniset_handover List length two, containing two elements:
#' 1. `pkgname`: The name of the target package.
#' 1. `funcname`: The name of the function in the target package handing over
#' the required values. See examples at \code{\link{uniset}}.
#' @param setupFunc Character length one. The name of the function in the target 
#' package performing the setup, i.e. the name of the function that is 
#' containing the \code{uniset} function \code{\link{uniset_setup}}. Defaults to 
#' 'NULL'; has to be changed.
#' @param silent Logical. If a confirmation should be printed. Defaults
#' to 'FALSE'
#' @section Important: This function is meant to be called from within the 
#' target package.
#' @return This function is called for its side effects, i.e to 
#' manually update / (re-)source the settings file. Returns (invisible)
#' 'FALSE' if the update was unsuccessful, otherwise an (invisible) list with
#' the settings sourced from the settings.R file. 
#' @section Note: Please refer to \code{\link{uniset}} for a link to examples 
#' and a real-world demo.
#' @examples{
#' \dontrun{
#' # to be called from within the target package
#' uniset_updateSettings(uniset_handover, "dogPack_demo_setup")
#' }
#' }
#' @export
uniset_updateSettings <- function(uniset_handover, setupFunc=NULL, silent=FALSE) {
    check_target_package_handover(uniset_handover)
    taPaList <- getUnisHandoverVariables(uniset_handover)
    taPaName <- taPaList$taPaName
    checkSetupFuncName(setupFunc, taPaName) # is stopping when setupFunc is left at NULL
    ######
    ok <- checkSettings(taPaList, setupFunc) # makes sure that we have the latest version of the settings.r file in the settings-home directory defined in .Renviron
    if (ok) {
        stn_obj <- sourceSettingsToObject(taPaList)
        if (!silent) {
            cat(paste(taPaName, "settings updated\n"))
        } # end if
        return(invisible(stn_obj))
    } else { # so if the settings check was not ok
        return(invisible(FALSE))
    } # end else
} # EOF

#' @title Automatically update Settings
#' @description Use this function within your code to automatically update the
#' settings from the users settings file
#' @details If 'autoUpdateSettings' in the local settings.R file is left at 'TRUE', 
#' the settings will be checked resp. updated automatically every time a function 
#' in the target package is calling \code{\link{uniset_autoUpS}}. 
#' @inheritParams uniset_updateSettings
#' @section Important: This function is meant to be called from within the 
#' target package.
#' @return Is primarily called for its side effects, i.e to automatically 
#' update / (re-)source the settings file. Returns (invisible) 'FALSE' if the
#' the update was unsuccessful, otherwise an (invisible) list with the settings
#' sourced from the settings.R file. 
#' @section Note: Please refer to \code{\link{uniset}} for a link to examples 
#' and a real-world demo.
#' @examples{
#' \dontrun{
#' # to be called from within the target package
#' uniset_autoUpS(uniset_handover, "dogPack_demo_setup")
#' }
#' }
#' @export
uniset_autoUpS <- function(uniset_handover, setupFunc=NULL) { # stops if somethings goes wrong
    check_target_package_handover(uniset_handover)
    taPaList <- getUnisHandoverVariables(uniset_handover)
        taPaObj <- taPaList$taPaObj
        taPaName <- taPaList$taPaName
    checkSetupFuncName(setupFunc, taPaName)
    ####
    stn <- sourceSettingsToObject(taPaList)
    if (is.null(stn)) {
        do_autoUpS <- TRUE
    } else {
        do_autoUpS <- stn$gen_autoUpdateSettings
        if (!is.logical(do_autoUpS)) {
        	do_autoUpS <- TRUE
        } # end if
    } # end else  
    #
    if (do_autoUpS) {
        return(invisible(uniset_updateSettings(uniset_handover, setupFunc,
            silent=TRUE))) # returns List or FALSE
    } else {
        return(invisible(FALSE))
    } # end else
} # EOF


#' @title Get Settings Object
#' description Source the list holding the key=value pairs from the settings.R
#' file. 
#' @inheritParams uniset_updateSettings
#' @section Important: This function is meant to be called from within the 
#' target package.
#' @return A list holding the key=value pairs from the settings.R file on
#' on success, NULL if sourcing the file was not successful. 
#' @examples{
#' \dontrun{
#' # to be called from within the target package
#' uniset_getstn(uniset_handover)
#' }
#' }
#' @export
uniset_getstn <- function(uniset_handover) {
    check_target_package_handover(uniset_handover)
    taPaList <- getUnisHandoverVariables(uniset_handover)
	stn <- sourceSettingsToObject(taPaList)
	if (is.null(stn)) {
	    return(NULL)
	} else {
        return(stn)
	}
} # EOF
