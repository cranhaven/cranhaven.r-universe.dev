######################################################################
####### Custom-tailored functions for package 'XXX_packageName' ######
######################################################################



# If not already there, move this file called "uniset_functions.R" into the "R"
# folder of your package 'XXX_packageName'

#######################
# The following functions
# checkOnTest() and
# test_getLocalStn()
# are custom-tailored by package 'uniset', and are required for the package
# 'XXX_packageName' to be able to run tests. (Otherwise the non-existent
# system-variable 'XXX_SH' in a remote test-run would be a problem.)

checkOnTest <- function() {
	if (exists("get_settings_from_XXX_packageName_package_root", where = .GlobalEnv)) {
		if (get("get_settings_from_XXX_packageName_package_root", pos = .GlobalEnv)) {
			return(TRUE)
		} else {
			return(FALSE)
		}
	} else {
		return(FALSE)
	}
} # EOF

test_getLocalStn <- function() {
	ptp <- path.package("XXX_packageName")
	# set up the stn object
	if (dir.exists(paste0(ptp, "/inst"))) { # to be able to run tests manually line by line
			stn <- source(paste0(ptp, "/inst/XXX_actualSettingsName"))$value
		} else {
			stn <- source(paste0(ptp, "/XXX_actualSettingsName"))$value
	} # end else
	return(stn)
} # EOF

#######################

# The following functions
# updateSettings(), 
# autoUpS() and
# getstn() 
# are custom-tailored by package 'uniset', and
# intended to be used inside functions defined in
# the package 'XXX_packageName':

# Can be used inside a function of the package 'XXX_packageName' to manually
# update the settings. If silent=FALSE, upon success a message
# will be displayed.
updateSettings <- function(silent=FALSE) {
	if (checkOnTest()) {
		return(test_getLocalStn())
	} # end if
	#
	stn <- uniset::uniset_updateSettings(get("uniset_handover"),
		setupFunc="XXX_setupFuncName", silent)
	return(invisible(stn))
} # EOF



# Can be used inside a function of the package 'XXX_packageName' to
# automatically update the settings. No message will be displayed
# upon success.
autoUpS <- function() {
	if (checkOnTest()) {
		return(test_getLocalStn())
	} # end if
	#
	stn <- uniset::uniset_autoUpS(get("uniset_handover"),
		setupFunc="XXX_setupFuncName")
	return(invisible(stn))
} # EOF



# Can be used inside a function of the package 'XXX_packageName' to
# conveniently get the list holding the settings, i.e. the key=value pairs
# from the file XXX_packageName_settings.R.
getstn <- function() {
	if (checkOnTest()) {
		return(test_getLocalStn())
	} # end if
	#
	stn <- uniset::uniset_getstn(get("uniset_handover"))
	if (is.null(stn)) {
	# gets returned as NULL if the file could not be sourced, that means
	# 'updateSettings()' has not been called yet. Hence, we have to force the
	# manual update here.
		stn <- uniset::uniset_updateSettings(get("uniset_handover"),
			setupFunc="XXX_setupFuncName", silent=TRUE)
	} # end if
	return(invisible(stn))
} # EOF

#######################

# The following function is required to hand over information to package
# "uniset". If you are not using Roxygen, delete the Raxygen-code (starting
# with "#'" and make sure that the function is exported into the namespace of
# package 'XXX_packageName'.


#' @title Handover to Uniset
#' @description Is handing over values to package uniset. These values are
#' required for the correct functioning of the dynamic settings file system
#' provided by \code{\link[uniset]{uniset}}.
#' @export
XXX_packageName_handover_to_uniset <- function() {
	return(list(pkgUniset_UserPackageName = "XXX_packageName",
				pkgUniset_RenvironSettingsHomeName = "XXX_SH",
				pkgUniset_SettingsObjectName = "XXX_obj",
				pkgUniset_SuffixForTemplate = "XXX_template"
				))
} # EOF

#######################
