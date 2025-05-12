##########################################################################
######## Settings file for package "XXX_packageName" #####################
##########################################################################


# For the developer of package "XXX_packageName":
# If not already there, move this file called 'XXX_actualSettingsName' into
# the "inst" folder of your package 'XXX_packageName'. (create an "inst"
# folder if it does not exist)
# Consider deleting this paragraph, as the USER of the target package should
# leave this file where it is.

# do NOT change the name of the object holding the list - in this
# case, 'XXX_obj'

#' Use the function 'getstn()' as defined in the package
# 'XXX_packageName' to directly get the list
# 'XXX_obj' (below).

XXX_obj <- list(
	# tag = value, # with a comma !!
		
	## general behavior
	gen_autoUpdateSettings = TRUE, 			## Do not delete this variable (but of course you can change its value)
	
	
		
	## block 1 (describe what this collection of variables is about)
	var1 = "foo",			## add a comment to describe what variables are about
	var2 = TRUE,			## comments on each key make it easier for the user to use the settings file
	var3 = 5,				## you can specify any type of variable
	name = "Henry", 
	anyName = "anyValue", 



	## block 2 (describe what this collection of variables is about)
	key1 = "bar",	
	key2 = 12345,	
	key3 = "theo",
	xxx = FALSE,



	## block 3 (describe what this collection of variables is about)
	abc5 = TRUE,  
	favouriteColor = "blue", 
	leastFavColor = "darkred", 
	abc7 = TRUE, 
	abc8 = FALSE, 



	name_a = "you get the picture",	
	name_b = TRUE, 
	name_c = 999999,
	petterson = "old",
	findus = "cat",
	mouse = "grey", 



	obey = TRUE,
	consume = TRUE,
	strength = 5000,
	
	
	
	######
	last = 0 # do not add anything below that
	## the last one without comma !!
) # end of list called 'XXX_obj'

 
# You can create as many key=value pairs as you want. 

# Key=value pairs have to specified exactly in the format as shown above,
# with a ',' (comma) after the value!
# (as this is nothing but a simple list, and in a list the key=value pairs
# have to be separated by a comma.)
# The keys have to be unique (obviously). 

# Do not change the name of the object holding the list - in
# this case, 'XXX_obj'

# empty lines and comments can be introduced or deleted at the users wish
# it is recommended to place comments, explanations etc. to a specific key
# directly into the line holding the key.
# comments have to be preceeded by a '#' 
