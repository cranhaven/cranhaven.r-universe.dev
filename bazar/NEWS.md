# bazar 1.0.11 (2019-03-15)

* 'get_all_pkgs()' no longer explores the whole list of installed packages by default. 
The default is now to search within attached packages only. 


# bazar 1.0.10 (2018-10-03)

* Add 'as.na()' S3 method, which transforms an object into an 'NA' object of 
the same type. 


# bazar 1.0.9 (2018-07-31)

* Add 'get_vars()'. This function extracts variables from a formula. 


# bazar 1.0.7 (2017-12-17)

* Add 'as.fun()' for several R models, including 'lm', 'rpart', 'gam', 
'randomForest'. 


# bazar 1.0.6

* Ternary operator 'condition %?% true %:% false' is added. 
* Function 'find_pkgs()' renamed into 'get_all_pkgs()' and corrected. 
* Add function 'get_all_funs()'. 
* Function 'normalize()' is now documented.  


# bazar 1.0.5

* Add functions 'top()' and 'bot()'.  
* Add function 'almost.unique()'.  


# bazar 1.0.4

* 'as.fun.character()' now provides the identified package as attribute. 


# bazar 1.0.3

* 'as.fun()' gains additional methods (for atomic vectors and data frames).


# bazar 1.0.2

* Add function 'normalize()'.


# bazar 1.0.1

* Creation of the 'almost.unique()' generic function.


# bazar 1.0.0

* 'as.fun()' gains a 'numeric' S3 method.


# bazar 0.1.9

* Change 'as.function()' into 'as.fun()'.


# bazar 0.1.8

* Create 'as.function.character()'.


# bazar 0.1.7

* There was a bug in is.wholenumber() when 'x' had missing values.


# bazar 0.1.6

* Slight correction in concat().


# bazar 0.1.5

* Slight correction in as.nlist(). 


# bazar 0.1.0

* Creation of the package 'bazar'
