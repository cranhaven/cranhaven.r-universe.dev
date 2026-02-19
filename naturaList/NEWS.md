# naturaList 0.5.2
* remove dependencies on rgdal, rgeos and maptools. 


# naturaList 0.5.1
* it includes the env_grid_filter function. The function select the best classified species occurrence regarding the species identification reliability in the environmental space. 

# naturaList 0.5.0
* grid_filter function was re-coded. The new code is faster than the older version, but it could produce different results from older version. 

# naturaList 0.4.3
* documentation update

# naturaList 0.4.2
* documentation update

# naturaList 0.4.1
* bug solved in the classify_occ function

# naturaList 0.4.0

* some arguments for column names of the occurrence dataset was changed. They are still working, with warning message informing they are deprecated and what is the current argument accepted. 

* bugs solved in `create_spec_df()`:
    * warning message included for specialists names without abbreviation letters
* bug solved in `classify_occ()`:
    * it is possible to use a list of specialists with only 1 row
* Classification level `taxonomy` was renamed to `not_spec_name`    

    
* bug solved in `map_module()`:
    * Before if a point was selected, when `Delete points with click` was activated, the selected points was deleted. Now `Delete points with click` always wait a click to delete a point. 
* new argument `action` in `map_module()`, which allows to choose if the output dataset will contain only the selected occurrences, or if the occurrences output dataset will be flagged about the choices of the user in the application.


# naturaList 0.3.1

* bugs solved in `classify_occ()`. Inclusion of a new dataset `cyathea.br` in the package.

# naturaList 0.3.0

* Added functions `clean_eval()` and `define_env_space`. These functions implements evaluation of cleaning procedures based in classification made by `classify_occ()`. The evaluation is based on the change of convex hull area of species occurrences between cleaned and raw occurrences dataset.
