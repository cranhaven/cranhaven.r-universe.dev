
message()

# global teardown
message("Cleaning test indices...")
remove_all_indices() 

# remove function
message("Removing helper functions...")
eget <- NULL
remove_all_indices <- NULL
count_nb_lines <- NULL
change_names <- NULL
produce_str <- NULL
push_test_datasets <- NULL

# remove variable
message("Removing helper variables...")
ds <- NULL
ds_modified <- NULL
s <- NULL
st <- NULL
d <- NULL
all_features <- NULL

single_index_name <- NULL
multiple_indice_names <- NULL
cpt_loop <- NULL
temp_filepath <- NULL

env_es_endpoint <- NULL
env_es_port <- NULL
env_es_username <- NULL
env_es_password <- NULL

es_endpoint <- NULL
es_port <- NULL
es_username <- NULL
es_password <- NULL

args <- NULL
kc <- NULL