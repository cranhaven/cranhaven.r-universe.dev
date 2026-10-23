This folder contains a fake database for testing. It does not contain any real data, but uses the structure of the real database.
It contains two subfolders:
db - simulated database (unpacked)
repo - simulated repository (zipped)


The zipped repo can be created from the unpacked database by running the script ButtR/R_prep/zip_dbs.R, but first edit the script to set the variable FOR_TESTING_ONLY <- TRUE