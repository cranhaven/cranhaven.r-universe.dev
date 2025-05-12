---
title: "NEWS"
output: html_document
---

# easycsv 1.0.2 09/08/2017

* Added `fread_folder()` for faster table reading with data.table::fread and suggests for data.table

# easycsv 1.0.3 10/08/2017

* 'txt' parameter no longer exists, changed to extension and now takes as argument either 'TXT','CSV' or 'BOTH', in lower or upper case, this extends to loadcsv_multi and fread_folder.

# easycsv 1.0.5 19/08/2017

*added Identify.OS and choose_dir, as utility functions.
meaning that even for Mac OSX and Linux users if the local directory in functions is not entered you can choose it interactively through a popup widget window.

# easycsv 1.0.6 30/11/2017

*adde fread_zip which is the fread version of loadcsvfromZIP, also added a prefix option for fread_folder and fread_zip, accepts only strings of length 1.


# easycsv 1.0.7 23/01/2018

* fixed choose_dir for linux, had a problem with capturing returned value

# easycsv 1.0.8 27/04/2018

* temporary fix to `fread_folder` to adapt to the changes in `data.table::fread`. 
