# Pandora 24.02.0

## Updates
- catch issue if resources are missing in tests
- automate documentation

# Pandora 23.12.0

## Features
- new parameters for the function `getRepositories()` to select and rename the columns of the result
- vignette and documentation were added to the package

# Pandora 23.11.2

## Features
- new function `getData()` was added, which enables data retrieval
- the function `dataOption()` returns a list of options for `utils::read.csv()` or 
  `openxlsx::read.xlsx()` or `readxl::read_excel`, respectively, that can be passed to `getData()`

# Pandora 23.11.1

## Updates
- export of `callAPI()` function, update of documentation

# Pandora 23.11.0

## Features
The main function that facilitate the data retrieval and aggregation from the API is:

 - `getData()` (_under development_)

The following are its sub-functions contained in this package: 

  - `getNetworks()` returns a data.frame containing available networks (groups in CKAN terminology)
    - optional filtering of names for a given string
  - `getRepositories()` returns a data.frame containing available repositories 
    - all or those within a specific network
    - optional filtering of meta information for a given string
  - `getFileTypes()` returns a data.frame containing available file types of a repository
    - all or those within a specific network or within a specific repository
    - optional filtering of meta information for a given string
  - `getResources()` returns a data.frame containing available resources within a repository
    - all or filtered by file type or those within a specific network or within a specific repository
    - optional filtering of meta information for a given string
