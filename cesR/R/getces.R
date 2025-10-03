#' Create a dataframe object for a CES survey.
#'
#' @description
#' `get_ces()` creates a dataframe object for a requested Canadian Election Study
#' survey using an associated survey code to call and download
#' the survey dataset. On creation of the data object, prints out the associated citation for use with
#' the requested dataset and a link to the original location of the data file.
#'
#' @param srvy A CES survey code call. See *Survey Code Calls* below.
#' `srvy` value must be a character string.
#' @param pos Environment assignment. Defaults to 1, which is an assignment to the global environment.
#'
#' @details
#'
#' ## Datasets
#' Datasets are loaded using .dta, .sav, or .tab file types. See *File Types* below for a list of included
#' CES datasets and their file type.
#' To quickly convert a dataset's values to factor type use
#' `labelled::to_factor()` on the dataset.
#'
#'
#' ## Survey Code Calls
#' * `ces2019_web` calls 2019 CES online survey dataset.
#' * `ces2019_phone` calls 2019 CES phone survey dataset.
#' * `ces2015_web` calls 2015 CES online survey dataset.
#' * `ces2015_phone` calls 2015 CES phone survey dataset.
#' * `ces2015_combo` calls 2015 CES combined (online/phone) dataset.
#' * `ces2011` calls 2011 CES survey dataset.
#' * `ces2008` calls 2008 CES survey dataset.
#' * `ces2004` calls 2004 CES survey dataset.
#' * `ces0411` calls 2004-2011 CES survey dataset.
#' * `ces0406` calls 2004-2006 CES survey dataset.
#' * `ces2000` calls 2000 CES survey dataset.
#' * `ces1997` calls 1997 CES survey dataset.
#' * `ces1993` calls 1993 CES survey dataset.
#' * `ces1988` calls 1988 CES survey dataset.
#' * `ces1984` calls 1984 CES survey dataset.
#' * `ces1974` calls 1974 CES survey dataset.
#' * `ces7480` calls 1974-1980 CES survey dataset.
#' * `ces72_jnjl` calls 1972 June-July CES survey dataset.
#' * `ces72_sep` calls 1972 September CES survey dataset.
#' * `ces72_nov` calls 1972 November CES survey dataset.
#' * `ces1968` calls 1968 CES survey dataset.
#' * `ces1965` calls 1965 CES survey dataset.
#'
#'
#' ## File Types
#' * CES 2019 online survey. Loaded as a .dta file type.
#' * CES 2019 phone survey. Loaded as a .tab file type.
#' * CES 2015 online survey. Loaded as a .dta file type.
#' * CES 2015 phone survey. Loaded as a .dta file type.
#' * CES 2015 combined survey. Loaded as a .dta file type.
#' * CES 2011 survey. Loaded as a .dta file type.
#' * CES 2008 survey. Loaded as a .sav file type.
#' * CES 2004 survey. Loaded as a .sav file type.
#' * CES 2004-2011 surveys. Loaded as a .dta file type.
#' * CES 2004-2006 surveys. Loaded as a .sav file type.
#' * CES 2000 survey. Loaded as a .sav file type.
#' * CES 1997 survey. Loaded as a. sav file type.
#' * CES 1993 survey. Loaded as a .sav file type.
#' * CES 1988 survey. Loaded as a .sav file type.
#' * CES 1974 survey. Loaded as a .sav file type.
#' * CES 1974-1980 surveys. Loaded as a .sav file type.
#' * CES 1972 June-July surveys. Loaded as a .sav file type.
#' * CES 1972 September survey. Loaded as a .sav file type.
#' * CES 1972 November survey. Loaded as a .sav file type.
#' * CES 1968 survey. Loaded as a .sav file type.
#' * CES 1965 survey. Loaded as a .sav file type.
#'
#' ## Incorrect/Repeated Code Calls
#' Incorrect (a non-existent survey code) will stop the function process and return an associated error message.
#' Repeated code calls will load in the raw version of the requested table.
#'
#' ## Extra Notes
#' Due to the naming of the columns in the 1965 and 1968 datasets it is recommended
#' to download the associated codebook for the requested dataset.
#'
#' @return The called \code{srvy} at the designated environment position \code{pos}.
#' Default environment position is set to be the global environment.
#'
#' @examples
#' \dontrun{
#' # call the 2019 CES online survey
#' get_ces("ces2019_web")
#'
#' # convert variables to factor
#' labelled::to_factor(ces2019_web)
#'
#' # preview dataset
#' head(ces2019_web)
#'}
#' @seealso `get_cescodes()` function help.



#library(haven)
#library(readr)
#library(utils)


#'@export
# function to call in CES survey from github repository
# code for the first section of the function is commented with how the function works,
# all following sections work in the same manner.

# 'get_ces' function, uses one variable 'srvy'
get_ces <- function(srvy, pos = 1){
  # if 'srvy' is in 'ces_codes' vector
  if(srvy %in% ces_codes){
    # if 'srvy' is equal to 'ces2019_web'
    if(srvy == "ces2019_web"){
      # create temporary file name holder
      hldr <- tempfile(fileext = ".dta")
      # if the file does not exist
      if(!file.exists(hldr)){
        # assign download url
        cesfile <- "https://dataverse.harvard.edu/api/access/datafile/:persistentId?persistentId=doi:10.7910/DVN/DUS88V/RZFNOV"
        # download the file from the url and assign file name from holder
        utils::download.file(cesfile, hldr, quiet = F, mode = "wb")
        # assign the data file to a globally available variable
        assign("ces2019_web", haven::read_dta(hldr, encoding = "latin1"), envir = as.environment(pos))
        # remove the temporary downloaded data file
        unlink(hldr, recursive = T)
        # print citation and link
        message(ref2019web)
      }
    }
    else if(srvy == "ces2019_phone"){
      hldr <- tempfile(fileext = ".tab")
      if(!file.exists(hldr)){
        cesfile <- "https://dataverse.harvard.edu/api/access/datafile/:persistentId?persistentId=doi:10.7910/DVN/8RHLG1/DW4GZZ"
        utils::download.file(cesfile, hldr, quiet = F, mode = "wb")
        assign("ces2019_phone", readr::read_tsv(hldr, show_col_types = F), envir = as.environment(pos))
        unlink(hldr, recursive = T)
        message(ref2019phone)
      }
    }
    else if(srvy == "ces2015_web"){
      hldr <- tempfile(fileext = ".zip")
      fldr <- paste0(tempdir(), "\\ces2015_web")
      if(!file.exists(hldr)){
        cesfile <- "https://ces-eec.sites.olt.ubc.ca/files/2018/07/CES15_CPSPES_Web_SSI-Full-Stata-14.zip"
        utils::download.file(cesfile, hldr, quiet = F)
        utils::unzip(hldr, exdir = fldr)
        datafile <- file.path(fldr, "CES15_CPS+PES_Web_SSI Full Stata 14.dta")
        assign("ces2015_web", haven::read_dta(hldr), envir = as.environment(pos))
        unlink(hldr, recursive = T)
        unlink(fldr, recursive = T)
        message(ref2015web)
      }
    }
    else if(srvy == "ces2015_phone"){
      hldr <- tempfile(fileext = ".zip")
      fldr <- paste0(tempdir(), "\\ces2015_phone")
      if(!file.exists(hldr)){
        cesfile <- "https://ces-eec.sites.olt.ubc.ca/files/2018/08/CES2015-phone-Stata.zip"
        utils::download.file(cesfile, hldr, quiet = F)
        utils::unzip(hldr, exdir = fldr)
        datafile <- file.path(fldr, "CES2015_CPS-PES-MBS_complete-v2.dta")
        assign("ces2015_phone", haven::read_dta(datafile, encoding = "latin1"), envir = as.environment(pos))
        unlink(hldr, recursive = T)
        unlink(fldr, recursive = T)
        message(ref2015phone)
      }
    }
    else if(srvy == "ces2015_combo"){
      hldr <- tempfile(fileext = ".zip")
      fldr <- paste0(tempdir(), "\\ces2015_combo")
      if(!file.exists(hldr)){
        cesfile <- "https://ces-eec.sites.olt.ubc.ca/files/2017/04/CES2015_Combined_Stata14.zip"
        utils::download.file(cesfile, hldr, quiet = F)
        utils::unzip(hldr, exdir = fldr)
        datafile <- file.path(fldr, "CES2015_Combined_Stata14.dta")
        assign("ces2015_combo", haven::read_dta(hldr), envir = as.environment(pos))
        unlink(hldr, recursive = T)
        unlink(fldr, recursive = T)
        message(ref2015combo)
      }
    }
    else if(srvy == "ces2011"){
      hldr <- tempfile(fileext = ".zip")
      fldr <- paste0(tempdir(), "\\ces2011")
      if(!file.exists(hldr)){
        cesfile <- "https://ces-eec.sites.olt.ubc.ca/files/2014/07/CES2011-final-1.zip"
        utils::download.file(cesfile, hldr, quiet = F)
        utils::unzip(hldr, exdir = fldr)
        datafile <- file.path(fldr, "CPS&PES&MBS&WEB_2011_final.dta")
        assign("ces2011", haven::read_dta(datafile), envir = as.environment(pos))
        unlink(hldr, recursive = T)
        unlink(fldr, recursive = T)
        message(ref2011)
      }
    }
    else if(srvy == "ces2008"){
      hldr <- tempfile(fileext = ".zip")
      fldr <- paste0(tempdir(), "\\ces2008")
      if(!file.exists(hldr)){
        cesfile <- "https://raw.github.com/hodgettsp/ces_data/master/extdata/CES-E-2008.zip"
        utils::download.file(cesfile, hldr, quiet = F)
        utils::unzip(hldr, exdir = fldr)
        datafile <- file.path(fldr, "CES2015_Combined_Stata14.dta")
        assign("ces2008", haven::read_sav(hldr), envir = as.environment(pos))
        unlink(hldr, recursive = T)
        unlink(fldr, recursive = T)
        message(ref2008)
      }
    }
    else if(srvy == "ces2004"){
      hldr <- tempfile(fileext = ".zip")
      fldr <- paste0(tempdir(), "\\ces2004")
      if(!file.exists(hldr)){
        cesfile <- "https://raw.github.com/hodgettsp/ces_data/master/extdata/CES-E-2004.zip"
        utils::download.file(cesfile, hldr, quiet = F)
        utils::unzip(hldr, exdir = fldr)
        datafile <- file.path(fldr, "CES-E-2004_F1.sav")
        assign("ces2004", haven::read_sav(hldr), envir = as.environment(pos))
        unlink(hldr, recursive = T)
        unlink(fldr, recursive = T)
        message(ref2004)
      }
    }
    else if(srvy == "ces0411"){
      hldr <- tempfile(fileext = ".zip")
      fldr <- paste0(tempdir(), "\\ces0411")
      if(!file.exists(hldr)){
        cesfile <- "https://ces-eec.sites.olt.ubc.ca/files/2014/07/CES_04060811_final_without-geo-data.zip"
        utils::download.file(cesfile, hldr, quiet = F)
        utils::unzip(hldr, exdir = fldr)
        datafile <- file.path(fldr, "CES_04060811_final_without-geo-data.dta")
        assign("ces0411", haven::read_dta(hldr, encoding = "latin1"), envir = as.environment(pos))
        unlink(hldr, recursive = T)
        unlink(fldr, recursive = T)
        message(ref0411)
      }
    }
    else if(srvy == "ces0406"){
      hldr <- tempfile(fileext = ".zip")
      fldr <- paste0(tempdir(), "\\ces0406")
      if(!file.exists(hldr)){
        cesfile <- "https://raw.github.com/hodgettsp/ces_data/master/extdata/CES-E-2004-2006.zip"
        utils::download.file(cesfile, hldr, quiet = F)
        utils::unzip(hldr, exdir = fldr)
        datafile <- file.path(fldr, "CES-E-2004-2006_F1.sav")
        assign("ces0406", haven::read_sav(hldr), envir = as.environment(pos))
        unlink(hldr, recursive = T)
        unlink(fldr, recursive = T)
        message(ref0406)
      }
    }
    else if(srvy == "ces2000"){
      hldr <- tempfile(fileext = ".zip")
      fldr <- paste0(tempdir(), "\\ces2000")
      if(!file.exists(hldr)){
        cesfile <- "https://raw.github.com/hodgettsp/ces_data/master/extdata/CES-E-2000.zip"
        utils::download.file(cesfile, hldr, quiet = F)
        utils::unzip(hldr, exdir = fldr)
        datafile <- file.path(fldr, "CES-E-2000_F1.sav")
        assign("ces2000", haven::read_sav(hldr), envir = as.environment(pos))
        unlink(hldr, recursive = T)
        unlink(fldr, recursive = T)
        message(ref2000)
      }
    }
    else if(srvy == "ces1997"){
      hldr <- tempfile(fileext = ".zip")
      fldr <- paste0(tempdir(), "\\ces1997")
      if(!file.exists(hldr)){
        cesfile <- "https://raw.github.com/hodgettsp/ces_data/master/extdata/CES-E-1997.zip"
        utils::download.file(cesfile, hldr, quiet = F)
        utils::unzip(hldr, exdir = fldr)
        datafile <- file.path(fldr, "CES-E-1997_F1.sav")
        assign("ces1997", haven::read_sav(hldr), envir = as.environment(pos))
        unlink(hldr, recursive = T)
        unlink(fldr, recursive = T)
        message(ref1997)
      }
    }
    else if(srvy == "ces1993"){
      hldr <- tempfile(fileext = ".zip")
      fldr <- paste0(tempdir(), "\\ces1993")
      if(!file.exists(hldr)){
        cesfile <- "https://raw.github.com/hodgettsp/ces_data/master/extdata/CES-E-1993.zip"
        utils::download.file(cesfile, hldr, quiet = F)
        utils::unzip(hldr, exdir = fldr)
        datafile <- file.path(fldr, "CES-E-1993_F1.sav")
        assign("ces1993", haven::read_sav(hldr), envir = as.environment(pos))
        unlink(hldr, recursive = T)
        unlink(fldr, recursive = T)
        message(ref1993)
      }
    }
    else if(srvy == "ces1988"){
      hldr <- tempfile(fileext = ".zip")
      fldr <- paste0(tempdir(), "\\ces1988")
      if(!file.exists(hldr)){
        cesfile <- "https://raw.github.com/hodgettsp/ces_data/master/extdata/CES-E-1988.zip"
        utils::download.file(cesfile, hldr, quiet = F)
        utils::unzip(hldr, exdir = fldr)
        datafile <- file.path(fldr, "CES-E-1988_F1.sav")
        assign("ces1988", haven::read_sav(hldr), envir = as.environment(pos))
        unlink(hldr, recursive = T)
        unlink(fldr, recursive = T)
        message(ref1988)
      }
    }
    else if(srvy == "ces1984"){
      hldr <- tempfile(fileext = ".zip")
      fldr <- paste0(tempdir(), "\\ces1984")
      if(!file.exists(hldr)){
        cesfile <- "https://raw.github.com/hodgettsp/ces_data/master/extdata/CES-E-1984.zip"
        utils::download.file(cesfile, hldr, quiet = F)
        utils::unzip(hldr, exdir = fldr)
        datafile <- file.path(fldr, "CES-E-1984_F1.sav")
        assign("ces1984", haven::read_sav(hldr), envir = as.environment(pos))
        unlink(hldr, recursive = T)
        unlink(fldr, recursive = T)
        message(ref1984)
      }
    }
    else if(srvy == "ces1974"){
      hldr <- tempfile(fileext = ".zip")
      fldr <- paste0(tempdir(), "\\ces1974")
      if(!file.exists(hldr)){
        cesfile <- "https://raw.github.com/hodgettsp/ces_data/master/extdata/CES-E-1974.zip"
        utils::download.file(cesfile, hldr, quiet = F)
        utils::unzip(hldr, exdir = fldr)
        datafile <- file.path(fldr, "CES-E-1974_F1.sav")
        assign("ces1974", haven::read_sav(hldr), envir = as.environment(pos))
        unlink(hldr, recursive = T)
        unlink(fldr, recursive = T)
        message(ref1974)
      }
    }
    else if(srvy == "ces7480"){
      hldr <- tempfile(fileext = ".zip")
      fldr <- paste0(tempdir(), "\\ces7480")
      if(!file.exists(hldr)){
        cesfile <- "https://raw.github.com/hodgettsp/ces_data/master/extdata/CES-E-1974-1980.zip"
        utils::download.file(cesfile, hldr, quiet = F)
        utils::unzip(hldr, exdir = fldr)
        datafile <- file.path(fldr, "CES-E-1974-1980_F1.sav")
        assign("ces7480", haven::read_sav(hldr), as.environment(pos))
        unlink(hldr, recursive = T)
        unlink(fldr, recursive = T)
        message(ref7480)
      }
    }
    else if(srvy == "ces72_jnjl"){
      hldr <- tempfile(fileext = ".zip")
      fldr <- paste0(tempdir(), "\\ces72jnjl")
      if(!file.exists(hldr)){
        cesfile <- "https://raw.github.com/hodgettsp/ces_data/master/extdata/CES-E-1972-jun-july.zip"
        utils::download.file(cesfile, hldr, quiet = F)
        utils::unzip(hldr, exdir = fldr)
        datafile <- file.path(fldr, "CES-E-1972-jun-july_F1.sav")
        assign("ces72_jnjl", haven::read_sav(hldr), as.environment(pos))
        unlink(hldr, recursive = T)
        unlink(fldr, recursive = T)
        message(ref72jnjl)
      }
    }
    else if(srvy == "ces72_sep"){
      hldr <- tempfile(fileext = ".zip")
      fldr <- paste0(tempdir(), "\\ces72sep")
      if(!file.exists(hldr)){
        cesfile <- "https://raw.github.com/hodgettsp/ces_data/master/extdata/CES-E-1972-sept.zip"
        utils::download.file(cesfile, hldr, quiet = F)
        utils::unzip(hldr, exdir = fldr)
        datafile <- file.path(fldr, "CES-E-1972-sept_F1.sav")
        assign("ces72_sep", haven::read_sav(hldr), as.environment(pos))
        unlink(hldr, recursive = T)
        unlink(fldr, recursive = T)
        message(ref72sep)
      }
    }
    else if(srvy == "ces72_nov"){
      hldr <- tempfile(fileext = ".zip")
      fldr <- paste0(tempdir(), "\\ces72nov")
      if(!file.exists(hldr)){
        cesfile <- "https://raw.github.com/hodgettsp/ces_data/master/extdata/CES-E-1972-nov.zip"
        utils::download.file(cesfile, hldr, quiet = F)
        utils::unzip(hldr, exdir = fldr)
        datafile <- file.path(fldr, "CES-E-1972-nov_F1.sav")
        assign("ces72_nov", haven::read_sav(hldr), as.environment(pos))
        unlink(hldr, recursive = T)
        unlink(fldr, recursive = T)
        message(ref72nov)
      }
    }
    else if(srvy == "ces1968"){
      hldr <- tempfile(fileext = ".zip")
      fldr <- paste0(tempdir(), "\\ces1968")
      if(!file.exists(hldr)){
        cesfile <- "https://raw.github.com/hodgettsp/ces_data/master/extdata/CES-E-1968.zip"
        utils::download.file(cesfile, hldr, quiet = F)
        utils::unzip(hldr, exdir = fldr)
        datafile <- file.path(fldr, "CES-E-1968_F1.sav")
        assign("ces1968", haven::read_sav(hldr), envir = as.environment(pos))
        unlink(hldr, recursive = T)
        unlink(fldr, recursive = T)
        message(ref1968)
        message("\n\nMESSAGE: It is recommended to download the codebook for this dataset to better understand the column names.")
      }
    }
    else if(srvy == "ces1965"){
      hldr <- tempfile(fileext = ".zip")
      fldr <- paste0(tempdir(), "\\ces1965")
      if(!file.exists(hldr)){
        cesfile <- "https://raw.github.com/hodgettsp/ces_data/master/extdata/CES-E-1965.zip"
        utils::download.file(cesfile, hldr, quiet = F)
        utils::unzip(hldr, exdir = fldr)
        datafile <- file.path(fldr, "CES-E-1965_F1.sav")
        assign("ces1965", haven::read_sav(hldr), envir = as.environment(pos))
        unlink(hldr, recursive = T)
        unlink(fldr, recursive = T)
        message(ref1965)
        message("\n\nIt is recommended to download the codebook for this dataset to better understand the column names.")
      }
    }
  }
  else{
    # if the provided code is not in the 'ces_codes' vector then stop process and print this message
    stop("Incorrect CES dataset code provided. Use function get_cescodes() for a printout of useable code calls.")
  }
}

#### THE FOLLOWING ARE THE PRINT STATEMENTS FOR THE CITATIONS AND A VECTOR OF THE SURVEY CALL NAMES.

# citations for print calls
ref2019web <- "TO CITE THIS SURVEY FILE:\n
- Stephenson, Laura B; Harell, Allison; Rubenson, Daniel; Loewen, Peter John, 2020, '2019 Canadian Election Study - Online Survey', https://doi.org/10.7910/DVN/DUS88V, Harvard Dataverse, V1\n
- Stephenson, Laura, Allison Harrel, Daniel Rubenson and Peter Loewen. Forthcoming. 'Measuring Preferences and Behaviour in the 2019 Canadian Election Study,' Canadian Journal of Political Science.\n
LINK: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/DUS88V"

ref2019phone <- "TO CITE THIS SURVEY FILE:\n
- Stephenson, Laura B; Harell, Allison; Rubenson, Daniel; Loewen, Peter John, 2020, '2019 Canadian Election Study - Phone Survey', https://doi.org/10.7910/DVN/8RHLG1, Harvard Dataverse, V1, UNF:6:eyR28qaoYlHj9qwPWZmmVQ== [fileUNF]\n
- Stephenson, Laura, Allison Harrel, Daniel Rubenson and Peter Loewen. Forthcoming. 'Measuring Preferences and Behaviour in the 2019 Canadian Election Study,' Canadian Journal of Political Science.\n
LINK: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/8RHLG1"

ref2015web <- "TO CITE THIS SURVEY FILE: Fournier, Patrick, Fred Cutler, Stuart Soroka and Dietlind Stolle. 2015. The 2015 Canadian Election Study. [dataset]\n
LINK: https://ces-eec.arts.ubc.ca/english-section/surveys/"

ref2015phone <- "TO CITE THIS SURVEY FILE: Fournier, Patrick, Fred Cutler, Stuart Soroka and Dietlind Stolle. 2015. The 2015 Canadian Election Study. [dataset]\n
LINK:https://ces-eec.arts.ubc.ca/english-section/surveys/"

ref2015combo <- "TO CITE THIS SURVEY FILE: Fournier, Patrick, Fred Cutler, Stuart Soroka and Dietlind Stolle. 2015. The 2015 Canadian Election Study. [dataset]\n
LINK: https://ces-eec.arts.ubc.ca/english-section/surveys/"

ref2011 <- "TO CITE THIS SURVEY FILE: Fournier, Patrick, Fred Cutler, Stuart Soroka and Dietlind Stolle. 2011. The 2011 Canadian Election Study. [dataset]\n
LINK: https://ces-eec.arts.ubc.ca/english-section/surveys/"


ref2008 <- "TO CITE THIS SURVEY FILE: Gidengil, E, Everitt, J, Fournier, P and Nevitte, N. 2009. The 2008 Canadian Election Study [dataset]. Toronto, Ontario, Canada: Institute for Social Research [producer and distributor].\n
LINK: http://odesi2.scholarsportal.info/webview/index.jsp?v=2&previousmode=table&analysismode=table&study=http%3A%2F%2F142.150.190.128%3A80%2Fobj%2FfStudy%2FCES-E-2008&mode=documentation&top=yes"


ref2004 <- "TO CITE THIS SURVEY FILE: Blais, A, Everitt, J, Fournier, P, Gidengil, E and Nevitte, N. 2005. The 2004 Canadian Election Study [dataset]. Toronto, Ontario, Canada: Institute for Social Research [producer and distributor].\n
LINK: http://odesi2.scholarsportal.info/webview/index.jsp?v=2&submode=abstract&study=http%3A%2F%2F142.150.190.128%3A80%2Fobj%2FfStudy%2FCES-E-2004&mode=documentation&top=yes"


ref0411 <- "TO CITE THIS SURVEY FILE: Fournier, P, Stolle, D, Soroka, S, Cutler, F, Blais, A, Everitt, J, Gidengil, E and Nevitte, N. 2011. The 2004-2011 Merged Canadian Election Study [dataset]. Toronto, Ontario, Canada: Institute for Social Research [producer and distributor].\n
LINK: https://ces-eec.arts.ubc.ca/english-section/surveys/"


ref0406 <- "TO CITE THIS SURVEY FILE: Blais, A, Everitt, J, Fournier, P and Nevitte, N. 2011. The 2011 Canadian Election Study [dataset]. Toronto, Ontario, Canada: Institute for Social Research [producer and distributor].\n
LINK: http://odesi2.scholarsportal.info/webview/index.jsp?v=2&submode=abstract&study=http%3A%2F%2F142.150.190.128%3A80%2Fobj%2FfStudy%2FCES-E-2004-2006&mode=documentation&top=yes"


ref2000 <- "TO CITE THIS SURVEY FILE: Blais, A, Gidengil, E, Nadeau, R and Nevitte, N. 2001. The 2000 Canadian Election Study [dataset]. Toronto, Ontario, Canada: Institute for Social Research [producer and distributor].\n
LINK: http://odesi2.scholarsportal.info/webview/index.jsp?v=2&submode=abstract&study=http%3A%2F%2F142.150.190.128%3A80%2Fobj%2FfStudy%2FCES-E-2000&mode=documentation&top=yes"


ref1997 <- "TO CITE THIS SURVEY FILE: Blais, A, Gidengil, E, Nadeau, R and Nevitte, N. 1998. The 1997 Canadian Election Study [dataset]. Toronto, Ontario, Canada: Institute for Social Research [producer and distributor].\n
LINK: http://odesi2.scholarsportal.info/webview/index.jsp?v=2&submode=abstract&study=http%3A%2F%2F142.150.190.128%3A80%2Fobj%2FfStudy%2FCES-E-1997&mode=documentation&top=yes"


ref1993 <- "TO CITE THIS SURVEY FILE: Blais, A, Brady, H, Gidengil, E, Johnston, R and Nevitte, N. 1994. The 1993 Canadian Election Study [dataset]. Toronto, Ontario, Canada: Institute for Social Research [producer and distributor].\n
LINK: http://odesi2.scholarsportal.info/webview/index.jsp?v=2&submode=abstract&study=http%3A%2F%2F142.150.190.128%3A80%2Fobj%2FfStudy%2FCES-E-1993&mode=documentation&top=yes"


ref1988 <- "TO CITE THIS SURVEY FILE: Johnston, R, Blais, A, Brady, H. E. and Cr\u00eate, J. 1989. The 1988 Canadian Election Study [dataset]. Toronto, Ontario, Canada: Institute for Social Research [producer and distributor].\n
LINK:http://odesi2.scholarsportal.info/webview/index.jsp?v=2&submode=abstract&study=http%3A%2F%2F142.150.190.128%3A80%2Fobj%2FfStudy%2FCES-E-1988&mode=documentation&top=yes"


ref1984 <- "TO CITE THIS SURVEY FILE: Lambert, R. D., Brown, S. D., Curtis, J. E., Kay, B. J. and Wilson, J. M. 1985. The 1984 Canadian Election Study [dataset]. Toronto, Ontario, Canada: Institute for Social Research [producer and distributor].\n
LINK: http://odesi2.scholarsportal.info/webview/index.jsp?v=2&submode=abstract&study=http%3A%2F%2F142.150.190.128%3A80%2Fobj%2FfStudy%2FCES-E-1984&mode=documentation&top=yes"


ref1974 <- "TO CITE THIS SURVEY FILE: Clarke, H, Jenson, J, LeDuc, L and Pammett, J. 1975. The 1974 Canadian Election Study [dataset]. Toronto, Ontario, Canada: Institute for Social Research [producer and distributor].\n
LINK: http://odesi2.scholarsportal.info/webview/index.jsp?v=2&submode=abstract&study=http%3A%2F%2F142.150.190.128%3A80%2Fobj%2FfStudy%2FCES-E-1974&mode=documentation&top=yes"


ref7480 <- "TO CITE THIS SURVEY FILE: Clarke, H, Jenson, J, LeDuc, L and Pammett, J. 1980. The 1974-1980 Merged Canadian Election Study [dataset]. Toronto, Ontario, Canada: Institute for Social Research [producer and distributor].\n
LINK: http://odesi2.scholarsportal.info/webview/index.jsp?v=2&submode=abstract&study=http%3A%2F%2F142.150.190.128%3A80%2Fobj%2FfStudy%2FCES-E-1974-1980&mode=documentation&top=yes"


ref72jnjl <- "TO CITE THIS SURVEY FILE: Ruban, C. 1972. The 1972 Canadian Election Study [dataset]. 2nd ICPSR version. Toronto, Ontario, Canada: Market Opinion Research (Canada) Ltd. [producer], 1972. Ann Arbor, MI: Interuniversity Consortium for Political and Social Research [distributor], 2001.\n
LINK: http://odesi2.scholarsportal.info/webview/index.jsp?v=2&submode=abstract&study=http%3A%2F%2F142.150.190.128%3A80%2Fobj%2FfStudy%2FCES-E-1972-jun-july&mode=documentation&top=yes"


ref72sep <- "TO CITE THIS SURVEY FILE: Ruban, C. 1972. The 1972 Canadian Election Study [dataset]. 2nd ICPSR version. Toronto, Ontario, Canada: Market Opinion Research (Canada) Ltd. [producer], 1972. Ann Arbor, MI: Interuniversity Consortium for Political and Social Research [distributor], 2001.\n
LINK: http://odesi2.scholarsportal.info/webview/index.jsp?v=2&submode=abstract&study=http%3A%2F%2F142.150.190.128%3A80%2Fobj%2FfStudy%2FCES-E-1972-sept&mode=documentation&top=yes"


ref72nov <- "TO CITE THIS SURVEY FILE: Ruban, C. 1972. The 1972 Canadian Election Study [dataset]. 2nd ICPSR version. Toronto, Ontario, Canada: Market Opinion Research (Canada) Ltd. [producer], 1972. Ann Arbor, MI: Interuniversity Consortium for Political and Social Research [distributor], 2001.\n
LINK: http://odesi2.scholarsportal.info/webview/index.jsp?v=2&submode=abstract&study=http%3A%2F%2F142.150.190.128%3A80%2Fobj%2FfStudy%2FCES-E-1972-nov&mode=documentation&top=yes"


ref1968 <- "TO CITE THIS SURVEY FILE: Meisel, J. 1968. The 1968 Canadian Election Study [dataset]. Inter-University Consortium for Political and Social Research, University of Michigan, Ann Arbor MI [Producer and distributor].\n
LINK: http://odesi2.scholarsportal.info/webview/index.jsp?v=2&submode=abstract&study=http%3A%2F%2F142.150.190.128%3A80%2Fobj%2FfStudy%2FCES-E-1968&mode=documentation&top=yes"


ref1965 <- "TO CITE THIS SURVEY FILE: Converse, P, Meisel, J, Pinard, M, Regenstreif, P and Schwartz, M. 1966. Canadian Election Survey, 1965. [Microdata File]. Inter-University Consortium for Political and Social Research, University of Michigan, Ann Arbor MI [Producer].\n
LINK: http://odesi2.scholarsportal.info/webview/index.jsp?v=2&submode=abstract&study=http%3A%2F%2F142.150.190.128%3A80%2Fobj%2FfStudy%2FCES-E-1965&mode=documentation&top=yes"

# ces data frame codes
ces_codes <- (c("ces2019_web", "ces2019_phone", "ces2015_web", "ces2015_phone", "ces2015_combo",
                "ces2011", "ces2008", "ces2004", "ces0411", "ces0406", "ces2000", "ces1997", "ces1993",
                "ces1988", "ces1984", "ces1974", "ces7480", "ces72_jnjl", "ces72_sep", "ces72_nov",
                "ces1968", "ces1965"))
