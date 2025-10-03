#' Creates a non-exhaustive dataframe of 21 variables with renamed columns.
#'
#' @description get_decon() creates a non-exhaustive dataset under the name
#'   decon consisting of 21 variables with renamed columns from the
#'   demographics, ideology, and economy sections of the 2019 CES online survey.
#'
#' @details NAs have not been removed. The politically left/right question
#'   variables (`lr_bef` and `lr_aft`) have also been joined into one column under the
#'   name `lr_scale`. All variables have been converted to factor type using
#'   `labelled::to_factor` and are listed below.
#'
#' @param pos Environment assignment. Defaults to 1, which is an assignment to the global environment.
#'
#' ## decon Variables
#'   \describe{ \item{\code{citizenship}}{Canadian citizenship status}
#'   \item{\code{yob}}{year of birth} \item{\code{gender}}{identified gender of
#'   the respondent} \item{\code{province_territory}}{Province or Territory of
#'   current residence} \item{\code{education}}{highest level of education
#'   completed} \item{\code{vote_likely}}{likelihood of voting on election day}
#'   \item{\code{vote_likely_ifable}}{likelihood to vote in first election for which respondent is eligible}
#'   \item{\code{votechoice}}{party most likely to vote for}
#'   \item{\code{votechoice_text}}{party most likely to vote for - text answers}
#'   \item{\code{votechoice_couldvote}}{party most likely to vote for if eligible to vote}
#'   \item{\code{votechoice_couldvote_text}}{party most likely to vote for if eligible to vote - text answers}
#'   \item{\code{votechoice_unlikely}}{party least likely to vote for}
#'   \item{\code{votechoice_unlikely_text}}{party least likely to vote for - text answers}
#'   \item{\code{votechoice_unlikely_couldvote}}{party least likely to vote for if eligible to vote}
#'   \item{\code{votechoice_unlikely_couldvote_text}}{party least likely to vote for if eligible to vote - text answers}
#'   \item{\code{vote_advancevote_choice}}{party voted for in the advanced ballot}
#'   \item{\code{vote_advancevote_choice_text}}{party voted for in the advanced ballot - text}
#'   \item{\code{vote_partylean}}{party toward which the respondent leans}
#'   \item{\code{vote_partylean_text}}{party toward which the respondent leans - text answers}
#'   \item{\code{vote_partylean_couldvote}}{party toward which the respondent leans if eligible}
#'   \item{\code{vote_partylean_couldvote_text}}{party toward which the respondent leans if eligible - text answers}
#'   \item{\code{votechoice_secondchoice}}{second choice party of respondent}
#'   \item{\code{votechoice_secondchoice_text}}{second choice party of respondent - text answers}
#'   \item{\code{votechoice_couldvote_secondchoice}}{second choice party of respondent if eligible}
#'   \item{\code{votechoice_couldvote_secondchoice_text}}{second choice party of respondent if eligible - text answers}
#'   \item{\code{votechoice_partynotvote_1}}{party respondent would note vote for - first ranking}
#'   \item{\code{votechoice_partynotvote_2}}{party respondent would note vote for - second ranking}
#'   \item{\code{votechoice_partynotvote_3}}{party respondent would note vote for - third ranking}
#'   \item{\code{votechoice_partynotvote_4}}{party respondent would note vote for - fourth ranking}
#'   \item{\code{votechoice_partynotvote_5}}{party respondent would note vote for - fifth ranking}
#'   \item{\code{votechoice_partynotvote_6}}{party respondent would note vote for - sixth ranking}
#'   \item{\code{votechoice_partynotvote_7}}{party respondent would note vote for - seventh ranking}
#'   \item{\code{votechoice_partynotvote_8}}{party respondent would note vote for - eighth ranking}
#'   \item{\code{votechoice_partynotvote_9}}{party respondent would note vote for - ninth ranking}
#'   \item{\code{votechoice_partynotvote_text}}{party respondent would note vote for - text answers}
#'   \item{\code{lr_scale}}{united column of lr_bef and lr_aft values;
#'   whether the respondent identifies on the political spectrum}
#'   \item{\code{lr_scale_bef}}{where the respondent identifies on the political
#'   spectrum; asked before party identification questions}
#'   \item{\code{lr_scale_aft}}{where the respondent identifies on the political
#'   spectrum; asked after party identification questions}
#'   \item{\code{religion}}{religion of respondent}
#'   \item{\code{sexuality_selected}}{sexual identity}
#'   \item{\code{sexuality_text}}{sexual identity; written answers}
#'   \item{\code{language_eng}}{language learned as child and still understand;
#'   selected response English} \item{\code{language_fr}}{language learned as a
#'   child and still understand; selected response French}
#'   \item{\code{language_abgl}}{language learned as a child and still
#'   understand; specified Aboriginal language}
#'   \item{\code{employment}}{employment status} \item{\code{income}}{total
#'   household income, before taxes, for the year 2018}
#'   \item{\code{income_cat}}{selected household income category}
#'   \item{\code{marital}}{marital status} \item{\code{econ_retro}}{response to
#'   question, 'over the past year, has Canada's economy:'}
#'   \item{\code{econ_fed}}{response to question, 'have the policies of the
#'   federal government made Canada's economy...'}
#'   \item{\code{econ_self}}{response to question, have the policies of the
#'   federal government made your financial situation...'} }
#'
#' @return The designed dataframe as a 'tbl_df' object under the name decon.
#'
#' @examples
#' \dontrun{
#' # call decon dataset
#' get_decon()
#'
#' # preview decon
#' head(decon)
#'}
#' @seealso For further details, see
#'   \url{https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/DUS88V}
#'    Stephenson, Laura B; Harell, Allison; Rubenson, Daniel; Loewen, Peter
#'   John, 2020, "2019 Canadian Election Study - Online Survey",
#'   \doi{10.7910/DVN/DUS88V}, Harvard Dataverse, V1


#library(dplyr)
#library(labelled)
#library(utils)

#'@export
# function to create 'decon' dataset
# does not use any variable calls
get_decon <- function(pos = 1){
    # if object does not exist in global environment
    if(!exists("decon")){
       # assign url to 'cesfile'
       cesfile <- "https://dataverse.harvard.edu/api/access/datafile/:persistentId?persistentId=doi:10.7910/DVN/DUS88V/RZFNOV"
       # assign temporary file with .dta extension to placeholder variable
       hldr <- tempfile(fileext = ".dta")
       # download the file from url assigned to 'cesfile' with file extension from the temporary placeholder
       utils::download.file(cesfile, hldr, quiet = F, mode = "wb")
       # assign data file to temporary data object
       ces2019_hldr <- haven::read_dta(hldr, encoding = "latin1")
       # create new data object with selected columns from temporary data object
       decon <- dplyr::select(ces2019_hldr, c(5:6, 8:10, 20:49, 69,76, 194, 223:227, 245, 250:251, 258, 123:125))
       # rename columns in new data object
       decon <- dplyr::rename(decon,
                              citizenship = 1,                                                        # rename column 1 to citizenship
                              yob = 2,                                                                # rename column 2 to yob
                              gender = 3,                                                             # rename column 3 to gender
                              province_territory = 4,                                                 # rename column 4 to province_territory
                              education = 5,                                                          # rename column 5 to education
                              vote_likely = 6,                                                        # rename column 6 to vote_likely
                              vote_likely_ifable = 7,                                                 # rename column 7 to vote_likely_ifable
                              votechoice = 8,                                                         # rename column 8 to votechoice
                              votechoice_text = 9,                                                    # rename column 9 to votechoice_text
                              votechoice_couldvote = 10,                                              # rename column 10 to votechoice_couldvote
                              votechoice_couldvote_text = 11,                                         # rename column 11 to votechoice_couldvote_text
                              vote_unlikely = 12,                                                     # rename column 12 to vote_unlikely
                              vote_unlikely_text = 13,                                                # rename column 13 to voter_unlikely_text
                              vote_unlikely_couldvote = 14,                                           # rename column 14 to vote_unlikely_couldvote
                              vote_unlikely_couldvote_text = 15,                                      # rename column 14 to vote_unlikely_couldvote_text
                              vote_advancevote_choice = 16,                                           # rename column 16 to vote_advancevote_choice
                              vote_advancevote_choice_text = 17,                                      # rename column 17 to vote_advancevote_choice_text
                              vote_partylean = 18,                                                    # rename column 18 to vote_partylean
                              vote_partylean_text = 19,                                               # rename column 19 to vote_partylean_text
                              vote_partylean_couldvote = 20,                                          # rename column 20 to vote_partylean_couldvote
                              vote_partylean_couldvote_text = 21,                                     # rename column 21 to vote_partylean_couldvote_text
                              votechoice_secondchoice = 22,                                           # rename column 22 to votechoice_secondchoice
                              votechoice_secondchoice_text = 23,                                      # rename column 23 to votechoice_secondchoice_text
                              votechoice_couldvote_secondchoice = 24,                                 # rename column 24 to votechoice_couldvote_secondchoice
                              votechoice_couldvote_secondchoice_text = 25,                            # rename column 25 to votechoice_couldvote_secondchoice_text
                              votechoice_partynotvote_1 = 26,                                         # rename column 26 to votechoice_partynotvote_1
                              votechoice_partynotvote_2 = 27,                                         # rename column 27 to votechoice_partynotvote_2
                              votechoice_partynotvote_3 = 28,                                         # rename column 28 to votechoice_partynotvote_3
                              votechoice_partynotvote_4 = 29,                                         # rename column 29 to votechoice_partynotvote_4
                              votechoice_partynotvote_5 = 30,                                         # rename column 30 to votechoice_partynotvote_5
                              votechoice_partynotvote_6 = 31,                                         # rename column 31 to votechoice_partynotvote_6
                              votechoice_partynotvote_7 = 32,                                         # rename column 32 to votechoice_partynotvote_7
                              votechoice_partynotvote_8 = 33,                                         # rename column 33 to votechoice_partynotvote_8
                              votechoice_partynotvote_9 = 34,                                         # rename column 34 to votechoice_partynotvote_9
                              votechoice_partynotvote_text = 35,                                      # rename column 35 to votechoice_partynotvote_text
                              lr_scale_bef = 36,                                                      # rename column 36 to lr_scale_bef
                              lr_scale_aft = 37,                                                      # rename column 37 to lr_scale_aft
                              religion = 38,                                                          # rename column 38 to religion
                              sexuality_selected = 39,                                                # rename column 39 to sexuality_selected
                              sexuality_text = 40,                                                    # rename column 40 to sexuality_text
                              language_eng = 41,                                                      # rename column 41 to language_eng
                              language_fr = 42,                                                       # rename column 42 to language_fr
                              language_abgl = 43,                                                     # rename column 43 to language_abgl
                              employment = 44,                                                        # rename column 44 to employment
                              income = 45,                                                            # rename column 45 to income
                              income_cat = 46,                                                        # rename column 46 to income_cat
                              marital = 47,                                                           # rename column 47 to marital
                              econ_retro = 48,                                                        # rename column 48 to econ_retro
                              econ_fed = 49,                                                          # rename column 49 to econ_fed
                              econ_self = 50)                                                         # rename column 50 to econ_self
       decon <- labelled::to_factor(decon)                                                            # convert variables to factors
       decon <- dplyr::mutate(decon, lr_scale_bef = as.character(lr_scale_bef))                       # reassign values in lr_scale_bef column as characters for uniting
       decon <- dplyr::mutate(decon, lr_scale_aft = as.character(lr_scale_aft))                       # reassign values in lr_scale_aft column as characters for uniting
       decon <- tidyr::unite(decon, "lr_scale", lr_scale_bef:lr_scale_aft, na.rm = T, remove = F)     # unite lr_scale_bef and lr_scale_aft columns into new column lr_scale
       decon <- dplyr::mutate_if(decon, is.character, list(~dplyr::na_if(., "")))                     # replaces empty cells in new lr column with NA
       assign("decon", dplyr::mutate(decon, ces_code = "ces2019_web", .before = 1), envir = as.environment(pos))
       # remove temporary data object
       rm(ces2019_hldr)
       # remove the temporary placeholder
       unlink(hldr, recursive = T, force = T)
       # print out a concatenation of the survey citation
       message("TO CITE THIS SURVEY FILE: Stephenson, Laura B; Harell, Allison; Rubenson, Daniel; Loewen, Peter John, 2020, '2019 Canadian Election Study - Online Survey',
           https://doi.org/10.7910/DVN/DUS88V, Harvard Dataverse, V1\nLINK: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/DUS88V")
    }
    else{
        # if the file does exist stop process and print this message
        stop("Warning: Dataframe already exists.")
    }
}
