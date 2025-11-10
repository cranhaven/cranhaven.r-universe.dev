#' Monophthong data for random sample of speakers from the ONZE corpus
#'
#' A dataset containing the the first and second formants, speech rate,
#' gender, and year of birth for 100 random speakers from the ONZE corpus.
#' 50 speakers are sampled with birth years before 1900 and 50 sampled with
#' birth years on or after 1900 to ensure a full span of the time period. Data
#' is present for the following NZE monophthongs, represented by Wells lexical
#' sets: DRESS, FLEECE, GOOSE, KIT, LOT, NURSE, START, STRUT, THOUGHT, TRAP. Data
#' for FOOT is excluded due to low token counts.
#'
#' This dataset is derived from the data made available in the supplementary
#' materials of Brand et al. (2021).
#'
#' @format A dataframe with 101572 rows and 8 variables:
#' \describe{
#'   \item{speaker}{Anonymised speaker code (factor).}
#'   \item{vowel}{Variable with Wells lexical sets for 10 NZE monophthongs. Levels:  DRESS, FLEECE, GOOSE, KIT, LOT, NURSE, START, STRUT, THOUGHT, TRAP (factor).}
#'   \item{F1_50}{First formant, extracted from vowel mid-point using LaBB-CAT interface with Praat.}
#'   \item{F2_50}{Second formant, extracted from vowel mid-point using LaBB-CAT interface with Praat.}
#'   \item{speech_rate}{Average speaker speech rate for whole recording.}
#'   \item{gender}{Gender of speaker, two levels: "M", "F" (factor).}
#'   \item{yob}{Year of birth of speaker.}
#'   \item{word}{Anonymised word code (factor).}
#' }
#' @source \url{https://osf.io/q4j29/}
#' @importFrom Rdpack reprompt
#'
#' @references
#'   Brand, James, Jen Hay, Lynn Clark, Kevin Watson & Márton Sóskuthy (2021):
#'   Systematic co-variation of monophthongs across speakers of New Zealand
#'   English. Journal of Phonetics. Elsevier. 88. 101096.
#'   doi:10.1016/j.wocn.2021.101096
"onze_vowels"

#' Monophthong data for speakers from the ONZE corpus
#'
#' A dataset containing the the first and second formants, speech rate,
#' gender, and year of birth for 481 speakers from the ONZE corpus.
#' 50 speakers are sampled with birth years before 1900 and 50 sampled with
#' birth years on or after 1900 to ensure a full span of the time period. Data
#' is present for the following NZE monophthongs, represented by Wells lexical
#' sets: DRESS, FLEECE, GOOSE, KIT, LOT, NURSE, START, STRUT, THOUGHT, TRAP. Data
#' for FOOT is excluded due to low token counts.
#'
#' This dataset is derived from the data made available in the supplementary
#' materials of Brand et al. (2021).
#'
#' @format A data frame with 414679 rows and 8 variables:
#' \describe{
#'   \item{speaker}{Anonymised speaker code (factor).}
#'   \item{vowel}{Variable with Wells lexical sets for 10 NZE monophthongs. Levels:  DRESS, FLEECE, GOOSE, KIT, LOT, NURSE, START, STRUT, THOUGHT, TRAP (factor).}
#'   \item{F1_50}{First formant, extracted from vowel mid-point using LaBB-CAT interface with Praat.}
#'   \item{F2_50}{Second formant, extracted from vowel mid-point using LaBB-CAT interface with Praat.}
#'   \item{speech_rate}{Average speaker speech rate for whole recording.}
#'   \item{gender}{Gender of speaker, two levels: "M", "F" (factor).}
#'   \item{yob}{Year of birth of speaker.}
#'   \item{word}{Anonymised word code (factor).}
#' }
#' @source \url{https://osf.io/q4j29/}
#'
#' @references
#'   Brand, James, Jen Hay, Lynn Clark, Kevin Watson & Márton Sóskuthy (2021):
#'   Systematic co-variation of monophthongs across speakers of New Zealand
#'   English. Journal of Phonetics. Elsevier. 88. 101096.
#'   doi:10.1016/j.wocn.2021.101096
"onze_vowels_full"

#' Speaker random intercepts from GAMMs for 100 ONZE speakers
#'
#' A dataset containing the speaker intercepts extracted from GAMM models fit in
#' Brand et al. (2021).
#'
#' @format A data frame with 100 rows and 21 variables: \describe{
#'   \item{speaker}{Anonymised speaker code (character).}
#'   \item{F1_DRESS}{Speaker intercept from GAMM model of DRESS F1.}
#'   \item{F2_DRESS}{Speaker intercept from GAMM model of DRESS F2.}
#'   \item{F1_FLEECE}{Speaker intercept from GAMM model of FLEECE F1.}
#'   \item{F2_FLEECE}{Speaker intercept from GAMM model of FLEECE F2.}
#'   \item{F1_GOOSE}{Speaker intercept from GAMM model of GOOSE F1.}
#'   \item{F2_GOOSE}{Speaker intercept from GAMM model of GOOSE F2.}
#'   \item{F1_KIT}{Speaker intercept from GAMM model of KIT F1.}
#'   \item{F2_KIT}{Speaker intercept from GAMM model of KIT F2.}
#'   \item{F1_LOT}{Speaker intercept from GAMM model of LOT F1.}
#'   \item{F2_LOT}{Speaker intercept from GAMM model of LOT F2.}
#'   \item{F1_NURSE}{Speaker intercept from GAMM model of NURSE F1.}
#'   \item{F2_NURSE}{Speaker intercept from GAMM model of NURSE F2.}
#'   \item{F1_START}{Speaker intercept from GAMM model of START F1.}
#'   \item{F2_START}{Speaker intercept from GAMM model of START F2.}
#'   \item{F1_STRUT}{Speaker intercept from GAMM model of STRUT F1.}
#'   \item{F2_STRUT}{Speaker intercept from GAMM model of STRUT F2.}
#'   \item{F1_THOUGHT}{Speaker intercept from GAMM model of THOUGHT F1.}
#'   \item{F2_THOUGHT}{Speaker intercept from GAMM model of THOUGHT F2.}
#'   \item{F1_TRAP}{Speaker intercept from GAMM model of TRAP F1.}
#'   \item{F2_TRAP}{Speaker intercept from GAMM model of TRAP F2.}
#' }
#' @source \url{https://osf.io/q4j29/}
#'
#' @references
#'   Brand, James, Jen Hay, Lynn Clark, Kevin Watson & Márton Sóskuthy (2021):
#'   Systematic co-variation of monophthongs across speakers of New Zealand
#'   English. Journal of Phonetics. Elsevier. 88. 101096.
#'   doi:10.1016/j.wocn.2021.101096
"onze_intercepts"

#' Speaker random intercepts for 418 ONZE speakers
#'
#' A dataset containing the speaker intercepts extracted from GAMM models fit in
#' Brand et al. (2021).
#'
#' @format A data frame with 481 rows and 21 variables: \describe{
#'   \item{speaker}{Anonymised speaker code.}
#'   \item{F1_DRESS}{Speaker intercept from GAMM model of DRESS F1.}
#'   \item{F2_DRESS}{Speaker intercept from GAMM model of DRESS F2.}
#'   \item{F1_FLEECE}{Speaker intercept from GAMM model of FLEECE F1.}
#'   \item{F2_FLEECE}{Speaker intercept from GAMM model of FLEECE F2.}
#'   \item{F1_GOOSE}{Speaker intercept from GAMM model of GOOSE F1.}
#'   \item{F2_GOOSE}{Speaker intercept from GAMM model of GOOSE F2.}
#'   \item{F1_KIT}{Speaker intercept from GAMM model of KIT F1.}
#'   \item{F2_KIT}{Speaker intercept from GAMM model of KIT F2.}
#'   \item{F1_LOT}{Speaker intercept from GAMM model of LOT F1.}
#'   \item{F2_LOT}{Speaker intercept from GAMM model of LOT F2.}
#'   \item{F1_NURSE}{Speaker intercept from GAMM model of NURSE F1.}
#'   \item{F2_NURSE}{Speaker intercept from GAMM model of NURSE F2.}
#'   \item{F1_START}{Speaker intercept from GAMM model of START F1.}
#'   \item{F2_START}{Speaker intercept from GAMM model of START F2.}
#'   \item{F1_STRUT}{Speaker intercept from GAMM model of STRUT F1.}
#'   \item{F2_STRUT}{Speaker intercept from GAMM model of STRUT F2.}
#'   \item{F1_THOUGHT}{Speaker intercept from GAMM model of THOUGHT F1.}
#'   \item{F2_THOUGHT}{Speaker intercept from GAMM model of THOUGHT F2.}
#'   \item{F1_TRAP}{Speaker intercept from GAMM model of TRAP F1.}
#'   \item{F2_TRAP}{Speaker intercept from GAMM model of TRAP F2.}
#' }
#' @source \url{https://osf.io/q4j29/}
#'
#' @references
#'   Brand, James, Jen Hay, Lynn Clark, Kevin Watson & Márton Sóskuthy (2021):
#'   Systematic co-variation of monophthongs across speakers of New Zealand
#'   English. Journal of Phonetics. Elsevier. 88. 101096.
#'   doi:10.1016/j.wocn.2021.101096
"onze_intercepts_full"

#' Formants from QuakeBox 1
#'
#' A dataset containing formant values, amplitude, articulation rate, and
#' following segment data for 10 New Zealand English monophthongs, along with
#' participant demographics.
#'
#' Original data was generated for Wilson Black et al. (2023).
#'
#' @format A data frame with 26331 rows and 14 variables:
#' \describe{
#'   \item{speaker}{Anonymised speaker code (char).}
#'   \item{vowel}{Wells lexical sets for 10 NZE monophthongs. Levels:  DRESS, FLEECE, GOOSE, KIT, LOT, NURSE, START, STRUT, THOUGHT, TRAP, FOOT (char).}
#'   \item{F1_50}{First formant in Hz, extracted from vowel mid-point using LaBB-CAT interface with Praat.}
#'   \item{F2_50}{Second formant in Hz, extracted from vowel mid-point using LaBB-CAT interface with Praat.}
#'   \item{participant_age_category}{Age category of speaker. Values: 18-25, 26-35, 36-45, ..., 76-85 (char).}
#'   \item{participant_gender}{Gender of participant. Values: M, F (char).}
#'   \item{participant_nz_ethnic}{New Zealand ethnic category of participant. Values: NZ mixed ethnicity, NZ European, Other (char).}
#'   \item{word_freq}{Frequency of word from which vowel token is taken in CELEX.}
#'   \item{word}{Anonymised word id (char).}
#'   \item{time}{Time in seconds at which vowel segment starts.}
#'   \item{vowel_duration}{Length of vowel in seconds.}
#'   \item{articulation_rate}{Articulation rate of utterance from which token is taken.}
#'   \item{following_segment_category}{Category of following segment. NB: liquids have already been removed. Levels: labial, velar, other (factor).}
#'   \item{amplitude}{Maximum amplitude of word from which vowel token is taken, generated by LaBB-CAT interface with Praat.}
#'}
#'
#' @source \url{https://osf.io/m8nkh/}
#'
#' @references
#'  Wilson Black, Joshua, Jennifer Hay, Lynn Clark & James Brand (2023): The
#'  overlooked effect of amplitude on within-speaker vowel variation.
#'  Linguistics Vanguard. Walter de Gruyter GmbH. 9(1). 173–189.
#'  doi:10.1515/lingvan-2022-0086
"qb_vowels"

#' Formant and amplitude for intervals of QuakeBox monologues
#'
#' QuakeBox monologues are divided into intervals of fixed length within mean
#' values are calcualted for formants, amplitude, and articulation rate. Data
#' from 77 speakers is provide (the same sample as `qb_vowels`).
#'
#' Two interval lengths are given: 60 seconds and 240 seconds.
#'
#' Formant data is z-scored by speaker and vowel, while the amplitude and
#' articulation rate are z-scored by speaker.
#'
#' Original data was generated for Wilson Black et al. (2023).
#'
#' @format A data frame with 53940 rows and 10 variables:
#' \describe{
#'   \item{interval_length}{Length of interval in seconds.}
#'   \item{speaker}{Anonymised speaker code (char).}
#'   \item{interval}{Time in seconds at which interval ends.}
#'   \item{articulation_rate}{Mean articulation rate within interval.}
#'   \item{amplitude}{Mean maximum amplitude within interval.}
#'   \item{DRESS_F1}{Speaker intercept from GAMM model of DRESS F1.}
#'   \item{DRESS_F2}{Speaker intercept from GAMM model of DRESS F2.}
#'   \item{FLEECE_F1}{Speaker intercept from GAMM model of FLEECE F1.}
#'   \item{FLEECE_F2}{Speaker intercept from GAMM model of FLEECE F2.}
#'   \item{GOOSE_F1}{Speaker intercept from GAMM model of GOOSE F1.}
#'   \item{GOOSE_F2}{Speaker intercept from GAMM model of GOOSE F2.}
#'   \item{KIT_F1}{Speaker intercept from GAMM model of KIT F1.}
#'   \item{KIT_F2}{Speaker intercept from GAMM model of KIT F2.}
#'   \item{LOT_F1}{Speaker intercept from GAMM model of LOT F1.}
#'   \item{LOT_F2}{Speaker intercept from GAMM model of LOT F2.}
#'   \item{NURSE_F1}{Speaker intercept from GAMM model of NURSE F1.}
#'   \item{NURSE_F2}{Speaker intercept from GAMM model of NURSE F2.}
#'   \item{START_F1}{Speaker intercept from GAMM model of START F1.}
#'   \item{START_F2}{Speaker intercept from GAMM model of START F2.}
#'   \item{STRUT_F1}{Speaker intercept from GAMM model of STRUT F1.}
#'   \item{STRUT_F2}{Speaker intercept from GAMM model of STRUT F2.}
#'   \item{THOUGHT_F1}{Speaker intercept from GAMM model of THOUGHT F1.}
#'   \item{THOUGHT_F2}{Speaker intercept from GAMM model of THOUGHT F2.}
#'   \item{TRAP_F1}{Speaker intercept from GAMM model of TRAP F1.}
#'   \item{TRAP_F2}{Speaker intercept from GAMM model of TRAP F2.}
#' }
#' @source \url{https://osf.io/m8nkh/}
#'
#' @references
#'  Wilson Black, Joshua, Jennifer Hay, Lynn Clark & James Brand (2023): The
#'  overlooked effect of amplitude on within-speaker vowel variation.
#'  Linguistics Vanguard. Walter de Gruyter GmbH. 9(1). 173–189.
#'  doi:10.1515/lingvan-2022-0086
"qb_intervals"

#' Similarity matrix from online perception test.
#'
#' Mean similarity ratings for 38 QuakeBox speakers from an online pairwise
#' similarity task. Random noise added.
#'
#' @format A 38x38 matrix
#'
"sim_matrix"
