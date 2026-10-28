# #' @title
# #' Add diagnoses to the input dataset
# #'
# #' @description
# #' This function takes a dataset and returns a diagnosis for each
# #' observation. For the function to run, the dataset must not contain any errors
# #' that [banff_launcher()] would have detected. Please prefer using
# #' [banff_launcher()] to run additional tests.
# #'
# #' @param banff_dataset A tibble object.
# #'
# #' @return
# #' A tibble object that contains additional variables with the diagnoses results.
# #'
# #' @examples
# #' {
# #'
# #' banff_dataset <- get_banff_template()
# #' add_diagnoses(banff_dataset)
# #'
# #' }
# #'
# #' @import dplyr
# #' @importFrom rlang .data
# #' @export
# add_diagnoses <- function(banff_dataset) {
#
#   # check input
#   banff_assessment <- suppressMessages(banff_dataset_evaluate(banff_dataset))
#
#   if(!is.null(attributes(banff_assessment)$error)){
#     message(attributes(banff_assessment)$error)
#     message("
# Use `banff_dataset_evaluate(banff_dataset)` to help you correcting your file.\n")
#     stop(call. = TRUE)
#   }
#
#   banff_diagnoses <- banff_dataset
#
#
#   ## creation of adequacy calculated
#   adequacy_input_copy <- calculate_adequacy(banff_dataset)
#   banff_dataset <-
#     banff_dataset %>%
#     mutate(
#       adequacy = adequacy_input_copy$`adequacy_calculated`)
#
#   banff_diagnoses <-
#     banff_diagnoses %>%
#     mutate(
#
#       nogn =
#         case_when(
#           .data$`agn` + .data$`cgn` == 0 ~ 0L,
#           TRUE ~ 1L),
#
#       ifta = pmax(.data$`ci_score`,.data$`ct_score`),
#       c4d =
#         case_when(
#           .data$`c4d_if` %in% c(0,1) & .data$`c4d_ihc` == 0 ~ 0L,
#           TRUE ~ 1L)
#     )
#
#   # atcmr variables
#   banff_diagnoses <-
#     banff_diagnoses %>%
#     mutate(
#       active_atcmr_code_3.TEMP =
#         case_when(
#           .data$`i_score` %in% c(2,3) & .data$`t_score` == 1          ~ 1L  ,  # 'BLD_a'
#           .data$`i_score` == 1        & .data$`t_score` %in% c(1,2,3) ~ 2L  ,  # 'BLD_i1_a'
#           TRUE                                                        ~ 0L) ,
#
#       active_atcmr_code_4.TEMP =
#         case_when(
#           .data$`v_score` == 3                                        ~ 5L  ,  # 'III_a'
#           .data$`v_score` == 2                                        ~ 4L  ,  # 'IIB_a'
#           .data$`v_score` == 1                                        ~ 3L  ,  # 'IIA_a'
#           .data$`i_score` %in% c(2,3) & .data$`t_score` == 3          ~ 2L  ,  # 'IB_a'
#           .data$`i_score` %in% c(2,3) & .data$`t_score` == 2          ~ 1L  ,  # 'IA_a'
#           TRUE                                                        ~ 0L
#         ))
#
#
#   banff_diagnoses <-
#     banff_diagnoses %>%
#     mutate(
#       chronic_atcmr_code_4.TEMP =
#         case_when(
#           .data$`monofibneoint` == 1                              ~ 8L  ,   # "II_c",
#           (.data$`bk`            == 0        &
#              .data$`infec`         == 0        &
#              .data$`i_ifta_score`  %in% c(2,3) &
#              .data$`ti_score`      %in% c(2,3) &
#              .data$`t_score`       == 3)                             ~ 7L  ,   # "IB_c",
#           (.data$`bk`            == 0        &
#              .data$`infec`         == 0        &
#              .data$`i_ifta_score`  %in% c(2,3) &
#              .data$`ti_score`      %in% c(2,3) &
#              .data$`t_score`       == 2)                             ~ 6L  ,   # "IA_c",
#           TRUE                                                     ~ 0L)
#     )
#
#   # aamr1
#   banff_diagnoses <-
#     banff_diagnoses %>%
#     mutate(
#       aamr11.1 = ifelse((.data$`active_atcmr_code_3.TEMP` + .data$`active_atcmr_code_4.TEMP`) == 0 &
#                           .data$`ptc_score` > 0,                            1L, 0L),
#
#       aamr11.2 = ifelse(.data$`g_score` > 0 &
#                           .data$`cgn` == 0 &
#                           .data$`agn` == 0,                                 1L, 0L),
#       aamr12  = ifelse((.data$`active_atcmr_code_3.TEMP` + .data$`active_atcmr_code_4.TEMP`) > 0 &
#                          .data$`g_score` + .data$`ptc_score` >= 2 &
#                          .data$`g_score` >= 1,                             1L, 0L),
#       aamr13  = ifelse( .data$`v_score` > 0,                              1L, 0L),
#       aamr14  = ifelse((.data$`atma`) > 0,                    1L, 0L), # SEBT remove  "+ .data$`atn`" to align with version 2022
#       aamr1   = ifelse( .data$`aamr11.1` == 1 |
#                           .data$`aamr11.2` == 1 |
#                           .data$`aamr12`   == 1 |
#                           .data$`aamr13`   == 1 |
#                           .data$`aamr14`   == 1,                            1L, 0L),
#
#     )
#
#   # The default for aamr22 is 0 (which take into consideration when both GN and (atcmr_bl or atcmr) are present)
#   # G + PTC must be at least 2 when GN is absent and atcmr_bl or atcmr also absent
#   # PTC must be at least 2 when GN is present and atcmr_bl or atcmr also absent
#   # G + PTC must be at least 2, and G must be at least 1, when GN is absent and atcmr_bl or atcmr is present
#   # activeabmr definition below because it must take camr into consideration (to verify) !!!!
#   # need to be double checked
#
#   banff_diagnoses <-
#     banff_diagnoses %>%
#     mutate(
#       aamr21    = ifelse(.data$`c4d_if`  %in% c(2,3) |
#                            .data$`c4d_ihc` %in% c(1,2,3),  1L, 0L),
#
#       aamr22.1   = ifelse(.data$`g_score` + .data$`ptc_score` >= 2 &
#                             .data$`cgn`   == 0 &
#                             .data$`agn`   == 0 &
#                             (.data$`active_atcmr_code_3.TEMP` + .data$`active_atcmr_code_4.TEMP`) == 0,            1L, 0L),
#
#       aamr22.2   = ifelse(.data$`ptc_score` >= 2 &
#                             (.data$`cgn` == 1 | .data$`agn` == 1) &
#                             (.data$`active_atcmr_code_3.TEMP` + .data$`active_atcmr_code_4.TEMP`) == 0,             1L, 0L),
#
#       aamr22.3   = ifelse(.data$`g_score` + .data$`ptc_score` >= 2 &
#                             .data$`g_score` >=1 &
#                             .data$`cgn` == 0 &
#                             .data$`agn` == 0 &
#                             (.data$`active_atcmr_code_3.TEMP` + .data$`active_atcmr_code_4.TEMP`) > 0,              1L, 0L),
#
#       aamr2     = ifelse(.data$`aamr21` == 1 |
#                            .data$`aamr22.1` == 1 |
#                            .data$`aamr22.2` == 1 |
#                            .data$`aamr22.3` == 1,                             1L, 0L))
#
#   banff_diagnoses <-
#     banff_diagnoses %>%
#     mutate(
#
#       aamr      = ifelse(.data$`aamr1` == 1 &
#                            .data$`aamr2` == 1 &
#                            .data$`dsa` == 1,                                 1L, 0L),
#
#       susp_aamr = ifelse(.data$`aamr` == 0 &
#                            .data$`aamr1` == 1 &
#                            .data$`aamr2` == 1 &
#                            .data$`dsa` != 1,                                 1L, 0L),
#
#       c4dneg_aamr = ifelse(.data$`dsa` == 1 &
#                              (.data$`aamr22.1` == 1 |
#                                 .data$`aamr22.2` == 1 |
#                                 .data$`aamr22.3` == 1) &
#                              .data$`aamr21` == 0L,                           1L, 0L),
#
#       # the previous conditon is used as .data$g_score + .data$ptc_score >= 2
#       # SEBT Definition of Microvascular inflammation/injury (MVI), DSA-negative and C4d-negative
#       susp_c4dneg_aamr = ifelse(.data$`c4dneg_aamr` == 0 &
#                                   (.data$`aamr22.1` == 1 |
#                                      .data$`aamr22.2` == 1 |
#                                      .data$`aamr22.3` == 1) &
#                                   .data$`aamr21` == 0 &
#                                   .data$`dsa` != 1,                         1L, 0L),
#
#       dsaneg_aamr = ifelse(.data$`aamr1` == 1 &
#                              .data$`aamr21` == 1 &
#                              .data$`dsa` == 0,                              1L, 0L),
#
#       susp_activeaamr  = ifelse(.data$`susp_aamr` == 1 |
#                                   .data$`susp_c4dneg_aamr` == 1,            1L, 0L)
#
#       # SEBT Probable AMR to add to the data dictionary and to the output diagnoses
#       prob_amr = ifelse(.data$`aamr1` == 1 &
#                           .data$`aamr2` == 0 &
#                           .data$`camr1` == 0 &
#                           .data$`dsa` == 1,                                 1L, 0L),
#
#
#     )
#
#
#
#
#   # define historical tcmr based any previous catcmr or atcmr record
#   # tcmr historical computation
#   # ordering by transplant date and biopsy date
#
#   banff_diagnoses <-
#     banff_diagnoses %>%
#     arrange(.data$`patient_id`, .data$`sc_date_bx`) %>%
#     group_by(.data$`patient_id`) %>%
#     mutate(
#       hist_dsa1 = cumsum(.data$`dsa`) - .data$`dsa`,
#       hist_dsa2 = cumsum(.data$`hist_dsa`)) %>%
#     ungroup %>%
#     group_by(.data$`patient_id`) %>%
#     mutate(
#       hist_tcmr1 = cumsum(.data$`active_atcmr_code_4.TEMP`) - .data$`active_atcmr_code_4.TEMP`,
#       hist_tcmr2 = cumsum(.data$`chronic_atcmr_code_4.TEMP`)- .data$`chronic_atcmr_code_4.TEMP`,
#       hist_tcmr3 = cumsum(.data$`hist_tcmr`)) %>%
#     ungroup %>%
#     mutate(
#       hist_dsa_calculated  = ifelse(.data$`hist_dsa1`  >= 1L |
#                                       .data$`hist_dsa2`  == 1L        , 1L, 0L),
#
#       hist_tcmr_calculated = ifelse(.data$`hist_tcmr1` >= 1L |
#                                       .data$`hist_tcmr2` >= 1L |
#                                       .data$`hist_tcmr3` >= 1L        , 1L, 0L))
#
#   # Check with Jan chronic vs chronic active (camr11 + camr12 vs camr1 = camr11 + camr12 + camr13)
#
#   banff_diagnoses <-
#     banff_diagnoses %>%
#     mutate(
#
#       camr11 = ifelse(.data$`cg_score`    != "0" &
#                         .data$`ctma`        ==  0  &
#                         .data$`cgn`         ==  0  &
#                         .data$`agn`         ==  0  ,    1L, 0L),
#
#       camr12 = ifelse(.data$`sptcbmml`    == 1L,      1L, 0L),
#
#       #camr13 = case_when(
#       # .data$`leuscint` == 1 & .data$`hist_tcmr_calculated` == 0L  ~ 1L,
#       #.data$`newaif`   == 1                                       ~ 1L, # SEBT removing for version 2022
#       #TRUE                                                        ~ 0L)
#     )
#
#   banff_diagnoses <-
#     banff_diagnoses %>%
#     mutate(
#
#       camr1 = ifelse(.data$`camr11` == 1L |
#                        .data$`camr12` == 1L,                          1L, 0L), # SEBT remove camr13 for version 2022
#
#       activeabmr = ifelse(
#         (.data$`aamr` == 1 |
#            .data$`c4dneg_aamr` == 1 |
#            .data$`dsaneg_aamr` == 1) &
#           .data$`camr1` == 0,                                         1L, 0L),
#
#       camr = ifelse(.data$`camr1` == 1 &
#                       .data$`aamr2` == 1 &
#                       (.data$`dsa` == 1 | .data$`aamr21` == 1),          1L, 0L),
#
#       susp_camr = ifelse(
#         .data$`camr` == 0 &
#           .data$`camr1` == 1 &
#           .data$`aamr2` == 1 &
#           .data$`dsa` != 1 &
#           .data$`aamr21` != 1,                                        1L, 0L)
#
#     )
#
#   ## C4d negative aamr
#   # removed | banff_diagnoses$dsaneg_camr == 1 because it is captured by the camr definition
#   banff_diagnoses <-
#     banff_diagnoses %>%
#     mutate(
#
#       c4dneg_camr      = ifelse(.data$`camr1`      == 1  &
#                                   .data$`dsa`        == 1 &
#                                   (.data$`aamr22.1`   == 1 |
#                                      .data$`aamr22.2` == 1 |
#                                      .data$`aamr22.3` == 1) &
#                                   .data$`aamr21`     == 0L,                 1L, 0L),
#
#       susp_c4dneg_camr = ifelse(.data$`c4dneg_camr` == 0 &
#                                   .data$`camr1`       == 1 &
#                                   (.data$`aamr22.1`    == 1 |
#                                      .data$`aamr22.2`  == 1 |
#                                      .data$`aamr22.3`  == 1) &
#                                   .data$`aamr21`      == 0L &
#                                   .data$`dsa`         != 1,                 1L, 0L),
#
#       dsaneg_camr      = ifelse(.data$`camr1` == 1 &
#                                   .data$`aamr21` == 1 &
#                                   .data$`dsa` == 0,                          1L, 0L),
#
#       chractabmr       = ifelse(.data$`camr` == 1 |
#                                   .data$`c4dneg_camr` == 1,                  1L, 0L),
#
#       susp_chractabmr  = ifelse(.data$`susp_camr` == 1 |
#                                   .data$`susp_c4dneg_camr` == 1,             1L, 0L)
#
#     )
#
#   # chronic abmr variables
#   # camr and aamr historical computation
#   banff_diagnoses <-
#     banff_diagnoses %>%
#     arrange(.data$`patient_id`, .data$`sc_date_bx`) %>%
#     group_by(.data$`patient_id`) %>%
#     mutate(
#       hist_camr1 = cumsum(.data$`chractabmr`) - .data$`chractabmr`,
#       hist_camr2 = cumsum(.data$`hist_camr`)) %>%
#     ungroup %>%
#     group_by(.data$`patient_id`) %>%
#     mutate(
#       hist_aamr1 = cumsum(.data$`activeabmr`) - .data$`activeabmr`,
#       hist_aamr2 = cumsum(.data$`hist_aamr`)) %>%
#     ungroup %>%
#     mutate(
#       hist_camr_calculated = ifelse(
#         .data$`hist_camr1` >= 1 | .data$`hist_camr2` >= 1, 1L, 0L),
#
#       hist_aamr_calculated = ifelse(
#         .data$`hist_aamr1` >= 1 | .data$`hist_aamr2` >= 1, 1L, 0L)
#     )
#
#
#   # hist.abmr variable is a future variable, which people input directly, instead of deriding data.
#   banff_diagnoses <-
#     banff_diagnoses %>%
#     mutate(
#
#       # ifelse((.data$hist.abmr == 1), 1L,
#       chrabmr3 = ifelse(.data$`hist_dsa_calculated`  == 1 |
#                           .data$`hist_camr_calculated` == 1 |
#                           .data$`hist_aamr_calculated` == 1,                 1L,0L),
#
#       chrabmr  = ifelse((.data$`camr11`   == 1 | .data$`camr12` == 1) &
#                           .data$`aamr2`    == 0 &
#                           .data$`chrabmr3` == 1,                            1L,0L),
#
#       c4d_only = ifelse(.data$`aamr21` == 1L &
#                           ( .data$`active_atcmr_code_3.TEMP` == 0 |
#                               .data$`active_atcmr_code_4.TEMP` == 0 |
#                               .data$`chronic_atcmr_code_4.TEMP` == 0 |
#                               .data$`activeabmr` == 0 |
#                               .data$`chractabmr` == 0 |
#                               .data$`chrabmr`    == 0 ) ,                      1L,0L)
#
#     )
#
#   # SEBT C4d staining with acute tubular injury (ATI)
#   # Create 2 variables: crossmatch (DSA positive / DSA negative) and abo_compatibility (ABO compatible / ABO incompatible)
#   banff_diagnoses <-
#     banff_diagnoses %>%
#     mutate(c4d_staining_atn = ifelse(.data$`atn` == 1 &
#                                        .data$`aamr21` == 1 &
#                                        .data$`aamr1` == 0 &
#                                        .data$`camr1` == 0, 1L,0L),
#            prob_amr = ifelse(.data$c4d_staining_atn == 1 &
#                                .data$`crossmatch` == 1 ~ 1L,0L),
#            accomodation = ifelse(.data$c4d_staining_atn == 1 &
#                                    .data$`abo_i` == 1 ~ 1L,0L))
#
#
#
#   # final abmr variable
#   # final suspicious abmr
#   banff_diagnoses <-
#     banff_diagnoses %>%
#     mutate(
#
#       final_abmr = case_when(
#         .data$`chractabmr` == 1  ~  4    ,
#         .data$`chrabmr`    == 1  ~  3    ,
#         .data$`activeabmr` == 1  ~  2    ,
#         .data$`c4d_only`   == 1  ~  1    ,
#         TRUE                   ~  NA),
#
#       final_susp_abmr = case_when(
#         .data$`susp_camr`         == 1  ~  2 ,
#         .data$`susp_c4dneg_camr`  == 1  ~  4 ,
#         .data$`susp_aamr`         == 1  ~  1 ,
#         .data$`susp_c4dneg_aamr`  == 1  ~  3 ,
#         TRUE                          ~  NA),
#
#       final_abmr_verified = case_when(
#         .data$`chractabmr`       == 1 ~ 6,
#         .data$`chrabmr`          == 1 ~ 4,
#         .data$`activeabmr`       == 1 ~ 3,
#         .data$`c4d_only`         == 1 ~ 1,
#         .data$`susp_camr`        == 1 ~ 5,
#         .data$`susp_c4dneg_camr` == 1 ~ 8,
#         .data$`susp_aamr`        == 1 ~ 2,
#         .data$`susp_c4dneg_aamr` == 1 ~ 7,
#         TRUE                        ~  NA)
#     )
#
#   banff_diagnoses <-
#     banff_diagnoses %>%
#     mutate(
#
#       diag_code_2 = case_when(
#         .data$`chractabmr`      == 1 ~ 5 ,
#         .data$`susp_chractabmr` == 1 ~ 4 ,
#         .data$`chrabmr`         == 1 ~ 6 ,
#         .data$`activeabmr`      == 1 ~ 3 ,
#         .data$`susp_activeaamr` == 1 ~ 2 ,
#         .data$`c4d_only`        == 1 ~ 1 ,
#         TRUE                 ~ NA),
#       diag_code_2_final_abmr      = ifelse(.data$`diag_code_2` %in% c(1,3,5,6), .data$`diag_code_2`,NA),
#       diag_code_2_final_susp_abmr = ifelse(.data$`diag_code_2` %in% c(2,4), .data$`diag_code_2`,NA)
#
#     ) %>%
#
#     mutate(
#
#       diag_code_4_active           = na_if(.data$`active_atcmr_code_4.TEMP` , 0),
#       diag_code_3                  =
#         case_when(
#
#           !is.na(.data$`diag_code_4_active`) ~ NA ,
#           TRUE ~ ifelse(.data$`active_atcmr_code_3.TEMP`  == 0,NA,1)),
#
#       diag_code_4_chronic          = na_if(.data$`chronic_atcmr_code_4.TEMP`, 0),
#       diag_code_5                  = .data$`ifta`,
#       diag_code_bk                 = ifelse(.data$`bk`    == 0,NA,1),
#       diag_code_ptld               = ifelse(.data$`ptld`  == 0,NA,2),
#       diag_code_cni                = ifelse(.data$`cni`   == 0,NA,3),
#       diag_code_atn                = ifelse(.data$`atn`   == 0,NA,4),
#       diag_code_cgn                = ifelse(.data$`cgn`   == 0,NA,5),
#       diag_code_agn                = ifelse(.data$`agn`   == 0,NA,6),
#       diag_code_infec              = ifelse(.data$`infec` == 0,NA,7),
#       diag_code_ain                = ifelse(.data$`ain`   == 0,NA,8))
#
#   banff_diagnoses <-
#     banff_diagnoses %>%
#     mutate(
#       diag_code_1 = ifelse(rowSums(is.na(select(
#         banff_diagnoses, starts_with("diag_code"),-"diag_code_5"))) ==
#           ncol(select(banff_diagnoses, starts_with("diag_code"),-"diag_code_5")), 1, NA))
#
#   banff_diagnoses <-
#     banff_diagnoses %>%
#     select(-contains('.TEMP'))
#
#   # replace adequacy by what it was originally
#   banff_diagnoses <-
#     banff_diagnoses %>%
#     bind_cols(adequacy_input_copy) %>%
#     mutate(adequacy = .data$`adequacy_input`) %>%
#     select(-"adequacy_input")
#
#   return(banff_diagnoses)
#
# }
#
# #' @title
# #' Calculate adequacy of each biopsy from glomeruli and arteries variables
# #'
# #' @description
# #' This function calculates adequacy of each biopsy (i.e., each observation)
# #' based on glomeruli and arteries variables (if provided).
# #'
# #' @param banff_dataset A tibble object.
# #'
# #' @return
# #' A tibble object with two variables: the calculated adequacy
# #' (adequacy_calculated) and the adequacy specified in input (adequacy_input).
# #'
# #' @examples
# #' {
# #'
# #' banff_dataset <- get_banff_template()
# #' calculate_adequacy(banff_dataset)
# #'
# #' }
# #'
# #' @import dplyr
# #' @importFrom rlang .data
# #' @export
# calculate_adequacy <- function(banff_dataset) {
#
#   banff_dataset <-
#     banff_dataset %>%
#     mutate(
#       adequacy_calculated = case_when(
#         !is.na(.data$`glomeruli`) &  is.na(.data$`arteries`)  ~ .data$`adequacy`,
#         is.na(.data$`glomeruli`) & !is.na(.data$`arteries`)  ~ .data$`adequacy`,
#         !is.na(.data$`glomeruli`) & !is.na(.data$`arteries`)  ~
#           ifelse((.data$`glomeruli` == 2 & .data$`arteries` == 2), 1 ,
#                  ifelse((.data$`glomeruli` == 0 & .data$`arteries` == 1), 2 , 3)),
#         TRUE                                  ~ .data$`adequacy`))
#
#   adequacy_input_copy <-
#     banff_dataset %>%
#     select("adequacy","adequacy_calculated") %>%
#     mutate(
#       adequacy_input = .data$`adequacy`,
#       adequacy = .data$`adequacy_calculated`) %>%
#     select(-"adequacy")
#
#   return(adequacy_input_copy)
#
# }
#
#
# # ------ original --------------------------------------------------------------
#
#
# #' @title
# #' Add diagnoses to the input dataset
# #'
# #' @description
# #' This function takes a dataset and returns a diagnosis for each
# #' observation. For the function to run, the dataset must not contain any errors
# #' that [banff_launcher()] would have detected. Please prefer using
# #' [banff_launcher()] to run additional tests.
# #'
# #' @param banff_dataset A tibble object.
# #'
# #' @return
# #' A tibble object that contains additional variables with the diagnoses results.
# #'
# #' @examples
# #' {
# #'
# #' banff_dataset <- get_banff_template()
# #' add_diagnoses(banff_dataset)
# #'
# #' }
# #'
# #' @import dplyr
# #' @importFrom rlang .data
# #' @export
# add_diagnoses <- function(banff_dataset) {
#
#   # check input
#   banff_assessment <- suppressMessages(banff_dataset_evaluate(banff_dataset))
#
#   if(!is.null(attributes(banff_assessment)$error)){
#     message(attributes(banff_assessment)$error)
#     message("
# Use `banff_dataset_evaluate(banff_dataset)` to help you correcting your file.\n")
#     stop(call. = TRUE)
#   }
#
#   banff_diagnoses <- banff_dataset
#
#
#   ## creation of adequacy calculated
#   adequacy_input_copy <- calculate_adequacy(banff_dataset)
#   banff_dataset <-
#     banff_dataset %>%
#     mutate(
#       adequacy = adequacy_input_copy$`adequacy_calculated`)
#
#   banff_diagnoses <-
#     banff_diagnoses %>%
#     mutate(
#
#       nogn =
#         case_when(
#           .data$`agn` + .data$`cgn` == 0 ~ 0L,
#           TRUE ~ 1L),
#
#       ifta = pmax(.data$`ci_score`,.data$`ct_score`),
#       c4d =
#         case_when(
#           .data$`c4d_if` %in% c(0,1) & .data$`c4d_ihc` == 0 ~ 0L,
#           TRUE ~ 1L)
#     )
#
#   # atcmr variables
#   banff_diagnoses <-
#     banff_diagnoses %>%
#     mutate(
#       active_atcmr_code_3.TEMP =
#         case_when(
#           .data$`i_score` %in% c(2,3) & .data$`t_score` == 1          ~ 1L  ,  # 'BLD_a'
#           .data$`i_score` == 1        & .data$`t_score` %in% c(1,2,3) ~ 2L  ,  # 'BLD_i1_a'
#           TRUE                                                        ~ 0L) ,
#
#       active_atcmr_code_4.TEMP =
#         case_when(
#           .data$`v_score` == 3                                        ~ 5L  ,  # 'III_a'
#           .data$`v_score` == 2                                        ~ 4L  ,  # 'IIB_a'
#           .data$`v_score` == 1                                        ~ 3L  ,  # 'IIA_a'
#           .data$`i_score` %in% c(2,3) & .data$`t_score` == 3          ~ 2L  ,  # 'IB_a'
#           .data$`i_score` %in% c(2,3) & .data$`t_score` == 2          ~ 1L  ,  # 'IA_a'
#           TRUE                                                        ~ 0L
#         ))
#
#
#   banff_diagnoses <-
#     banff_diagnoses %>%
#     mutate(
#       chronic_atcmr_code_4.TEMP =
#         case_when(
#           .data$`monofibneoint` == 1                              ~ 8L  ,   # "II_c",
#           (.data$`bk`            == 0        &
#              .data$`infec`         == 0        &
#              .data$`i_ifta_score`  %in% c(2,3) &
#              .data$`ti_score`      %in% c(2,3) &
#              .data$`t_score`       == 3)                             ~ 7L  ,   # "IB_c",
#           (.data$`bk`            == 0        &
#              .data$`infec`         == 0        &
#              .data$`i_ifta_score`  %in% c(2,3) &
#              .data$`ti_score`      %in% c(2,3) &
#              .data$`t_score`       == 2)                             ~ 6L  ,   # "IA_c",
#           TRUE                                                     ~ 0L)
#     )
#
#   # aamr1
#   banff_diagnoses <-
#     banff_diagnoses %>%
#     mutate(
#       aamr11.1 = ifelse((.data$`active_atcmr_code_3.TEMP` + .data$`active_atcmr_code_4.TEMP`) == 0 &
#                           .data$`ptc_score` > 0,                            1L, 0L),
#
#       aamr11.2 = ifelse(.data$`g_score` > 0 &
#                           .data$`cgn` == 0 &
#                           .data$`agn` == 0,                                 1L, 0L),
#       aamr12  = ifelse((.data$`active_atcmr_code_3.TEMP` + .data$`active_atcmr_code_4.TEMP`) > 0 &
#                          .data$`g_score` + .data$`ptc_score` >= 2 &
#                          .data$`g_score` >= 1,                             1L, 0L),
#       aamr13  = ifelse( .data$`v_score` > 0,                              1L, 0L),
#       aamr14  = ifelse((.data$`atma` + .data$`atn`) > 0,                    1L, 0L),
#       aamr1   = ifelse( .data$`aamr11.1` == 1 |
#                           .data$`aamr11.2` == 1 |
#                           .data$`aamr12`   == 1 |
#                           .data$`aamr13`   == 1 |
#                           .data$`aamr14`   == 1,                            1L, 0L),
#
#     )
#
#   # The default for aamr22 is 0 (which take into consideration when both GN and (atcmr_bl or atcmr) are present)
#   # G + PTC must be at least 2 when GN is absent and atcmr_bl or atcmr also absent
#   # PTC must be at least 2 when GN is present and atcmr_bl or atcmr also absent
#   # G + PTC must be at least 2, and G must be at least 1, when GN is absent and atcmr_bl or atcmr is present
#   # activeabmr definition below because it must take camr into consideration (to verify) !!!!
#   # need to be double checked
#
#   banff_diagnoses <-
#     banff_diagnoses %>%
#     mutate(
#       aamr21    = ifelse(.data$`c4d_if`  %in% c(2,3) |
#                            .data$`c4d_ihc` %in% c(1,2,3),  1L, 0L),
#
#       aamr22.1   = ifelse(.data$`g_score` + .data$`ptc_score` >= 2 &
#                             .data$`cgn`   == 0 &
#                             .data$`agn`   == 0 &
#                             (.data$`active_atcmr_code_3.TEMP` + .data$`active_atcmr_code_4.TEMP`) == 0,            1L, 0L),
#
#       aamr22.2   = ifelse(.data$`ptc_score` >= 2 &
#                             (.data$`cgn` == 1 | .data$`agn` == 1) &
#                             (.data$`active_atcmr_code_3.TEMP` + .data$`active_atcmr_code_4.TEMP`) == 0,             1L, 0L),
#
#       aamr22.3   = ifelse(.data$`g_score` + .data$`ptc_score` >= 2 &
#                             .data$`g_score` >=1 &
#                             .data$`cgn` == 0 &
#                             .data$`agn` == 0 &
#                             (.data$`active_atcmr_code_3.TEMP` + .data$`active_atcmr_code_4.TEMP`) > 0,              1L, 0L),
#
#       aamr2     = ifelse(.data$`aamr21` == 1 |
#                            .data$`aamr22.1` == 1 |
#                            .data$`aamr22.2` == 1 |
#                            .data$`aamr22.3` == 1,                             1L, 0L))
#
#   banff_diagnoses <-
#     banff_diagnoses %>%
#     mutate(
#
#       aamr      = ifelse(.data$`aamr1` == 1 &
#                            .data$`aamr2` == 1 &
#                            .data$`dsa` == 1,                                 1L, 0L),
#
#       susp_aamr = ifelse(.data$`aamr` == 0 &
#                            .data$`aamr1` == 1 &
#                            .data$`aamr2` == 1 &
#                            .data$`dsa` != 1,                                 1L, 0L),
#
#       c4dneg_aamr = ifelse(.data$`dsa` == 1 &
#                              (.data$`aamr22.1` == 1 |
#                                 .data$`aamr22.2` == 1 |
#                                 .data$`aamr22.3` == 1) &
#                              .data$`aamr21` == 0L,                           1L, 0L),
#
#       # the previous conditon is used as .data$g_score + .data$ptc_score >= 2
#       susp_c4dneg_aamr = ifelse(.data$`c4dneg_aamr` == 0 &
#                                   (.data$`aamr22.1` == 1 |
#                                      .data$`aamr22.2` == 1 |
#                                      .data$`aamr22.3` == 1) &
#                                   .data$`aamr21` == 0 &
#                                   .data$`dsa` != 1,                         1L, 0L),
#
#       dsaneg_aamr = ifelse(.data$`aamr1` == 1 &
#                              .data$`aamr21` == 1 &
#                              .data$`dsa` == 0,                              1L, 0L),
#
#       susp_activeaamr  = ifelse(.data$`susp_aamr` == 1 |
#                                   .data$`susp_c4dneg_aamr` == 1,            1L, 0L)
#     )
#
#
#
#
#   # define historical tcmr based any previous catcmr or atcmr record
#   # tcmr historical computation
#   # ordering by transplant date and biopsy date
#
#   banff_diagnoses <-
#     banff_diagnoses %>%
#     arrange(.data$`patient_id`, .data$`sc_date_bx`) %>%
#     group_by(.data$`patient_id`) %>%
#     mutate(
#       hist_dsa1 = cumsum(.data$`dsa`) - .data$`dsa`,
#       hist_dsa2 = cumsum(.data$`hist_dsa`)) %>%
#     ungroup %>%
#     group_by(.data$`patient_id`) %>%
#     mutate(
#       hist_tcmr1 = cumsum(.data$`active_atcmr_code_4.TEMP`) - .data$`active_atcmr_code_4.TEMP`,
#       hist_tcmr2 = cumsum(.data$`chronic_atcmr_code_4.TEMP`)- .data$`chronic_atcmr_code_4.TEMP`,
#       hist_tcmr3 = cumsum(.data$`hist_tcmr`)) %>%
#     ungroup %>%
#     mutate(
#       hist_dsa_calculated  = ifelse(.data$`hist_dsa1`  >= 1L |
#                                       .data$`hist_dsa2`  == 1L        , 1L, 0L),
#
#       hist_tcmr_calculated = ifelse(.data$`hist_tcmr1` >= 1L |
#                                       .data$`hist_tcmr2` >= 1L |
#                                       .data$`hist_tcmr3` >= 1L        , 1L, 0L))
#
#   # Check with Jan chronic vs chronic active (camr11 + camr12 vs camr1 = camr11 + camr12 + camr13)
#
#   banff_diagnoses <-
#     banff_diagnoses %>%
#     mutate(
#
#       camr11 = ifelse(.data$`cg_score`    != "0" &
#                         .data$`ctma`        ==  0  &
#                         .data$`cgn`         ==  0  &
#                         .data$`agn`         ==  0  ,    1L, 0L),
#
#       camr12 = ifelse(.data$`sptcbmml`    == 1L,      1L, 0L),
#
#       camr13 = case_when(
#         .data$`leuscint` == 1 & .data$`hist_tcmr_calculated` == 0L  ~ 1L,
#         .data$`newaif`   == 1                                       ~ 1L,
#         TRUE                                                        ~ 0L)
#     )
#
#   banff_diagnoses <-
#     banff_diagnoses %>%
#     mutate(
#
#       camr1 = ifelse(.data$`camr11` == 1L |
#                        .data$`camr12` == 1L |
#                        .data$`camr13` == 1L,                          1L, 0L),
#
#       activeabmr = ifelse(
#         (.data$`aamr` == 1 |
#            .data$`c4dneg_aamr` == 1 |
#            .data$`dsaneg_aamr` == 1) &
#           .data$`camr1` == 0,                                         1L, 0L),
#
#       camr = ifelse(.data$`camr1` == 1 &
#                       .data$`aamr2` == 1 &
#                       (.data$`dsa` == 1 | .data$`aamr21` == 1),          1L, 0L),
#
#       susp_camr = ifelse(
#         .data$`camr` == 0 &
#           .data$`camr1` == 1 &
#           .data$`aamr2` == 1 &
#           .data$`dsa` != 1 &
#           .data$`aamr21` != 1,                                        1L, 0L)
#
#     )
#
#   ## C4d negative aamr
#   # removed | banff_diagnoses$dsaneg_camr == 1 because it is captured by the camr definition
#   banff_diagnoses <-
#     banff_diagnoses %>%
#     mutate(
#
#       c4dneg_camr      = ifelse(.data$`camr1`      == 1  &
#                                   .data$`dsa`        == 1 &
#                                   (.data$`aamr22.1`   == 1 |
#                                      .data$`aamr22.2` == 1 |
#                                      .data$`aamr22.3` == 1) &
#                                   .data$`aamr21`     == 0L,                 1L, 0L),
#
#       susp_c4dneg_camr = ifelse(.data$`c4dneg_camr` == 0 &
#                                   .data$`camr1`       == 1 &
#                                   (.data$`aamr22.1`    == 1 |
#                                      .data$`aamr22.2`  == 1 |
#                                      .data$`aamr22.3`  == 1) &
#                                   .data$`aamr21`      == 0L &
#                                   .data$`dsa`         != 1,                 1L, 0L),
#
#       dsaneg_camr      = ifelse(.data$`camr1` == 1 &
#                                   .data$`aamr21` == 1 &
#                                   .data$`dsa` == 0,                          1L, 0L),
#
#       chractabmr       = ifelse(.data$`camr` == 1 |
#                                   .data$`c4dneg_camr` == 1,                  1L, 0L),
#
#       susp_chractabmr  = ifelse(.data$`susp_camr` == 1 |
#                                   .data$`susp_c4dneg_camr` == 1,             1L, 0L)
#
#     )
#
#   # chronic abmr variables
#   # camr and aamr historical computation
#   banff_diagnoses <-
#     banff_diagnoses %>%
#     arrange(.data$`patient_id`, .data$`sc_date_bx`) %>%
#     group_by(.data$`patient_id`) %>%
#     mutate(
#       hist_camr1 = cumsum(.data$`chractabmr`) - .data$`chractabmr`,
#       hist_camr2 = cumsum(.data$`hist_camr`)) %>%
#     ungroup %>%
#     group_by(.data$`patient_id`) %>%
#     mutate(
#       hist_aamr1 = cumsum(.data$`activeabmr`) - .data$`activeabmr`,
#       hist_aamr2 = cumsum(.data$`hist_aamr`)) %>%
#     ungroup %>%
#     mutate(
#       hist_camr_calculated = ifelse(
#         .data$`hist_camr1` >= 1 | .data$`hist_camr2` >= 1, 1L, 0L),
#
#       hist_aamr_calculated = ifelse(
#         .data$`hist_aamr1` >= 1 | .data$`hist_aamr2` >= 1, 1L, 0L)
#     )
#
#
#   # hist.abmr variable is a future variable, which people input directly, instead of deriding data.
#   banff_diagnoses <-
#     banff_diagnoses %>%
#     mutate(
#
#       # ifelse((.data$hist.abmr == 1), 1L,
#       chrabmr3 = ifelse(.data$`hist_dsa_calculated`  == 1 |
#                           .data$`hist_camr_calculated` == 1 |
#                           .data$`hist_aamr_calculated` == 1,                 1L,0L),
#
#       chrabmr  = ifelse((.data$`camr11`   == 1 | .data$`camr12` == 1) &
#                           .data$`aamr2`    == 0 &
#                           .data$`chrabmr3` == 1,                            1L,0L),
#
#       c4d_only = ifelse(.data$`aamr21` == 1L &
#                           ( .data$`active_atcmr_code_3.TEMP` == 0 |
#                               .data$`active_atcmr_code_4.TEMP` == 0 |
#                               .data$`chronic_atcmr_code_4.TEMP` == 0 |
#                               .data$`activeabmr` == 0 |
#                               .data$`chractabmr` == 0 |
#                               .data$`chrabmr`    == 0 ) ,                      1L,0L)
#
#     )
#
#   # final abmr variable
#   # final suspicious abmr
#   banff_diagnoses <-
#     banff_diagnoses %>%
#     mutate(
#
#       final_abmr = case_when(
#         .data$`chractabmr` == 1  ~  4    ,
#         .data$`chrabmr`    == 1  ~  3    ,
#         .data$`activeabmr` == 1  ~  2    ,
#         .data$`c4d_only`   == 1  ~  1    ,
#         TRUE                   ~  NA),
#
#       final_susp_abmr = case_when(
#         .data$`susp_camr`         == 1  ~  2 ,
#         .data$`susp_c4dneg_camr`  == 1  ~  4 ,
#         .data$`susp_aamr`         == 1  ~  1 ,
#         .data$`susp_c4dneg_aamr`  == 1  ~  3 ,
#         TRUE                          ~  NA),
#
#       final_abmr_verified = case_when(
#         .data$`chractabmr`       == 1 ~ 6,
#         .data$`chrabmr`          == 1 ~ 4,
#         .data$`activeabmr`       == 1 ~ 3,
#         .data$`c4d_only`         == 1 ~ 1,
#         .data$`susp_camr`        == 1 ~ 5,
#         .data$`susp_c4dneg_camr` == 1 ~ 8,
#         .data$`susp_aamr`        == 1 ~ 2,
#         .data$`susp_c4dneg_aamr` == 1 ~ 7,
#         TRUE                        ~  NA)
#     )
#
#   banff_diagnoses <-
#     banff_diagnoses %>%
#     mutate(
#
#       diag_code_2 = case_when(
#         .data$`chractabmr`      == 1 ~ 5 ,
#         .data$`susp_chractabmr` == 1 ~ 4 ,
#         .data$`chrabmr`         == 1 ~ 6 ,
#         .data$`activeabmr`      == 1 ~ 3 ,
#         .data$`susp_activeaamr` == 1 ~ 2 ,
#         .data$`c4d_only`        == 1 ~ 1 ,
#         TRUE                 ~ NA),
#       diag_code_2_final_abmr      = ifelse(.data$`diag_code_2` %in% c(1,3,5,6), .data$`diag_code_2`,NA),
#       diag_code_2_final_susp_abmr = ifelse(.data$`diag_code_2` %in% c(2,4), .data$`diag_code_2`,NA)
#
#     ) %>%
#
#     mutate(
#
#       diag_code_4_active           = na_if(.data$`active_atcmr_code_4.TEMP` , 0),
#       diag_code_3                  =
#         case_when(
#
#           !is.na(.data$`diag_code_4_active`) ~ NA ,
#           TRUE ~ ifelse(.data$`active_atcmr_code_3.TEMP`  == 0,NA,1)),
#
#       diag_code_4_chronic          = na_if(.data$`chronic_atcmr_code_4.TEMP`, 0),
#       diag_code_5                  = .data$`ifta`,
#       diag_code_bk                 = ifelse(.data$`bk`    == 0,NA,1),
#       diag_code_ptld               = ifelse(.data$`ptld`  == 0,NA,2),
#       diag_code_cni                = ifelse(.data$`cni`   == 0,NA,3),
#       diag_code_atn                = ifelse(.data$`atn`   == 0,NA,4),
#       diag_code_cgn                = ifelse(.data$`cgn`   == 0,NA,5),
#       diag_code_agn                = ifelse(.data$`agn`   == 0,NA,6),
#       diag_code_infec              = ifelse(.data$`infec` == 0,NA,7),
#       diag_code_ain                = ifelse(.data$`ain`   == 0,NA,8))
#
#   banff_diagnoses <-
#     banff_diagnoses %>%
#     mutate(
#       diag_code_1 = ifelse(rowSums(is.na(select(
#         banff_diagnoses, starts_with("diag_code"),-"diag_code_5"))) ==
#           ncol(select(banff_diagnoses, starts_with("diag_code"),-"diag_code_5")), 1, NA))
#
#   banff_diagnoses <-
#     banff_diagnoses %>%
#     select(-contains('.TEMP'))
#
#   # replace adequacy by what it was originally
#   banff_diagnoses <-
#     banff_diagnoses %>%
#     bind_cols(adequacy_input_copy) %>%
#     mutate(adequacy = .data$`adequacy_input`) %>%
#     select(-"adequacy_input")
#
#   return(banff_diagnoses)
#
# }
#
# #' @title
# #' Calculate adequacy of each biopsy from glomeruli and arteries variables
# #'
# #' @description
# #' This function calculates adequacy of each biopsy (i.e., each observation)
# #' based on glomeruli and arteries variables (if provided).
# #'
# #' @param banff_dataset A tibble object.
# #'
# #' @return
# #' A tibble object with two variables: the calculated adequacy
# #' (adequacy_calculated) and the adequacy specified in input (adequacy_input).
# #'
# #' @examples
# #' {
# #'
# #' banff_dataset <- get_banff_template()
# #' calculate_adequacy(banff_dataset)
# #'
# #' }
# #'
# #' @import dplyr
# #' @importFrom rlang .data
# #' @export
# calculate_adequacy <- function(banff_dataset) {
#
#   banff_dataset <-
#     banff_dataset %>%
#     mutate(
#       adequacy_calculated = case_when(
#         !is.na(.data$`glomeruli`) &  is.na(.data$`arteries`)  ~ .data$`adequacy`,
#         is.na(.data$`glomeruli`) & !is.na(.data$`arteries`)  ~ .data$`adequacy`,
#         !is.na(.data$`glomeruli`) & !is.na(.data$`arteries`)  ~
#           ifelse((.data$`glomeruli` == 2 & .data$`arteries` == 2), 1 ,
#                  ifelse((.data$`glomeruli` == 0 & .data$`arteries` == 1), 2 , 3)),
#         TRUE                                  ~ .data$`adequacy`))
#
#   adequacy_input_copy <-
#     banff_dataset %>%
#     select("adequacy","adequacy_calculated") %>%
#     mutate(
#       adequacy_input = .data$`adequacy`,
#       adequacy = .data$`adequacy_calculated`) %>%
#     select(-"adequacy")
#
#   return(adequacy_input_copy)
#
# }
#
#
#
#
#
#
#
#
#
#
#
#
