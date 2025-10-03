## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(cesR)

## -----------------------------------------------------------------------------
get_cescodes()

## ---- eval = F----------------------------------------------------------------
#  get_ces("ces2019_web")
#  
#  head(ces2019_web)
#  # A tibble: 6 x 620
#    cps19_StartDate     cps19_EndDate       cps19_ResponseId            cps19_consent
#    <dttm>              <dttm>              <chr>                           <dbl+lbl>
#  1 2019-09-13 08:09:44 2019-09-13 08:36:19 R_1OpYXEFGzHRUpjM 1 [I consent to partic~
#  2 2019-09-13 08:39:09 2019-09-13 08:57:06 R_2qdrL3J618rxYW0 1 [I consent to partic~
#  3 2019-09-13 10:01:19 2019-09-13 10:27:29 R_USWDAPcQEQiMmNb 1 [I consent to partic~
#  4 2019-09-13 10:05:37 2019-09-13 10:50:53 R_3IQaeDXy0tBzEry 1 [I consent to partic~
#  5 2019-09-13 10:05:52 2019-09-13 10:32:53 R_27WeMQ1asip2cMD 1 [I consent to partic~
#  6 2019-09-13 10:10:20 2019-09-13 10:29:45 R_3LiGZcCWJEcWV4P 1 [I consent to partic~
#  # ... with 616 more variables: cps19_citizenship <dbl+lbl>, cps19_yob <dbl+lbl>,
#  #   cps19_yob_2001_age <dbl+lbl>, cps19_gender <dbl+lbl>,
#  #   cps19_province <dbl+lbl>, cps19_education <dbl+lbl>, cps19_demsat <dbl+lbl>,
#  #   cps19_imp_iss <chr>, cps19_imp_iss_party <dbl+lbl>,
#  #   cps19_imp_iss_party_7_TEXT <chr>, cps19_imp_loc_iss <chr>,
#  #   cps19_imp_loc_iss_p <dbl+lbl>, cps19_imp_loc_iss_p_7_TEXT <chr>,
#  #   cps19_interest_gen_1 <dbl>, cps19_interest_elxn_1 <dbl>, ...

## ---- eval = F----------------------------------------------------------------
#  colnames(ces2019_web)
#  
#  get_question("ces2019_web", "cps19_province")
#  Which province or territory are you currently living in?

## ---- eval = F----------------------------------------------------------------
#  get_preview("ces2019_web", 10)
#  # A tibble: 10 x 620
#     cps19_StartDate     cps19_EndDate       cps19_ResponseId  cps19_consent
#     <dttm>              <dttm>              <chr>             <fct>
#   1 2019-09-13 08:09:44 2019-09-13 08:36:19 R_1OpYXEFGzHRUpjM I consent to particip~
#   2 2019-09-13 08:39:09 2019-09-13 08:57:06 R_2qdrL3J618rxYW0 I consent to particip~
#   3 2019-09-13 10:01:19 2019-09-13 10:27:29 R_USWDAPcQEQiMmNb I consent to particip~
#   4 2019-09-13 10:05:37 2019-09-13 10:50:53 R_3IQaeDXy0tBzEry I consent to particip~
#   5 2019-09-13 10:05:52 2019-09-13 10:32:53 R_27WeMQ1asip2cMD I consent to particip~
#   6 2019-09-13 10:10:20 2019-09-13 10:29:45 R_3LiGZcCWJEcWV4P I consent to particip~
#   7 2019-09-13 10:14:47 2019-09-13 10:32:32 R_1Iu8R1UlYzVMycz I consent to particip~
#   8 2019-09-13 10:15:39 2019-09-13 10:30:59 R_2EcS26hqrcVYlab I consent to particip~
#   9 2019-09-13 10:15:48 2019-09-13 10:37:45 R_3yrt44wqQ1d4VRn I consent to particip~
#  10 2019-09-13 10:16:08 2019-09-13 10:40:14 R_10OBmXJyvn8feYQ I consent to particip~
#  # ... with 616 more variables: cps19_citizenship <fct>, cps19_yob <fct>,
#  #   cps19_yob_2001_age <fct>, cps19_gender <fct>, cps19_province <fct>,
#  #   cps19_education <fct>, cps19_demsat <fct>, cps19_imp_iss <chr>,
#  #   cps19_imp_iss_party <fct>, cps19_imp_iss_party_7_TEXT <chr>,
#  #   cps19_imp_loc_iss <chr>, cps19_imp_loc_iss_p <fct>,
#  #   cps19_imp_loc_iss_p_7_TEXT <chr>, cps19_interest_gen_1 <dbl>,
#  #   cps19_interest_elxn_1 <dbl>, cps19_v_likely <fct>, ...

## ---- eval = F----------------------------------------------------------------
#  get_decon()
#  
#  head(decon)
#  # A tibble: 6 x 52
#    ces_code    citizenship      yob   gender  province_territory education     vote_likely
#    <chr>       <fct>            <fct> <fct>   <fct>              <fct>         <fct>
#  1 ces2019_web Canadian citizen 1989  A woman Quebec             Master's deg~ Certain to~
#  2 ces2019_web Canadian citizen 1998  A woman Quebec             Master's deg~ Certain to~
#  3 ces2019_web Canadian citizen 2000  A woman Ontario            Some univers~ Certain to~
#  4 ces2019_web Canadian citizen 1998  A man   Ontario            Some univers~ Certain to~
#  5 ces2019_web Canadian citizen 2000  A woman Ontario            Completed se~ Certain to~
#  6 ces2019_web Canadian citizen 1999  A woman Ontario            Some univers~ Certain to~
#  # ... with 45 more variables: vote_likely_ifable <fct>, votechoice <fct>,
#  #   votechoice_text <chr>, votechoice_couldvote <fct>, votechoice_couldvote_text <chr>,
#  #   vote_unlikely <fct>, vote_unlikely_text <chr>, vote_unlikely_couldvote <fct>,
#  #   vote_unlikely_couldvote_text <chr>, vote_advancevote_choice <fct>,
#  #   vote_advancevote_choice_text <chr>, vote_partylean <fct>, vote_partylean_text <chr>,
#  #   vote_partylean_couldvote <fct>, vote_partylean_couldvote_text <chr>,
#  #   votechoice_secondchoice <fct>, votechoice_secondchoice_text <chr>, ...

