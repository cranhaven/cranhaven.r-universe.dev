#' NHANES 2017-2018: Demographics and Blood Pressure
#'
#' A merged dataset from the National Health and Nutrition Examination Survey
#' (NHANES) 2017-2018 cycle, combining demographic characteristics with blood
#' pressure measurements. Covers all 9,254 sampled participants; blood pressure
#' variables are `NA` for the 550 interview-only participants (`ridstatr == 1`).
#'
#' @format A data frame with 9,254 rows and 14 variables:
#' \describe{
#'   \item{seqn}{Respondent sequence number (unique identifier, join key).}
#'   \item{sdmvpsu}{Masked variance pseudo-PSU. Use as the cluster ID for
#'     variance estimation. See Details.}
#'   \item{sdmvstra}{Masked variance pseudo-stratum. Use as the stratification
#'     variable for variance estimation. See Details.}
#'   \item{wtmec2yr}{Full-sample 2-year MEC examination weight. Use for any
#'     analysis involving examination measurements (e.g., blood pressure).}
#'   \item{wtint2yr}{Full-sample 2-year interview weight. Use for analyses
#'     based on interview data only.}
#'   \item{ridstatr}{Interview/examination status: `1` = interview only,
#'     `2` = both interview and MEC examination.}
#'   \item{riagendr}{Gender: `1` = male, `2` = female.}
#'   \item{ridageyr}{Age in years at screening, top-coded at 80.}
#'   \item{ridreth3}{Race/Hispanic origin (6 categories): `1` = Mexican
#'     American, `2` = Other Hispanic, `3` = Non-Hispanic White,
#'     `4` = Non-Hispanic Black, `6` = Non-Hispanic Asian,
#'     `7` = Other/Multiracial.}
#'   \item{indfmpir}{Ratio of family income to the federal poverty level
#'     (continuous, 0–5; values >5 are top-coded at 5).}
#'   \item{dmdeduc2}{Education level for adults 20+: `1` = Less than 9th grade,
#'     `2` = 9th–11th grade, `3` = High school graduate/GED, `4` = Some
#'     college/AA, `5` = College graduate or above.}
#'   \item{bpxsy1}{Systolic blood pressure, 1st reading (mm Hg). `NA` if not
#'     examined.}
#'   \item{bpxdi1}{Diastolic blood pressure, 1st reading (mm Hg). `NA` if not
#'     examined.}
#'   \item{bpxpls}{60-second pulse rate (beats per minute). `NA` if not
#'     examined.}
#' }
#'
#' @details
#' **Survey design:** Taylor series linearization. When creating a survey
#' design object, use `sdmvpsu` as the cluster ID, `sdmvstra` as the stratum,
#' and `wtmec2yr` as the weight for examination-based analyses:
#'
#' ```r
#' svy <- as_survey(nhanes_2017,
#'   ids     = sdmvpsu,
#'   strata  = sdmvstra,
#'   weights = wtmec2yr
#' )
#' ```
#'
#' Use `wtint2yr` instead of `wtmec2yr` for interview-only variables
#' (e.g., income, education).
#'
#' **Metadata:**
#' All columns carry variable labels and value labels as R attributes,
#' automatically extracted into surveycore's metadata system when you call
#' `as_survey()`.
#'
#' - **Variable labels** (`"label"` attribute): A human-readable description of
#'   each column. Example: `attr(nhanes_2017$riagendr, "label")` returns
#'   `"Gender"`.
#' - **Value labels** (`"labels"` attribute): A named numeric vector mapping
#'   each code to its meaning. Example: `attr(nhanes_2017$riagendr, "labels")`
#'   returns `c(Male = 1, Female = 2)`.
#'
#' **Source files:** DEMO_J.xpt (demographics) merged with BPX_J.xpt (blood
#' pressure) on `seqn`. Prepared by `data-raw/download-nhanes.R`.
#'
#' @source
#' National Center for Health Statistics, CDC. NHANES 2017-2018 Continuous
#' Survey.
#' \url{https://www.cdc.gov/nchs/nhanes/}
#'
#' @examples
#' # All 9,254 participants (interview + exam)
#' head(nhanes_2017)
#'
#' # Restrict to exam participants for blood pressure analysis
#' exam_only <- nhanes_2017[nhanes_2017$ridstatr == 2, ]
#'
#' # Inspect variable label
#' attr(nhanes_2017$riagendr, "label")
#'
#' # Inspect value labels
#' attr(nhanes_2017$riagendr, "labels")
#'
#' # Inspect value labels for race/ethnicity
#' attr(nhanes_2017$ridreth3, "labels")
"nhanes_2017"


#' ACS PUMS 2022 1-Year: Wyoming Persons
#'
#' All person records from the 2022 American Community Survey (ACS) 1-Year
#' Public Use Microdata Sample (PUMS) for Wyoming (state FIPS 56). Wyoming is
#' the least-populous U.S. state, making this the smallest state-level PUMS
#' file — ideal for fast tests and examples.
#'
#' @format A data frame with 5,962 rows and 96 variables.
#' Columns `pwgtp1` through `pwgtp80` are the 80 successive difference
#' replicate weights for variance estimation; the remaining 16 variables are:
#'
#' - `puma`: Public Use Microdata Area code. Use as the cluster ID (PSU)
#'   for variance estimation.
#' - `st`: State FIPS code (all 56 = Wyoming).
#' - `pwgtp`: Person weight. Represents the number of people in the
#'   Wyoming population that this record represents.
#' - `agep`: Age (0--99 years).
#' - `sex`: Sex (1 = male, 2 = female).
#' - `rac1p`: Recoded detailed race (1 = White alone, 2 = Black or African
#'   American alone, 3 = American Indian alone, 6 = Asian alone,
#'   9 = Two or more races).
#' - `hisp`: Recoded Hispanic origin (01 = Not Spanish/Hispanic/Latino;
#'   02--24 = specific Hispanic origin).
#' - `schl`: Educational attainment (24 categories: 01 = no schooling,
#'   16 = regular high school diploma, 21 = bachelor's degree,
#'   24 = doctorate degree).
#' - `esr`: Employment status recode (1 = civilian employed at work,
#'   2 = civilian employed with job but not at work, 3 = unemployed,
#'   4 = Armed Forces at work, 5 = Armed Forces not at work,
#'   6 = Not in labor force).
#' - `pincp`: Total person income in the past 12 months (dollars, signed;
#'   negative values indicate a net loss). Multiply by `adjinc / 1e6` to
#'   adjust to constant dollars.
#' - `wagp`: Wages or salary income in the past 12 months (dollars).
#'   `NA` if not applicable.
#' - `hicov`: Health insurance coverage (1 = with health insurance,
#'   2 = without health insurance).
#' - `dis`: Disability recode (1 = with a disability, 2 = without a
#'   disability).
#' - `povpip`: Income-to-poverty ratio (0--501; 501 means 501% or more).
#' - `wkhp`: Usual hours worked per week in the past 12 months. `NA` if
#'   not in the labor force.
#' - `adjinc`: Adjustment factor for income and earnings. Divide by
#'   1,000,000 and multiply income variables to convert to 2022 constant
#'   dollars.
#'
#' @details
#' **Survey design:** Successive difference replication (SDR). Use
#' `as_survey_replicate()` with all 80 replicate weights:
#'
#' ```r
#' svy <- as_survey_replicate(
#'   acs_pums_wy,
#'   weights    = pwgtp,
#'   repweights = pwgtp1:pwgtp80,
#'   type       = "successive-difference"
#' )
#' ```
#'
#' **Income adjustment:** Income variables (`pincp`, `wagp`) are in survey-year
#' dollars. Multiply by `adjinc / 1e6` to convert to 2022 inflation-adjusted
#' dollars before comparing across ACS years.
#'
#' **Metadata:**
#' The ACS PUMS source is a plain CSV with no embedded labels. Columns in
#' `acs_pums_wy` carry no `"label"`, `"labels"`, or `"question_preface"`
#' attributes. Variable descriptions are documented here in `?acs_pums_wy` and
#' in `data-raw/README.md`. Use `set_var_label()` and
#' `set_val_labels()` to attach labels manually before analysis if needed.
#'
#' @source
#' U.S. Census Bureau. 2022 ACS 1-Year PUMS.
#' \url{https://www.census.gov/programs-surveys/acs/microdata/access.html}
#'
#' @examples
#' # Wyoming population represented
#' sum(acs_pums_wy$pwgtp)
#'
#' # Age distribution
#' hist(acs_pums_wy$agep, main = "Age distribution, Wyoming 2022", xlab = "Age")
#'
#' # Confirm 80 replicate weights are present
#' sum(grepl("^pwgtp[0-9]", names(acs_pums_wy)))
"acs_pums_wy"


#' ANES 2024: American National Election Studies Time Series
#'
#' A 19-variable extract from the 2024 American National Election Studies
#' (ANES) Time Series Study, a landmark biennial pre- and post-election survey
#' of the American electorate. Fielded via face-to-face interview and web
#' (n = 5,521). This extract uses the FTF + Web combined design variables
#' (`v240103a`–`v240103d`), the recommended set for most analyses.
#'
#' @format A data frame with 5,521 rows and 19 variables:
#' \describe{
#'   \item{v240103a}{Pre-election weight (FTF+Web combined). Use for
#'     variables asked before November 5, 2024.}
#'   \item{v240103b}{Post-election weight (FTF+Web combined). Use for
#'     variables asked after November 5, 2024.}
#'   \item{v240103c}{PSU (FTF+Web combined). Use as the cluster ID for
#'     variance estimation.}
#'   \item{v240103d}{Stratum (FTF+Web combined). Use as the stratification
#'     variable.}
#'   \item{v240001}{2024 Time Series Case ID. Unique respondent identifier.}
#'   \item{v240003}{Sample type: `1` = Panel, `2` = Fresh Web, `3` = Fresh
#'     FTF, `4` = GSS.}
#'   \item{v240002c}{Pre/Post interview completion: `1` = Pre-election only,
#'     `2` = Pre- and post-election.}
#'   \item{v243002}{State FIPS code.}
#'   \item{v243007}{Census region: `1` = Northeast, `2` = Midwest,
#'     `3` = South, `4` = West.}
#'   \item{v241458x}{Age on Election Day (summary). Top-coded at 80.
#'     `-2` = missing.}
#'   \item{v241550}{Sex: `1` = male, `2` = female.}
#'   \item{v241501x}{Race/ethnicity (5-category summary): White non-Hispanic,
#'     Black non-Hispanic, Hispanic, Asian/NHPI non-Hispanic,
#'     Other/Multiracial non-Hispanic.}
#'   \item{v241465x}{Education (5-category summary): `1` = less than HS,
#'     `2` = HS diploma, `3` = some college, `4` = bachelor's degree,
#'     `5` = graduate degree.}
#'   \item{v241566x}{Household income (28 categories from < $5,000 to
#'     $250,000+).}
#'   \item{v241177}{Liberal-conservative self-placement (7-point scale):
#'     `1` = extremely liberal, `7` = extremely conservative.
#'     `99` = haven't thought about this.}
#'   \item{v241222}{Party identification strength: `1` = strong,
#'     `2` = not very strong.}
#'   \item{v241223}{Party identification lean (Independents): `1` = closer to
#'     Republican, `2` = neither, `3` = closer to Democrat.}
#'   \item{v242066}{Did respondent vote for President (POST): `1` = yes,
#'     `2` = no.}
#'   \item{v242067}{Presidential vote choice (POST): `1` = Harris,
#'     `2` = Trump, `3` = RFK Jr., `4` = West, `5` = Stein, `6` = Other.}
#' }
#'
#' @details
#' **Survey design:** Stratified cluster — use Taylor series linearization.
#' Two weights are available depending on whether the analysis uses pre- or
#' post-election variables:
#'
#' ```r
#' # Pre-election analysis (party ID, ideology, candidate preference)
#' svy_pre <- as_survey(anes_2024,
#'   ids     = v240103c,
#'   strata  = v240103d,
#'   weights = v240103a,
#'   nest    = TRUE
#' )
#'
#' # Post-election analysis (validated vote choice)
#' svy_post <- as_survey(anes_2024,
#'   ids     = v240103c,
#'   strata  = v240103d,
#'   weights = v240103b,
#'   nest    = TRUE
#' )
#' ```
#'
#' **Missing value codes:** The ANES uses negative integer codes for missing
#' data throughout: `-9` = Refused, `-8` = Don't know, `-4` = Technical error,
#' `-1` = Inapplicable, and others. These must be recoded to `NA` before
#' analysis. Check `attr(anes_2024$v241177, "labels")` for the full set of
#' codes for a given variable.
#'
#' **Metadata:**
#' All columns carry variable labels and value labels as R attributes from the
#' original Stata file, automatically extracted into surveycore's metadata
#' system when you call `as_survey()`.
#'
#' - **Variable labels** (`"label"` attribute): A human-readable description of
#'   each column. Example: `attr(anes_2024$v241550, "label")` returns
#'   `"PRE: What is your sex?"` (or similar ANES phrasing).
#' - **Value labels** (`"labels"` attribute): A named numeric vector mapping
#'   each code to its meaning, including all missing-value codes. Example:
#'   `attr(anes_2024$v241550, "labels")` returns a vector with entries for
#'   `Male`, `Female`, and the applicable negative missing codes.
#'
#' @source
#' American National Election Studies. 2024 Time Series Study.
#' Available at electionstudies.org (free account required to download raw
#' data; the processed `.rda` is included in the package).
#' Prepared by `data-raw/prepare-anes-2024.R`.
#'
#' @examples
#' # Variables in the dataset
#' names(anes_2024)
#'
#' # Create pre-election design
#' svy <- as_survey(
#'   anes_2024,
#'   ids = v240103c,
#'   strata = v240103d,
#'   weights = v240103a,
#'   nest = TRUE
#' )
#'
#' # Inspect variable label (ANES uses opaque V-codes; labels give context)
#' attr(anes_2024$v241177, "label")
#'
#' # Inspect value labels, including missing-value codes
#' attr(anes_2024$v241177, "labels")
"anes_2024"


#' GSS 2024: General Social Survey
#'
#' A 27-variable extract from the 2024 General Social Survey (GSS), one of
#' the longest-running sociological surveys in the United States (fielded
#' annually or biennially since 1972). All 3,309 respondents from the 2024
#' cross-section are included.
#'
#' @format A data frame with 3,309 rows and 27 variables:
#' \describe{
#'   \item{vpsu}{Variance primary sampling unit. Use as the cluster ID for
#'     variance estimation.}
#'   \item{vstrat}{Variance stratum. Use as the stratification variable.}
#'   \item{wtssps}{Person post-stratification weight. Standard analysis
#'     weight.}
#'   \item{wtssnrps}{Person post-stratification weight adjusted for
#'     differential non-response. Preferred when non-response bias is a
#'     concern.}
#'   \item{id}{Respondent ID. Unique case identifier.}
#'   \item{year}{Survey year (all `2024` in this extract).}
#'   \item{ballot}{Ballot form (`A`, `B`, `C`, or `D`). The GSS uses a
#'     split-ballot design; not all questions appear on every ballot.
#'     Inapplicable items are coded `-100`.}
#'   \item{age}{Age in years (`89` = 89 or older).}
#'   \item{sex}{Sex: `1` = male, `2` = female.}
#'   \item{race}{Race: `1` = white, `2` = black, `3` = other.}
#'   \item{hispanic}{Hispanic origin: `1` = not Hispanic; `2`–`50` = specific
#'     Hispanic origin.}
#'   \item{educ}{Highest year of school completed (0–20 years).}
#'   \item{degree}{Highest degree: `0` = less than HS, `1` = high school,
#'     `2` = associate, `3` = bachelor's, `4` = graduate.}
#'   \item{income16}{Total family income (26 categories from < $1,000 to
#'     $170,000+).}
#'   \item{marital}{Marital status: `1` = married, `2` = widowed,
#'     `3` = divorced, `4` = separated, `5` = never married.}
#'   \item{wrkstat}{Labor force status: `1` = full time, `2` = part time,
#'     `3` = temporarily not working, `4` = unemployed, `5` = retired,
#'     `6` = in school, `7` = keeping house, `8` = other.}
#'   \item{hrs1}{Hours worked last week (for employed respondents only).}
#'   \item{adults}{Number of adults in household (`8` = 8 or more).}
#'   \item{partyid}{Party identification: `0` = strong Democrat,
#'     `3` = Independent, `6` = strong Republican, `7` = other party.}
#'   \item{polviews}{Political views: `1` = extremely liberal,
#'     `7` = extremely conservative.}
#'   \item{happy}{General happiness: `1` = very happy, `2` = pretty happy,
#'     `3` = not too happy.}
#'   \item{health}{Self-rated health: `1` = excellent, `2` = good,
#'     `3` = fair, `4` = poor.}
#'   \item{trust}{Social trust: `1` = most people can be trusted,
#'     `2` = can't be too careful, `3` = depends.}
#'   \item{natfare}{Government spending on welfare: `1` = too little,
#'     `2` = about right, `3` = too much.}
#'   \item{abany}{Abortion for any reason: `1` = yes, `2` = no.}
#'   \item{attend}{Religious service attendance: `0` = never,
#'     `8` = several times a week.}
#'   \item{relig}{Religious preference: `1` = Protestant, `2` = Catholic,
#'     `3` = Jewish, `4` = none, and others.}
#' }
#'
#' @details
#' **Survey design:** Stratified multi-stage cluster — use Taylor series
#' linearization:
#'
#' ```r
#' svy <- as_survey(gss_2024,
#'   ids     = vpsu,
#'   strata  = vstrat,
#'   weights = wtssps,      # or wtssnrps for non-response-adjusted weight
#'   nest    = TRUE
#' )
#' ```
#'
#' **Missing value codes:** The GSS uses a consistent system of negative
#' integer codes for missing data across all variables:
#'
#' | Code | Meaning |
#' |------|---------|
#' | `-100` | Inapplicable (question not asked of this respondent) |
#' | `-99` | No answer |
#' | `-98` | Don't know |
#' | `-97` | Skipped on web |
#' | `-90` | Refused |
#'
#' These codes are stored as value labels on every column (check
#' `attr(gss_2024$happy, "labels")`). Recode them to `NA` before analysis.
#'
#' **Split-ballot design:** The `ballot` variable indicates which question
#' module a respondent received. Variables asked only on some ballots will
#' have `-100` (Inapplicable) for respondents on other ballots.
#'
#' **Metadata:**
#' All columns carry variable labels and value labels as R attributes from the
#' original SPSS file, automatically extracted into surveycore's metadata
#' system when you call `as_survey()`.
#'
#' - **Variable labels** (`"label"` attribute): A human-readable description of
#'   each column. Example: `attr(gss_2024$happy, "label")` returns
#'   `"GENERAL HAPPINESS"`.
#' - **Value labels** (`"labels"` attribute): A named numeric vector mapping
#'   each code to its meaning, including all missing-value codes. Example:
#'   `attr(gss_2024$happy, "labels")` returns entries for `Very happy`,
#'   `Pretty happy`, `Not too happy`, and the negative missing codes.
#'
#' @source
#' NORC at the University of Chicago. General Social Survey 2024.
#' \url{https://gss.norc.org} (free account required to download raw data;
#' the processed `.rda` is included in the package).
#' Prepared by `data-raw/prepare-gss-2024.R`.
#'
#' @examples
#' # Variables in the dataset
#' names(gss_2024)
#'
#' # Create survey design
#' svy <- as_survey(
#'   gss_2024,
#'   ids = vpsu,
#'   strata = vstrat,
#'   weights = wtssps,
#'   nest = TRUE
#' )
#'
#' # Inspect variable label
#' attr(gss_2024$happy, "label")
#'
#' # Inspect value labels (includes GSS missing-value codes)
#' attr(gss_2024$happy, "labels")
#'
#' # Split-ballot: how many respondents per ballot form?
#' table(gss_2024$ballot)
"gss_2024"


#' Pew NPORS 2025: National Public Opinion Reference Survey
#'
#' The 2025 National Public Opinion Reference Survey (NPORS), conducted
#' February 5 – June 18, 2025, by Pew Research Center (n = 5,022). An
#' address-based sample (ABS) drawn from the USPS Computerized Delivery
#' Sequence File, with respondents completing the survey online, by paper,
#' or by telephone in English or Spanish. All 65 columns from the public
#' release file are retained.
#'
#' @format A data frame with 5,022 rows and 65 variables. The 11 `smuse_*`
#' variables form a battery asking about social media platform use and share a
#' `"question_preface"` attribute. All other variables are documented
#' individually below:
#' \describe{
#'   \item{respid}{Case ID. Unique respondent identifier.}
#'   \item{stratum}{Sampling stratum (10 levels, defined by census block
#'     group demographics).}
#'   \item{basewt}{Base weight — inverse probability of selection, with
#'     adaptive mode adjustment.}
#'   \item{weight}{Final weight — `basewt` after raking to Census population
#'     targets. Use for all population-level estimates.}
#'   \item{mode}{Data collection mode: `1` = Online, `2` = Paper,
#'     `3` = Phone.}
#'   \item{language}{Language interview completed in: `1` = English,
#'     `2` = Spanish.}
#'   \item{languageinitial}{Language interview started in.}
#'   \item{interview_start}{Interview start timestamp.}
#'   \item{interview_end}{Interview end timestamp.}
#'   \item{econ1mod}{Economic conditions in your community today
#'     (Excellent / Good / Fair / Poor).}
#'   \item{econ1bmod}{Economic conditions one year from now
#'     (Better / Worse / Same).}
#'   \item{comtype2}{Community type: Urban / Suburban / Rural.}
#'   \item{unity}{Americans united vs. divided on values.}
#'   \item{crimesafe}{Area safety in terms of crime (Extremely safe – Not at
#'     all safe).}
#'   \item{govprotct}{Government's role in protecting people from themselves.}
#'   \item{moregunimpact}{Impact of more gun ownership on crime.}
#'   \item{fin_sit}{Household financial situation (Comfortable – Can't meet
#'     basics).}
#'   \item{vet1}{Military service in household.}
#'   \item{vol12_cps}{Volunteered for any organization in past 12 months.}
#'   \item{eminuse}{Uses internet or email at least occasionally.}
#'   \item{intmob}{Accesses internet on a mobile device.}
#'   \item{intfreq}{Internet use frequency (6 categories).}
#'   \item{intfreq_collapsed}{Internet use frequency (4 categories, derived).}
#'   \item{home4nw2}{Subscribes to home internet service.}
#'   \item{bbhome}{Home internet type (dial-up, broadband, etc.).}
#'   \item{smuse_fb}{Facebook. Part of social media use battery (see Details).}
#'   \item{smuse_yt}{YouTube. Part of social media use battery (see Details).}
#'   \item{smuse_x}{X (formerly Twitter). Part of social media use battery.}
#'   \item{smuse_ig}{Instagram. Part of social media use battery.}
#'   \item{smuse_sc}{Snapchat. Part of social media use battery.}
#'   \item{smuse_wa}{WhatsApp. Part of social media use battery.}
#'   \item{smuse_tt}{TikTok. Part of social media use battery.}
#'   \item{smuse_rd}{Reddit. Part of social media use battery.}
#'   \item{smuse_bsk}{Bluesky. Part of social media use battery.}
#'   \item{smuse_th}{Threads. Part of social media use battery.}
#'   \item{smuse_ts}{Truth Social. Part of social media use battery.}
#'   \item{radio}{Listens to radio.}
#'   \item{device1a}{Has a cell phone.}
#'   \item{smart2}{Cell phone is a smartphone.}
#'   \item{nhisll}{Has a working landline telephone at home.}
#'   \item{relig}{Current religion (12 categories).}
#'   \item{religcat1}{Religion (4 categories: Protestant, Catholic,
#'     Unaffiliated, Other).}
#'   \item{born}{Born-again or evangelical Christian.}
#'   \item{attendper}{In-person religious service attendance (6 categories).}
#'   \item{attendonline2}{Online/TV religious service participation
#'     (6 categories).}
#'   \item{relimp}{Importance of religion in life (Very – Not at all).}
#'   \item{pray}{Prayer frequency outside of services (7 categories).}
#'   \item{educcat}{Education level (categorical).}
#'   \item{hisp}{Hispanic origin.}
#'   \item{racecmb}{Race (5 categories).}
#'   \item{racethn}{Race-ethnicity (5 categories including Asian
#'     non-Hispanic).}
#'   \item{agegrp}{Age in 13 five-year groups.}
#'   \item{agecat}{Age (4 categories: 18-29, 30-49, 50-64, 65+).}
#'   \item{birthplace}{U.S. born vs. foreign born.}
#'   \item{gender}{Gender (man / woman / other).}
#'   \item{adults}{Number of adults in household.}
#'   \item{inc_sdt1}{Total family income (8 categories from < $30,000 to
#'     $150,000+).}
#'   \item{cregion}{Census region (NE / MW / S / W).}
#'   \item{metro}{Metropolitan area indicator.}
#'   \item{registration}{Registered to vote at current address.}
#'   \item{party}{Party affiliation (Rep / Dem / Ind / Other).}
#'   \item{partyln}{Party lean for Independents (Rep / Dem).}
#'   \item{partysum}{Party summary (Rep+Lean Rep / Dem+Lean Dem / No lean).}
#'   \item{voted2024}{Voted in the 2024 presidential election.}
#'   \item{votegen_post}{2024 presidential vote choice (Trump / Harris /
#'     Other).}
#' }
#'
#' @details
#' **Survey design:** Stratified address-based sample with raking
#' post-stratification — use Taylor series linearization. NPORS has no PSU
#' (each address is its own unit, effectively a stratified SRS):
#'
#' ```r
#' svy <- as_survey(pew_npors_2025,
#'   strata  = stratum,
#'   weights = weight
#' )
#' ```
#'
#' Use `basewt` instead of `weight` for sensitivity analyses comparing
#' pre- and post-raking estimates.
#'
#' **Social media battery:** All 11 `smuse_*` variables share the question
#' stem `"Please indicate whether or not you ever use the following websites
#' or apps."` Values: `1` = Selected, `2` = Not selected, `99` = Refused.
#' Each variable additionally carries a `"question_preface"` attribute with
#' this shared stem.
#'
#' **Metadata:**
#' All columns carry variable labels and value labels as R attributes from the
#' original SPSS file. The 11 `smuse_*` battery variables additionally carry
#' a `"question_preface"` attribute with the shared question stem. All three
#' attribute types are automatically extracted into surveycore's metadata
#' system when you call `as_survey()`.
#'
#' - **Variable labels** (`"label"` attribute): A human-readable description of
#'   each column — for `smuse_*` variables this is just the platform name
#'   (e.g., `"Facebook"`). Example: `attr(pew_npors_2025$smuse_fb, "label")`
#'   returns `"Facebook"`.
#' - **Value labels** (`"labels"` attribute): A named numeric vector mapping
#'   each code to its meaning. Example:
#'   `attr(pew_npors_2025$smuse_fb, "labels")` returns
#'   `c(Selected = 1, "Not selected" = 2, Refused = 99)`.
#' - **Question preface** (`"question_preface"` attribute): The shared question
#'   stem for battery items, set on all `smuse_*` columns. Example:
#'   `attr(pew_npors_2025$smuse_fb, "question_preface")` returns
#'   `"Please indicate whether or not you ever use the following websites or
#'   apps."`.
#'
#' @source
#' Pew Research Center. 2025 National Public Opinion Reference Survey.
#' \url{https://www.pewresearch.org/datasets/} (free account required to
#' download raw data; the processed `.rda` is included in the package).
#' Prepared by `data-raw/prepare-pew-npors-2025.R`.
#'
#' @examples
#' # Variables in the dataset
#' names(pew_npors_2025)
#'
#' # Create survey design (no PSU for ABS design)
#' svy <- as_survey(
#'   pew_npors_2025,
#'   strata = stratum,
#'   weights = weight
#' )
#'
#' # Inspect variable label
#' attr(pew_npors_2025$smuse_fb, "label")
#'
#' # Inspect value labels
#' attr(pew_npors_2025$smuse_fb, "labels")
#'
#' # Inspect question preface (shared stem for all smuse_* battery items)
#' attr(pew_npors_2025$smuse_fb, "question_preface")
"pew_npors_2025"


#' Pew Jewish Americans 2020
#'
#' The extended survey dataset from Pew Research Center's 2019-2020 Survey
#' of U.S. Jews, fielded November 19, 2019 – June 3, 2020 (n = 5,881).
#' Respondents were drawn from a national, stratified random sample of
#' residential mailing addresses with oversampling of households likely to
#' contain Jewish respondents. The dataset carries 100 jackknife replicate
#' weights alongside the main weight.
#'
#' @format A data frame with 5,881 rows and 130 variables. Variables
#' `extweight1`–`extweight100` are jackknife replicate weights; the remaining
#' 30 variables are:
#' \describe{
#'   \item{extweight}{Full-sample base weight. Use for all estimates.}
#'   \item{extweight1}{Jackknife replicate weight 1 of 100.}
#'   \item{extweight2}{Jackknife replicate weight 2 of 100.}
#'   \item{extweight3}{Jackknife replicate weight 3 of 100.}
#'   \item{extweight4}{Jackknife replicate weight 4 of 100.}
#'   \item{extweight5}{Jackknife replicate weight 5 of 100.}
#'   \item{extweight6}{Jackknife replicate weight 6 of 100.}
#'   \item{extweight7}{Jackknife replicate weight 7 of 100.}
#'   \item{extweight8}{Jackknife replicate weight 8 of 100.}
#'   \item{extweight9}{Jackknife replicate weight 9 of 100.}
#'   \item{extweight10}{Jackknife replicate weight 10 of 100.}
#'   \item{extweight11}{Jackknife replicate weight 11 of 100.}
#'   \item{extweight12}{Jackknife replicate weight 12 of 100.}
#'   \item{extweight13}{Jackknife replicate weight 13 of 100.}
#'   \item{extweight14}{Jackknife replicate weight 14 of 100.}
#'   \item{extweight15}{Jackknife replicate weight 15 of 100.}
#'   \item{extweight16}{Jackknife replicate weight 16 of 100.}
#'   \item{extweight17}{Jackknife replicate weight 17 of 100.}
#'   \item{extweight18}{Jackknife replicate weight 18 of 100.}
#'   \item{extweight19}{Jackknife replicate weight 19 of 100.}
#'   \item{extweight20}{Jackknife replicate weight 20 of 100.}
#'   \item{extweight21}{Jackknife replicate weight 21 of 100.}
#'   \item{extweight22}{Jackknife replicate weight 22 of 100.}
#'   \item{extweight23}{Jackknife replicate weight 23 of 100.}
#'   \item{extweight24}{Jackknife replicate weight 24 of 100.}
#'   \item{extweight25}{Jackknife replicate weight 25 of 100.}
#'   \item{extweight26}{Jackknife replicate weight 26 of 100.}
#'   \item{extweight27}{Jackknife replicate weight 27 of 100.}
#'   \item{extweight28}{Jackknife replicate weight 28 of 100.}
#'   \item{extweight29}{Jackknife replicate weight 29 of 100.}
#'   \item{extweight30}{Jackknife replicate weight 30 of 100.}
#'   \item{extweight31}{Jackknife replicate weight 31 of 100.}
#'   \item{extweight32}{Jackknife replicate weight 32 of 100.}
#'   \item{extweight33}{Jackknife replicate weight 33 of 100.}
#'   \item{extweight34}{Jackknife replicate weight 34 of 100.}
#'   \item{extweight35}{Jackknife replicate weight 35 of 100.}
#'   \item{extweight36}{Jackknife replicate weight 36 of 100.}
#'   \item{extweight37}{Jackknife replicate weight 37 of 100.}
#'   \item{extweight38}{Jackknife replicate weight 38 of 100.}
#'   \item{extweight39}{Jackknife replicate weight 39 of 100.}
#'   \item{extweight40}{Jackknife replicate weight 40 of 100.}
#'   \item{extweight41}{Jackknife replicate weight 41 of 100.}
#'   \item{extweight42}{Jackknife replicate weight 42 of 100.}
#'   \item{extweight43}{Jackknife replicate weight 43 of 100.}
#'   \item{extweight44}{Jackknife replicate weight 44 of 100.}
#'   \item{extweight45}{Jackknife replicate weight 45 of 100.}
#'   \item{extweight46}{Jackknife replicate weight 46 of 100.}
#'   \item{extweight47}{Jackknife replicate weight 47 of 100.}
#'   \item{extweight48}{Jackknife replicate weight 48 of 100.}
#'   \item{extweight49}{Jackknife replicate weight 49 of 100.}
#'   \item{extweight50}{Jackknife replicate weight 50 of 100.}
#'   \item{extweight51}{Jackknife replicate weight 51 of 100.}
#'   \item{extweight52}{Jackknife replicate weight 52 of 100.}
#'   \item{extweight53}{Jackknife replicate weight 53 of 100.}
#'   \item{extweight54}{Jackknife replicate weight 54 of 100.}
#'   \item{extweight55}{Jackknife replicate weight 55 of 100.}
#'   \item{extweight56}{Jackknife replicate weight 56 of 100.}
#'   \item{extweight57}{Jackknife replicate weight 57 of 100.}
#'   \item{extweight58}{Jackknife replicate weight 58 of 100.}
#'   \item{extweight59}{Jackknife replicate weight 59 of 100.}
#'   \item{extweight60}{Jackknife replicate weight 60 of 100.}
#'   \item{extweight61}{Jackknife replicate weight 61 of 100.}
#'   \item{extweight62}{Jackknife replicate weight 62 of 100.}
#'   \item{extweight63}{Jackknife replicate weight 63 of 100.}
#'   \item{extweight64}{Jackknife replicate weight 64 of 100.}
#'   \item{extweight65}{Jackknife replicate weight 65 of 100.}
#'   \item{extweight66}{Jackknife replicate weight 66 of 100.}
#'   \item{extweight67}{Jackknife replicate weight 67 of 100.}
#'   \item{extweight68}{Jackknife replicate weight 68 of 100.}
#'   \item{extweight69}{Jackknife replicate weight 69 of 100.}
#'   \item{extweight70}{Jackknife replicate weight 70 of 100.}
#'   \item{extweight71}{Jackknife replicate weight 71 of 100.}
#'   \item{extweight72}{Jackknife replicate weight 72 of 100.}
#'   \item{extweight73}{Jackknife replicate weight 73 of 100.}
#'   \item{extweight74}{Jackknife replicate weight 74 of 100.}
#'   \item{extweight75}{Jackknife replicate weight 75 of 100.}
#'   \item{extweight76}{Jackknife replicate weight 76 of 100.}
#'   \item{extweight77}{Jackknife replicate weight 77 of 100.}
#'   \item{extweight78}{Jackknife replicate weight 78 of 100.}
#'   \item{extweight79}{Jackknife replicate weight 79 of 100.}
#'   \item{extweight80}{Jackknife replicate weight 80 of 100.}
#'   \item{extweight81}{Jackknife replicate weight 81 of 100.}
#'   \item{extweight82}{Jackknife replicate weight 82 of 100.}
#'   \item{extweight83}{Jackknife replicate weight 83 of 100.}
#'   \item{extweight84}{Jackknife replicate weight 84 of 100.}
#'   \item{extweight85}{Jackknife replicate weight 85 of 100.}
#'   \item{extweight86}{Jackknife replicate weight 86 of 100.}
#'   \item{extweight87}{Jackknife replicate weight 87 of 100.}
#'   \item{extweight88}{Jackknife replicate weight 88 of 100.}
#'   \item{extweight89}{Jackknife replicate weight 89 of 100.}
#'   \item{extweight90}{Jackknife replicate weight 90 of 100.}
#'   \item{extweight91}{Jackknife replicate weight 91 of 100.}
#'   \item{extweight92}{Jackknife replicate weight 92 of 100.}
#'   \item{extweight93}{Jackknife replicate weight 93 of 100.}
#'   \item{extweight94}{Jackknife replicate weight 94 of 100.}
#'   \item{extweight95}{Jackknife replicate weight 95 of 100.}
#'   \item{extweight96}{Jackknife replicate weight 96 of 100.}
#'   \item{extweight97}{Jackknife replicate weight 97 of 100.}
#'   \item{extweight98}{Jackknife replicate weight 98 of 100.}
#'   \item{extweight99}{Jackknife replicate weight 99 of 100.}
#'   \item{extweight100}{Jackknife replicate weight 100 of 100.}
#'   \item{qkey}{Unique respondent identifier.}
#'   \item{jewishcat}{Jewish identity category: `1` = Jews By Religion,
#'     `2` = Jews Of No Religion, `3` = Jewish Background,
#'     `4` = Jewish Affinity, `5` = Respondent Not Jewish In Any Way.}
#'   \item{finalmode}{Collection mode: `1` = Screener And Extended Survey
#'     Via Cawi, `2` = Screener And Extended Survey Via Teleform,
#'     `3` = Screener Via Cawi, Extended Survey Via Teleform.}
#'   \item{region}{Census region: `1` = Northeast, `2` = Midwest,
#'     `3` = South, `4` = West.}
#'   \item{sexask}{Sex: `1` = Male, `2` = Female, `99` = Not Answered.}
#'   \item{age4cat}{Age: `1` = 18-29, `2` = 30-49, `3` = 50-64, `4` = 65+;
#'     `999` = No Answer.}
#'   \item{educ4cat}{Education: `1` = High School Or Less,
#'     `2` = Some College, `3` = College Graduate, `4` = Postgrad Degree;
#'     `99` = No Answer.}
#'   \item{religmod}{Current religion (24 categories including Jewish
#'     subgroups and combinations).}
#'   \item{hisp}{Hispanic origin: `1` = Yes, `2` = No, `99` = Not Answered.}
#'   \item{racecmb}{Race (5 categories).}
#'   \item{racethn}{Race-ethnicity (4 categories).}
#'   \item{presapp}{Presidential approval (Trump): `1` = Strongly Approve,
#'     `2` = Somewhat Approve, `3` = Somewhat Disapprove,
#'     `4` = Strongly Disapprove, `99` = Not Answered.}
#'   \item{track}{Right track/wrong track:
#'     `1` = Generally Headed In The Right Direction,
#'     `2` = Off On The Wrong Track, `99` = Not Answered.}
#'   \item{satisfpersmod}{Personal life satisfaction: `1` = Excellent,
#'     `2` = Good, `3` = Only Fair, `4` = Poor, `99` = Not Answered.}
#'   \item{localrating}{Community as a place to live: `1` = Excellent,
#'     `2` = Good, `3` = Only Fair, `4` = Poor, `99` = Not Answered.}
#'   \item{relconsider_a}{Jewish. Battery 1: religious identity
#'     (select-all-that-apply). See Details for question text.}
#'   \item{relconsider_b}{Catholic. Battery 1: religious identity.}
#'   \item{relconsider_c}{Mormon. Battery 1: religious identity.}
#'   \item{relconsider_d}{Muslim. Battery 1: religious identity.}
#'   \item{relraised_a}{Jewish. Battery 2: religious background
#'     (select-all-that-apply). See Details for question text.}
#'   \item{relraised_b}{Catholic. Battery 2: religious background.}
#'   \item{relraised_c}{Mormon. Battery 2: religious background.}
#'   \item{relraised_d}{Muslim. Battery 2: religious background.}
#'   \item{discrim_a}{Evangelical Christians. Battery 3: discrimination
#'     perceptions (rating scale). See Details for question text.}
#'   \item{discrim_b}{Muslims. Battery 3: discrimination perceptions.}
#'   \item{discrim_c}{Jews. Battery 3: discrimination perceptions.}
#'   \item{discrim_d}{Blacks. Battery 3: discrimination perceptions.}
#'   \item{discrim_e}{Hispanics. Battery 3: discrimination perceptions.}
#'   \item{discrim_f}{Gays and lesbians. Battery 3: discrimination
#'     perceptions.}
#' }
#'
#' @details
#' **Survey design:** Jackknife replication — use `as_survey_replicate()`
#' with all
#' 100 replicate weights:
#'
#' ```r
#' svy <- as_survey_replicate(
#'   pew_jewish_2020,
#'   weights    = extweight,
#'   repweights = extweight1:extweight100,
#'   type       = "JK1"
#' )
#' ```
#'
#' **Jewish identity classification:** The `jewishcat` variable classifies
#' respondents into five mutually exclusive categories used in the published
#' Pew report. Use `jewishcat` rather than constructing your own
#' classification from the raw religion variables.
#'
#' **Battery question stems:**
#'
#' - **Battery 1** (`relconsider_a`–`relconsider_d`): `"ASIDE from religion,
#'   do you consider yourself to be any of the following in any way (for
#'   example ethnically, culturally or because of your family's background)?"`
#'   Values: `1` = Yes, Consider Myself This, `2` = No, Do Not Consider
#'   Myself This, `99` = Refused.
#' - **Battery 2** (`relraised_a`–`relraised_d`): `"Please indicate whether
#'   you were raised in any of the following traditions or had a parent from
#'   any of the following backgrounds."` Values: `1` = Yes, Was Raised In
#'   This Tradition Or Had A Parent From This Background, `2` = No, Was Not
#'   Raised In This Tradition And Did Not Have A Parent From This Background,
#'   `99` = Refused.
#' - **Battery 3** (`discrim_a`–`discrim_f`): `"Please tell us how much
#'   discrimination there is against each of these groups in our society
#'   today."` Values: `1` = A Lot, `2` = Some, `3` = Not Much,
#'   `4` = None At All, `99` = Not Answered.
#'
#' **Metadata:**
#' All columns carry variable labels and value labels as R attributes from the
#' original Stata file. The three battery variable groups additionally carry a
#' `"question_preface"` attribute with the shared question stem. All three
#' attribute types are automatically extracted into surveycore's metadata
#' system when you call `as_survey_replicate()`.
#'
#' - **Variable labels** (`"label"` attribute): A human-readable description of
#'   each column — for battery items this is the unique item text (e.g.,
#'   `"Jewish"`). Example: `attr(pew_jewish_2020$relconsider_a, "label")`
#'   returns `"Jewish"`.
#' - **Value labels** (`"labels"` attribute): A named numeric vector mapping
#'   each code to its meaning. Example:
#'   `attr(pew_jewish_2020$relconsider_a, "labels")` returns
#'   `c("Yes, Consider Myself This" = 1, "No, Do Not Consider Myself This" = 2,
#'   Refused = 99)`.
#' - **Question preface** (`"question_preface"` attribute): The shared question
#'   stem for each battery group. Example:
#'   `attr(pew_jewish_2020$discrim_a, "question_preface")` returns
#'   `"Please tell us how much discrimination there is against each of these
#'   groups in our society today."`.
#'
#' @source
#' Pew Research Center. Jewish Americans in 2020 (Extended Dataset).
#' \url{https://www.pewresearch.org/datasets/} (free account required to
#' download raw data; the processed `.rda` is included in the package).
#' Prepared by `data-raw/prepare-pew-jewish-2020.R`.
#'
#' @examples
#' # Design variables
#' head(pew_jewish_2020[, c("qkey", "extweight", "jewishcat")])
#'
#' # Confirm 100 replicate weights are present
#' sum(grepl("^extweight[0-9]", names(pew_jewish_2020)))
#'
#' # Inspect variable label (unique item text for battery variable)
#' attr(pew_jewish_2020$discrim_a, "label")
#'
#' # Inspect value labels
#' attr(pew_jewish_2020$discrim_a, "labels")
#'
#' # Inspect question preface (shared stem across the battery)
#' attr(pew_jewish_2020$discrim_a, "question_preface")
#'
#' # Jewish identity distribution (use jewishcat, not raw religion vars)
#' table(pew_jewish_2020$jewishcat)
"pew_jewish_2020"

#' Nationscape Wave 1: July 18, 2019
#'
#' The first weekly wave of the Democracy Fund + UCLA Nationscape survey,
#' fielded July 18–24, 2019. Approximately 6,250 completed online interviews
#' drawn from the Lucid respondent exchange platform using a non-probability
#' quota design, with raking weights calibrated to ACS demographic targets
#' and 2016 presidential vote choice.
#'
#' @details
#' This dataset is the first of 77 weekly waves collected from July 2019
#' through January 2021. The full survey ran in three phases:
#'
#' | Phase | Weeks | Dates | Approx. N |
#' |-------|-------|-------|-----------|
#' | Phase 1 | 1–24 | Jul 18, 2019 – Dec 26, 2019 | 150,000 |
#' | Phase 2 | 25–50 | Jan 2, 2020 – Jun 25, 2020 | 162,500 |
#' | Phase 3 | 51–77 | Jul 2, 2020 – Jan 12, 2021 | 168,750 |
#'
#' Only Wave 1 is bundled in the package because 77 waves × ~6,250 rows
#' would be prohibitively large. To obtain the full dataset by phase, use the
#' prepare scripts in `data-raw/` (see the Source section).
#'
#' **Survey design:**
#' The Nationscape is a calibrated non-probability sample (quota design with
#' raking weights). Use [as_survey_nonprob()] — it is designed specifically
#' for this use case and will gain bootstrap re-calibration variance in Phase
#' 2.5:
#'
#' ```r
#' svy <- as_survey_nonprob(ns_wave1, weights = weight)
#' ```
#'
#' **Metadata:**
#' All substantive columns carry variable labels (`"label"` attribute) set
#' during data preparation. Battery items additionally carry a
#' `"question_preface"` attribute with the shared question stem. Value
#' labels (`"labels"` attribute) are present for all coded response items.
#'
#' **Battery structure:**
#' Most multi-item question groups follow a `{battery}_{item}` naming
#' convention. All items within a battery share an identical
#' `"question_preface"` attribute:
#'
#' | Battery prefix | Preface summary | N items |
#' |---------------|-----------------|---------|
#' | `news_sources_*` | News sources used in past week | 13 |
#' | `group_favorability_*` | Favorability toward named groups | 13 |
#' | `cand_favorability_*` | Favorability toward named candidates | 9 |
#' | `trump_*` | Trump head-to-head matchups | 10 |
#' | `pence_*` | Pence head-to-head matchups | 5 |
#' | `cand_truth_*` | Whether each candidate tells the truth | 6 |
#' | `cand_facts_*` | Whether each candidate relies on facts | 6 |
#' | `racial_attitudes_*` | Agree/disagree racial attitude items | 4 |
#' | `gender_attitudes_*` | Agree/disagree gender attitude items | 4 |
#' | `discrimination_*` | Perceived discrimination by group | 6 |
#'
#' Three policy batteries share the same Agree/Disagree/Neither scale:
#' `wall`, `cap_carbon`, `environment`, `guns_bg`, `mctaxes`, `estate_tax`,
#' `raise_upper_tax`, `college`, `abortion_waiting`, `abortion_never`,
#' `abortion_conditions`, `late_term_abortion`, `abortion_insurance`,
#' `guaranteed_jobs`, `green_new_deal`, `gun_registry`,
#' `immigration_separation`, `immigration_system`, `immigration_wire`,
#' `impeach_trump`, `israel`, `marijuana`, `maternityleave`,
#' `medicare_for_all`, `military_size`, `minwage`, `muslimban`,
#' `oil_and_gas`, `reparations`, `right_to_work`, `ten_commandments`,
#' `trade`, `trans_military`, `uctaxes2`, `vouchers`, `gov_insurance`,
#' `public_option`, `health_subsidies`, `path_to_citizenship`, `dreamers`,
#' `deportation`, `ban_guns`, `ban_assault_rifles`, `limit_magazines`.
#'
#' @format A data frame with approximately 6,250 rows and 171 variables
#' (170 survey variables plus `wave_id` added by the prepare script).
#' \describe{
#'   \item{response_id}{Unique respondent ID (integer).}
#'   \item{start_date}{Interview date (character, `"YYYY-MM-DD"` format).}
#'   \item{wave_id}{Wave identifier: `"ns20190718"` for all rows in this
#'     dataset.}
#'   \item{weight}{Raking weight calibrated to ACS demographic targets and
#'     2016 presidential vote choice. Use for all population-level estimates.}
#'   \item{right_track}{Country direction: `1` = Right direction,
#'     `2` = Wrong track, `3` = Not sure.}
#'   \item{economy_better}{Economy outlook: `1` = Better, `2` = Worse,
#'     `3` = Same, `4` = Not sure.}
#'   \item{interest}{Political interest (4-pt): `1` = Very interested,
#'     `4` = Not at all interested.}
#'   \item{registration}{Voter registration: `1` = Registered,
#'     `2` = Not registered, `3` = Not eligible.}
#'   \item{pres_approval}{Trump presidential approval: `1` = Strongly approve,
#'     `2` = Somewhat approve, `3` = Somewhat disapprove,
#'     `4` = Strongly disapprove.}
#'   \item{vote_intention}{2020 vote intention: `1` = Trump,
#'     `2` = Democratic candidate, `3` = Other, `4` = Don't plan to vote,
#'     `5` = Not sure.}
#'   \item{vote_2016}{2016 presidential vote. See labels.}
#'   \item{vote_2016_other_text}{Write-in for `vote_2016` "other" choice.}
#'   \item{consider_trump}{Would consider voting for Trump: `1` = Yes,
#'     `2` = No.}
#'   \item{not_trump}{Reason for not considering Trump (open text).}
#'   \item{primary_party}{Primary vote party: `1` = Democratic,
#'     `2` = Republican, `3` = Other.}
#'   \item{dem_vote_intent}{Democratic primary vote intention. See labels.}
#'   \item{dem_vote_intent_TEXT}{Write-in for `dem_vote_intent` "other".}
#'   \item{rank_dems_1}{Top-ranked Democratic presidential candidate.
#'     See labels.}
#'   \item{rank_dems_2}{Second-ranked Democratic candidate. See labels.}
#'   \item{rank_dems_3}{Third-ranked Democratic candidate. See labels.}
#'   \item{replace_trump}{Wants non-Trump Republican nominee: `1` = Yes,
#'     `2` = No, `3` = Not sure.}
#'   \item{house_intent}{U.S. House vote intention: `1` = Democrat,
#'     `2` = Republican, `3` = Other, `4` = Won't vote, `5` = Not sure.}
#'   \item{senate_intent}{U.S. Senate vote intention. Same codes as
#'     `house_intent`.}
#'   \item{governor_intent}{Governor vote intention. Same codes as
#'     `house_intent`.}
#'   \item{news_sources_facebook}{Used social media for political news in past
#'     week: `1` = Selected, `2` = Not selected. See `"question_preface"`
#'     attribute for shared question stem. Same coding for all `news_sources_*`
#'     variables.}
#'   \item{news_sources_cnn}{Used CNN for political news.}
#'   \item{news_sources_msnbc}{Used MSNBC for political news.}
#'   \item{news_sources_fox}{Used Fox News for political news.}
#'   \item{news_sources_network}{Used network news (ABC/CBS/NBC/PBS).}
#'   \item{news_sources_localtv}{Used local TV news.}
#'   \item{news_sources_telemundo}{Used Telemundo or Univision.}
#'   \item{news_sources_npr}{Used NPR.}
#'   \item{news_sources_amtalk}{Used AM talk radio.}
#'   \item{news_sources_new_york_times}{Used a national newspaper.}
#'   \item{news_sources_local_newspaper}{Used a local newspaper.}
#'   \item{news_sources_other}{Used another news source: `1` = Selected,
#'     `2` = Not selected.}
#'   \item{news_sources_other_TEXT}{Write-in for `news_sources_other`.}
#'   \item{group_favorability_whites}{Favorability toward Whites: `1` = Very
#'     favorable, `2` = Somewhat favorable, `3` = Somewhat unfavorable,
#'     `4` = Very unfavorable, `5` = Not sure. Same coding for all
#'     `group_favorability_*` variables.}
#'   \item{group_favorability_blacks}{Favorability toward Blacks.}
#'   \item{group_favorability_latinos}{Favorability toward Latinos.}
#'   \item{group_favorability_asians}{Favorability toward Asians.}
#'   \item{group_favorability_christians}{Favorability toward Christians.}
#'   \item{group_favorability_socialists}{Favorability toward Socialists.}
#'   \item{group_favorability_muslims}{Favorability toward Muslims.}
#'   \item{group_favorability_labor_unions}{Favorability toward labor unions.}
#'   \item{group_favorability_the_police}{Favorability toward the police.}
#'   \item{group_favorability_undocumented}{Favorability toward undocumented
#'     immigrants.}
#'   \item{group_favorability_lgbt}{Favorability toward gays and lesbians.}
#'   \item{group_favorability_republicans}{Favorability toward Republicans.}
#'   \item{group_favorability_democrats}{Favorability toward Democrats.}
#'   \item{cand_favorability_trump}{Favorability toward Donald Trump. Same
#'     5-point scale as `group_favorability_*` variables.}
#'   \item{cand_favorability_obama}{Favorability toward Barack Obama.}
#'   \item{cand_favorability_cortez}{Favorability toward Alexandria
#'     Ocasio-Cortez.}
#'   \item{cand_favorability_biden}{Favorability toward Joe Biden.}
#'   \item{cand_favorability_harris}{Favorability toward Kamala Harris.}
#'   \item{cand_favorability_buttigieg}{Favorability toward Pete Buttigieg.}
#'   \item{cand_favorability_warren}{Favorability toward Elizabeth Warren.}
#'   \item{cand_favorability_sanders}{Favorability toward Bernie Sanders.}
#'   \item{cand_favorability_pence}{Favorability toward Mike Pence.}
#'   \item{trump_biden}{Trump vs. Biden head-to-head: `1` = Trump, `2` = Biden,
#'     `3` = Not sure. Same coding for all `trump_*` matchup variables.}
#'   \item{trump_sanders}{Trump vs. Sanders.}
#'   \item{trump_harris}{Trump vs. Harris.}
#'   \item{trump_warren}{Trump vs. Warren.}
#'   \item{trump_buttigieg}{Trump vs. Buttigieg.}
#'   \item{trump_booker}{Trump vs. Cory Booker.}
#'   \item{trump_castro}{Trump vs. Julian Castro.}
#'   \item{trump_gabbard}{Trump vs. Tulsi Gabbard.}
#'   \item{trump_gillibrand}{Trump vs. Kirsten Gillibrand.}
#'   \item{trump_orourke}{Trump vs. Beto O'Rourke.}
#'   \item{pence_biden}{Pence vs. Biden head-to-head: `1` = Pence, `2` = Biden,
#'     `3` = Not sure. Same coding for all `pence_*` matchup variables.}
#'   \item{pence_buttigieg}{Pence vs. Buttigieg.}
#'   \item{pence_harris}{Pence vs. Harris.}
#'   \item{pence_sanders}{Pence vs. Sanders.}
#'   \item{pence_warren}{Pence vs. Warren.}
#'   \item{cand_truth_donald_trump}{Whether Donald Trump cares about telling
#'     the truth: `1` = Yes, `2` = No, `3` = Not sure. Same coding for all
#'     `cand_truth_*` variables.}
#'   \item{cand_truth_elizabeth_warren}{Whether Elizabeth Warren cares about
#'     the truth.}
#'   \item{cand_truth_joe_biden}{Whether Joe Biden cares about the truth.}
#'   \item{cand_truth_bernie_sanders}{Whether Bernie Sanders cares about
#'     the truth.}
#'   \item{cand_truth_pete_buttigieg}{Whether Pete Buttigieg cares about
#'     the truth.}
#'   \item{cand_truth_kamala_harris}{Whether Kamala Harris cares about
#'     the truth.}
#'   \item{cand_facts_donald_trump}{Whether Donald Trump relies on facts vs.
#'     hunches: `1` = Facts and evidence, `2` = Hunches, `3` = Not sure. Same
#'     coding for all `cand_facts_*` variables.}
#'   \item{cand_facts_elizabeth_warren}{Whether Elizabeth Warren relies on
#'     facts.}
#'   \item{cand_facts_joe_biden}{Whether Joe Biden relies on facts.}
#'   \item{cand_facts_bernie_sanders}{Whether Bernie Sanders relies on facts.}
#'   \item{cand_facts_pete_buttigieg}{Whether Pete Buttigieg relies on facts.}
#'   \item{cand_facts_kamala_harris}{Whether Kamala Harris relies on facts.}
#'   \item{racial_attitudes_tryhard}{Agree/disagree: minorities should work
#'     their way up without special favors. `1` = Strongly agree, `2` = Agree,
#'     `3` = Neither, `4` = Disagree, `5` = Strongly disagree. Same scale for
#'     all `racial_attitudes_*` and `gender_attitudes_*` variables.}
#'   \item{racial_attitudes_generations}{Agree/disagree: generations of slavery
#'     make it difficult for Blacks to work out of the lower class.}
#'   \item{racial_attitudes_marry}{Agree/disagree: I prefer close relatives
#'     marry someone from the same race.}
#'   \item{racial_attitudes_date}{Agree/disagree: it's alright for Blacks and
#'     Whites to date.}
#'   \item{gender_attitudes_maleboss}{Agree/disagree: more comfortable with a
#'     male boss than female boss.}
#'   \item{gender_attitudes_logical}{Agree/disagree: women are just as capable
#'     of thinking logically as men.}
#'   \item{gender_attitudes_opportunity}{Agree/disagree: increased opportunities
#'     for women have improved quality of life.}
#'   \item{gender_attitudes_complain}{Agree/disagree: women who complain about
#'     harassment cause more problems than they solve.}
#'   \item{discrimination_blacks}{Perceived discrimination against Blacks:
#'     `1` = A great deal, `2` = A lot, `3` = A little, `4` = None at all,
#'     `5` = Not sure. Same scale for all `discrimination_*` variables.}
#'   \item{discrimination_whites}{Perceived discrimination against Whites.}
#'   \item{discrimination_muslims}{Perceived discrimination against Muslims.}
#'   \item{discrimination_christians}{Perceived discrimination against
#'     Christians.}
#'   \item{discrimination_women}{Perceived discrimination against Women.}
#'   \item{discrimination_men}{Perceived discrimination against Men.}
#'   \item{sen_knowledge}{U.S. Senate knowledge question. See labels.}
#'   \item{sc_knowledge}{U.S. Supreme Court knowledge question. See labels.}
#'   \item{pid3}{3-category party ID: `1` = Democrat, `2` = Republican,
#'     `3` = Independent, `4` = Something else.}
#'   \item{pid7_legacy}{7-point party ID (legacy coding). See labels.}
#'   \item{strength_democrat}{Strength of Democratic ID (conditional on
#'     `pid3 == 1`). See labels.}
#'   \item{strength_republican}{Strength of Republican ID (conditional on
#'     `pid3 == 2`). See labels.}
#'   \item{lean_independent}{Partisan lean of Independents (conditional on
#'     `pid3 == 3`). See labels.}
#'   \item{ideo5}{5-point ideological self-placement: `1` = Very liberal,
#'     `5` = Very conservative.}
#'   \item{employment}{Employment status (selected choice). See labels.}
#'   \item{employment_other_text}{Write-in for `employment` "other".}
#'   \item{foreign_born}{Born outside the U.S.: `1` = Yes, `2` = No.}
#'   \item{language}{Primary language at home. See labels.}
#'   \item{religion}{Religious affiliation (selected choice). See labels.}
#'   \item{religion_other_text}{Write-in for `religion` "other".}
#'   \item{is_evangelical}{Born-again or evangelical Christian: `1` = Yes,
#'     `2` = No.}
#'   \item{orientation_group}{Sexual orientation. See labels.}
#'   \item{in_union}{Labor union membership: `1` = Yes, `2` = No,
#'     `3` = Non-union household, `4` = Not sure.}
#'   \item{household_gun_owner}{Household gun ownership: `1` = Yes, `2` = No,
#'     `3` = Not sure.}
#'   \item{wall}{Support building a wall on the southern U.S. border:
#'     `1` = Strongly support, `2` = Somewhat support, `3` = Somewhat oppose,
#'     `4` = Strongly oppose, `5` = Not sure. Same scale for all policy items
#'     through `limit_magazines`. See `"question_preface"` attribute on each
#'     variable for the exact shared question stem.}
#'   \item{cap_carbon}{Support capping carbon emissions.}
#'   \item{environment}{Support large-scale government investment in
#'     environmental technology.}
#'   \item{guns_bg}{Support requiring background checks for all gun purchases.}
#'   \item{mctaxes}{Support cutting taxes for families making < $100K/year.}
#'   \item{estate_tax}{Support eliminating the estate tax.}
#'   \item{raise_upper_tax}{Support raising taxes on families making > $600K.}
#'   \item{college}{Support ensuring all students can graduate from state
#'     colleges debt-free.}
#'   \item{abortion_waiting}{Support requiring a waiting period and ultrasound
#'     before an abortion.}
#'   \item{abortion_never}{Support never permitting abortion.}
#'   \item{abortion_conditions}{Support permitting abortion in cases other than
#'     rape/incest/life at risk.}
#'   \item{late_term_abortion}{Support permitting late-term abortion.}
#'   \item{abortion_insurance}{Support allowing employers to decline abortion
#'     coverage.}
#'   \item{guaranteed_jobs}{Support guaranteeing jobs for all Americans.}
#'   \item{green_new_deal}{Support enacting a Green New Deal.}
#'   \item{gun_registry}{Support creating a public registry of gun ownership.}
#'   \item{immigration_separation}{Support separating children from parents
#'     prosecuted for illegal border crossing.}
#'   \item{immigration_system}{Support shifting to a merit-based immigration
#'     system.}
#'   \item{immigration_wire}{Support requiring proof of citizenship to wire
#'     money internationally.}
#'   \item{impeach_trump}{Support impeaching President Trump.}
#'   \item{israel}{Support withdrawing military support for Israel.}
#'   \item{marijuana}{Support legalizing marijuana.}
#'   \item{maternityleave}{Support requiring 12 weeks of paid maternity leave.}
#'   \item{medicare_for_all}{Support Medicare-for-All.}
#'   \item{military_size}{Support reducing the size of the U.S. military.}
#'   \item{minwage}{Support raising the minimum wage to $15/hour.}
#'   \item{muslimban}{Support banning people from predominantly Muslim
#'     countries.}
#'   \item{oil_and_gas}{Support removing barriers to domestic oil and gas
#'     drilling.}
#'   \item{reparations}{Support granting reparations to descendants of slaves.}
#'   \item{right_to_work}{Support allowing people to work in unionized
#'     workplaces without paying union dues.}
#'   \item{ten_commandments}{Support displaying the Ten Commandments in public
#'     schools and courthouses.}
#'   \item{trade}{Support limiting trade with other countries.}
#'   \item{trans_military}{Support allowing transgender people to serve in the
#'     military.}
#'   \item{uctaxes2}{Support raising taxes on families making > $250K.}
#'   \item{vouchers}{Support providing tax-funded vouchers for private or
#'     religious schools.}
#'   \item{gov_insurance}{Support providing government-run health insurance to
#'     all Americans.}
#'   \item{public_option}{Support providing the option to purchase
#'     government-run insurance.}
#'   \item{health_subsidies}{Support subsidizing health insurance for lower
#'     income people not on Medicaid.}
#'   \item{path_to_citizenship}{Support creating a path to citizenship for
#'     all undocumented immigrants.}
#'   \item{dreamers}{Support a path to citizenship for DREAMers.}
#'   \item{deportation}{Support deporting all undocumented immigrants.}
#'   \item{ban_guns}{Support banning all guns.}
#'   \item{ban_assault_rifles}{Support banning assault rifles.}
#'   \item{limit_magazines}{Support limiting gun magazines to 10 bullets.}
#'   \item{age}{Respondent age in years.}
#'   \item{gender}{Gender: `1` = Male, `2` = Female, `3` = Other.}
#'   \item{census_region}{Census region: `1` = Northeast, `2` = Midwest,
#'     `3` = South, `4` = West.}
#'   \item{hispanic}{Hispanic or Latino origin: `1` = Yes, `2` = No.}
#'   \item{race_ethnicity}{Race/ethnicity (6 categories). See labels.}
#'   \item{household_income}{Household income (7 brackets). See labels.}
#'   \item{education}{Educational attainment (6 categories). See labels.}
#'   \item{state}{U.S. state of residence (2-letter abbreviation).}
#'   \item{congress_district}{Congressional district.}
#' }
#'
#' @source
#' Democracy Fund Voter Study Group / UCLA. Nationscape Data Set, version
#' December 2021. \url{https://www.voterstudygroup.org/data/nationscape}
#' (free download; academic research use). Prepared by
#' `data-raw/prepare-nationscape-phase1.R`.
#'
#' For full methodology, see the Nationscape User Guide and the
#' Representative Assessment report in
#' `data-raw/nationscape/Nationscape-User-Guide-2021Dec.pdf`.
#'
#' @references
#' Tausanovitch, Chris and Lynn Vavreck. 2021. Democracy Fund + UCLA
#' Nationscape, July 18–24, 2019 — Wave 1 (version 20210301). Retrieved from
#' voterstudygroup.org/data/nationscape.
#'
#' Rivers, Douglas and Delia Bailey. 2009. "Inference from matched samples in
#' the 2008 U.S. national elections." Proceedings of the Joint Statistical
#' Meetings, Social Statistics Section.
#'
#' @examples
#' # Design variables
#' head(ns_wave1[, c("response_id", "weight", "age", "gender")])
#'
#' # Inspect a battery item's metadata
#' attr(ns_wave1$group_favorability_blacks, "label")
#' attr(ns_wave1$group_favorability_blacks, "question_preface")
#' attr(ns_wave1$news_sources_cnn, "labels")
#'
#' # Create a calibrated survey design (correct approach for raked
#' # non-prob samples)
#' svy <- as_survey_nonprob(ns_wave1, weights = weight)
#' get_freqs(svy, pres_approval)
#'
#' # Party identification distribution
#' table(ns_wave1$pid3)
"ns_wave1"


#' California Academic Performance Index 2000: Simple Random Sample
#'
#' A simple random sample from the 2000 California Academic Performance
#' Index (API) study. 200 schools were randomly sampled. This is the same
#' underlying data as `apisrs` in the `survey` package, reformatted to
#' surveycore conventions.
#'
#' @format A data frame with 200 rows and 38 variables:
#' \describe{
#'   \item{pw}{Sampling weight (inverse probability of selection).}
#'   \item{fpc}{FPC (number of schools in the California API system).}
#'   \item{cds}{County/district/school code (character, 14-digit).}
#'   \item{snum}{School number (integer).}
#'   \item{dnum}{District number (integer).}
#'   \item{name}{Short school name (character).}
#'   \item{sname}{Full school name (character).}
#'   \item{dname}{District name (character).}
#'   \item{cname}{County name (character).}
#'   \item{cnum}{County number (integer).}
#'   \item{api00}{API score 2000 (integer).}
#'   \item{api99}{API score 1999 (integer).}
#'   \item{target}{API growth target (integer).}
#'   \item{growth}{API score change, \code{api00 - api99} (integer).}
#'   \item{pcttest}{Percent of students tested (integer).}
#'   \item{sch_wide}{Met school-wide growth target (integer, 0 = No, 1 = Yes).}
#'   \item{comp_imp}{Met comparable improvement target (integer, 0 = No, 1 = Yes).}
#'   \item{both}{Met both targets (integer, 0 = No, 1 = Yes).}
#'   \item{awards}{Eligible for awards program (integer, 0 = No, 1 = Yes).}
#'   \item{stype}{School type (integer): 1 = Elementary, 2 = High, 3 = Middle.}
#'   \item{yr_rnd}{Year-round school (integer, 0 = No, 1 = Yes).}
#'   \item{meals}{Percent of students receiving free meals (integer).}
#'   \item{ell}{Number of English language learners (integer).}
#'   \item{mobility}{Percent of students in first year at school (integer).}
#'   \item{enroll}{Total number of students (integer).}
#'   \item{api_stu}{Number of students included in API 2000 (integer).}
#'   \item{acs_k3}{Average class size, grades K--3 (integer; \code{NA} for high and middle schools).}
#'   \item{acs_46}{Average class size, grades 4--6 (integer; \code{NA} for high schools and some others).}
#'   \item{acs_core}{Average class size, core academic courses (integer; \code{NA} for most elementary schools).}
#'   \item{not_hsg}{Percent of parents who did not complete high school (integer).}
#'   \item{hsg}{Percent of parents who are high school graduates (integer).}
#'   \item{some_col}{Percent of parents with some college (integer).}
#'   \item{col_grad}{Percent of parents who are college graduates (integer).}
#'   \item{grad_sch}{Percent of parents with graduate school education (integer).}
#'   \item{avg_ed}{Average parent education level (numeric).}
#'   \item{pct_resp}{Percent of parents who responded to the survey (integer).}
#'   \item{full}{Percent of teachers fully credentialed (integer).}
#'   \item{emer}{Percent of teachers on emergency credentials (integer).}
#' }
#'
#' @details
#' **Survey design:** Simple random sample. Use `as_survey()` with
#' `weights = pw` and `fpc = fpc`:
#'
#' ```r
#' svy <- as_survey(
#'   ca_api_2000,
#'   weights = pw,
#'   fpc = fpc
#' )
#' ```
#'
#' **Missing values:** Several columns have `NA` for schools where the value is
#' inapplicable: `acs_k3` (grades K--3) is `NA` for high schools and middle
#' schools, where those grade spans do not exist; `acs_46` (grades 4--6) is
#' `NA` for all high schools and some elementary and middle schools; `acs_core` is `NA` for
#' most elementary schools.
#'
#' **Metadata:** All 38 columns carry `"label"` attributes (human-readable
#' variable descriptions). The six categorical columns (`stype`, `sch_wide`,
#' `comp_imp`, `both`, `awards`, `yr_rnd`) additionally carry `"labels"`
#' attributes mapping integer codes to category names, compatible with
#' surveycore's metadata system.
#'
#' **Relationship to `apisrs`:** This dataset contains the same observations
#' as `survey::apisrs`, with three differences: (1) the all-`NA` `flag`
#' column is dropped; (2) factor columns are stored as plain integers with
#' `labels` attributes; (3) column names are in snake_case.
#'
#' @source
#' Lumley T (2004). *Analysis of complex survey samples.* Journal of Statistical
#' Software, 9(1):1--19. Data distributed with the `survey` R package.
#'
#' California Department of Education, Academic Performance Index 2000.
#'
#' @examples
#' head(ca_api_2000[, c("pw", "fpc", "api00", "enroll")])
#'
#' # Create an SRS design
#' svy <- as_survey(ca_api_2000, weights = pw, fpc = fpc)
#' svy
#'
#' # Inspect variable label
#' attr(ca_api_2000$api00, "label")
#'
#' # Inspect value labels for school type
#' attr(ca_api_2000$stype, "labels")
"ca_api_2000"
