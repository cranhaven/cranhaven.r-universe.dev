# SouthKoreAPIs - Access South Korean Data via Public APIs and Curated Datasets
# Version 0.1.1
# Copyright (c) 2025 Renzo Caceres Rossi
# Licensed under the MIT License.
# See the LICENSE file in the root directory for full license text.


#' Middle East respiratory syndrome in South Korea, 2015
#'
#' This dataset, MERSKorea2015_list, is a list containing two data frames with information
#' collected during the first weeks of the Middle East respiratory syndrome (MERS-CoV) outbreak
#' in South Korea in 2015. The data was initially gathered by the Epidemic Intelligence group at
#' the European Centre for Disease Prevention and Control (ECDC).
#'
#' The dataset name has been kept as 'MERSKorea2015_list' to avoid confusion with other datasets
#' in the R ecosystem. This naming convention helps distinguish this dataset as part of the
#' SouthKoreAPIs package and assists users in identifying its specific characteristics.
#' The suffix 'list' indicates that the object is a list containing multiple related data frames.
#' The original content has not been modified in any way.
#'
#' @name MERSKorea2015_list
#' @format A list of 2 elements:
#' \describe{
#'   \item{linelist}{A data frame with 162 observations and 15 variables:
#'     \describe{
#'       \item{id}{Case identifier (character)}
#'       \item{age}{Age of the individual (integer)}
#'       \item{age_class}{Age class of the individual (character)}
#'       \item{sex}{Sex of the individual (factor with 2 levels)}
#'       \item{place_infect}{Place of infection (factor with 2 levels)}
#'       \item{reporting_ctry}{Reporting country (factor with 2 levels)}
#'       \item{loc_hosp}{Location or hospital (factor with 13 levels)}
#'       \item{dt_onset}{Date of symptom onset (Date)}
#'       \item{dt_report}{Date of case report (Date)}
#'       \item{week_report}{Week of report (factor with 5 levels)}
#'       \item{dt_start_exp}{Start date of exposure (Date)}
#'       \item{dt_end_exp}{End date of exposure (Date)}
#'       \item{dt_diag}{Date of diagnosis (Date)}
#'       \item{outcome}{Outcome of the case (factor with 2 levels)}
#'       \item{dt_death}{Date of death, if applicable (Date)}
#'     }
#'   }
#'   \item{contacts}{A data frame with 98 observations and 4 variables:
#'     \describe{
#'       \item{from}{ID of the source case (character)}
#'       \item{to}{ID of the contact case (character)}
#'       \item{exposure}{Type of exposure (factor with 5 levels)}
#'       \item{diff_dt_onset}{Difference in days between onset dates (integer)}
#'     }
#'   }
#' }
#' @source Data taken from the \pkg{outbreaks} package version 1.9.0
#' @usage data(MERSKorea2015_list)
#' @export
load("data/MERSKorea2015_list.rda")
NULL


#' Bone quality in South Koreans
#'
#' This dataset, KoreanBoneDensity_df, is a data frame containing bone mass density measurements
#' of South Korean subjects at three body locations. It includes demographic information such as
#' sex, age, height, and weight, along with bone mass density values for the lumbar spine, hip, and neck.
#'
#' The dataset name has been kept as 'KoreanBoneDensity_df' to avoid confusion with other datasets
#' in the R ecosystem. This naming convention helps distinguish this dataset as part of the
#' SouthKoreAPIs package and assists users in identifying its specific characteristics.
#' The suffix 'df' indicates that the dataset is a data frame object. The original content has not been modified
#' in any way.
#'
#' @name KoreanBoneDensity_df
#' @format A data frame with 969 observations and 7 variables:
#' \describe{
#'   \item{Sex}{Sex of the subject (factor with 2 levels)}
#'   \item{Age}{Age of the subject in years (integer)}
#'   \item{Height}{Height of the subject in centimeters (numeric)}
#'   \item{Weight}{Weight of the subject in kilograms (numeric)}
#'   \item{LumbarBMD}{Bone mass density at the lumbar spine (numeric)}
#'   \item{HipBMD}{Bone mass density at the hip (numeric)}
#'   \item{NeckBMD}{Bone mass density at the neck (numeric)}
#' }
#' @source Data taken from the \pkg{SRMData} package version 1.0.1
#' @usage data(KoreanBoneDensity_df)
#' @export
load("data/KoreanBoneDensity_df.rda")
NULL



#' A Sample of Korean General Social Survey Data, 2023
#'
#' This dataset, KoreanSocialSurvey_tbl_df, is a tibble containing a sample of data
#' from the Korean General Social Survey (KGSS) conducted in 2023. It includes
#' demographic, social, and attitudinal variables for respondents.
#'
#' The dataset name has been kept as 'KoreanSocialSurvey_tbl_df' to avoid confusion with other datasets
#' in the R ecosystem. This naming convention helps distinguish this dataset as part of the
#' SouthKoreAPIs package and assists users in identifying its specific characteristics.
#' The suffix 'tbl_df' indicates that the dataset is a tibble object. The original content has not been modified
#' in any way.
#'
#' @name KoreanSocialSurvey_tbl_df
#' @format A tibble with 1123 observations and 13 variables:
#' \describe{
#'   \item{year}{Survey year (numeric)}
#'   \item{respid}{Respondent identifier (numeric)}
#'   \item{age}{Age of the respondent (numeric)}
#'   \item{female}{Gender indicator: 1 = female, 0 = male (numeric)}
#'   \item{employed}{Employment status indicator (numeric)}
#'   \item{unived}{University education indicator (numeric)}
#'   \item{netuse}{Internet use indicator (numeric)}
#'   \item{ideo}{Political ideology score (numeric)}
#'   \item{si_gbh}{Regional code or classification (numeric)}
#'   \item{satisfin}{Satisfaction with financial situation (numeric)}
#'   \item{fp_mord}{Attitude toward moral issues (numeric)}
#'   \item{fpcat}{Category for family planning or related topics (character)}
#'   \item{cntryaffq}{Country affiliation or related attitude (character)}
#' }
#' @source Data taken from the \pkg{simqi} package version 0.2.0
#' @usage data(KoreanSocialSurvey_tbl_df)
#' @export
load("data/KoreanSocialSurvey_tbl_df.rda")
NULL


#' South Korea COVID-19 dataset
#'
#' This dataset, SouthKoreaCovid19_tbl_df, is a tibble containing COVID-19 data
#' for South Korea from January 20th 2019 to March 20th 2020. It includes
#' epidemiological, demographic, and geographic variables.
#'
#' The dataset name has been kept as 'SouthKoreaCovid19_tbl_df' to avoid confusion with other datasets
#' in the R ecosystem. This naming convention helps distinguish this dataset as part of the
#' SouthKoreAPIs package and assists users in identifying its specific characteristics.
#' The suffix 'tbl_df' indicates that the dataset is a tibble object. The original content has not been modified
#' in any way.
#'
#' @name SouthKoreaCovid19_tbl_df
#' @format A tibble with 244 observations and 11 variables:
#' \describe{
#'   \item{n_covid1}{Number of COVID-19 cases (numeric)}
#'   \item{Morbidity}{Morbidity rate (numeric)}
#'   \item{high_sch_p}{Proportion of population with high school education (numeric)}
#'   \item{Healthcare_access}{Healthcare access index (numeric)}
#'   \item{diff_sd}{Difference in standard deviation or related metric (numeric)}
#'   \item{Crowding}{Crowding index (numeric)}
#'   \item{Migration}{Migration rate (numeric)}
#'   \item{Health_behavior}{Health behavior index (numeric)}
#'   \item{x}{Longitude coordinate (numeric)}
#'   \item{y}{Latitude coordinate (numeric)}
#'   \item{ln_total}{Natural log of total cases or population (numeric)}
#' }
#' @source Data taken from the \pkg{gwzinbr} package version 0.1.0
#' @usage data(SouthKoreaCovid19_tbl_df)
#' @export
load("data/SouthKoreaCovid19_tbl_df.rda")
NULL



#' Births in South Korea
#'
#' This dataset, SouthKoreaBirths_tbl_df, is a tibble containing births and mid-year population
#' data for South Korea by age of mother, region, and calendar year from 2011 to 2023.
#' It also includes regional data on GDP per capita (2023) and population density (2020).
#'
#' The dataset name has been kept as 'SouthKoreaBirths_tbl_df' to avoid confusion with other datasets
#' in the R ecosystem. This naming convention helps distinguish this dataset as part of the
#' SouthKoreAPIs package and assists users in identifying its specific characteristics.
#' The suffix 'tbl_df' indicates that the dataset is a tibble object. The original content has not been modified
#' in any way.
#'
#' @name SouthKoreaBirths_tbl_df
#' @format A tibble with 1,872 observations and 7 variables:
#' \describe{
#'   \item{age}{Age group of the mother (character)}
#'   \item{region}{Region name (factor with 16 levels)}
#'   \item{time}{Calendar year (integer)}
#'   \item{births}{Number of births (integer)}
#'   \item{popn}{Mid-year population (integer)}
#'   \item{gdp_pc_2023}{GDP per capita in 2023 (numeric)}
#'   \item{dens_2020}{Population density in 2020 (character)}
#' }
#' @source Data taken from the \pkg{bage} package version 0.9.4
#' @usage data(SouthKoreaBirths_tbl_df)
#' @export
load("data/SouthKoreaBirths_tbl_df.rda")
NULL


#' Seoul's Population and Area Data for Districts (2012)
#'
#' This dataset, SeoulDistrictPop_df, is a data frame containing the 2012 population and area
#' data for each of the districts in the city of Seoul, South Korea. It also includes
#' information on the founding year of each district.
#'
#' The dataset name has been kept as 'SeoulDistrictPop_df' to avoid confusion with other datasets
#' in the R ecosystem. This naming convention helps distinguish this dataset as part of the
#' SouthKoreAPIs package and assists users in identifying its specific characteristics.
#' The suffix 'df' indicates that the dataset is a base R data frame object. The original content has not been modified
#' in any way.
#'
#' @name SeoulDistrictPop_df
#' @format A data frame with 25 observations and 5 variables:
#' \describe{
#'   \item{District}{Name of the district (character)}
#'   \item{City}{Name of the city (character)}
#'   \item{Pop.2012}{Population in 2012 (integer)}
#'   \item{Area}{Area of the district in square kilometers (numeric)}
#'   \item{Founded}{Year the district was founded (character)}
#' }
#' @source Data taken from the \pkg{micromapST} package version 3.0.4
#' @usage data(SeoulDistrictPop_df)
#' @export
load("data/SeoulDistrictPop_df.rda")
NULL



#' Olympic Heptathlon Results - Seoul 1988
#'
#' This dataset, HeptathlonSeoul1988_df, is a data frame containing the results of the
#' Olympic heptathlon competition held in Seoul in 1988. It includes performance
#' metrics for each of the seven events as well as the total score.
#'
#' The dataset name has been kept as 'HeptathlonSeoul1988_df' to avoid confusion with other datasets
#' in the R ecosystem. This naming convention helps distinguish this dataset as part of the
#' SouthKoreAPIs package and assists users in identifying its specific characteristics.
#' The suffix 'df' indicates that the dataset is a base R data frame object. The original content has not been modified
#' in any way.
#'
#' @name HeptathlonSeoul1988_df
#' @format A data frame with 25 observations and 8 variables:
#' \describe{
#'   \item{hurdles}{Time in seconds for the 100m hurdles (numeric)}
#'   \item{highjump}{Height in meters for the high jump (numeric)}
#'   \item{shot}{Distance in meters for the shot put (numeric)}
#'   \item{run200m}{Time in seconds for the 200m run (numeric)}
#'   \item{longjump}{Distance in meters for the long jump (numeric)}
#'   \item{javelin}{Distance in meters for the javelin throw (numeric)}
#'   \item{run800m}{Time in seconds for the 800m run (numeric)}
#'   \item{score}{Total heptathlon score (integer)}
#' }
#' @source Data taken from the \pkg{HSAUR3} package version 1.0-15
#' @usage data(HeptathlonSeoul1988_df)
#' @export
load("data/HeptathlonSeoul1988_df.rda")
NULL



#' Administrative Areas of Seoul, South Korea
#'
#' This dataset, SeoulAdminAreas_sf, is an \code{sf} object containing polygon geometries for the
#' 25 administrative areas of Seoul, Republic of Korea. It includes the area names, associated
#' integer values, and polygon geometry data.
#'
#' The dataset name has been kept as 'SeoulAdminAreas_sf' to avoid confusion with other datasets
#' in the R ecosystem. This naming convention helps distinguish this dataset as part of the
#' SouthKoreAPIs package and assists users in identifying its specific characteristics.
#' The suffix 'sf' indicates that the dataset is a simple features object. The original content has not been modified
#' in any way.
#'
#' @name SeoulAdminAreas_sf
#' @format An \code{sf} object (tibble) with 25 observations and 3 variables:
#' \describe{
#'   \item{name}{Name of the administrative area (character)}
#'   \item{value}{Associated value or identifier (integer)}
#'   \item{geometry}{Polygon geometry data (\code{sfc_POLYGON})}
#' }
#' @source Data taken from the \pkg{valuemap} package version 2.0.4
#' @usage data(SeoulAdminAreas_sf)
#' @export
load("data/SeoulAdminAreas_sf.rda")
NULL


#' H3 Addresses within Seoul, South Korea
#'
#' This dataset, SeoulH3Data_tbl_df, is a tibble containing H3 resolution level 8 addresses
#' within Seoul, Republic of Korea, along with associated numeric values. The H3 geospatial
#' indexing system is used for representing hexagonal cells covering the area of Seoul.
#'
#' The dataset name has been kept as 'SeoulH3Data_tbl_df' to avoid confusion with other datasets
#' in the R ecosystem. This naming convention helps distinguish this dataset as part of the
#' SouthKoreAPIs package and assists users in identifying its specific characteristics.
#' The suffix 'tbl_df' indicates that the dataset is a tibble object. The original content has not been modified
#' in any way.
#'
#' @name SeoulH3Data_tbl_df
#' @format A tibble with 1,329 observations and 2 variables:
#' \describe{
#'   \item{name}{H3 address at resolution level 8 (character)}
#'   \item{value}{Associated numeric value (numeric)}
#' }
#' @source Data taken from the \pkg{valuemap} package version 2.0.4
#' @usage data(SeoulH3Data_tbl_df)
#' @export
load("data/SeoulH3Data_tbl_df.rda")
NULL



#' Korean Auto Ownership Data
#'
#' This dataset, AutoOwnershipKorea_df, is a data frame containing information on auto ownership
#' in South Korea, along with related economic indicators. It includes data on gross national product,
#' car prices, and oil prices over a series of years.
#'
#' The dataset name has been kept as 'AutoOwnershipKorea_df' to avoid confusion with other datasets
#' in the R ecosystem. This naming convention helps distinguish this dataset as part of the
#' SouthKoreAPIs package and assists users in identifying its specific characteristics.
#' The suffix 'df' indicates that the dataset is a data frame object. The original content has not been modified
#' in any way.
#'
#' @name AutoOwnershipKorea_df
#' @format A data frame with 10 observations and 5 variables:
#' \describe{
#'   \item{Year}{Year of observation (numeric)}
#'   \item{AO}{Auto ownership level (numeric)}
#'   \item{GNP}{Gross National Product (numeric)}
#'   \item{CP}{Car price (numeric)}
#'   \item{OP}{Oil price (numeric)}
#' }
#' @source Data taken from the \pkg{SenSrivastava} package version 2015.6.2
#' @usage data(AutoOwnershipKorea_df)
#' @export
load("data/AutoOwnershipKorea_df.rda")
NULL


#' Korean and English Column Names
#'
#' This dataset, NFIColumnNames_df, is a data frame containing Korean and English translations
#' of column names, along with their standardized column identifiers. It is intended to assist
#' users in mapping between Korean-language variables and their English equivalents in related datasets.
#'
#' The dataset name has been kept as 'NFIColumnNames_df' to avoid confusion with other datasets
#' in the R ecosystem. This naming convention helps distinguish this dataset as part of the
#' SouthKoreAPIs package and assists users in identifying its specific characteristics.
#' The suffix 'df' indicates that the dataset is a data frame object. The original content has not been modified
#' in any way.
#'
#' @name NFIColumnNames_df
#' @format A data frame with 174 observations and 3 variables:
#' \describe{
#'   \item{Korean_Column_Name}{Column name in Korean (character)}
#'   \item{English_Name}{Column name in English (character)}
#'   \item{Column_Name}{Standardized column identifier (character)}
#' }
#' @source Data taken from the \pkg{knfi} package version 1.0.1.9
#' @usage data(NFIColumnNames_df)
#' @export
load("data/NFIColumnNames_df.rda")
NULL


#' 2017 Korea Presidential Election Data
#'
#' This dataset, KoreanElection2017_df, is a data frame containing information from
#' the 2017 presidential election in South Korea. It includes precinct- and city-level data
#' along with demographic and socioeconomic indicators related to the voting population.
#'
#' The dataset name has been kept as 'KoreanElection2017_df' to avoid confusion with other datasets
#' in the R ecosystem. This naming convention helps distinguish this dataset as part of the
#' SouthKoreAPIs package and assists users in identifying its specific characteristics.
#' The suffix 'df' indicates that the dataset is a data frame object. The original content has not been modified
#' in any way.
#'
#' @name KoreanElection2017_df
#' @format A data frame with 1250 observations and 9 variables:
#' \describe{
#'   \item{PrecinctCode}{Precinct code (integer)}
#'   \item{CityCode}{City code (integer)}
#'   \item{CandidateName}{Candidate identifier code (integer)}
#'   \item{AveAge}{Average age of the voting population (numeric)}
#'   \item{AveYearEdu}{Average years of education (numeric)}
#'   \item{AveHousePrice}{Average house price (numeric)}
#'   \item{AveInsurance}{Average insurance enrollment indicator or count (integer)}
#'   \item{VoteRate}{Voter turnout rate (numeric)}
#'   \item{NumVote}{Number of votes cast (integer)}
#' }
#' @source Data taken from the \pkg{KPC} package version 0.1.2
#' @usage data(KoreanElection2017_df)
#' @export
load("data/KoreanElection2017_df.rda")
NULL


#' Korean Regional Data (2014–2016 Averages)
#'
#' This dataset, RegionalKorea_df, is a data frame containing average regional-level
#' socioeconomic, demographic, health, and environmental indicators for South Korea
#' over the period 2014–2016.
#'
#' The dataset name has been kept as 'RegionalKorea_df' to avoid confusion with other datasets
#' in the R ecosystem. This naming convention helps distinguish this dataset as part of the
#' SouthKoreAPIs package and assists users in identifying its specific characteristics.
#' The suffix 'df' indicates that the dataset is a data frame object. The original content has not been modified
#' in any way.
#'
#' @name RegionalKorea_df
#' @format A data frame with 268 observations and 23 variables:
#' \describe{
#'   \item{id}{Region identifier (integer)}
#'   \item{metro}{Metropolitan area indicator or name (character)}
#'   \item{region}{Region name (character)}
#'   \item{type}{Region type classification (integer)}
#'   \item{grdp}{Gross Regional Domestic Product (numeric)}
#'   \item{regpop}{Regional population (numeric)}
#'   \item{popgrowth}{Population growth rate (numeric)}
#'   \item{eq5d}{EQ-5D health index (numeric)}
#'   \item{deaths}{Number of deaths (numeric)}
#'   \item{drink}{Alcohol consumption rate (numeric)}
#'   \item{hdrink}{Heavy drinking rate (numeric)}
#'   \item{smoke}{Smoking rate (numeric)}
#'   \item{aged}{Proportion of elderly population (numeric)}
#'   \item{divorce}{Divorce rate (numeric)}
#'   \item{medrate}{Medical service utilization rate (numeric)}
#'   \item{gcomp}{Gini coefficient or related inequality measure (numeric)}
#'   \item{vehipc}{Vehicles per capita (numeric)}
#'   \item{accpv}{Accidents per vehicle (numeric)}
#'   \item{dumppc}{Illegal dumping incidents per capita (numeric)}
#'   \item{stratio}{Student–teacher ratio (numeric)}
#'   \item{deathrate}{Death rate (numeric)}
#'   \item{pctmale}{Percentage of male population (numeric)}
#'   \item{accpc}{Accidents per capita (numeric)}
#' }
#' @source Data taken from the \pkg{loedata} package version 1.0.1
#' @usage data(RegionalKorea_df)
#' @export
load("data/RegionalKorea_df.rda")
NULL



#' Annual Origin–Destination Migration Flows Between Korean Regions
#'
#' This dataset, migrationflows_tbl_df, is a tibble containing annual migration flows between
#' South Korea's first-level administrative regions from 2012 to 2020. It includes geographic,
#' economic, and demographic indicators for both origin and destination regions.
#'
#' The dataset name has been kept as 'migrationflows_tbl_df' to avoid confusion with other datasets
#' in the R ecosystem. This naming convention helps distinguish this dataset as part of the
#' SouthKoreAPIs package and assists users in identifying its specific characteristics.
#' The suffix 'tbl_df' indicates that the dataset is a tibble object. The original content has not been modified
#' in any way.
#'
#' @name migrationflows_tbl_df
#' @format A tibble with 2,601 observations and 20 variables:
#' \describe{
#'   \item{orig}{Origin region name (character)}
#'   \item{dest}{Destination region name (character)}
#'   \item{year}{Year of migration flow (integer)}
#'   \item{flow}{Number of migrants moving from origin to destination (integer)}
#'   \item{dist_cent}{Distance between region centroids (numeric)}
#'   \item{dist_min}{Minimum distance between regions (numeric)}
#'   \item{dist_pw}{Pairwise distance measure (numeric)}
#'   \item{contig}{Contiguity indicator: TRUE if regions share a border, FALSE otherwise (logical)}
#'   \item{orig_pop}{Population of the origin region (numeric)}
#'   \item{dest_pop}{Population of the destination region (numeric)}
#'   \item{orig_area}{Area of the origin region in square meters (numeric)}
#'   \item{dest_area}{Area of the destination region in square meters (numeric)}
#'   \item{orig_gdp_pc}{GDP per capita in the origin region (numeric)}
#'   \item{orig_ginc_pc}{Gross income per capita in the origin region (numeric)}
#'   \item{orig_iinc_pc}{Individual income per capita in the origin region (numeric)}
#'   \item{orig_pconsum_pc}{Private consumption per capita in the origin region (numeric)}
#'   \item{dest_gdp_pc}{GDP per capita in the destination region (numeric)}
#'   \item{dest_ginc_pc}{Gross income per capita in the destination region (numeric)}
#'   \item{dest_iinc_pc}{Individual income per capita in the destination region (numeric)}
#'   \item{dest_pconsum_pc}{Private consumption per capita in the destination region (numeric)}
#' }
#' @source Data taken from the \pkg{migest} package version 2.0.5
#' @usage data(migrationflows_tbl_df)
#' @export
load("data/migrationflows_tbl_df.rda")
NULL



#' Korea Stock Price Index 200 (KOSPI 200)
#'
#' This dataset, KOSPI200_list, is a list containing historical data for the Korea Stock Price Index 200 (KOSPI 200).
#' It includes a vector of dates and the corresponding index values over time.
#'
#' The dataset name has been kept as 'KOSPI200_list' to avoid confusion with other datasets
#' in the R ecosystem. This naming convention helps distinguish this dataset as part of the
#' SouthKoreAPIs package and assists users in identifying its specific characteristics.
#' The suffix 'list' indicates that the dataset is stored as a list object. The original content has not been modified
#' in any way.
#'
#' @name KOSPI200_list
#' @format A list with 2 components:
#' \describe{
#'   \item{date}{A Date vector of length 896 representing the observation dates}
#'   \item{index}{A numeric vector of length 896 representing the KOSPI 200 index values}
#' }
#' @source Data taken from the \pkg{EMD} package version 1.5.9
#' @usage data(KOSPI200_list)
#' @export
load("data/KOSPI200_list.rda")
NULL


#' Solar Radiation Observations in South Korea
#'
#' This dataset, SolarRadiation_df, is a data frame containing hourly solar radiation measurements
#' recorded at three locations in South Korea: Seoul, Daegu, and Busan. Observations cover the period
#' from September 1, 2003, to September 29, 2003, and were obtained from the Korea Meteorological Administration.
#'
#' The dataset name has been kept as 'SolarRadiation_df' to avoid confusion with other datasets
#' in the R ecosystem. This naming convention helps distinguish this dataset as part of the
#' SouthKoreAPIs package and assists users in identifying its specific characteristics.
#' The suffix 'df' indicates that the dataset is a data frame object. The original content has not been modified
#' in any way.
#'
#' @name SolarRadiation_df
#' @format A data frame with 696 observations and 4 variables:
#' \describe{
#'   \item{Date}{Date and time of observation (POSIXct)}
#'   \item{Seoul}{Solar radiation in Seoul (numeric)}
#'   \item{Daegu}{Solar radiation in Daegu (numeric)}
#'   \item{Busan}{Solar radiation in Busan (numeric)}
#' }
#' @source Data taken from the \pkg{EPT} package version 0.7.6
#' @usage data(SolarRadiation_df)
#' @export
load("data/SolarRadiation_df.rda")
NULL



#' Korean Demographics (2000-2022)
#'
#' This dataset, demographicsKR_tbl_df, is a tibble containing demographic data of South Korea
#' from 2000 to 2022. It includes values and rates for birth, death, natural growth, marriage,
#' and divorce, organized by date and region. The dataset preserves the original structure from
#' its source on Kaggle.
#'
#' The dataset name has been kept as 'demographicsKR_tbl_df' to maintain consistency with the naming
#' conventions in the SouthKoreAPIs package. The suffix 'tbl_df' indicates that this is a tibble
#' data frame. The original content has not been modified in any way.
#'
#' @name demographicsKR_tbl_df
#' @format A tibble with 4,860 observations and 12 variables:
#' \describe{
#'   \item{Date}{Date of the record (character)}
#'   \item{Region}{Region name in South Korea (character)}
#'   \item{Birth}{Number of births (numeric)}
#'   \item{Birth_rate}{Birth rate (per 1,000 people) (numeric)}
#'   \item{Death}{Number of deaths (numeric)}
#'   \item{Death_rate}{Death rate (per 1,000 people) (numeric)}
#'   \item{Divorce}{Number of divorces (numeric)}
#'   \item{Divorce_rate}{Divorce rate (per 1,000 people) (numeric)}
#'   \item{Marriage}{Number of marriages (numeric)}
#'   \item{Marriage_rate}{Marriage rate (per 1,000 people) (numeric)}
#'   \item{Natural_growth}{Difference between births and deaths (numeric)}
#'   \item{Natural_growth_rate}{Natural growth rate (per 1,000 people) (numeric)}
#' }
#' @source Data obtained from Kaggle: \url{https://www.kaggle.com/datasets/alexandrepetit881234/korean-demographics-20002022}
#' @usage data(demographicsKR_tbl_df)
#' @export
load("data/demographicsKR_tbl_df.rda")
NULL


#' Mosquito Indicator in Seoul, Korea (2016-2019)
#'
#' This dataset, SeoulMosquito_tbl_df, is a tibble containing daily mosquito indicator data
#' and weather measurements for Seoul, South Korea, from 2016 to 2019. The mosquito indicator
#' represents the number of mosquitoes per specific area. The dataset also includes daily
#' precipitation and temperature metrics (mean, minimum, and maximum).
#' The dataset preserves the original structure from its source on Kaggle.
#'
#' The dataset name has been kept as 'SeoulMosquito_tbl_df' to maintain consistency with the naming
#' conventions in the SouthKoreAPIs package. The suffix 'tbl_df' indicates that this is a tibble
#' data frame. The original content has not been modified in any way.
#'
#' @name SeoulMosquito_tbl_df
#' @format A tibble with 1,342 observations and 6 variables:
#' \describe{
#'   \item{date}{Observation date (Date)}
#'   \item{mosquito_Indicator}{Number of mosquitoes per specific area (numeric)}
#'   \item{rain(mm)}{Daily precipitation in millimeters (numeric)}
#'   \item{mean_T(℃)}{Mean daily temperature in degrees Celsius (numeric)}
#'   \item{min_T(℃)}{Minimum daily temperature in degrees Celsius (numeric)}
#'   \item{max_T(℃)}{Maximum daily temperature in degrees Celsius (numeric)}
#' }
#' @source Data obtained from Kaggle: \url{https://www.kaggle.com/datasets/kukuroo3/mosquito-indicator-in-seoul-korea}
#' @usage data(SeoulMosquito_tbl_df)
#' @export
load("data/SeoulMosquito_tbl_df.rda")
NULL


#' All KPop Idols
#'
#' This dataset, KPopIdols_tbl_df, is a tibble containing a complete detailed database of all current
#' KPop idols, both male and female. It includes each idol's stage name, full name, Korean name,
#' Korean stage name, date of birth, group name, country, height, weight, birthplace, gender,
#' and Instagram handle. The dataset preserves the original structure from its source on Kaggle.
#'
#' The dataset name has been kept as 'KPopIdols_tbl_df' to maintain consistency with the naming
#' conventions in the SouthKoreAPIs package. The suffix 'tbl_df' indicates that this is a tibble
#' data frame. The original content has not been modified in any way.
#'
#' @name KPopIdols_tbl_df
#' @format A tibble with 1,666 observations and 12 variables:
#' \describe{
#'   \item{Stage Name Stage Name}{Stage name of the idol (character)}
#'   \item{Full Name Full Name}{Full name of the idol (character)}
#'   \item{Korean Name Korean Name}{Korean name of the idol (character)}
#'   \item{K. Stage Name K. Stage Name}{Stage name written in Korean (character)}
#'   \item{Date of Birth Date of Birth}{Date of birth (character)}
#'   \item{Group Group}{Name of the group the idol belongs to (character)}
#'   \item{Country Country}{Country of origin (character)}
#'   \item{Height Height}{Height in centimeters (numeric)}
#'   \item{Weight Weight}{Weight in kilograms (numeric)}
#'   \item{Birthplace Birthplace}{Place of birth (character)}
#'   \item{Gender Gender}{Gender of the idol (character)}
#'   \item{Instagram Instagram}{Instagram handle or profile URL (character)}
#' }
#' @source Data obtained from Kaggle: \url{https://www.kaggle.com/datasets/onlyrohit/all-kpop-idols}
#' @usage data(KPopIdols_tbl_df)
#' @export
load("data/KPopIdols_tbl_df.rda")
NULL


#' Korea Natural Gas Sales with Temperature
#'
#' This dataset, GasSales_Korea_tbl_df, is a tibble containing monthly natural gas sales data
#' with corresponding average temperatures for provinces of South Korea. It includes sales
#' figures for each province, the national total, and temperature information, organized by
#' year and month. The dataset preserves the original structure from its source on Kaggle.
#'
#' The dataset name has been kept as 'GasSales_Korea_tbl_df' to maintain consistency with the naming
#' conventions in the SouthKoreAPIs package. The suffix 'tbl_df' indicates that this is a tibble
#' data frame. The original content has not been modified in any way.
#'
#' @name GasSales_Korea_tbl_df
#' @format A tibble with 252 observations and 21 variables:
#' \describe{
#'   \item{Year}{Year of observation (numeric)}
#'   \item{Month}{Month of observation (numeric)}
#'   \item{Temperature}{Average temperature in degrees Celsius (numeric)}
#'   \item{Gangwondo}{Gas sales in Gangwon-do province (numeric)}
#'   \item{Seoul}{Gas sales in Seoul (numeric)}
#'   \item{Gyeonggido}{Gas sales in Gyeonggi-do province (numeric)}
#'   \item{Incheon}{Gas sales in Incheon (numeric)}
#'   \item{Gyeongsangnamdo}{Gas sales in Gyeongsangnam-do province (numeric)}
#'   \item{Gyeongsangbukdo}{Gas sales in Gyeongsangbuk-do province (numeric)}
#'   \item{Gwangju}{Gas sales in Gwangju (numeric)}
#'   \item{Daegu}{Gas sales in Daegu (numeric)}
#'   \item{Daejeon}{Gas sales in Daejeon (numeric)}
#'   \item{Busan}{Gas sales in Busan (numeric)}
#'   \item{Sejong}{Gas sales in Sejong (numeric)}
#'   \item{Ulsan}{Gas sales in Ulsan (numeric)}
#'   \item{Jeollanamdo}{Gas sales in Jeollanam-do province (numeric)}
#'   \item{Jeollabukdo}{Gas sales in Jeollabuk-do province (numeric)}
#'   \item{Jeju}{Gas sales in Jeju province (numeric)}
#'   \item{Chungcheongnamdo}{Gas sales in Chungcheongnam-do province (numeric)}
#'   \item{Chungcheongbukdo}{Gas sales in Chungcheongbuk-do province (numeric)}
#'   \item{Sum}{Total gas sales in South Korea (numeric)}
#' }
#' @source Data obtained from Kaggle: \url{https://www.kaggle.com/datasets/zxtzxt30/korea-monthly-gas-sales-with-temperature}
#' @usage data(GasSales_Korea_tbl_df)
#' @export
load("data/GasSales_Korea_tbl_df.rda")
NULL




