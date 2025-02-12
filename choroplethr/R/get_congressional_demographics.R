globalVariables(c("Race", "Percent"))

#' Get a handful of demographic variables on US Congressional Districts from the US Census Bureau as a data.frame.
#' 
#' The data comes from the American Community Survey (ACS). The variables are: total population, percent White 
#' not Hispanic, Percent Black or African American not Hispanic, percent Asian not Hispanic,
#' percent Hispanic all races, per-capita income, median rent and median age.
#' @param year The year the survey was published
#' @param survey The survey. Either "acs5" or "acs1"
#' 
#' @importFrom tidycensus get_acs
#' @export
get_congressional_district_demographics = function(year=2018, survey="acs5")
{
  # get race data
  df = tidycensus::get_acs("congressional district", table="B03002", output="wide", cache_table=TRUE, year=year, survey=survey)
  total_population = df$B03002_001E
  df_race = data.frame(region           = df$GEOID,
                       total_population = total_population,
                       percent_white    = round(df$B03002_003E / total_population * 100),
                       percent_black    = round(df$B03002_004E / total_population * 100),
                       percent_asian    = round(df$B03002_006E / total_population * 100),
                       percent_hispanic = round(df$B03002_012E / total_population * 100))
  
  # per capita income 
  df_income = tidycensus::get_acs("congressional district", table="B19301", output="wide", cache_table=TRUE, year=year, survey=survey)
  df_income = df_income[, c("GEOID", "B19301_001E")]
  colnames(df_income) = c("region", "per_capita_income")
  
  # median rent
  df_rent = tidycensus::get_acs("congressional district", table="B25058", output="wide", cache_table=TRUE, year=year, survey=survey)
  df_rent = df_rent[, c("GEOID", "B25058_001E")]
  colnames(df_rent) = c("region", "median_rent")

  # median age
  df_age = tidycensus::get_acs("congressional district", table="B01002", output="wide", cache_table=TRUE, year=year, survey=survey)
  df_age = df_age[, c("GEOID", "B01002_001E")]
  colnames(df_age) = c("region", "median_age")
  
  df_demographics = merge(df_race        , df_income, all.x=TRUE)
  df_demographics = merge(df_demographics, df_rent  , all.x=TRUE)  
  df_demographics = merge(df_demographics, df_age   , all.x=TRUE)
  
  filter_to_voting_congressional_districts(df_demographics)
}

#' Remove non-voting Congressional Districts from a data.frame
#'
#' The data.frame must have a column named region with a 4-character Congressional District code.
#' Remove districts that have a district code of 98 (non-voting) or ZZ (undefined district). See 
#' https://www.census.gov/geographies/mapping-files/2019/dec/rdo/116-congressional-district-bef.html
#' At the time this function was created, tidycensus returned 5 non-voting districts. See 
#' https://github.com/walkerke/tidycensus/issues/277
#' @param df A data.frame. Must have a column named region that contains character vectors of length 4. The first 2 characters should be a state FIPS code and the second 2 characters should be a Congressional District Number
#' @importFrom dplyr filter
#' @export
filter_to_voting_congressional_districts = function(df)
{
  stopifnot("region" %in% colnames(df))
  stopifnot(is.character(df$region))
  stopifnot(all(nchar(df$region) == 4))
  
  invalid_district_numbers = c("98", "ZZ")
  df = dplyr::filter(df, !(substr(region, 3, 4) %in% invalid_district_numbers))
  
  # there are exactly 435 voting members of the house of representatives, 
  # so we should never return more than that many values
  # https://en.wikipedia.org/wiki/List_of_United_States_congressional_districts
  stopifnot(length(unique(df$region)) <= 435)
  
  df
}

#' Create box plots to visualize race and ethnicity by party
#' 
#' Requires a data.frame with specific column names. In practice, the data.frame is expected
#' to come from a function like ?get_congressional_districts and then merged with a data.frame 
#' that has column "party".
#' 
#' @param df A data.frame with columns "party", "percent_white", "percent_black", "percent_asian", "percent_hispanic"
#' @importFrom ggplot2 ggplot geom_boxplot scale_fill_manual
#' @importFrom tidyr pivot_longer
#' @export
#' @examples 
#' data("df_congress116_demographics")
#' data("df_congress116_party")
#' df = merge(df_congress116_demographics, df_congress116_party)
#' # Race and Ethnicity of the 116th Congressional Districts using data from
#' # the 2018 5-year American Community Survey
#' visualize_df_by_race_ethnicity_party(df) 
visualize_df_by_race_ethnicity_party = function(df) 
{
  required_colnames = c("party", "percent_white", "percent_black", "percent_asian", "percent_hispanic")
  stopifnot(required_colnames %in% colnames(df))
  
  df_race = df[, c("party", "percent_white", "percent_black", "percent_asian", "percent_hispanic")]
  colnames(df_race) = c("Party", "White", "Black", "Asian", "Hispanic")
  df_race = tidyr::pivot_longer(df_race, c("White", "Black", "Asian", "Hispanic"), names_to="Race", values_to="Percent")
  
  ggplot2::ggplot(df_race, aes(Race, Percent)) + 
    ggplot2::geom_boxplot(aes(fill = Party)) + 
    ggplot2::scale_fill_manual(values = c("blue", "red"))   
}