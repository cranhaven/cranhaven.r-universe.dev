utils::globalVariables(c("District", "Member", "Party", "fips.character", "region", "district.number"))

# The data comes in as table #6 from wikipedia
# this function scrapes that data, puts it into a df,
# and then selects just the relevant columns
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_table
#' @importFrom dplyr select
get_current_congressional_rep_raw_data = function()
{
  url    = "https://en.m.wikipedia.org/wiki/List_of_current_members_of_the_United_States_House_of_Representatives"
  file   = xml2::read_html(url)
  tables = rvest::html_nodes(file, "table")
  
  # Table #6 is titled "Voting members by state"
  reps = rvest::html_table(tables[6], fill = TRUE)
  reps = as.data.frame(reps)
  
  # While the spaces will appear as spaces, they are actually encoded as &nbsp; 
  # which leads to problems later on. This code fixes that.
  # See https://stackoverflow.com/questions/63126514/error-subsetting-data-frame-from-xml2-and-rvest/63126714#63126714
  # and https://github.com/tidyverse/rvest/issues/284
  reps$District = gsub("\u00A0", " ", reps$District, fixed = TRUE)
  reps$Member   = gsub("\u00A0", " ", reps$Member  , fixed = TRUE)
  reps$Party    = gsub("\u00A0", " ", reps$Party.1 , fixed = TRUE)

  # We really only care about 2 columns: "District" and "Party" 
  # But we include Member (i.e. their name) for debugging purposes
  reps = dplyr::select(reps, District, Member, Party)
  reps[, c("District", "Party", "Member")]
}

# add a new column, "state.fips", to the df
# the df must have a column named "state", with the state in all lowercase letters
add_state_fips_code = function(df) 
{
  stopifnot("state.name" %in% colnames(df))
  
  # merge with "state.regions" df in the choroplethrMaps package
  data("state.regions", package="choroplethrMaps", envir=environment())
  state.regions = state.regions[, c("region", "fips.character")]
  colnames(state.regions) = c("state.name", "fips.character")
  
  merge(df, state.regions)
}

# the district column comes in as "Alaska at-large" or "New York 3".
# We want all state information in one column, and all district numeric information in another column
#' @importFrom stringr str_extract str_pad
split_district_column = function(df)
{
  # step 1: split out the numerical data into a column called "district.number"
  # replace "at-large" with "00"
  df$District = gsub("at-large", "00", df$District)
  # create a separate column with district numbers
  df$district.number = stringr::str_extract(df$District, pattern="[0-9]+") 
  # add a leading 0 if necessary
  df$district.number = stringr::str_pad(df$district.number, width=2, side="left", pad="0")
  # remove the numbers from the original vector (we want them to be just state names)
  
  # for reasons I do not understand, the below line does not work
  # df$District = stringr::str_replace(df$Distrct, "[0-9]+", "")
  # but breaking it up into multiple steps with a temporary variable does
  tmp = df$District
  tmp = stringr::str_replace(tmp, "[0-9]+", "")
  df$District = tmp
  
  # step 2: split out the state information into a column called "state.name"
  df$state.name = df$District
  df$state.name = trimws(df$state.name)
  df$state.name = tolower(df$state.name)
    
  # step 3: delete the original column
  df$District = NULL
  
  df
}

# revert party affiliations that changed since the inauguration of this congress
# do this because we care about the relationship between demographics and party affiliation
# that people voted for. This is obscured by the following changes that happened since election
revert_changes = function(df)
{
  # Michigan 03. Justin Amash changed party affiliation while in office 
  # https://en.wikipedia.org/wiki/Michigan%27s_3rd_congressional_district
  df[df$District == "Michigan 3", "Party"] = "Republican"
  
  # California 50. Duncan Hunter (R) resigned
  # https://en.wikipedia.org/wiki/California%27s_50th_congressional_district. 
  df[df$District == "California 50", "Party"]  = "Republican"
  df[df$District == "California 50", "Member"] = "Duncan Hunter"
  
  # Georgia 5. John Lewis (D) died 
  # https://en.wikipedia.org/wiki/Georgia%27s_5th_congressional_district
  df[df$District == "Georgia 5", "Party"]  = "Democratic"
  df[df$District == "Georgia 5", "Member"] = "John Lewis"
  
  # North Carolina 11. Mark Meadows (R) resigned: 
  # https://en.wikipedia.org/wiki/North_Carolina%27s_11th_congressional_district
  df[df$District == "North Carolina 11", "Party"]  = "Republican"
  df[df$District == "North Carolina 11", "Member"] = "Mark Meadows"
  
  # Texas 4. John Ratcliffe resigned: 
  # https://en.wikipedia.org/wiki/Texas%27s_4th_congressional_district
  df[df$District == "Texas 4", "Party"]  = "Republican"
  df[df$District == "Texas 4", "Member"] = "John Ratcliffe"
  
  df
}

# The df comes in with a column named District with values like "Alabama 1" or "Alaska at-large"
# We need a column called "region" that has this same information encoded like "0101" (Alabama first district)
# or "0200" ("Alaska at-large"). 
# The pattern is "state fips code" + "district #". In the case of "at large" district # is 00.
# We need this because the Census API uses this, and we want to add demographics
#' @importFrom tidyr separate unite
add_geoid_to_congressional_rep_data = function(df) 
{
  # "Alabama 1" -> should be 2 columns, one with state.name and one with district.number
  df = split_district_column(df) 
  
  # we need a fips code for the state as well
  df = add_state_fips_code(df)  
  
  # we can now add a new column, "region" that is the state fips code + district.number column
  df = tidyr::unite(df, "region", fips.character, district.number, sep="", remove=FALSE)
  
  # pretty up the df
  colnames(df) = tolower(colnames(df))
  
  df = df[, c("region", "party", "state.name", "district.number", "member")] 
  dplyr::arrange(df, region)
}

get_congress116_party_data = function()
{
  df = get_current_congressional_rep_raw_data()
  df = revert_changes(df)
  add_geoid_to_congressional_rep_data(df)
}