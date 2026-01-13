library(amberr)

# Start Amber session
a <- amber.login(username = "<username>", password = "<password>", url = "https://treocapa-lt.inserm.fr/api")

# Extract all interviews from a study (filter by campaign can be added)
itws <- amber.interviews(a, study = "LIFTUP")
# Extract completed interviews from specific study
itws <- amber.interviews(a, study = "LIFTUP", query = list(state='completed'))

# exclude interviews from the test campaigns
itws <- itws %>%
  filter(!is.na(identifier))

# End Amber session
amber.logout(a)


library(lubridate)

# Count the completed interviews per month
itws %>%
  # merge updated date with explicit filling date
  mutate(fillingDate = ifelse(is.na(fillingDate), updatedAt, fillingDate)) %>%
  # convert character to date type
  mutate(fillingDate = as.Date(fillingDate)) %>%
  # extract the month from the date
  mutate(month = floor_date(fillingDate, "month")) %>%
  # group by the extracted month and count
  group_by(month) %>%
  summarize(count = n())


library(dplyr)
library(tidyjson)

# Extract interview steps
steps <- itws %>%
  # select some columns: 'steps' contains collected data for each step
  select(code, identifier, steps) %>%
  # change steps type to JSON type
  as.tbl_json(json.column = "steps") %>%
  # steps is a JSON array, then make a row per step
  gather_array %>%
  # make columns per step values (first level only)
  spread_all(recursive = F) %>%
  # select some columns
  select(code, identifier, name, state)

# Flatten constent step data
consent_data <- steps %>%
  # filter only consent steps
  filter(name == 'CONSENT') %>%
  # in the step object, use the data object (contains the answers of the participant)
  enter_object(data) %>%
  # make one column per consent data
  spread_all

# Make some analysis on step data
consent_data %>%
  # count per agreement values (possible values are 0 or 1)
  count(AGREEMENT)

# Flatten step actions
consent_actions <- steps %>%
  # filter only consent steps
  filter(name == 'CONSENT') %>%
  # in the step object, use the actions array (contains the actions done on this step)
  enter_object(actions) %>%
  # make one row per action
  gather_array %>%
  # make one column per action's value
  # note: there is a 'user' column if the action is performed by a registered user (= not the participant)
  # note: type possible values are
  #   * 'init' (step without data),
  #   * 'pause' (step is paused),
  #   * 'complete' (step is completed),
  #   * 'invalid' (step is to be skipped (condition not satisfied)),
  #   * 'reopen' (step is forced to be reopened, by investigator)
  spread_all

# Turn action timestamp into date
consent_actions %>%
  mutate(date = as.POSIXct(timestamp / 1000, origin = "1970-01-01"))

# Flatten questionnaire step data
questionnaire_data <- steps %>%
  # filter only questionnaire steps
  filter(name == 'QUESTIONNAIRE') %>%
  # in the step object, use the data object (contains the answers of the participant)
  enter_object(data) %>%
  # make one column per questionnaire data
  spread_all

# Count answers to a specific question, grouped by step state
questionnaire_data %>%
  count(state, HEALTH.SIT)

# Use actions to explicit the date of completion
steps %>%
  filter(name == 'QUESTIONNAIRE') %>%
  enter_object(actions) %>%
  gather_array %>%
  spread_all %>%
  filter(state == 'completed', type == 'complete') %>%
  mutate(date = as.POSIXct(timestamp / 1000, origin = "1970-01-01"))
