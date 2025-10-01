# SouthKoreAPIs - Access South Korean Data via Public APIs and Curated Datasets
# Version 0.1.1
# Copyright (c) 2025 Renzo Caceres Rossi
# Licensed under the MIT License.
# See the LICENSE file in the root directory for full license text.


# utils.R


utils::globalVariables(c(


  # Variables of the get_southkorea_child_mortality function
  "indicator", "country", "year", "value",

  # Variables of the get_southkorea_cpi function
  "indicator", "country", "year", "value",

  # Variables of the get_southkorea_energy_use function

  "indicator","country","year","value",

  # Variables of the get_southkorea_gdp function
  "indicator","country","year","value","value_label",

  # Variables of the get_southkorea_holidays function
  "date","local_name","name",

  # Variables of the get_southkorea_hospital_beds function
  "indicator","country","year","value",

  # Variables of the get_southkorea_literacy_rate function
  "indicator","country","year","value",

  # Variables of the get_southkorea_life_expectancy function
  "indicator","country","year","value",

  # Variables of the get_southkorea_population function
  "indicator","country","year","value","value_label",

  # Variables of the get_southkorea_unemployment function
  "indicator","country","year","value",


  # Variables of the get_country_info_kr function
  "name_common","name_official","region","subregion","capital","area","population","languages"

))

utils::globalVariables(c("data"))
