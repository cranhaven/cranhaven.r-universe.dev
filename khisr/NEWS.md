# khisr 1.0.6

* Improved tests to skip tests when the server is down

# khisr 1.0.5

* **Improved Authentication**: Now supports optional authentication for API calls using the `auth` argument. This strengthens security by allowing you to control access to your data.

* **Clearer Error Messages**: Provides more informative error messages to help you identify and troubleshoot issues more efficiently.

# khisr 1.0.4

* Improved Credential Handling:
    - `khis_has_cred()`: Now ensures credentials are valid before returning TRUE, preventing unauthorized access.
    - `khis_cred()`: Includes validation to accept only valid credentials, reducing errors.

# khisr 1.0.3

## New features

* Introduced experimental functions for enhanced data retrieval:
    - `get_data_elements_with_category_options()`: Fetch data elements along with their associated category option values.
    - `get_organisations_by_level()`: Retrieve organizations filtered by level.
    - `get_analytics_by_level()`: Obtain analytics table data
    - `get_data_sets_by_level()`: Retrieve data set reporting rate metrics.
    
* Added the `khis_base_url()` to obtain the DHIS2 API URL.

* Introduced `get_organisation_unit_levels()` to retrieve the available organisation levels in the DHIS2 instance.

## Minor improvements and fixes

* Made the package generic to support any DHIS2 instance and updated the documentation accordingly.

* Modified  the `khis_cred()` to require the `base_url` argument and deprecated the default value.

# khisr 1.0.2

* Updated the `khis_cred_clear()` to reset the `base_url` back to KHIS API.

* Updated the `khis_cred()` to allow `base_url` be set from the config file.

# khisr 1.0.1

* Initial CRAN submission.
