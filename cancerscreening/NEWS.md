# cancerscreening 1.1.1

* This release streamlines the `khisr` package by focusing on its core functionality: 
  retrieving screening data for the MOH 745 screening tool.

* **Refactored Functionality:** Previously available functions for retrieving data 
  element metadata, organization unit metadata, and formatted datasets have been 
  migrated to the dedicated `khisr` package. This enhances code organization and 
  avoids redundancy.
  
* **New Function:** We've introduced the `get_screening_reporting_analytics()`
  function to retrieve reporting metrics associated with the MOH 745 screening 
  tool.

# cancerscreening 1.1.0

### Improved data retrieval efficiency

* Updated `get_data_elements_metadata()` and `get_organisation_units_metadata()` to retrieve only necessary data, enhancing performance.
 
* Introduced levels (`country`, `county`, `subcounty`, `ward`, `facility`) to functions, allowing users to specify the scope of data retrieval.

### Enhanced functionality:

* Upgraded the package to utilize the `khisr` package for data retrieval.

* Renamed `get_analytics()` to `get_analytics_formatted()` to avoid conflicts with `khisr`.

* Added `get_data_sets_formatted()` for data set reporting metrics retrieval.

* Consolidated functionalities in `get_data_elements_metadata()`: retrieves data elements, categories, and removes the need for `get_category_options_metadata()`.

* Implemented functions to retrieve laboratory diagnostics data:
  - `get_lab_fluid_cytology()`
  - `get_lab_tissue_histology()`
  - `get_lab_bone_marrow()`
  - `get_lab_fna()`
  - `get_lab_smears()`
  
### Streamlined API:

* Removed unnecessary `elements` and `categories` parameters from functions due to optimized data retrieval.

* Repurposed `organizations` parameter to control the retrieved organizations using organization IDs.

# cancerscreening 1.0.2

* Initial CRAN submission.
