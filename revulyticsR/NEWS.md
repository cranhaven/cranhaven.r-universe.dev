# revulyticsR 0.0.3

* `get_new_users()` created. For a given period of time (a day, week, or month) Revulytics' API summarizes and returns the number of new users. With this function you can return daily, weekly, or monthly new users for multiple product ids.
* Removed unnecessary messages being printed to console by `get_daily_client_properties`.
* `get_raw_data_files` to retrieve the list of available raw data files and download URL for each file.

# revulyticsR 0.0.2

* Added a `RETRY()` to safely retry an API request certain number of times before returning a error code.
* Added `get_daily_client_properties()` to pull daily values of properties for a product within a given date range.

# revulyticsR 0.0.1

* `get_active_users()` created. For a given period of time (a day, week, or month) Revulytics' API summarizes and returns the number of active users. With this function you can return daily, weekly, or monthly active users for multiple product ids.
* `get_categories_and_events()` created. For a list of product ids get all of the categories and events that have been defined (and identify it each is a basic or advanced). This can then be passed into subsequent queries to pull data on multiple events.
* `get_product_properties()` created. For a list of product ids get all of the product properties, both standard and custom. This can then be passed into `get_client_metadata()`.
* `get_client_metadata()` created. For a list of product ids get selected properties for each client, which is essentially metadata.  This works by pulling all of the clients that are installed within specified date range.

# revulyticsR 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
* Created initial package structure.
* Authentication/login method created.