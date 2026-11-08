# ibmAcousticR
Connect to Your 'IBM Acoustic' Data!

## Purpose

ibmAcousticR facilitates making a connection to the IBM Acoustic email campaign management API and executing various queries. The IBM Acoustic API documentation is available at <https://developer.ibm.com/customer-engagement/docs/>.


## Installation

The development version can be installed from GitHub: `devtools::install_github("chrisumphlett/ibmAcousticR")` .

The package is available on CRAN: `install.packages("ibmAcousticR")`.


## Usage

Before utilizing this package you must obtain the proper credentials from IBM Acoustic. This will require someone with administrator privileges to create or obtain the Client Id and Client Secret and to create individual Refresh Tokens.

Once you have these parameters you should first connect to the API using `acoustic_auth()`.

> access_token <- acoustic_auth(org_client_id = "abc", org_client_secret = "xyz", my_refresh_token = "123")

If you are able to authenticate you will obtain an access token. Access tokens are granted temporarily, expiring after four hours. 

The access token is then used to provide the authentication when you submit calls to the API to obtain data. You will also need to know your "pod number" (the number that appears in the URL of your Silverpop portal). 

Some API calls will return data back to your R session; others will submit a job that will allow you to download your file from the Silverpop portal (you must be able to log in). You can get the report within the portal from Resources -> Data Jobs. You may optionally have these sent to an SFTP server provided by Acoustic.

Currently the package has two functions for obtaining data, `get_all_contacts()` and `get_contact_list()`. Each submits a job to be retrieved from the Acoustic/Silverpop portal. See the function documentation for more information. A simple example of using it is below:

> job_id <- get_all_contacts(1, access_token)
> get_job_status(1, access_token, "123456789")

You can get a list of programs which can be joined to the email events using the `program_id` with `get_programs()`:

> get_programs(1, access_token, "2020-01-01", "2020-05-31")