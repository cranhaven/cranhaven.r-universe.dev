\name{rmytarget-package}
\alias{rmytarget-package}
\alias{rmytarget}
\docType{package}
\title{Paxkage for load data from 'MyTarget' API.}
\description{Load data from API 'MyTarget' into R, by agancy clients and campaigns}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{rmytarget}
\packageIndices{rmytarget}
}
\author{Alexey Seleznev}
\references{
\href{https://target.my.com/help/advertisers/api-marketing/ru}{1. MyTarget API documents}
\href{https://target.my.com/adv/api-marketing/doc/stat-v2}{2. MyTarget API statistic methods}
\href{ https://target.my.com/help/advertisers/api_authorization/ru}{3. Authorization}
}

\keyword{ package }
\examples{
\dontrun{
# Get objects
## Get ads
Ads <- myTarGetAdList(login = "my_test_client")

## Get campaigns
Campaigns <- myTarGetCampaignList(login = "my_test_client")

# Get statistic
# base metrics by campaigns
base_data <- myTarGetTotalData(date_from   = Sys.Date() - 7,
                               date_to     = Sys.Date(),
                               object_type = "campaigns",
                               object_id = Campaigns$id,
                               metrics = "base",
                               stat_type = "day",
                               login = "my_test_client", 
                               token_path = "mytarget_token")

# all metrics by campaigns
all_data <- myTarGetTotalData(date_from   = Sys.Date() - 7,
                              date_to     = Sys.Date(),
                              object_type = "campaigns",
                              object_id = Campaigns$id,
                              metrics = "all",
                              stat_type = "day",
                              login = "my_test_client", 
                              token_path = "mytarget_token")

# custom set of metric by campaigns
custom_data <- myTarGetTotalData(date_from   = Sys.Date() - 7,
                                 date_to     = Sys.Date(),
                                 object_type = "campaigns",
                                 object_id = Campaigns$id,
                                 metrics = c("base", "tps", "viral"),
                                 stat_type = "day",
                                 login = "my_test_client", 
                                 token_path = "mytarget_token")


# if have note objects id, base metrics by ads
base_data2 <- myTarGetStats(date_from   = as.Date("2013-01-01"),
                                date_to     = Sys.Date(),
                                object_type = "banners",
                                metrics = "base",
                                stat_type = "day",
                                login = "my_test_client", 
                                token_path = "mytarget_token")
}
}
