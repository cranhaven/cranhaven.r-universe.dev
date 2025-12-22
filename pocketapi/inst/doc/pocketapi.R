## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- include=FALSE-----------------------------------------------------------
library(httptest)
start_vignette("introduction_to_pocketapi")
Sys.setenv(POCKET_CONSUMER_KEY = "24256-xsadas")
Sys.setenv(POCKET_ACCESS_TOKEN = "35435-badasdasd")

## -----------------------------------------------------------------------------
library(pocketapi)

pocket_items <- pocket_get(state = "all")

## -----------------------------------------------------------------------------
class(pocket_items)

nrow(pocket_items)

table(pocket_items$status) # reading status

## ---- warning=FALSE, message=FALSE--------------------------------------------
library(dplyr)

# quick re-code: new "reading_status" variable with labels that are intuitive to understand
pocket_items$reading_status <- recode(pocket_items$status, `0` = "unread", `1` = "archived")

# using dplyr functions on the data: grouping the data and then summarizing it
pocket_items %>%
  group_by(reading_status) %>%
  summarise(word_count_avg = mean(word_count),
            word_count_sd = sd(word_count),
            base_group = n())

# using base-R functions on the data: t-test
t.test(word_count ~ reading_status, data = pocket_items)

## ---- fig.width = 6-----------------------------------------------------------
library(ggplot2)

ggplot(data = pocket_items) + 
  geom_density(aes(x = word_count, group = reading_status, fill = reading_status), alpha = 0.25) +
  labs(title = "Article Length of Read / Unread Items in Pocket",
       x = "Word Count",
       y = "Density", 
       fill = "Reading Status",
       caption = "Data: Articles from an example account.
       \nn=20 unread and n=33 read/archived articles") + 
  theme_minimal() + 
  theme(legend.position = "top")

## ---- warning=FALSE, message=FALSE--------------------------------------------

new_urls <- c("https://www.correlaid.org/blog", "https://correlaid.org/about")

pocketapi::pocket_add(add_urls = new_urls, success = FALSE, tags = c("data4good", "correlaid"))

## ---- eval=FALSE, warning=FALSE, message=FALSE--------------------------------
#  
#  # First perform pocket_get() to receive a data frame containing your items, including the items' IDs
#  
#  pocketapi::pocket_delete(item_ids = c("242234", "694834"))
#  

## ---- eval=FALSE, warning=FALSE, message=FALSE--------------------------------
#  
#  # Adds four new tags to two items
#  pocketapi::pocket_tag(action_name = "tags_add",
#                        item_ids = c("242234", "694834"),
#                        tags = c("boring", "done_reading", "newspaper", "german"))
#  
#  # Note: No tags needed, affects all items with tag "german"
#  pocketapi::pocket_tag(action_name = "tag_delete",
#                        tags = "german")
#  
#  # Renames the tag "newspaper" into "politics" for all items
#  pocketapi::pocket_tag(action_name = "tag_rename",
#                        tags = c("newspaper", "politics"))
#  
#  # Removes the tag "boring" from the two items
#  pocketapi::pocket_tag(action_name = "tags_remove",
#                        item_ids = c("242234", "694834"),
#                        tags = "boring")
#  
#  # Replaces all existing tags with these three new ones
#  pocketapi::pocket_tag(action_name = "tags_replace",
#                        item_ids = c("242234", "694834"),
#                        tags = c("interesting", "economics", "longread"))
#  
#  # Clears the two items from all tags we have assigned previously
#  pocketapi::pocket_tag(action_name = "tags_clear",
#                        item_ids = c("242234", "694834"))
#  
#  

## ---- include=FALSE-----------------------------------------------------------
end_vignette()

