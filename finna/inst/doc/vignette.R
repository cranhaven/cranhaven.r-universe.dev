## ----message = FALSE, warning = FALSE-----------------------------------------
library(finna)
record <- search_finna("sibelius")
print(record)

## ----message = FALSE, warning = FALSE-----------------------------------------
library(finna)
record <- search_finna(query = '"orkesterimusiikki"', type = "Subject", lng = "en-gb")
print(record)

## ----message = FALSE, warning = FALSE-----------------------------------------
library(finna)
phrase <- search_finna("bicycle")
print(phrase)

## ----message = FALSE, warning = FALSE-----------------------------------------
library(finna)
search_oper <- search_finna("+economics Keynes”)")
print(search_oper)

## ----message = FALSE, warning = FALSE-----------------------------------------
library(finna)
search_oper <- search_finna("economics !-Keynes”)")
print(search_oper)

## ----message = FALSE, warning = FALSE-----------------------------------------
library(finna)
search_oper <- search_finna("!-economics”)")
print(search_oper)

## ----message = FALSE, warning = FALSE-----------------------------------------
fuzzy_search <- search_finna("roam~")
print(fuzzy_search)

## ----message = FALSE, warning = FALSE-----------------------------------------
fuzzy_search <- search_finna("economics Keynes~10")
print(fuzzy_search)

## ----message = FALSE, warning = FALSE-----------------------------------------
record <- search_finna("sibelius", filters = c("free_online_boolean:1"))
print(record)

## ----message = FALSE, warning = FALSE-----------------------------------------
record <- search_finna("laatokka",filters = c('~building:"0/SA-kuva/"'))
print(record)

## ----message = FALSE, warning = FALSE-----------------------------------------
record <- search_finna("sibelius", filters = c('~media_type_str_mv:"0/image/"'))
print(record)

## ----message = FALSE, warning = FALSE-----------------------------------------
record <- search_finna("sibelius", filters = c('~format:"1/Book/AudioBook/"'))
print(record)

## ----message = FALSE, warning = FALSE-----------------------------------------
record <- search_finna("sibelius", filters = c('search_daterange_mv:"overlap|[-5000 TO 5000]"'))
print(record)

## ----message = FALSE, warning = FALSE-----------------------------------------
record <- search_finna("trump", filters = c('{!geofilt sfield=location_geo pt=61.663987171517796,24.17263895273209 d=212.53603751769646}'))
print(record)

## ----message = FALSE, warning = FALSE-----------------------------------------
record <- search_finna("trump", filters = c('{!geofilt sfield=location_geo pt=39.3130504637139,-76.33021295070648 d=281.83790818401854}'))
print(record)

## ----message = FALSE, warning = FALSE-----------------------------------------
record <- search_finna("trump", filters = c('{!geofilt sfield=location_geo pt=61.663987171517796,24.17263895273209 d=212.53603751769646},author_facet:"Häkkinen,Hannu"'))
print(record)

## ----message = FALSE, warning = FALSE-----------------------------------------
record <- search_finna("era:'2010-luku'", filters = c('building:"0/3AMK/"'))


## ----message = FALSE, warning = FALSE-----------------------------------------
record <- search_finna('era:"2010-luku"', filters = c('~building:"0/3AMK/"', 'finna.deduplication:"1"'))
print(record)

## ----message = FALSE, warning = FALSE-----------------------------------------
record <- search_finna('era:"2010-luku"', filters = c('~building:"0/3AMK/"', 'finna.deduplication:"1"'))
result_count <- attr(record, "result_count")
print(result_count)

## ----message = FALSE, warning = FALSE-----------------------------------------
record <- search_finna('era:"2010-luku"', filters = c('~building:"0/3AMK/"', 'finna.deduplication:"0"'))
print(record)

## ----message = FALSE, warning = FALSE-----------------------------------------
record <- search_finna('era:"2010-luku"', filters = c('~building:"0/3AMK/"', 'finna.deduplication:"0"'))
result_count <- attr(record, "result_count")
print(result_count)

## ----message = FALSE, warning = FALSE-----------------------------------------
results <- search_finna(
  query = "musiikki OR taide OR tanssi OR teatteri",
  filters = c(
    '~hierarchy_parent_title:"Institutional Repository"',
    '~format_ext_str_mv:"1/Thesis/Gradu/"',
    '~format_ext_str_mv:"1/Thesis/Masters/"',
    '~format_ext_str_mv:"1/Thesis/MastersPolytechnic/"',
    '~format_ext_str_mv:"1/Thesis/Thesis/"',
    '~format_ext_str_mv:"1/Thesis/Licentiate/"',
    '~format_ext_str_mv:"0/OtherText/"',
    '~format_ext_str_mv:"0/Journal/"',
    '~format_ext_str_mv:"0/Book/"',
    'free_online_boolean:"1"'
  ),
  type = "AllFields",
  lng = "en-gb",
  prettyPrint = TRUE
)

# Print the results
print(results)

