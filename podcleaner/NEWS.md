# podcleaner 0.1.2

## New parameters for combine_match_general_to_trades

* `distance`: boolean flag specifying whether (`TRUE`) or not (`FALSE`) a column 
'distance' showing the string distance between records used for their matching 
and calculated using the method specified below should be added to the output 
dataset.  
* `matches`: boolean flag specifying whether (`TRUE`) or not (`FALSE`) a column 
'match' showing general directory matches' name and address(es) should be added 
to the output dataset.


# podcleaner 0.1.1

## Bug fixes

* globals_regex_house_to_address becomes ";\\s\\b(?:[bh][op](?:use)?|res(?:idence)?)\\b\\.?.*".  
* globals_regex_house_split_trade becomes: "(?:^|[;,\u201e\\s]*)(?<![\\-])?\\b(?:res(?:id)?(?:ence)?|(?:(?:[bdht]|li|jh)[aop])(?:[ui\\/]se)?s?)\\b[.,\u201e\\s]+".


# podcleaner 0.1.0

* 2021-20-28

First submission to CRAN.
