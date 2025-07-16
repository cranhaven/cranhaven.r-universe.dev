# lsnstat 1.0.1

* Add new arguments `filter_list`,`sort` and `verbose` respectively to make the filtering easier, enable sorting the fetched dataframe and optionally display relevant informations. 

* Implement a new exported function `get_lsn_dataset_list` to fetch the available dataset structure and corresponding informations.

* Create new internal functions: `get_endpoint` and `from_filter_list_to_sql`, which will respectively bypass the user collection prompts and interpret R list filtering.
