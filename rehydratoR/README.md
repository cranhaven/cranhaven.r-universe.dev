# rehydratoR

This R package facilitates replication of Twitter-based research by providing a convenient function to download lists of tweets.

The input for the package is a list of tweet ID numbers. See [https://archive.org/details/gaza-tweets](https://archive.org/details/gaza-tweets) for an example. 

The output of the package are the tweets downloaded as a [tibble](https://CRAN.R-project.org/package=tibble) or as JSON files. Examples for both are below.

This package limits the rate of tweet downloading so Twitter's 90,000 tweet/15 minute limit is not exceeded. If you choose to download the tweets to JSON files, then a new JSON file will be created for every 90,000 tweet ID numbers.

Tweets that have been deleted or made private cannot be downloaded.

## Getting Started

Users must acquire a *consumer key*, *consumer secret*, *access token*, and *access token secret* from [https://developer.twitter.com](https://developer.twitter.com) on their own.

### Examples

[Tibble Download Example](https://github.com/kevincoakley/rehydratoR/blob/master/example/example_tibble_download.R)
	
[JSON File Download Example](https://github.com/kevincoakley/rehydratoR/blob/master/example/example_json_file_download.R)

## Contributing

kevincoakley, with zacharyst sending annoying e-mails.

## Version History

0.5.2

* Added a parameter called group_start that takes the list of split tweet IDs and keeps only those from group_start to the final list. That way, if a download is interrupted, which is likely for large corpuses, the user can restart the download at the group_start chunk, not from the beginning.

* Added a line to print an estimate of how long a download will take

0.5.1

* Prepping for CRAN release

0.5.0

* Renamed Project to rehydratoR

0.4.0

* Save tweets to JSON files

0.3.0

* Changed rate limiting from waiting 915 seconds after every loop to timing the length of every loop and ensuring every loop is at least 915 seconds

0.2.0

* Rate limiting
* Added example file

0.1.0

* Initial Release

## Authors

* **Kevin Coakley** - *programming, testing* 
* **Zachary Steinert-Threlkeld** - *requirements, testing, promoting*

## License

This project is licensed under the BSD License - see the [LICENSE.md](LICENSE.md) file for details

## Acknowledgments

* Thank you to the support Christine Kirkpatrick and the San Diego Supercomputer Center at UCSD.
