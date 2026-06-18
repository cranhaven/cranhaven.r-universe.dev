# OJS Scraper for R

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/ojsr)](https://cran.r-project.org/package=ojsr)
[![R-CMD-check](https://github.com/gastonbecerra/ojsr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/gastonbecerra/ojsr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The aim of this package is to aid you in crawling OJS archives, issues, articles, galleys, and search results, and retrieving/scraping metadata from articles. **ojsr functions rely on OJS routing conventions** to compose the URL for different scraping scenarios.

# Installation

From CRAN:

```r
install.packages('ojsr') 
```

From Github:

```r
install.packages('devtools') 
devtools::install_github("gastonbecerra/ojsr")
```

# ojsr functions

- **`get_issues_from_archive()`**: scrapes issues URLs from OJS issues archive
- **`get_articles_from_issue()`**: scrapes articles URLs from the ToC of OJS issues
- **`get_articles_from_search()`**: scrapes OJS search results for a given criteria to retrieve articles URLs
- **`get_galleys_from_article()`**: scrapes galleys URLs from OJS articles
- **`get_html_meta_from_article()`**: scrapes metadata from OJS articles HTML
- **`get_oai_meta_from_article()`**: retrieves OAI records for OJS articles
- **`parse_base_url()`**: parses URLs against OJS routing conventions to retrieve the base URL
- **`parse_oai_url()`**: parses URLs against OJS routing conventions to retrieve the OAI protocol URL

# Example

Let's say we want to collect metadata from some journals to compare their top keywords. We have the journals' names and URLs, and can use ojsr to scrap their issues, articles and metadata.

```{r}

library(dplyr) 
library(ojsr)

journals <- data.frame ( cbind(
    name = c( "Revista Evaluar", "PSocial" ),
    url = c( "https://revistas.unc.edu.ar/index.php/revaluar", "https://publicaciones.sociales.uba.ar/index.php/psicologiasocial")
  ), stringsAsFactors = FALSE )

# we are using the journal URL as input to retrieve the issues
issues <- ojsr::get_issues_from_archive(input_url = journals$url) 

# we are using the issues URL we just scraped as an input to retrieve the articles
articles <- ojsr::get_articles_from_issue(input_url = issues$output_url)

# we are using the articles URL we just scraped as an input to retrieve the metadata
metadata <- ojsr::get_html_meta_from_article(input_url = articles$output_url)

# let's parse the base URLs from journals and metadata, so we can bind by journal
journals$base_url <- ojsr::parse_base_url(journals$url)
metadata$base_url <- ojsr::parse_base_url(metadata$input_url)

metadata %>% filter(meta_data_name=="citation_keywords") %>% # filtering only keywords
  left_join(journals) %>% # include journal names
  group_by(base_url, keyword = meta_data_content) %>% tally(sort=TRUE) 

```
