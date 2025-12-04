# oncrawlR

### Description

This R package implements methods for querying SEO data from OnCrawl and uses a basic authentication with an API key. 

NB : To get this API key, you need to have a Oncrawl [Business plan](https://www.oncrawl.com/pricing-crawler/).

The script is explained step by step on my blog post [Complete API guide with R (package)](https://data-seo.com/2019/06/15/oncrawl-complete-api-guide-with-r/).


## Install
```
#CRAN R (official version)
install.packages("oncrawlR")

#Github (dev version)
library(devtools)
devtools::install_github("voltek62/oncrawlR")
```

## Getting started
Get your oncrawlR API key and load the oncrawlR package.

This key must be copied to the root of your project in a txt file : oncrawl_configuration.txt

```r
token = YOURAPIKEY
debug = FALSE
api = https://app.oncrawl.com/api/v2/
```

```
library(oncrawlR)
```

## Main oncrawlR functions

### 1. initAPI 
This function allows you to test and initialize the API. It returns “ok” if your key is correct.

```
initAPI("oncrawl_configuration.txt")
```

### 2. listProjects 
This function allows you to retrieve all your projects, and in particular the IDs of the latest crawls.

```
listProjects <- listProjects()
```

### 3. listPages 
This function allows you to retrieve all crawled pages. You have to pass it an argument that represents the crawl ID.

```
pages <- listPages(crawlId)
pages_fetched <- filter(pages,fetched=="True")
```

### 4. listLogs 
This function allows you to retrieve all data from the logs. You must pass it an argument that represents the project ID. It is important to ensure that the logs are received correctly in order to use this function.

```
logs <- listLogs(projectId)
```

### 5. DALEX: oncrawlTrainModel & oncrawlExplainModel

I’m a fan of DALEX: Descriptive mAchine Learning EXplanations to understand ML models. 
The name is inspired by a recurring villain of Doctor Who who spends his time saying:  Explain! 

Models are becoming more and more sophisticated due to the increasing computing power of computers and the complexity of data sources.

When you take XgBoost or neural networks, for example, they are configured with thousands or even millions of possibilities.
It is difficult to understand the relationship between the input variables and the model results, it is called a black box. 
These ML models are used because of their high performance, but their lack of interpretability remains one of their biggest weaknesses.

Unfortunately for SEO, we need to know the impact of each variable on the final model predictions. 
This is where the DALEX package comes in: <a href="https://pbiecek.github.io/DALEX_docs/" target="_blank">https://pbiecek.github.io/DALEX_docs/</a>
This package allows us to develop an understanding of a very large number of ML models.

#### Full example

```
list <- oncrawlTrainModel(pages_fetched, 200)

# display confusion matrix
print(list$matrix)

# display ROC curve
plot(list$roc)

# explain your model with DALEX
model <- oncrawlExplainModel(list$model, list$x, list$y, 4, ".")

# display importance variables
print(model$plot)

# save imortance variables
ggsave("variable_importance.jpg",model$plot, width = 5, height= 4.83, units="in", dpi=100)

```

### 6. DALEX: oncrawlCreateGraph

```
# get all explainers
list_importance_variable <- model$factors

# explain variable 1 and save graph into factor_top1 file
factor_top <- list_importance_variable[1]
oncrawlCreateGraph(factor_top,"factor_top1.jpg", 500/100, 300/100,".")
```

### 7. oncrawlCreateDashboard

```
last_crawl_ids <- unlist(listProjects$projects$crawl_ids[1])

# last four crawls
last_crawl_top <- head(last_crawl_ids,4)

for (crawlId in last_crawl_top) {

  print(crawlId)

  crawlInfo <- getCrawl(crawlId)

  crawl_config <- crawlInfo$crawl$crawl_config

  #an UTC timestamp in milliseconds.
  date_in_ms <- crawlInfo$crawl$created_at

  sec <- date_in_ms / 1000
  date_format <- as.POSIXct(sec, origin="1970-01-01", tz=Sys.timezone())
  date_format <- substr(date_format,1,10)

  assign(paste("infocrawl_d_", date_format, sep = ""), crawlInfo$crawl)

  #if unarchived
  if(crawlInfo$crawl$link_status=="live") {
    pages <- listPages(crawlId)

    if ( any(grepl("analytics_entrances_seo_google",names(pages))) ) {
      assign(paste("crawl_d_", date_format, sep = ""), pages)
    }
    else {
      print("pas de data analytics")
    }

  } else {
    pages <- ""
    print(paste("you need to unarchive the crawl ",date_format))
  }
}


all <- ls()
objects <- all[which(grepl("^crawl_d_", all))]


res <- data.frame(date=character()
                  ,value=integer()
                  ,metric=character()
)

for (obj in objects) {

  result <- get(obj)

  date <- substr(obj,9,nchar(obj))

  pages_in_structure <- nrow(result)

  pages_crawled <- nrow(filter(result,fetched=="True"))

  res <- rbind(res, data.frame(date=date,
                               value=pages_crawled,
                               metric="Pages Crawled"))

  poorContent <- nrow(filter(result, word_count_range == "xs"))

  res <- rbind(res, data.frame(date=date,
                               value=poorContent,
                               metric="Poor Content"))

  code3xx <- nrow(filter(result, status_code == "301"))

  res <- rbind(res, data.frame(date=date,
                               value=code3xx,
                               metric="Code 3XX"))

  code4xx <- nrow(filter(result, status_code == "404"))

  res <- rbind(res, data.frame(date=date,
                               value=code4xx,
                               metric="Code 4XX"))

  code5xx <- nrow(filter(result, status_code == "500"))

  res <- rbind(res, data.frame(date=date,
                               value=code5xx,
                               metric="Code 5XX"))

  pages_in_structure <- nrow(filter(result, depth>0))

  res <- rbind(res, data.frame(date=date,
                               value=pages_in_structure,
                               metric="Pages in structure"))

  indexable_canonical_pages <- nrow(filter(result, meta_robots_index == "True" & status_code_range == "ok" & canonical_evaluation != "not_matching" & fetched == "True"))

  res <- rbind(res, data.frame(date=date,
                               value=indexable_canonical_pages,
                               metric="Indexable canonical pages"))

  #traffic mais depth=0 = orphan
  seo_active_orphan_pages <- nrow(filter(result, is.na(depth)
                                         & analytics_entrances_seo_google >0 ))

  res <- rbind(res, data.frame(date=date,
                               value=seo_active_orphan_pages,
                               metric="Active orphan pages"))

  #traffic > 1 = active page
  seo_active_traffic <- nrow(filter(result, analytics_entrances_seo_google >0 ))

  res <- rbind(res, data.frame(date=date,
                               value=seo_active_traffic,
                               metric="Active traffic pages"))

  # total traffic
  seo_visits_google <- sum(result$analytics_entrances_seo_google)

  res <- rbind(res, data.frame(date=date,
                               value=seo_visits_google,
                               metric="Visits Google"))


  seo_visits_orphan_pages_google <- sum(filter(result, is.na(depth)
                                               & analytics_entrances_seo_google>0 )$analytics_entrances_seo_google)

  res <- rbind(res, data.frame(date=date,
                               value=seo_visits_orphan_pages_google,
                               metric="Visits orphan pages Google"))


}

res$value <- round(res$value)
oncrawlCreateDashboard(res, "metricSEO.png", 500, ".")

```

### 8. Get aggregations

```
# Pages in structure
jsonTxt = '{"aggs": [{"oql": {"field": ["fetched", "equals", "true"]}}]}'
agg <- listPagesAggs(crawlId, jsonTxt)
page_crawled = agg[[1]]$rows

# Compliant pages
jsonTxt = '{"aggs": [{"oql": {
  "and": [
    {"field": ["meta_robots_index", "equals", "true"]},
    {"field": ["status_code_range", "equals", "ok"]},
    {"field": ["canonical_evaluation", "not_equals", "not_matching"]},
    {"field": ["parsed_html", "equals", "true"]}
    ]
}}]}'
agg <- listPagesAggs(crawlId, jsonTxt)
compliant = agg[[1]]$rows

# Crawled Pages by Google Ratio
jsonTxt = '{"aggs": [{"oql": {"field": ["crawled_by_googlebot", "equals", "true"]}}]}'
agg <- listPagesAggs(crawlId, jsonTxt)
page_crawed_google = setValue(agg[[1]]$rows)
crawl_ratio = page_crawed_google / page_crawled

# Hits by Google
jsonTxt = '{"aggs": [{"oql": {"field": ["crawled_by_googlebot", "equals", "true"]}, "value": "googlebot_hits:sum"}]}'
agg <- listPagesAggs(crawlId, jsonTxt)
hits <- agg[[1]]$rows

```

## Feedbacks
Questions and feedbacks welcome!

You want to contribute ? Open a pull request ;-) If you encounter a bug or want to suggest an enhancement, please open an issue.
