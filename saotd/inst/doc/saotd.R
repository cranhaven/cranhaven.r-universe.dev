## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, eval = TRUE)
                      # eval = requireNamespace("rtweet", quietly = TRUE))

options(rmarkdown.html_vignette.check_title = FALSE)

## ----workflow, echo=FALSE, out.width="100%"-----------------------------------
knitr::include_graphics(path = "saotd_workflow.png")

## ----packages, warning=FALSE, message=FALSE-----------------------------------
library(saotd)
library(dplyr)
library(stringr)
library(knitr)

## ----tweet_acquire, echo=TRUE, eval=FALSE, cache=TRUE, cache.path='saotd_cache/'----
#  consumer_api_key <- "XXXXXXXXXXXXXXXXXXXXXXXXX"
#  consumer_api_secret_key <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
#  access_token <- "XXXXXXXXXXXXXXXXXX-XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
#  access_token_secret <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
#  
#  hashtags <- c("#job", "#Friday", "#fail", "#icecream", "#random", "#kitten", "#airline")
#  
#  tweets <- tweet_acquire(
#    twitter_app = "twitter_app",
#    consumer_api_key = Sys.getenv('consumer_api_key'),
#    consumer_api_secret_key = Sys.getenv('consumer_api_secret_key'),
#    access_token = Sys.getenv('access_token'),
#    access_token_secret = Sys.getenv('access_token_secret'),
#    query = "#icecream",
#    num_tweets = 100,
#    distinct = TRUE)

## ----raw_tweets, cache=TRUE, cache.path='saotd_cache/'------------------------
set.seed(4321)

data("raw_tweets")
TD <- raw_tweets %>% 
  dplyr::sample_n(size = 5000, 
                  replace = TRUE)

## ----tidy, cache=TRUE, cache.path='saotd_cache/'------------------------------
TD_Tidy <- 
  saotd::tweet_tidy(
    DataFrame = TD)

TD_Tidy$Token[1:9] %>% 
  knitr::kable("html")

## ----unigram, cache=TRUE, cache.path='saotd_cache/'---------------------------
saotd::unigram(DataFrame = TD) %>% 
  dplyr::top_n(10) %>% 
  knitr::kable("html", caption = "Twitter data Uni-Grams")

## ----bigram, message=FALSE, cache=TRUE, cache.path='saotd_cache/'-------------
saotd::bigram(DataFrame = TD) %>% 
  dplyr::top_n(10) %>% 
  knitr::kable("html", caption = "Twitter data Bi-Grams")

## ----trigram, message=FALSE, cache=TRUE, cache.path='saotd_cache/'------------
saotd::trigram(DataFrame = TD) %>% 
  dplyr::top_n(10) %>% 
  knitr::kable("html", caption = "Twitter data Tri-Grams")

## ----merge, message=FALSE, message=FALSE, error=FALSE, cache=TRUE, cache.path='saotd_cache/'----
TD_Merge <- 
  merge_terms(
    DataFrame = TD, 
    term = "cancelled flight", 
    term_replacement = "cancelled_flight")

## ----merged_unigram, message=FALSE, cache=TRUE, cache.path='saotd_cache/'-----
saotd::unigram(DataFrame = TD_Merge) %>% 
  dplyr::top_n(10) %>% 
  knitr::kable("html", caption = "Twitter data Uni-Grams")

## ----merged_bigram, message=FALSE, cache=TRUE, cache.path='saotd_cache/'------
saotd::bigram(DataFrame = TD_Merge) %>% 
  dplyr::top_n(10) %>% 
  knitr::kable("html", caption = "Twitter data Bi-Grams")

## ----merged_trigram, message=FALSE, cache=TRUE, cache.path='saotd_cache/'-----
saotd::trigram(DataFrame = TD_Merge) %>% 
  dplyr::top_n(10) %>% 
  knitr::kable("html", caption = "Twitter data Tri-Grams")

## ----bigram_network, fig.align='center', cache=TRUE, cache.path='saotd_cache/'----
TD_Bigram <- saotd::bigram(DataFrame = TD_Merge)

saotd::bigram_network(
  BiGramDataFrame = TD_Bigram,
  number = 30,
  layout = "fr",
  edge_color = "blue",
  node_color = "black",
  node_size = 3,
  set_seed = 1234)

## ----corr_network, fig.align='center', cache=TRUE, cache.path='saotd_cache/'----
TD_Corr <- 
  saotd::word_corr(
    DataFrameTidy = TD_Tidy, 
    number = 100, 
    sort = TRUE)

saotd::word_corr_network(
  WordCorr = TD_Corr, 
  Correlation = .1, 
  layout = "fr", 
  edge_color = "blue", 
  node_color = "black", 
  node_size = 1)

## ----scores, cache=TRUE, cache.path='saotd_cache/'----------------------------
TD_Scores <- 
  saotd::tweet_scores(
    DataFrameTidy = TD_Tidy,
    HT_Topic = "hashtag")

## ----posneg_words, fig.align='center', cache=TRUE, cache.path='saotd_cache/'----
saotd::posneg_words(
  DataFrameTidy = TD_Tidy, 
  num_words = 10)

## ----filtered_posneg_words, fig.align='center', cache=TRUE, cache.path='saotd_cache/'----
saotd::posneg_words(
  DataFrameTidy = TD_Tidy, 
  num_words = 10, 
  filterword = "fail")

## ----max_scores, cache=TRUE, cache.path='saotd_cache/'------------------------
saotd::tweet_max_scores(
  DataFrameTidyScores = TD_Scores,
  HT_Topic = "hashtag")

## ----min_scores, cache=TRUE, cache.path='saotd_cache/'------------------------
saotd::tweet_min_scores(
  DataFrameTidyScores = TD_Scores,
  HT_Topic = "hashtag")

## ----filtered_max_scores, cache=TRUE, cache.path='saotd_cache/'---------------
saotd::tweet_max_scores(
  DataFrameTidyScores = TD_Scores, 
  HT_Topic = "hashtag", 
  HT_Topic_Selection = "United")

## ----number_topics_plot, eval=FALSE, fig.align='center', warning=FALSE, message=FALSE, results="hide", cache=TRUE, cache.path='saotd_cache/'----
#  saotd::number_topics(
#    DataFrame = TD,
#    num_cores = 4L,
#    min_clusters = 2,
#    max_clusters = 12,
#    skip = 1,
#    set_seed = 1234)

## ----lda_tuning_plot, echo=FALSE, out.width="100%"----------------------------
knitr::include_graphics(path = "lda_topics.png")

## ----topics, fig.align='center', warning=FALSE, message=FALSE, results='hide', cache=TRUE, cache.path='saotd_cache/'----
TD_Topics <- 
  saotd::tweet_topics(
    DataFrame = TD, 
    clusters = 5, 
    method = "Gibbs", 
    set_seed = 1234, 
    num_terms = 10)

## ----rename_topics, cache=TRUE, cache.path='saotd_cache/'---------------------
TD_Topics <- TD_Topics %>% 
  dplyr::mutate(Topic = stringr::str_replace_all(Topic, "^1$", "luggage")) %>% 
  dplyr::mutate(Topic = stringr::str_replace_all(Topic, "^2$", "gate_delay")) %>% 
  dplyr::mutate(Topic = stringr::str_replace_all(Topic, "^3$", "customer_service")) %>% 
  dplyr::mutate(Topic = stringr::str_replace_all(Topic, "^4$", "enjoy")) %>% 
  dplyr::mutate(Topic = stringr::str_replace_all(Topic, "^5$", "other_delay"))

## ----topic_tidy, cache=TRUE, cache.path='saotd_cache/'------------------------
TD_Topics_Tidy <- 
  saotd::tweet_tidy(
    DataFrame = TD_Topics)

TD_Topics_Scores <- 
  saotd::tweet_scores(
    DataFrameTidy = TD_Topics_Tidy,
    HT_Topic = "topic")

## ----topic_max_scores, cache=TRUE, cache.path='saotd_cache/'------------------
saotd::tweet_max_scores(
  DataFrameTidyScores = TD_Topics_Scores,
  HT_Topic = "topic")

## ----topic_min_scores, cache=TRUE, cache.path='saotd_cache/'------------------
saotd::tweet_min_scores(
  DataFrameTidyScores = TD_Topics_Scores,
  HT_Topic = "topic")

## ----topic_filtered_max_scores, cache=TRUE, cache.path='saotd_cache/'---------
saotd::tweet_max_scores(
  DataFrameTidyScores = TD_Topics_Scores,
                        HT_Topic = "topic",
                        HT_Topic_Selection = "luggage")

## ----corpus_distribution, fig.align='center', cache=TRUE, cache.path='saotd_cache/'----
saotd::tweet_corpus_distribution(
  DataFrameTidyScores = TD_Scores, 
  color = "black", 
  fill = "white")

## ----tweet_distribution, fig.align='center', cache=TRUE, cache.path='saotd_cache/'----
saotd::tweet_distribution(
  DataFrameTidyScores = TD_Scores, 
  HT_Topic = "hashtag", 
  bin_width = 1, 
  color = "black", 
  fill = "white")

## ----box_plot, fig.align='center', cache=TRUE, cache.path='saotd_cache/'------
saotd::tweet_box(
  DataFrameTidyScores = TD_Scores, 
  HT_Topic = "hashtag")

## ----violin_plot, fig.align='center', cache=TRUE, cache.path='saotd_cache/'----
saotd::tweet_violin(
  DataFrameTidyScores = TD_Scores,
  HT_Topic = "hashtag")

## ----time_plot, fig.align='center', warning=FALSE, message=FALSE, cache=TRUE, cache.path='saotd_cache/'----
saotd::tweet_time(
  DataFrameTidyScores = TD_Scores,
  HT_Topic = "hashtag")

## ----topic_corpus_distribution, fig.align='center', cache=TRUE, cache.path='saotd_cache/'----
saotd::tweet_corpus_distribution(
  DataFrameTidyScores = TD_Topics_Scores, 
  color = "black", 
  fill = "white")

## ----topic_tweet_distribution, fig.align='center', cache=TRUE, cache.path='saotd_cache/'----
saotd::tweet_distribution(
  DataFrameTidyScores = TD_Topics_Scores, 
  HT_Topic = "topic", 
  bin_width = 1, 
  color = "black", 
  fill = "white")

## ----topic_box_plot, fig.align='center', cache=TRUE, cache.path='saotd_cache/'----
saotd::tweet_box(
  DataFrameTidyScores = TD_Topics_Scores,
  HT_Topic = "topic")

## ----topic_violin_plot, fig.align='center', cache=TRUE, cache.path='saotd_cache/'----
saotd::tweet_violin(
  DataFrameTidyScores = TD_Topics_Scores,
  HT_Topic = "topic")

## ----topic_time, fig.align='center', warning=FALSE, message=FALSE, cache=TRUE, cache.path='saotd_cache/'----
saotd::tweet_time(
  DataFrameTidyScores = TD_Topics_Scores,
  HT_Topic = "topic")

