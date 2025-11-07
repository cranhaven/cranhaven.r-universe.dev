#' Plot perplexity score of various LDA models
#'
#' Plot perplexity score of various LDA models.
#'
#' Plot perplexity score of various LDA models. `plot_perplexity()` fits
#' different LDA models for `k` topics in the range
#' between `start` and `end`. For each
#' LDA model, the perplexity score is plotted against the corresponding value of
#' `k`.
#' Plotting the perplexity score of various LDA models
#' can help in identifying the optimal number of topics to fit an LDA model for.
#' `plot_perplexity()` is based on `LDA()` from the package
#' \pkg{topicmodels}.
#'
#' @param df Data frame containing abstracts and PubMed-IDs.
#' @param start Integer. Minimum amount of `k` topics for the LDA model to fit. Must
#' be >=2.
#' @param end Integer. Maximum amount of `k` topics for the LDA model to fit.
#' @param stopwords Data frame containing stop words.
#' @param method String. Either `"gibbs"` or `"VEM"`.
#' @param control Control parameters for LDA modeling. For more information,
#' see the documentation of the `LDAcontrol` class in the \pkg{topicmodels}
#' package.
#' @param col.abstract Column containing abstracts.
#' @param col.pmid Column containing PubMed-ID.
#' @param title String. Plot title.
#'
#' @return Elbow plot displaying perplexity scores of different LDA models.
#'
#' @seealso [fit_lda()]
#'
#' @family LDA functions
#'
#' @export
plot_perplexity <- function(df,
                            start = 2,
                            end = 5,
                            stopwords = stopwords_miretrieve,
                            method = "gibbs",
                            control = NULL,
                            col.abstract = Abstract,
                            col.pmid = PMID,
                            title = NULL) {

  if(is.null(title)) {
    title <- "Perplexity plot"
  }

  if(!is.double(start) | !is.double(end)) {
    stop("'start' and 'end' must be a double")
  }

  if(start < 2) {
    stop("'start must be a double of at least 2'")
  }

  method <- stringr::str_to_lower(method)

  if(is.null(control)) {
    if(method == "gibbs") {
      control <- list(alpha = 0.5, seed=1234, iter = 500, thin = 1)
    } else if(method == "vem") {
      control <- list(alpha = 0.5, seed=1234)
    }
  }



  dtm <- make_dtm(df_count = df,
                  stopwords = stopwords,
                  col.abstract = {{col.abstract}},
                  col.pmid = {{col.pmid}}) # helper function miRetrieve

  perplexity_catcher <- c()

  for (i in seq(start, end, 1)) {
    model <- topicmodels::LDA(x = dtm,
                              k = i,
                              method = method,
                              control = control)

    perplexity_score <- topicmodels::perplexity(object = model,
                                                newdata = dtm)

    perplexity_catcher <- c(perplexity_catcher, perplexity_score)
  }

  df_perplexity <- data.frame("k" = seq(start, end, 1), "Perplexity" = perplexity_catcher) %>%
    dplyr::as_tibble()

  plot <- ggplot(df_perplexity, aes(x = factor(k), y = Perplexity)) +
    geom_point(color = "#188CDF") +
    geom_line(aes(group=1), color = "#188CDF") +
    theme_minimal() +
    xlab("k") +
    ggtitle(title)

  return(plot)
}

#' Fit LDA-model
#'
#' Fit LDA-model with `k` topics.
#'
#' Fit LDA-model with `k` topics from a data frame.
#' `fit_lda()` is based on `LDA()` from the package
#' \pkg{topicmodels}.
#'
#' @param df Data frame containing abstracts and PubMed-IDs.
#' @param k Integer. Number of topics to fit. Must be >=2.
#' @param stopwords Data frame containing stop words.
#' @param method String. Either `"gibbs"` or `"VEM"`.
#' @param control Control parameters for LDA modeling. For more information,
#' see the documentation of the `LDAcontrol` class in the \pkg{topicmodels}
#' package.
#' @param seed Integer. Seed for reproducibility.
#' @param col.abstract Column containing abstracts.
#' @param col.pmid Column containing PubMed-ID.
#'
#' @return LDA-model.
#'
#' @seealso [plot_perplexity()]
#'
#' @family LDA functions
#'
#' @importFrom magrittr %>%
#'
#' @export
fit_lda <- function(df,
                    k,
                    stopwords = stopwords_miretrieve,
                    method = "gibbs",
                    control = NULL,
                    seed = 42,
                    col.abstract = Abstract,
                    col.pmid = PMID) {

  method <- stringr::str_to_lower(method)

  if(is.null(control)) {
    if(method == "gibbs") {
      control <- list(alpha = 0.5, seed=seed, iter = 500, thin = 1)
    } else if(method == "vem") {
      control <- list(alpha = 0.5, seed=seed)
    }
  }

  lda_model <- df %>%
    make_dtm(stopwords = stopwords,
             col.abstract = {{col.abstract}},
             col.pmid = {{col.pmid}}) %>%
    topicmodels::LDA(k = k,
                     method = method,
                     control = control)

  return(lda_model)
}

#' Plot terms associated with LDA-fitted topics
#'
#' Plot terms associated with LDA-fitted topics.
#'
#' Plot terms associated with LDA-fitted topics. For each topic in the LDA-model,
#' the top terms are plotted. Plotting `top.terms` for each topic can help
#' identifying its subject.
#'
#' @param lda_model LDA-model.
#' @param top.terms Integer. Top terms to plot per topic.
#' @param title String. Plot title.
#'
#' @return Bar plot with top terms per topic.
#'
#' @family LDA functions
#'
#' @importFrom magrittr %>%
#' @importFrom stats reorder
#'
#' @export
plot_lda_term <- function(lda_model,
                          top.terms = 10,
                          title = NULL) {

  if(is.null(title)) {
    title <- "Terms per topic"
  }

  # gets probabilities for words
  tidy_model_beta <- tidytext::tidy(lda_model, matrix = "beta")

  # gets top words per topic
  top_words <- tidy_model_beta %>%
    dplyr::group_by(topic) %>%
    dplyr::top_n(top.terms, beta) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(topic, -beta)

  # plots top words per topic
  df_ <- top_words %>%
    dplyr::mutate(term = reorder(term, beta))

  plot <- ggplot(df_, aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    theme_classic() +
    facet_wrap(~ topic, scales = "free") +
    coord_flip() +
    xlab("Term") +
    ylab("Topic probability ('beta')") +
    ggtitle(title)

  plot <- pretty_breaks_miretrieve(plot, df_$beta)

  return(plot)
}

#' Assign topics based on LDA model
#'
#' Assign topics to abstracts based on an LDA model.
#'
#' Assign topic to abstracts based on an LDA model.
#' To identify the subject of a topic, use `plot_lda_term()`.
#'
#' @param df Data frame to assign topics to. Should be the same data frame
#' that the LDA model was fitted on.
#' @param lda_model LDA-model.
#' @param topic.names Character vector. Vector containing names of the
#' new topics. Must have the same length as the number of topics `lda_model`
#' was fitted on.
#' @param col.pmid Symbol. Column containing PubMed-IDs.
#'
#' @return Data frame with topics assigned to each abstract based on an
#' LDAmodel.
#'
#' @seealso [fit_lda()], [plot_lda_term()], [assign_topic()]
#'
#' @family LDA functions
#'
#' @importFrom magrittr %>%
#'
#' @export
assign_topic_lda <- function(df,
                             lda_model,
                             topic.names,
                             col.pmid = PMID) {
  # number of topics
  k <- lda_model@k

  if(k != length(topic.names)) {
    stop("Number of topic names provided is unequal to k of the LDA model.
    Please provide as many topic names as k.")
  }

  # gets probabilities per topic
  tidy_model_gamma <- tidytext::tidy(lda_model, matrix = "gamma")

  #return(tidy_model_gamma)

  # assigns topic to PMIDs
  topic_gamma <- tidy_model_gamma %>%
    dplyr::group_by(document) %>%
    dplyr::mutate(max_score = max(gamma)) %>%
    dplyr::mutate(Topic = ifelse(gamma == max_score,
                                 as.character(topic),
                                 "no topic")) %>%
    dplyr::filter(Topic != "no topic") %>%
    dplyr::select(document, Topic) %>%
    dplyr::distinct() %>%
    dplyr::rename(PMID = document) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(PMID = as.integer(PMID))

  # assigns new names to topics
  for (i in seq_along(topic.names)) {
    topic_gamma <- topic_gamma %>%
      dplyr::mutate(Topic = ifelse(Topic == i,
                                   topic.names[i],
                                   Topic))
  }

  # Joins original df with topic_gamma df
  df <- df %>%
    dplyr::rename("PMID" = {{col.pmid}}) %>%
    dplyr::left_join(topic_gamma, by = "PMID")

  return(df)
}

